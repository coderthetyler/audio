const std = @import("std");
const c = @import("alsa.zig");
const Atomic = std.atomic.Atomic;

const DEBUG = true;

// resources:
// - "C++ in the Audio Industry", Timur Doumler, CppCon 2015
// - "Thread Synchronization In Real-Time Audio Processing with RCU (Read-Copy-Update)", Timur Doumler
// - "C++ atomics, from basic to advanced", Fedor Pikus, CppCon 2017
// - "Read, Copy, Update, then what? RCU for non-kernel programmers", Fedor Pikus, CppCon 2017
// - "Real time 101", David Rowland & Fabian Renn Giles, Meeting C++ 2019
// - "Atomic Weapons: The C++ Memory Model and Modern Hardware", Herb Sutter, C++ and Beyond 2012

// rules:
// 1. AT enqueues requests & makes them atomically available to MT
// 2. MT receives requests & mutates state accordingly; responds to AT with state important state changes
// 3. AT uses MT responses to update source states, free resources, etc

pub fn main() !void {
    // TODO publish responses from MT atomically (to avoid reading torn source states)
    // TODO record timing stats for any MT frame that processes requests (should take longer)
    // TODO experiment with array of sources, to preserve stable indices; perhaps that could outperform linear for each source/sound request?
    // TODO per-source gain control
    // TODO gradual global gain control
    // TODO macos backend
    // TODO DSP effects on sources
    // TODO audio engine state machine
    // TODO benchmark fifos
    // TODO less awkward Sound.render() API
    // TODO set ALSA backend thread priority
    // TODO pin event queue pages
    // TODO budget memory allocations for sound; actually have a counter for all audio memory usage
    // TODO enumerate & select playback device

    debug("@sizeOf(MixerRequest) = {d}\n", .{@sizeOf(MixerRequest)});
    debug("@sizeOf(MixerResponse) = {d}\n", .{@sizeOf(MixerResponse)});
    debug("@sizeOf(Middleware.requestQueue) = {d}\n", .{@sizeOf(@TypeOf(Middleware.requestQueue))});
    debug("@sizeOf(Middleware.responseQueue) = {d}\n", .{@sizeOf(@TypeOf(Middleware.responseQueue))});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    const Backend = AlsaBackend;

    // Middleware must init before mixer thread starts to avoid data race
    Middleware.init(alloc);
    try Backend.init();
    defer {
        Backend.shutdown();
        Middleware.shutdown();
    }

    var buffer: [1024]u8 = undefined;
    const stdin = std.io.getStdIn();
    var stdinReader = stdin.reader();
    std.debug.print("> ", .{});
    while (try stdinReader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        if (line.len == 0) {
            // do nothing
        } else if (std.mem.eql(u8, line, "quit")) {
            break;
        } else if (line.len >= 4 + 1 and std.mem.eql(u8, line[0..4], "sine")) {
            if (parseInt(line[4 + 1 ..])) |freq| {
                _ = Middleware.requestSine(@intCast(freq));
            }
        } else if (line.len >= 4 + 1 and std.mem.eql(u8, line[0..4], "loop")) {
            if (parseInt(line[4 + 1 ..])) |id| {
                _ = Middleware.requestPlaySound(SourceParams{
                    .soundId = id,
                    .startTime = 0,
                    .loop = true,
                    .paused = false,
                });
            }
        } else if (line.len >= 3 + 1 and std.mem.eql(u8, line[0..3], "wav")) {
            const soundPath = line[3 + 1 ..];
            _ = Middleware.requestWav(soundPath) catch |err| switch (err) {
                error.BadSoundFile => {
                    warn("Bad sound file: {s}\n", .{soundPath});
                },
            };
        } else if (line.len >= 6 + 1 and std.mem.eql(u8, line[0..6], "unload")) {
            if (parseInt(line[6 + 1 ..])) |id| {
                Middleware.requestUnregisterSound(id);
            }
        } else if (line.len >= 4 + 1 and std.mem.eql(u8, line[0..4], "play")) {
            if (parseInt(line[4 + 1 ..])) |id| {
                _ = Middleware.requestPlaySound(SourceParams{
                    .soundId = id,
                    .startTime = 0,
                    .loop = false,
                    .paused = false,
                });
            }
        } else if (line.len >= 3 + 1 and std.mem.eql(u8, line[0..3], "end")) {
            if (parseInt(line[3 + 1 ..])) |id| {
                Middleware.requestRemoveSource(id);
            }
        } else if (line.len >= 5 + 1 and std.mem.eql(u8, line[0..5], "pause")) {
            if (parseInt(line[5 + 1 ..])) |id| {
                Middleware.requestPauseSource(id);
            }
        } else if (line.len >= 7 + 1 and std.mem.eql(u8, line[0..7], "unpause")) {
            if (parseInt(line[7 + 1 ..])) |id| {
                Middleware.requestResumeSource(id);
            }
        } else if (line.len >= 4 + 1 and std.mem.eql(u8, line[0..4], "gain")) {
            if (parseFloat(line[4 + 1 ..])) |level| {
                Middleware.globalGain.set(level);
            }
        } else {
            std.debug.print("Unknown cmd: {s}\n", .{line});
        }
        Middleware.publishRequests();
        Middleware.processEvents();
        std.debug.print("> ", .{});
    }
    std.debug.print("Goodbye\n", .{});
}

// SOURCES
const SourceParams = struct {
    id: u64 = 0,
    soundId: u64,
    startTime: usize,
    loop: bool,
    paused: bool,
};
const Source = struct {
    var idGen = Atomic(u64).init(0);

    id: u64,
    sound: *Sound,
    time: usize,
    loop: bool,
    paused: bool,
    isComplete: bool = false,

    // Called from mixer thread
    fn render(self: *@This(), target: []f32) void {
        if (self.paused) {
            @memset(target, 0.0);
            return;
        }
        const framesWritten = self.sound.read(self.time, target, self.loop);
        if (framesWritten != target.len) {
            self.isComplete = true;
        }
        self.time += framesWritten;
    }
};

// SOUNDS

const SoundType = enum {
    wav,
    sine,
};
const Sound = struct {
    var idGen = Atomic(u64).init(0);

    id: u64,
    data: union(SoundType) {
        wav: Wav,
        sine: Sine,
    },

    fn loadWav(allocator: std.mem.Allocator, path: []const u8) !@This() {
        return .{
            .id = idGen.fetchAdd(1, .Monotonic),
            .data = .{ .wav = try Wav.load(allocator, path) },
        };
    }
    fn sine(rate: u32, channels: u32, hz: u32) @This() {
        return .{
            .id = idGen.fetchAdd(1, .Monotonic),
            .data = .{ .sine = .{ .rate = rate, .channels = channels, .hz = hz } },
        };
    }
    fn read(self: @This(), from: usize, target: []f32, loop: bool) usize {
        return switch (self.data) {
            .wav => |wav| wav.read(from, target, loop),
            .sine => |sin| sin.read(from, target),
        };
    }
    fn free(self: @This()) void {
        switch (self.data) {
            .wav => |wav| wav.free(Middleware.wavAllocator),
            .sine => {},
        }
    }
};
const Sine = struct {
    rate: u32,
    channels: u32,
    hz: u32,

    fn read(self: @This(), from: usize, target: []f32) usize {
        const SPACING_SEC = 1.0 / @as(f64, @floatFromInt(self.rate));
        var elapsed_time = @as(f64, @floatFromInt(from)) * SPACING_SEC;
        const FRAMES_TO_WRITE = target.len / self.channels;
        for (0..FRAMES_TO_WRITE) |i| {
            target[i * 2] = @floatCast(@sin(elapsed_time * 2 * std.math.pi * @as(f64, @floatFromInt(self.hz))));
            elapsed_time += SPACING_SEC;
        }
        return target.len;
    }
};
pub const Wav = struct {
    const Header = packed struct {
        magic: u32, // "RIFF"
        fileSize: u32, // overall file size
        fileType: u32, // "WAVE"
        fmtHeader: u32, // "fmt\0"
        formatLen: u32, // length of format data
        formatType: u16, // type of format (1 = PCM)
        numChannels: u16, // 1 for mono
        sampleRate: u32, // in hertz, e.g. 44100 == 44.1kHz
        byterate: u32, // sampleRate * bitsPerSample * numChannels / 8
        bytesPerSample: u16, // bitsPerSample * numChannels / 8
        bitsPerSample: u16, // bit depth
        dataHeader: u32, // "data"
        dataSize: u32, // size of data section
    };

    samples: []f32,
    channels: u32,
    sampleRateHz: u32,

    pub fn load(allocator: std.mem.Allocator, path: []const u8) !@This() {
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();
        const reader = file.reader();
        const header = try reader.readStruct(Header);
        const data = try reader.readAllAlloc(allocator, header.dataSize);
        errdefer allocator.free(data);
        if (header.bitsPerSample != 16) {
            return error.UnexpectedBitDepth;
        }
        const sampleCount = data.len * @sizeOf(u8) / @sizeOf(i16);

        // TODO support stereo & mono
        // TODO support other bit depths, convert automatically to f32
        // TODO something something little/big endian
        const rawSamples = @as([*]i16, @ptrCast(@alignCast(data.ptr)))[0..@intCast(sampleCount)];
        const samples = try allocator.alloc(f32, sampleCount);
        errdefer allocator.free(samples);
        const max: f32 = @floatFromInt(@max(std.math.maxInt(i16), -std.math.minInt(i16)));
        for (rawSamples, samples) |*in, *out| {
            out.* = @as(f32, @floatFromInt(in.*)) / max;
        }
        allocator.free(data);

        return @This(){
            .samples = samples,
            .sampleRateHz = header.sampleRate,
            .channels = header.numChannels,
        };
    }

    fn read(self: @This(), from: usize, target: []f32, loop: bool) usize {
        if (!loop and from >= self.samples.len) return 0;
        const fromMod = from % self.samples.len;
        const remainingFrames = self.samples.len - fromMod;
        var framesToWrite = @min(target.len, remainingFrames);
        @memcpy(target[0..framesToWrite], self.samples[fromMod .. fromMod + framesToWrite]);
        if (framesToWrite != target.len) {
            if (loop) {
                @memcpy(target[framesToWrite..], self.samples[0 .. target.len - framesToWrite]);
                framesToWrite = target.len;
            } else {
                @memset(target[framesToWrite..], 0.0);
            }
        }
        // TODO generalize to support arbitrary channel counts & not just stereo
        // TODO must be that sample rate matches playback rate
        return framesToWrite;
    }

    pub inline fn at(self: @This(), index: anytype) i16 {
        return self.buffer[index % self.samples.len];
    }

    pub fn free(self: @This(), allocator: std.mem.Allocator) void {
        allocator.free(self.samples);
    }
};

const MixerRequestType = enum {
    registerSound,
    unregisterSound,
    playSound,
    removeSource,
    pauseSource,
    resumeSource,
};
const MixerRequest = struct {
    var idGen = Atomic(usize).init(0);

    id: usize,
    payload: union(MixerRequestType) {
        registerSound: Sound,
        unregisterSound: u64,
        playSound: SourceParams,
        removeSource: u64,
        pauseSource: u64,
        resumeSource: u64,
    },

    pub fn registerSound(sound: Sound) @This() {
        return create(.{ .registerSound = sound });
    }
    pub fn unregisterSound(id: u64) @This() {
        return create(.{ .unregisterSound = id });
    }
    pub fn playSound(params: SourceParams) @This() {
        return create(.{ .playSound = params });
    }
    pub fn removeSource(id: u64) @This() {
        return create(.{ .removeSource = id });
    }
    pub fn pauseSource(id: u64) @This() {
        return create(.{ .pauseSource = id });
    }
    pub fn resumeSource(id: u64) @This() {
        return create(.{ .resumeSource = id });
    }
    fn create(payload: anytype) @This() {
        const req = .{
            .id = idGen.fetchAdd(1, .Monotonic),
            .payload = payload,
        };
        debug("req {d}- {any}\n", .{ req.id, req.payload });
        return req;
    }
};
const MixerResponseType = enum {
    tooManySounds,
    tooManySources,
    soundRegistered,
    soundUnregistered,
    sourceRemoved,
    sourceAdded,
    sourceCompleted,
};
const MixerResponse = struct {
    id: usize, // response has same ID has request
    payload: union(MixerResponseType) {
        tooManySounds: Sound,
        tooManySources: Source,
        soundRegistered: Sound,
        soundUnregistered: Sound,
        sourceRemoved: Source,
        sourceAdded: Source,
        sourceCompleted: Source,
    },
    pub fn tooManySources(req: MixerRequest, source: Source) @This() {
        return .{ .id = req.id, .payload = .{ .tooManySources = source } };
    }
    pub fn tooManySounds(req: MixerRequest, sound: Sound) @This() {
        return .{ .id = req.id, .payload = .{ .tooManySounds = sound } };
    }
    pub fn soundRegistered(req: MixerRequest, sound: Sound) @This() {
        return .{ .id = req.id, .payload = .{ .soundRegistered = sound } };
    }
    pub fn soundUnregistered(req: MixerRequest, sound: Sound) @This() {
        return .{ .id = req.id, .payload = .{ .soundUnregistered = sound } };
    }
    pub fn sourceRemoved(req: MixerRequest, source: Source) @This() {
        return .{ .id = req.id, .payload = .{ .sourceRemoved = source } };
    }
    pub fn sourceAdded(req: MixerRequest, source: Source) @This() {
        return .{ .id = req.id, .payload = .{ .sourceAdded = source } };
    }
    pub fn sourceCompleted(source: Source) @This() {
        return .{ .id = MixerRequest.idGen.fetchAdd(1, .Monotonic), .payload = .{ .sourceCompleted = source } };
    }
};

// data:
// - swapback array of playing sources
// - swapback array of available sounds
// - global gain control
// - request fifo
// - response fifo

const Middleware = struct {
    const RATE_HZ: usize = 44100;
    const CHANNELS: usize = 2;
    const MAX_SOURCES: usize = 16;
    const MAX_SOUNDS: usize = 4;
    const MAX_EVENTS: usize = 64;

    // Owned by audio thread
    var wavAllocator: std.mem.Allocator = undefined;

    // Owned by mixer thread
    var sources: Swapback(Source, MAX_SOURCES) = undefined;
    var sounds: Swapback(Sound, MAX_SOUNDS) = undefined;

    // Shared by mixer & audio thread
    var globalGain: GainControl = undefined;
    var requestQueue: RequestQueue(MixerRequest, MAX_EVENTS) = undefined;
    var responseQueue: ResponseQueue(MixerResponse, MAX_EVENTS) = undefined;
    var isResponseQueueInOverrun: Atomic(bool) = undefined;

    // Performance statistics (DEBUG ONLY)
    var targetNs = Atomic(i64).init(0);
    var processNs = Atomic(i64).init(0);

    fn init(allocator: std.mem.Allocator) void {
        wavAllocator = allocator;
        sources = Swapback(Source, MAX_SOURCES).init();
        sounds = Swapback(Sound, MAX_SOUNDS).init();
        requestQueue = RequestQueue(MixerRequest, MAX_EVENTS).init();
        responseQueue = ResponseQueue(MixerResponse, MAX_EVENTS).init();
        isResponseQueueInOverrun = Atomic(bool).init(false);
        globalGain = GainControl.init();
    }

    /// Not safe to call this until mixer thread has joined the main thread
    fn shutdown() void {
        processEvents(); // take ownership of any straggling in-flight events
        for (0..sounds.len()) |i| {
            sounds.get(i).free();
        }
    }

    /// Called from audio thread to process events from mixer thread
    fn processEvents() void {
        if (isResponseQueueInOverrun.load(.Monotonic)) {
            warn("processEvents() too slow, possibly leaking memory or corrupting playback state", .{});
            isResponseQueueInOverrun.store(false, .Monotonic);
        }
        debug("targetNs = {d}, processNs = {d}\n", .{ targetNs.load(.SeqCst), processNs.load(.SeqCst) });
        while (responseQueue.removeFirst()) |resp| {
            debug("resp {d}- {any}\n", .{ resp.id, resp.payload });
            switch (resp.payload) {
                .soundRegistered => |sound| {
                    std.debug.print("Sound registered: {d}\n", .{sound.id});
                },
                .soundUnregistered => |sound| {
                    sound.free();
                    std.debug.print("Sound freed: {d}\n", .{sound.id});
                },
                .sourceRemoved => |source| {
                    std.debug.print("Source removed: {d}\n", .{source.id});
                },
                .sourceAdded => |source| {
                    std.debug.print("Source added: {d}\n", .{source.id});
                },
                .sourceCompleted => |source| {
                    std.debug.print("Source completed: {d}\n", .{source.id});
                },
                .tooManySounds => |sound| {
                    std.debug.print("Too many sounds, unloading {d}\n", .{sound.id});
                    sound.free();
                },
                .tooManySources => |source| {
                    std.debug.print("Too many sources, not playing {d}\n", .{source.id});
                },
            }
        }
    }
    fn requestWav(path: []const u8) !Sound {
        const s = Sound.loadWav(wavAllocator, path) catch return error.BadSoundFile;
        debug("WAV loaded: {d} = {s}\n", .{ s.id, path });
        sendRequest(MixerRequest.registerSound(s));
        return s;
    }
    fn requestSine(hz: u32) Sound {
        const s = Sound.sine(44100, 1, hz);
        sendRequest(MixerRequest.registerSound(s));
        return s;
    }
    fn requestUnregisterSound(id: u64) void {
        sendRequest(MixerRequest.unregisterSound(id));
    }
    fn requestPlaySound(params: SourceParams) u64 {
        var modifiedParams = params;
        modifiedParams.id = Source.idGen.fetchAdd(1, .Monotonic);
        sendRequest(MixerRequest.playSound(modifiedParams));
        return modifiedParams.id;
    }
    fn requestRemoveSource(id: u64) void {
        sendRequest(MixerRequest.removeSource(id));
    }
    fn requestPauseSource(id: u64) void {
        sendRequest(MixerRequest.pauseSource(id));
    }
    fn requestResumeSource(id: u64) void {
        sendRequest(MixerRequest.resumeSource(id));
    }
    inline fn sendRequest(req: MixerRequest) void {
        requestQueue.addLast(req) catch |err| switch (err) {
            error.Overrun => {
                warn("Request queue overrun! Discarding request.\n", .{});
            },
        };
    }
    inline fn publishRequests() void {
        requestQueue.publishToReader();
    }

    // Called from the mixer thread to process requests from the audio thread
    inline fn mixerPlaySound(request: MixerRequest, params: SourceParams) void {
        if (mixerFindSound(params.soundId)) |i| {
            const sound = &sounds.data[i];
            const source = Source{
                .id = params.id,
                .sound = sound,
                .time = params.startTime,
                .loop = params.loop,
                .paused = params.paused,
            };
            sources.add(source) catch |err| switch (err) {
                error.Overrun => {
                    mixerSendResponse(MixerResponse.tooManySources(request, source));
                    return;
                },
            };
            mixerSendResponse(MixerResponse.sourceAdded(request, source));
        } else {
            // TODO error: no sound with index
        }
    }
    inline fn mixerRemoveSource(request: MixerRequest, id: u64) void {
        if (mixerFindSource(id)) |i| {
            const removedSource = sources.removeAt(i) catch |err| switch (err) {
                error.InvalidIndex => unreachable,
            };
            mixerSendResponse(MixerResponse.sourceRemoved(request, removedSource));
        } else {
            // TODO error: source does not exist
        }
    }
    inline fn mixerRegisterSound(request: MixerRequest, s: Sound) void {
        sounds.add(s) catch |err| switch (err) {
            error.Overrun => {
                mixerSendResponse(MixerResponse.tooManySounds(request, s));
                return;
            },
        };
        mixerSendResponse(MixerResponse.soundRegistered(request, s));
    }
    inline fn mixerUnregisterSound(request: MixerRequest, id: u64) void {
        if (mixerFindSound(id)) |i| {
            const unregisteredSound = sounds.removeAt(i) catch |err| switch (err) {
                error.InvalidIndex => unreachable,
            };

            // remove all sources using the now-unregistered sound
            var src: usize = 0;
            while (src != sources.len()) {
                if (sources.get(src).sound.id == id) {
                    _ = sources.removeAt(src) catch |err| switch (err) {
                        error.InvalidIndex => unreachable,
                    };
                } else {
                    src += 1;
                }
            }
            mixerSendResponse(MixerResponse.soundUnregistered(request, unregisteredSound));
        } else {
            // TODO error: sound does not exist
        }
    }
    inline fn mixerResumeSource(req: MixerRequest, id: u64) void {
        _ = req;
        if (mixerFindSource(id)) |i| {
            sources.data[i].paused = false;
        } else {
            // TODO error: no such source
        }
    }
    inline fn mixerPauseSource(req: MixerRequest, id: u64) void {
        _ = req;
        if (mixerFindSource(id)) |i| {
            sources.data[i].paused = true;
        } else {
            // TODO error: no such source
        }
    }
    inline fn mixerSendResponse(resp: MixerResponse) void {
        responseQueue.addLast(resp) catch |err| switch (err) {
            error.Overrun => {
                isResponseQueueInOverrun.store(true, .Monotonic);
            },
        };
    }
    inline fn mixerFindSound(id: u64) ?usize {
        for (0..sounds.len()) |i| {
            if (sounds.get(i).id == id) return i;
        }
        return null;
    }
    inline fn mixerFindSource(id: u64) ?usize {
        for (0..sources.len()) |i| {
            if (sources.get(i).id == id) return i;
        }
        return null;
    }

    fn render(target: []f32) void {
        var startTimestamp: i128 = undefined;
        if (DEBUG) startTimestamp = std.time.nanoTimestamp();

        // process requests from other threads
        while (requestQueue.removeFirst()) |req| {
            switch (req.payload) {
                .playSound => |params| mixerPlaySound(req, params),
                .removeSource => |id| mixerRemoveSource(req, id),
                .registerSound => |s| mixerRegisterSound(req, s),
                .unregisterSound => |s| mixerUnregisterSound(req, s),
                .pauseSource => |id| mixerPauseSource(req, id),
                .resumeSource => |id| mixerResumeSource(req, id),
            }
        }
        // remove completed sources
        var i: usize = 0;
        while (i != sources.len()) {
            if (sources.get(i).isComplete) {
                const source = sources.removeAt(i) catch |err| switch (err) {
                    error.InvalidIndex => unreachable,
                };
                mixerSendResponse(MixerResponse.sourceCompleted(source));
            } else {
                i += 1;
            }
        }
        // mix sources into output buffer
        for (0..target.len / CHANNELS) |t| {
            var mix: f32 = 0;
            var value = [_]f32{0.0};
            for (0..sources.len()) |s| {
                sources.data[s].render(&value);
                mix += value[0];
            }
            mix *= globalGain.next();
            if (mix > 1.0) mix = 1.0;
            if (mix < -1.0) mix = -1.0;
            target[t * 2 + 0] = mix;
            target[t * 2 + 1] = mix;
        }

        if (DEBUG) {
            processNs.store(@intCast(std.time.nanoTimestamp() - startTimestamp), .SeqCst);
            targetNs.store(@intCast(target.len * 1_000_000_000 / RATE_HZ), .SeqCst);
        }
    }
};

// BACKENDS

const AlsaBackend = struct {
    // sound card sends interrupt to CPU after each period
    // typically, the period size is half the buffer size, i.e. double buffering
    // sound cards have their own constraints on valid values for the period size & buffer size
    // latency is determined by the buffer size & sample rate: samples / samples/sec = sec

    const RATE_HZ: c_uint = 44100; // TODO unify with Middleware's RATE_HZ
    const CHANNELS: c_uint = 2; // TODO unify with MIDDLEWARE's CHANNELS
    const BUF_FRAMES: c.snd_pcm_uframes_t = 4096; // TODO experiment with larger & smaller buffers + make sure period sizes are at most BUF_FRAMES
    const MAX_SOUNDS: usize = 4;
    const MAX_VOICES: usize = 4;

    var mixerThread: std.Thread = undefined;
    var shutdownRequested = Atomic(bool).init(false);

    var player: *c.snd_pcm_t = undefined;
    var avail: c.snd_pcm_sframes_t = undefined;
    var buffer = [_]f32{0.0} ** (BUF_FRAMES * CHANNELS);

    fn init() !void {
        var hw: *c.snd_pcm_hw_params_t = undefined;
        var sw: *c.snd_pcm_sw_params_t = undefined;

        var rate: c_uint = RATE_HZ;
        var dir: c_int = 0;
        var err: c_int = 0;

        // TODO handle error codes; fallback to a NullBackend if initialization fails
        _ = c.snd_pcm_open(&player, "default", .playback, .block);

        _ = c.snd_pcm_hw_params_malloc(&hw);
        _ = c.snd_pcm_hw_params_any(player, hw);
        _ = c.snd_pcm_hw_params_set_access(player, hw, .rw_interleaved);
        err = c.snd_pcm_hw_params_set_format(player, hw, .float_le);
        if (err != 0) {
            warn("unsupported sample format: {s}\n", .{c.snd_strerror(err)});
        }
        // _ = c.snd_pcm_hw_params_set_rate_resample(player, hw, 0); // TODO disable software resampling; implement my own? or use only sounds with the target rate.
        _ = c.snd_pcm_hw_params_set_rate_near(player, hw, &rate, &dir);
        if (rate != RATE_HZ) {
            warn("Rate {d} not supported, use {d} instead\n", .{ RATE_HZ, rate });
        }
        _ = c.snd_pcm_hw_params_set_channels(player, hw, CHANNELS);
        _ = c.snd_pcm_hw_params(player, hw);
        _ = c.snd_pcm_hw_params_free(hw);

        _ = c.snd_pcm_sw_params_malloc(&sw);
        _ = c.snd_pcm_sw_params_current(player, sw);
        _ = c.snd_pcm_sw_params_set_avail_min(player, sw, BUF_FRAMES);
        _ = c.snd_pcm_sw_params_set_start_threshold(player, sw, 0);
        _ = c.snd_pcm_sw_params(player, sw);
        _ = c.snd_pcm_sw_params_free(sw);

        _ = c.snd_pcm_prepare(player);

        mixerThread = try std.Thread.spawn(.{}, mixer_thread_worker, .{});
        try mixerThread.setName("Mixer");
    }

    fn shutdown() void {
        shutdownRequested.store(true, .Monotonic);
        mixerThread.join();
        _ = c.snd_pcm_close(player);
    }

    fn mixer_thread_worker() void {
        var err: c_int = undefined;
        while (!shutdownRequested.load(.Monotonic)) {
            err = c.snd_pcm_wait(player, 1000);
            if (err < 0) {
                err = c.snd_pcm_recover(player, err, 0);
                if (err < 0) {
                    warn("Failed to recover from failed wait: {s}\n", .{c.snd_strerror(err)});
                    break;
                } else {
                    warn("Recovered from wait failure\n", .{});
                }
            }
            avail = c.snd_pcm_avail_update(player);
            if (avail < 0) {
                warn("avail_update: {s}\n", .{c.snd_strerror(@intCast(avail))});
                break;
            }
            avail = @min(avail, BUF_FRAMES);
            Middleware.render(buffer[0..@intCast(avail * CHANNELS)]);
            err = @intCast(c.snd_pcm_writei(player, @ptrCast(buffer[0..].ptr), @intCast(avail)));
            if (err < 0) {
                err = c.snd_pcm_recover(player, @intCast(err), 0);
                if (err < 0) {
                    warn("failed to recover from writei: {s}\n", .{c.snd_strerror(@intCast(err))});
                }
            }
            if (err != avail) {
                warn("render callback failed!\n", .{});
                break;
            }
        }
    }
};

// Gain control with transition ramp to avoid clicks
pub const GainControl = struct {
    level: Atomic(f32),

    pub fn init() @This() {
        return .{
            .level = Atomic(f32).init(1.0),
        };
    }

    pub fn set(self: *@This(), level: f32) void {
        self.level.store(level, .SeqCst);
    }

    pub inline fn next(self: *@This()) f32 {
        return self.level.load(.SeqCst);
    }
};

// Swapback array is used to manage sounds & sources
pub fn Swapback(comptime T: anytype, comptime N: comptime_int) type {
    return struct {
        count: usize,
        data: [N]T,

        pub inline fn init() @This() {
            return .{ .count = 0, .data = undefined };
        }

        pub inline fn get(self: *@This(), i: usize) T {
            return self.data[i];
        }

        pub inline fn len(self: *@This()) usize {
            return self.count;
        }

        /// Returns `false` only if the index is invalid.
        pub fn removeAt(self: *@This(), i: usize) !T {
            if (i >= self.count) return error.InvalidIndex;
            const obj = self.data[i];
            if (self.count != 1 and i != self.count - 1) {
                self.data[i] = self.data[self.count - 1];
            }
            self.count -= 1;
            return obj;
        }

        /// Returns `false` only if the array is full.
        pub fn add(self: *@This(), obj: T) !void {
            if (self.count == N) return error.Overrun;
            self.data[self.count] = obj;
            self.count += 1;
        }
    };
}

// RING BUFFERS

// Single-producer, single-consumer, null on underrun, alert on overrun
// Used to receive responses from the realtime mixer thread, e.g. sound is no longer in use & can be deallocated
pub fn ResponseQueue(comptime T: anytype, comptime N: comptime_int) type {
    return struct {
        read: Atomic(usize),
        write: Atomic(usize),
        data: [N]T = undefined,

        pub fn init() @This() {
            return .{
                .read = Atomic(usize).init(0),
                .write = Atomic(usize).init(0),
            };
        }

        pub fn addLast(self: *@This(), obj: T) !void {
            const r = self.read.load(.SeqCst);
            const w = self.write.load(.SeqCst);
            const nextW = (w + 1) % N;
            if (nextW == r) return error.Overrun;
            self.data[w] = obj;
            self.write.store(nextW, .SeqCst);
        }

        pub fn removeFirst(self: *@This()) ?T {
            const w = self.write.load(.SeqCst);
            const r = self.read.load(.SeqCst);
            if (r == w) return null; // Underrun
            const obj = self.data[r];
            self.read.store((r + 1) % N, .SeqCst);
            return obj;
        }
    };
}

// Multiple-producer, single-consumer, null on underrun, alert on overrun
// Used to send notifications to the realtime mixer thread
pub fn RequestQueue(
    comptime Obj: anytype,
    comptime N: comptime_int,
) type {
    return struct {
        read: Atomic(usize),
        write: usize,
        publish: Atomic(usize),
        data: [N]Obj = undefined,

        pub fn init() @This() {
            return .{
                .read = Atomic(usize).init(0),
                .write = 0,
                .publish = Atomic(usize).init(0),
            };
        }

        // Called from non-realtime audio thread; fine if it blocks
        pub fn addLast(self: *@This(), obj: Obj) !void {
            var r = self.read.load(.SeqCst);
            var nextW = (self.write + 1) % N;
            if (nextW == r) return error.Overrun;
            self.data[self.write] = obj;
            self.write = nextW;
        }
        pub fn publishToReader(self: *@This()) void {
            self.publish.store(self.write, .SeqCst);
        }

        // Called from realtime mixer thread; must not block
        pub fn removeFirst(self: *@This()) ?Obj {
            const p = self.publish.load(.SeqCst);
            const r = self.read.load(.SeqCst);
            if (p == r) {
                return null;
            }
            const obj = self.data[r];
            const nextR = (r + 1) % N;
            self.read.store(nextR, .SeqCst);
            return obj;
        }
    };
}

fn parseFloat(str: []const u8) ?f32 {
    const value = std.fmt.parseFloat(f32, str) catch |err| switch (err) {
        error.InvalidCharacter => {
            std.debug.print("Not a float: '{s}'\n", .{str});
            return null;
        },
    };
    return value;
}
fn parseInt(idStr: []const u8) ?u64 {
    const id = std.fmt.parseInt(u64, idStr, 10) catch |err| switch (err) {
        error.Overflow => {
            std.debug.print("Input too large: '{s}'\n", .{idStr});
            return null;
        },
        error.InvalidCharacter => {
            std.debug.print("Not an integer: '{s}'\n", .{idStr});
            return null;
        },
    };
    return id;
}

inline fn debug(comptime fmt: []const u8, args: anytype) void {
    if (DEBUG) std.debug.print("[DEBUG] " ++ fmt, args);
}
inline fn warn(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("[WARN] " ++ fmt, args);
}
inline fn info(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
}
