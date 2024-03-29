const std = @import("std");
const builtin = @import("builtin");
const Atomic = std.atomic.Atomic;
const math = @import("math.zig");
const assert = std.debug.assert;

const DEBUG = true;

// resources:
// - "A Guide Through The Linux Sound API Jungle", Lennart Poettering
// - "A Tutorial on Using the ALSA Audio API", Paul Davis
// - "C++ in the Audio Industry", Timur Doumler, CppCon 2015
// - "Thread Synchronization In Real-Time Audio Processing with RCU (Read-Copy-Update)", Timur Doumler
// - "C++ atomics, from basic to advanced", Fedor Pikus, CppCon 2017
// - "Read, Copy, Update, then what? RCU for non-kernel programmers", Fedor Pikus, CppCon 2017
// - "Real time 101", David Rowland & Fabian Renn Giles, Meeting C++ 2019
// - "Atomic Weapons: The C++ Memory Model and Modern Hardware", Herb Sutter, C++ and Beyond 2012

// HRTF: for headphones
// Panning: for speakers
// Ambisonics: for speakers, but also used in headphones when cheap scene rotations are required?

pub fn main() !void {
    // TODO how to reset phases when changing pitch?
    // TODO improve performance of FFT impl
    // TODO make pitching optional; see if it reduces playback time for non-object sources
    // TODO vector-base amplitude panning for speakers
    // TODO headphone binauralization via convolution with interpolated HRIR
    // TODO ambisonics for speakers
    // TODO can i get away with a single queue shared by both audio & mixer threads?
    // TODO thread sanitizer?
    // TODO benchmark FIFO & optimize (fix all the false sharing i've ignored until now)
    // TODO flush denormals to zero when doing mixing; need to ensure they are restored in the event of a crash
    // TODO ramped global gain control
    // TODO audio engine state machine
    // TODO less awkward Sound.render() API; bonus points for SIMD
    // TODO set ALSA backend thread priority
    // TODO pin pages being used by mixer?
    // TODO budget memory allocations for sound; actually have a counter for all audio memory usage
    // TODO enumerate & select playback device
    // TODO fixed point mixing?

    debug("@sizeOf(Sound) = {d}\n", .{@sizeOf(Sound)});
    debug("@sizeOf(Source) = {d}\n", .{@sizeOf(Source)});
    debug("@sizeOf(MessageFromAudioThread) = {d}\n", .{@sizeOf(MessageFromAudioThread)});
    debug("@sizeOf(MessageFromMixerThread) = {d}\n", .{@sizeOf(MessageFromMixerThread)});
    debug("@sizeOf(Middleware.mixerThreadInbox) = {d}\n", .{@sizeOf(@TypeOf(Middleware.mixerThreadInbox))});
    debug("@sizeOf(Middleware.audioThreadInbox) = {d}\n", .{@sizeOf(@TypeOf(Middleware.audioThreadInbox))});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    const Backend = switch (builtin.os.tag) {
        .linux => AlsaBackend,
        .macos => MacOsBackend,
        else => @compileError("no supported backend for taret operating system"),
    };

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
                _ = Middleware.requestPlaySound(PlaySoundParams{ .soundId = id, .loop = true });
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
                _ = Middleware.requestPlaySound(PlaySoundParams{ .soundId = id });
            }
        } else if (line.len >= 3 + 1 and std.mem.eql(u8, line[0..3], "end")) {
            if (parseInt(line[3 + 1 ..])) |id| {
                Middleware.requestRemoveSource(id);
            }
        } else if (line.len >= 5 + 1 and std.mem.eql(u8, line[0..5], "pause")) {
            if (parseInt(line[5 + 1 ..])) |id| {
                Middleware.requestUpdateSource(.{ .id = id, .pause = true });
            }
        } else if (line.len >= 7 + 1 and std.mem.eql(u8, line[0..7], "unpause")) {
            if (parseInt(line[7 + 1 ..])) |id| {
                Middleware.requestUpdateSource(.{ .id = id, .pause = false });
            }
        } else if (line.len >= 4 + 1 and std.mem.eql(u8, line[0..4], "gain")) {
            if (parseFloat(line[4 + 1 ..])) |level| {
                Middleware.globalGain.set(level);
            }
        } else if (line.len >= 5 + 1 and std.mem.eql(u8, line[0..5], "sgain")) {
            const hasSpace = blk: {
                for (5 + 1..line.len - 1) |i| {
                    if (line[i] == ' ') break :blk i;
                }
                break :blk null;
            };
            if (hasSpace) |sp| {
                if (parseInt(line[5 + 1 .. sp])) |id| {
                    if (parseFloat(line[sp + 1 ..])) |gain| {
                        Middleware.requestUpdateSource(.{ .id = id, .level = gain });
                    }
                }
            }
        } else if (line.len >= 6 + 1 and std.mem.eql(u8, line[0..6], "spitch")) {
            const hasSpace = blk: {
                for (6 + 1..line.len - 1) |i| {
                    if (line[i] == ' ') break :blk i;
                }
                break :blk null;
            };
            if (hasSpace) |sp| {
                if (parseInt(line[6 + 1 .. sp])) |id| {
                    if (parseFloat(line[sp + 1 ..])) |pitch| {
                        Middleware.requestUpdateSource(.{ .id = id, .pitch = pitch });
                    }
                }
            }
        } else {
            std.debug.print("Unknown cmd: {s}\n", .{line});
        }
        Middleware.processMessages();
        std.debug.print("> ", .{});
    }
    std.debug.print("Goodbye\n", .{});
}

const UpdateSourceParams = struct {
    id: u64,
    loop: ?bool = null,
    pause: ?bool = null,
    level: ?f32 = null,
    pitch: ?f32 = null,
};
const PlaySoundParams = struct {
    id: u64 = 0,
    soundId: u64,
    level: f32 = 1.0,
    startTime: usize = 0,
    loop: bool = false,
    paused: bool = false,
    pitch: f32 = 1,
};
const Source = struct {
    const FFT_SIZE: usize = 2048; // in samples; 256 samples @ 48kHz ~ 5ms latency
    const FFT_STEP: usize = 256; // 256 / 64 = 4-fold oversampling
    const FFT_RATIO: usize = FFT_SIZE / FFT_STEP;
    const WINDOW = w: {
        var window: [FFT_SIZE]f32 = undefined;
        for (0..FFT_SIZE) |i| {
            @setEvalBranchQuota(2048);
            window[i] = 0.5 * (1 - @cos(std.math.tau * @as(f32, @floatFromInt(i)) / @as(f32, @floatFromInt(FFT_SIZE)))); // Hann window
        }
        break :w window;
    };
    const FrequencyBin = struct {
        k: f32,
        mag: f32,
    };

    id: u64,
    // playback state
    time: usize,
    isComplete: bool = false,
    sound: *Sound,
    // pitching state
    outstanding: usize = 0, // always < FFT_STEP
    block: usize = 0, // always in range [0, FFT_SIZE/FFT_STEP)
    // TODO allocate buffers separately from Source
    input: [FFT_SIZE]f32 = [_]f32{0} ** FFT_SIZE,
    fft: [FFT_SIZE]math.Complex = [_]math.Complex{math.Complex.of(0)} ** FFT_SIZE,
    summedPhase: [FFT_SIZE]f32 = [_]f32{0} ** FFT_SIZE,
    priorPhase: [FFT_SIZE]f32 = [_]f32{0} ** FFT_SIZE,
    analysis: [FFT_SIZE]FrequencyBin = undefined,
    synthesis: [FFT_SIZE]FrequencyBin = undefined,
    output: [FFT_SIZE]f32 = [_]f32{0} ** FFT_SIZE,
    // parameters
    level: f32,
    loop: bool,
    paused: bool,
    pitch: f32,

    inline fn wrap(value: f32) f32 {
        const valuei: i32 = @intFromFloat(value);
        return value - @as(f32, @floatFromInt(valuei + @rem(valuei, 2)));
    }

    inline fn processNextInputBlock(self: *@This()) void {
        // pull next block of samples from sound
        const writeHead: usize = self.block * FFT_STEP;
        const framesWritten = self.sound.read(self.time, self.input[writeHead .. writeHead + FFT_STEP], self.loop, self.level);
        if (framesWritten < FFT_STEP) { // unlikely branch
            @memset(self.input[writeHead + framesWritten .. writeHead + FFT_STEP], 0);
            self.isComplete = true; // TODO delay by FFT_SIZE samples, to ensure last FFT_SIZE samples are drained
        }
        self.time += framesWritten;

        // pitch shift
        for (0..FFT_SIZE) |k| {
            const i = (k + writeHead) & (FFT_SIZE - 1);
            self.fft[k] = math.Complex.of(self.input[i] * WINDOW[k]);
        }
        math.fft(&self.fft);
        const FFT_HALF = FFT_SIZE >> 1;
        for (0..FFT_HALF + 1) |k| {
            const mag = self.fft[k].magnitude();
            const phase = self.fft[k].wrapped_phase();
            const priorPhase = self.priorPhase[k];
            self.priorPhase[k] = phase;
            const estBin = blk: {
                var tmp = phase - priorPhase;
                tmp -= @as(f32, @floatFromInt(k)) * std.math.tau / @as(f32, @floatFromInt(FFT_RATIO));
                tmp = wrap(tmp / std.math.pi) * std.math.pi;
                tmp /= std.math.tau;
                tmp *= @floatFromInt(FFT_RATIO);
                tmp += @floatFromInt(k);
                break :blk tmp;
            };
            self.analysis[k] = .{
                .k = estBin,
                .mag = mag,
            };
        }
        @memset(&self.synthesis, .{ .k = 0.0, .mag = 0.0 });
        for (0..FFT_HALF + 1) |k| {
            const j: usize = @intFromFloat(@as(f32, @floatFromInt(k)) * self.pitch);
            if (j <= FFT_HALF) {
                self.synthesis[j].mag += self.analysis[k].mag;
                self.synthesis[j].k = self.analysis[k].k * self.pitch;
            }
        }
        for (0..FFT_HALF + 1) |k| {
            const diffRadians = blk: {
                var tmp = self.synthesis[k].k;
                tmp -= @as(f32, @floatFromInt(k));
                tmp /= @as(f32, @floatFromInt(FFT_RATIO));
                tmp *= std.math.tau;
                tmp += @as(f32, @floatFromInt(k)) / @as(f32, @floatFromInt(FFT_RATIO)) * std.math.tau;
                break :blk tmp;
            };
            self.summedPhase[k] += diffRadians;
            self.summedPhase[k] = wrap(self.summedPhase[k] / std.math.pi) * std.math.pi;
            self.fft[k] = math.Complex.polar(self.synthesis[k].mag, self.summedPhase[k]);
        }
        for (FFT_HALF + 1..FFT_SIZE) |k| {
            self.fft[k] = self.fft[FFT_SIZE - k].conjugate();
        }
        math.ifft(&self.fft);
        for (0..FFT_SIZE) |i| {
            const k = (i + (FFT_SIZE - writeHead)) & (FFT_SIZE - 1);
            self.output[i] += self.fft[k].real * (WINDOW[k] * 8.0 / 3.0) / FFT_RATIO;
        }

        // advance block ptr
        self.block += 1;
        self.block &= (FFT_RATIO - 1);
    }

    // Called from mixer thread
    fn render(self: *@This(), target: []f32) void {
        if (self.paused) {
            @memset(target, 0.0);
            return;
        }

        // read any outstanding portion of the oldest block
        var targeti: usize = 0;
        var readHead: usize = (self.block * FFT_STEP) + (FFT_STEP - self.outstanding);
        if (self.outstanding <= target.len) {
            @memcpy(target[0..self.outstanding], self.output[readHead .. readHead + self.outstanding]);
            @memset(self.output[readHead .. readHead + self.outstanding], 0);
            targeti += self.outstanding;
            self.processNextInputBlock();
        } else {
            // degenerate branch: working with really small callback sizes OR too-large block sizes
            @memcpy(target[0..target.len], self.output[readHead .. readHead + target.len]);
            self.outstanding -= target.len;
            return;
        }

        // read as many full blocks as possible into target
        var remaining = target.len - targeti;
        while (remaining >= FFT_STEP) {
            // read oldest full block of output into target
            readHead = self.block * FFT_STEP;
            @memcpy(target[targeti .. targeti + FFT_STEP], self.output[readHead..(readHead + FFT_STEP)]);
            @memset(self.output[readHead..(readHead + FFT_STEP)], 0);
            targeti += FFT_STEP;
            remaining -= FFT_STEP;
            self.processNextInputBlock();
        }
        assert(remaining < FFT_STEP);

        // read a portion of next oldest full block into target (the outstanding portion is read on next callback)
        assert(targeti + remaining == target.len);
        readHead = self.block * FFT_STEP;
        @memcpy(target[targeti..], self.output[readHead .. readHead + remaining]);
        @memset(self.output[readHead .. readHead + remaining], 0.0);
        self.outstanding = FFT_STEP - remaining;
        targeti += remaining;
    }
};

// SOUNDS
const SoundType = enum {
    wav,
    sine,
};
const Sound = struct {
    id: u64,
    data: union(SoundType) {
        wav: Wav,
        sine: Sine,
    },

    fn loadWav(allocator: std.mem.Allocator, path: []const u8, id: u64) !@This() {
        return .{
            .id = id,
            .data = .{ .wav = try Wav.load(allocator, path) },
        };
    }
    fn sine(rate: u32, channels: u32, hz: u32, id: u64) @This() {
        return .{
            .id = id,
            .data = .{ .sine = .{ .rate = rate, .channels = channels, .hz = hz } },
        };
    }
    fn read(self: @This(), from: usize, target: []f32, loop: bool, level: f32) usize {
        return switch (self.data) {
            .wav => |wav| wav.read(from, target, loop, level),
            .sine => |sin| sin.read(from, target, level),
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

    fn read(self: @This(), from: usize, target: []f32, level: f32) usize {
        const SPACING_SEC = 1.0 / @as(f64, @floatFromInt(self.rate));
        var elapsed_time = @as(f64, @floatFromInt(from)) * SPACING_SEC;
        const FRAMES_TO_WRITE = target.len / self.channels;
        for (0..FRAMES_TO_WRITE) |i| { // TODO channels
            target[i] = @floatCast(@sin(elapsed_time * 2 * std.math.pi * @as(f64, @floatFromInt(self.hz))));
            target[i] *= level;
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

    fn read(self: @This(), from: usize, target: []f32, loop: bool, level: f32) usize {
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
        for (0..framesToWrite) |i| {
            target[i] *= level;
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

const MessageFromAudioThread = struct {
    id: usize,
    payload: union(enum) {
        registerSound: Sound,
        unregisterSound: u64,
        playSound: PlaySoundParams,
        removeSource: u64,
        updateSource: UpdateSourceParams,
    },
};
const MessageFromMixerThread = struct {
    id: usize,
    payload: union(enum) {
        tooManySounds: Sound,
        tooManySources: Source,
        soundRegistered: Sound,
        soundUnregistered: Sound,
        sourceRemoved: Source,
        sourceAdded: Source,
        sourceCompleted: Source,
    },
};

const Middleware = struct {
    // design:
    // 1. Audio Thread enqueues requests & makes them atomically available to Mixer Thread
    // 2. MT receives requests & mutates state accordingly; responds to AT with state important state changes
    // 3. AT uses MT responses to update source states, free resources, etc

    const RATE_HZ: usize = 44100;
    const CHANNELS: usize = 2;
    const MAX_SOURCES: usize = 16;
    const MAX_SOUNDS: usize = 4;
    const MAX_EVENTS: usize = 64;

    var sourceIdGen: Atomic(u64) = undefined;
    var soundIdGen: Atomic(u64) = undefined;
    var messageIdGen = Atomic(usize).init(0);
    var globalGain: GainControl = undefined;

    // Owned by audio thread
    var wavAllocator: std.mem.Allocator = undefined;

    // Owned by mixer thread
    var sources: Swapback(Source, MAX_SOURCES) = undefined;
    var sounds: Swapback(Sound, MAX_SOUNDS) = undefined;
    var sourceBuffer: [AlsaBackend.BUF_FRAMES]f32 = undefined; // TODO make backend-independent

    // Shared by mixer & audio thread
    var mixerThreadInbox: RtFifo(MessageFromAudioThread, MAX_EVENTS) = undefined;
    var audioThreadInbox: RtFifo(MessageFromMixerThread, MAX_EVENTS) = undefined;
    var isAudioThreadInboxFull: Atomic(bool) = undefined;

    // Performance statistics (DEBUG ONLY)
    const MixerInvocationStats = struct {
        rate: u32,
        frames: usize,
        nsDuration: u64,

        fn ratio(self: @This()) f64 {
            const nsFrames = @as(u64, self.frames * 1_000_000_000 / self.rate);
            return @as(f64, @floatFromInt(nsFrames)) / @as(f64, @floatFromInt(self.nsDuration));
        }

        fn ms_per_s(self: @This()) f64 {
            return @as(f64, @floatFromInt(self.nsDuration)) / 1_000_000.0 * @as(f64, @floatFromInt(self.frames)) / @as(f64, @floatFromInt(self.rate)); // 10^3 ns per us; 10^3 us per ms
        }
    };
    var latestStats = Publishable(MixerInvocationStats).init(.{ .rate = RATE_HZ, .frames = 1, .nsDuration = 1 });
    var publishStats = Publishable(MixerInvocationStats).init(.{ .rate = RATE_HZ, .frames = 1, .nsDuration = 1 });

    /// Must be called before the mixer thread starts
    fn init(allocator: std.mem.Allocator) void {
        wavAllocator = allocator;
        sources = Swapback(Source, MAX_SOURCES).init();
        sounds = Swapback(Sound, MAX_SOUNDS).init();
        mixerThreadInbox = RtFifo(MessageFromAudioThread, MAX_EVENTS).init();
        audioThreadInbox = RtFifo(MessageFromMixerThread, MAX_EVENTS).init();
        sourceIdGen = Atomic(u64).init(0);
        soundIdGen = Atomic(u64).init(0);
        isAudioThreadInboxFull = Atomic(bool).init(false);
        globalGain = GainControl.init();
    }

    /// Not safe to call this until mixer thread has joined the main thread
    fn shutdown() void {
        processMessages(); // take ownership of any straggling in-flight events
        for (0..sounds.len()) |i| {
            sounds.get(i).free();
        }
    }

    // AUDIO -> MIXER THREAD
    fn requestWav(path: []const u8) !Sound {
        const s = Sound.loadWav(wavAllocator, path, soundIdGen.fetchAdd(1, .Monotonic)) catch return error.BadSoundFile;
        debug("WAV loaded: {d} = {s}\n", .{ s.id, path });
        sendToMixerThread(.{ .registerSound = s });
        return s;
    }
    fn requestSine(hz: u32) Sound {
        const s = Sound.sine(44100, 1, hz, soundIdGen.fetchAdd(1, .Monotonic));
        sendToMixerThread(.{ .registerSound = s });
        return s;
    }
    fn requestUnregisterSound(id: u64) void {
        sendToMixerThread(.{ .unregisterSound = id });
    }
    fn requestSourceUpdate(params: UpdateSourceParams) void {
        sendToMixerThread(.{ .updateSource = params });
    }
    fn requestPlaySound(params: PlaySoundParams) u64 {
        var modifiedParams = params;
        modifiedParams.id = sourceIdGen.fetchAdd(1, .Monotonic);
        sendToMixerThread(.{ .playSound = modifiedParams });
        return modifiedParams.id;
    }
    fn requestRemoveSource(id: u64) void {
        sendToMixerThread(.{ .removeSource = id });
    }
    fn requestUpdateSource(params: UpdateSourceParams) void {
        sendToMixerThread(.{ .updateSource = params });
    }
    inline fn sendToMixerThread(payload: anytype) void {
        const req = MessageFromAudioThread{
            .id = messageIdGen.fetchAdd(1, .SeqCst),
            .payload = payload,
        };
        mixerThreadInbox.enqueueMessage(req) catch |err| switch (err) {
            error.Full => {
                warn("Request queue overrun! Discarding request.\n", .{});
            },
        };
    }

    /// Called from audio thread to process events from mixer thread
    fn processMessages() void {
        _ = mixerThreadInbox.releaseMessages();
        if (isAudioThreadInboxFull.load(.Monotonic)) {
            warn("processMessages() too slow or mixer sending too many messages, possibly leaking memory or corrupting playback state", .{});
            isAudioThreadInboxFull.store(false, .Monotonic);
        }
        if (DEBUG) {
            const ls = latestStats.read();
            const ps = publishStats.read();
            debug("latest = ({d:.2} ms/s x {d:.1}), publish = ({d:.2} ms/s x {d:.1})\n", .{ ls.ms_per_s(), ls.ratio(), ps.ms_per_s(), ps.ratio() });
        }
        while (audioThreadInbox.receiveMessage()) |resp| {
            debug("resp {d}--\n", .{resp.id});
            switch (resp.payload) {
                .soundRegistered => |sound| {
                    info("! Sound registered: {d}\n", .{sound.id});
                },
                .soundUnregistered => |sound| {
                    sound.free();
                    info("! Sound freed: {d}\n", .{sound.id});
                },
                .sourceRemoved => |source| {
                    info("! Source removed: {d}\n", .{source.id});
                },
                .sourceAdded => |source| {
                    info("! Source added: {d}\n", .{source.id});
                },
                .sourceCompleted => |source| {
                    info("! Source completed: {d}\n", .{source.id});
                },
                .tooManySounds => |sound| {
                    info("! Too many sounds, unloading {d}\n", .{sound.id});
                    sound.free();
                },
                .tooManySources => |source| {
                    info("! Too many sources, not playing {d}\n", .{source.id});
                },
            }
        }
    }

    // MIXER -> AUDIO THREAD
    inline fn mtPlaySound(request: MessageFromAudioThread, params: PlaySoundParams) void {
        if (mtFindSound(params.soundId)) |i| {
            const sound = &sounds.data[i];
            const source = Source{
                .id = params.id,
                .sound = sound,
                .level = params.level,
                .time = params.startTime,
                .loop = params.loop,
                .paused = params.paused,
                .pitch = params.pitch,
            };
            sources.add(source) catch |err| switch (err) {
                error.Full => {
                    sendToAudioThread(request, .{ .tooManySources = source });
                    return;
                },
            };
            sendToAudioThread(request, .{ .sourceAdded = source });
        } else {
            // TODO error: no sound with index
        }
    }
    inline fn mtRemoveSource(request: MessageFromAudioThread, id: u64) void {
        if (mtFindSource(id)) |i| {
            const removedSource = sources.removeAt(i) catch |err| switch (err) {
                error.InvalidIndex => unreachable,
            };
            sendToAudioThread(request, .{ .sourceRemoved = removedSource });
        } else {
            // TODO error: source does not exist
        }
    }
    inline fn mtRegisterSound(req: MessageFromAudioThread, s: Sound) void {
        sounds.add(s) catch |err| switch (err) {
            error.Full => {
                sendToAudioThread(req, .{ .tooManySounds = s });
                return;
            },
        };
        sendToAudioThread(req, .{ .soundRegistered = s });
    }
    inline fn mtUnregisterSound(req: MessageFromAudioThread, id: u64) void {
        if (mtFindSound(id)) |i| {
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
            sendToAudioThread(req, .{ .soundUnregistered = unregisteredSound });
        } else {
            // TODO error: sound does not exist
        }
    }
    inline fn mtUpdateSource(req: MessageFromAudioThread, params: UpdateSourceParams) void {
        _ = req;
        if (mtFindSource(params.id)) |i| {
            const source = &sources.data[i];
            if (params.pause) |paused| source.paused = paused;
            if (params.loop) |loop| source.loop = loop;
            if (params.level) |level| source.level = level;
            if (params.pitch) |pitch| source.pitch = pitch;
        } else {
            // TODO error: no such source
        }
    }
    inline fn mtFindSound(id: u64) ?usize {
        for (0..sounds.len()) |i| {
            if (sounds.get(i).id == id) return i;
        }
        return null;
    }
    inline fn mtFindSource(id: u64) ?usize {
        for (0..sources.len()) |i| {
            if (sources.get(i).id == id) return i;
        }
        return null;
    }
    inline fn sendToAudioThread(req: ?MessageFromAudioThread, payload: anytype) void {
        const resp = MessageFromMixerThread{
            .id = if (req) |r| r.id else messageIdGen.fetchAdd(1, .SeqCst),
            .payload = payload,
        };
        audioThreadInbox.enqueueMessage(resp) catch |err| switch (err) {
            error.Full => isAudioThreadInboxFull.store(true, .Monotonic),
        };
    }

    fn render(target: []f32) void {
        var startTimestamp: i128 = undefined;
        if (DEBUG) startTimestamp = std.time.nanoTimestamp();

        // process messages from audio thread
        while (mixerThreadInbox.receiveMessage()) |req| {
            switch (req.payload) {
                .playSound => |params| mtPlaySound(req, params),
                .removeSource => |id| mtRemoveSource(req, id),
                .registerSound => |s| mtRegisterSound(req, s),
                .unregisterSound => |s| mtUnregisterSound(req, s),
                .updateSource => |params| mtUpdateSource(req, params),
            }
        }
        // remove completed sources & send them to audio thread for any required deallocation
        var i: usize = 0;
        while (i != sources.len()) {
            if (sources.get(i).isComplete) {
                const source = sources.removeAt(i) catch |err| switch (err) {
                    error.InvalidIndex => unreachable,
                };
                sendToAudioThread(null, .{ .sourceCompleted = source });
            } else {
                i += 1;
            }
        }
        const didPublish = audioThreadInbox.releaseMessages();

        // mix sources into output buffer
        const sampleCount = target.len / CHANNELS;
        if (sampleCount > sourceBuffer.len) unreachable; // TODO need some way to guarantee this per-backend
        @memset(target, 0);
        for (0..sources.len()) |s| {
            sources.data[s].render(sourceBuffer[0..sampleCount]);
            for (0..sampleCount) |t| {
                target[2 * t + 0] += sourceBuffer[t];
                target[2 * t + 1] += sourceBuffer[t];
            }
        }
        for (0..sampleCount) |t| {
            var mixL = target[2 * t + 0];
            var mixR = target[2 * t + 1];
            const gain = globalGain.next();
            mixL *= gain;
            mixR *= gain;
            if (mixL > 1.0) mixL = 1.0;
            if (mixL < -1.0) mixL = -1.0;
            if (mixR > 1.0) mixR = 1.0;
            if (mixR < -1.0) mixR = -1.0;
            target[t * 2 + 0] = mixL;
            target[t * 2 + 1] = mixR;
        }

        if (DEBUG) {
            const stats = MixerInvocationStats{
                .rate = RATE_HZ,
                .frames = target.len / CHANNELS,
                .nsDuration = @intCast(std.time.nanoTimestamp() - startTimestamp),
            };
            latestStats.publish(stats);
            if (didPublish) {
                publishStats.publish(stats);
            }
        }
    }
};

// BACKENDS

const MacOsBackend = struct {
    const CoreAudio = @import("core_audio.zig");
    const AudioUnit = @import("audio_unit.zig");

    const RATE_HZ: c_uint = 44100; // TODO unify with Middleware's RATE_HZ
    const CHANNELS: c_uint = 2; // TODO unify with MIDDLEWARE's CHANNELS

    var audioUnit: AudioUnit.AudioUnit = undefined;

    fn init() !void {
        const defaultOutput = try CoreAudio.AudioSystemObject.queryDefaultOutputDevice();
        // TODO handle default output device change
        // try CoreAudio.AudioSystemObject.addDefaultOutputDeviceCallback(&onDefaultOutputDeviceChange);
        const auhal = try AudioUnit.AudioUnit.newAUHAL();
        errdefer auhal.dispose() catch unreachable;
        try auhal.setInputStreamFormat(CoreAudio.AudioStreamBasicDescription.lpcm(f32, @floatFromInt(RATE_HZ), @intCast(CHANNELS)));
        try auhal.setOutputDevice(defaultOutput);
        try auhal.setRenderCallback(&audioRenderCallback);
        try auhal.initialize();
        errdefer auhal.uninitialize() catch unreachable;
        try auhal.start();
        audioUnit = auhal;
    }

    fn shutdown() void {
        audioUnit.stop() catch unreachable;
        audioUnit.uninitialize() catch unreachable;
        audioUnit.dispose() catch unreachable;
    }

    fn audioRenderCallback(
        _: [*]u8,
        _: *AudioUnit.AudioUnitRenderActionFlags,
        _: *const CoreAudio.AudioTimeStamp,
        _: u32,
        inNumberFrames: u32,
        ioData: ?*CoreAudio.AudioBufferList,
    ) CoreAudio.OSStatus {
        const samples: [*]f32 = @ptrCast(@alignCast(ioData.?.mBuffers[0].mData.?));
        Middleware.render(samples[0..@intCast(inNumberFrames * CHANNELS)]);
        return 0; // no error
    }
};

const AlsaBackend = struct {
    const c = @import("alsa.zig");
    // sound card sends interrupt to CPU after each period
    // typically, the period size is half the buffer size, i.e. double buffering
    // sound cards have their own constraints on valid values for the period size & buffer size
    // latency is determined by the buffer size & sample rate: samples / samples/sec = sec

    const RATE_HZ: c_uint = 44100; // TODO unify with Middleware's RATE_HZ
    const CHANNELS: c_uint = 2; // TODO unify with MIDDLEWARE's CHANNELS
    const BUF_FRAMES: c.snd_pcm_uframes_t = 4096; // TODO experiment with larger & smaller buffers + make sure period sizes are at most BUF_FRAMES

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
            if (self.count == N) return error.Full;
            self.data[self.count] = obj;
            self.count += 1;
        }
    };
}

pub fn RtFifo(
    comptime Obj: anytype,
    comptime N: comptime_int,
) type {
    return struct {
        read: Atomic(usize),
        write: usize,
        available: Atomic(usize),
        data: [N]Obj = undefined,

        pub fn init() @This() {
            return .{
                .read = Atomic(usize).init(0),
                .write = 0,
                .available = Atomic(usize).init(0),
            };
        }
        /// Enqueue a message to be sent to the reading thread.
        /// Must call #releaseMessages() to make enqueued messages available to the reading thread.
        pub fn enqueueMessage(self: *@This(), obj: Obj) !void {
            var r = self.read.load(.SeqCst);
            var nextW = (self.write + 1) % N;
            if (nextW == r) return error.Full;
            self.data[self.write] = obj;
            self.write = nextW;
        }
        /// Release all enqueued messages to the reading thread.
        pub fn releaseMessages(self: *@This()) bool {
            return self.available.swap(self.write, .SeqCst) != self.write;
        }
        /// Called by the reading thread to receive a single message.
        pub fn receiveMessage(self: *@This()) ?Obj {
            const a = self.available.load(.SeqCst);
            const r = self.read.load(.SeqCst);
            if (a == r) {
                return null;
            }
            const obj = self.data[r];
            const nextR = (r + 1) % N;
            self.read.store(nextR, .SeqCst);
            return obj;
        }
    };
}

fn Publishable(comptime T: type) type {
    return struct {
        buffer: [2]T,
        write: Atomic(usize),
        pub fn init(initialValue: T) @This() {
            return .{
                .buffer = [_]T{initialValue} ** 2,
                .write = Atomic(usize).init(0),
            };
        }
        /// Supports single consumer
        pub fn read(self: *@This()) T {
            // TODO don't swap every time? need to have some flag(s) in the write atomic to indicate if the data has been written since last read
            const w = self.write.load(.SeqCst);
            const r = 1 - w;
            const value = self.buffer[r];
            self.write.store(r, .SeqCst);
            return value;
        }
        /// Supports single producer
        pub fn publish(self: *@This(), value: T) void {
            const w = self.write.load(.SeqCst);
            self.buffer[w] = value;
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
