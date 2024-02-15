const std = @import("std");
const c = @import("alsa.zig");
const Atomic = std.atomic.Atomic;

// resources:
// - "C++ in the Audio Industry", Timur Doumler, CppCon 2015
// - "Thread Synchronization In Real-Time Audio Processing with RCU (Read-Copy-Update)", Timur Doumler
// - "C++ atomics, from basic to advanced", Fedor Pikus, CppCon 2017
// - "Read, Copy, Update, then what? RCU for non-kernel programmers", Fedor Pikus, CppCon 2017
// - "Real time 101", David Rowland & Fabian Renn Giles, Meeting C++ 2019

// things an audio PAL can do:
// - list playback devices
// - change playback device
// - react to changes in devices
// - adjust playback volume
// - consume render callback

// - deferred reclamation
// - publishing protocol (mutex, spin lock (atomic), atomic data, CAS exchange loop)
// - RCU

// sound card sends interrupt to CPU after each period
// typically, the period size is half the buffer size, i.e. double buffering
// sound cards have their own constraints on valid values for the period size & buffer size
// latency is determined by the buffer size & sample rate: samples / samples/sec = sec

// idea: budget memory allocations for sound
// TODO research deferred reclamation
// TODO spin-on-write pointer swap impl

fn getInt(idStr: []const u8) ?u64 {
    const id = std.fmt.parseInt(u64, idStr, 10) catch |err| switch (err) {
        error.Overflow => {
            std.debug.print("Input too large: {s}\n", .{idStr});
            return null;
        },
        error.InvalidCharacter => {
            std.debug.print("Not an integer: {s}\n", .{idStr});
            return null;
        },
    };
    return id;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    const Backend = AlsaBackend;

    try Backend.init();
    Middleware.init(alloc);
    defer Middleware.shutdown();
    defer Backend.shutdown();

    var buffer: [1024]u8 = undefined;
    const stdin = std.io.getStdIn();
    var stdinReader = stdin.reader();
    while (try stdinReader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        if (std.mem.eql(u8, line, "q")) {
            break;
        } else if (line.len >= 2 and std.mem.eql(u8, line[0..1], "l")) {
            const soundPath = line[2..];
            const sound = Middleware.loadSound(soundPath) catch |err| switch (err) {
                error.BadSoundFile => {
                    std.debug.print("Bad sound file: {s}\n", .{soundPath});
                    continue;
                },
            };
            std.debug.print("Load success: {s}, id={d}\n", .{ soundPath, sound.id });
        } else if (line.len >= 2 and std.mem.eql(u8, line[0..1], "u")) {
            const soundId = getInt(line[2..]).?;
            Middleware.requestUnloadSound(soundId);
            std.debug.print("Unload sound requested: {d}\n", .{soundId});
        } else if (line.len >= 2 and std.mem.eql(u8, line[0..1], "p")) {
            const idStr = line[2..];
            const id = getInt(idStr) orelse continue;
            const sourceId = Middleware.requestPlaySound(SourceParams{
                .soundId = id,
                .startTime = 0,
                .loop = false,
                .paused = false,
            });
            std.debug.print("Play sound requested {d}\n", .{sourceId});
        } else if (line.len >= 2 and std.mem.eql(u8, line[0..1], "k")) {
            const idStr = line[2..];
            const id = getInt(idStr) orelse continue;
            Middleware.requestRemoveSource(id);
            std.debug.print("Killed {d}\n", .{id});
        } else if (line.len >= 4 + 1 and std.mem.eql(u8, line[0..4], "sine")) {
            const freq = getInt(line[5..]).?;
            const sine = Middleware.addSine(@intCast(freq));
            std.debug.print("Add sine request: {any}\n", .{sine});
        } else {
            std.debug.print("unknown cmd: {s}\n", .{line});
        }
        Middleware.processEvents();
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
        const framesWritten = self.sound.data.read(self.time, target);
        if (framesWritten != target.len) {
            self.isComplete = true;
        }
        self.time += framesWritten;
        // TODO implement loop
    }
};

// SOUNDS

const SoundType = enum {
    wav,
    sine,
};
const SoundData = union(SoundType) {
    wav: Wav,
    sine: Sine,

    fn read(self: @This(), from: usize, target: []f32) usize {
        return switch (self) {
            .wav => |wav| wav.read(from, target),
            .sine => |sin| sin.read(from, target),
        };
    }

    fn free(self: @This()) void {
        switch (self) {
            .wav => |wav| wav.free(Middleware.wavAllocator),
            .sine => {},
        }
    }
};
const Sound = struct {
    var idGen = Atomic(u64).init(0);

    id: u64,
    data: SoundData,

    fn init(data: SoundData) @This() {
        return .{
            .id = idGen.fetchAdd(1, .Monotonic),
            .data = data,
        };
    }

    fn free(self: @This()) void {
        self.data.free();
    }

    fn loadWav(allocator: std.mem.Allocator, path: []const u8) !@This() {
        return init(.{ .wav = try Wav.load(allocator, path) });
    }

    fn sine(rate: u32, channels: u32, hz: u32) @This() {
        return init(.{ .sine = .{ .rate = rate, .channels = channels, .hz = hz } });
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

    fn read(self: @This(), from: usize, target: []f32) usize {
        if (from >= self.samples.len) return 0;
        const remainingFrames = self.samples.len - from;
        const framesToWrite = @min(target.len, remainingFrames);
        @memcpy(target[0..framesToWrite], self.samples[from .. from + framesToWrite]);
        if (framesToWrite != target.len) {
            @memset(target[framesToWrite..], 0.0);
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

const MixerNotificationType = enum {
    registerSound,
    unregisterSound,
    playSound,
    removeSource,
};
const MixerNotification = union(MixerNotificationType) {
    registerSound: Sound,
    unregisterSound: u64,
    playSound: SourceParams,
    removeSource: u64,
};
const MixerResponseType = enum {
    soundUnregistered,
    sourceRemoved,
    sourceAdded,
    sourceCompleted,
};
const MixerResponse = union(MixerResponseType) {
    soundUnregistered: Sound,
    sourceRemoved: Source,
    sourceAdded: Source,
    sourceCompleted: Source,
};

const Middleware = struct {
    const RATE_HZ: usize = 44100;
    const CHANNELS: usize = 2;
    const MAX_SOURCES: usize = 16;
    const MAX_SOUNDS: usize = 4;
    const MAX_EVENTS: usize = 64;

    var wavAllocator: std.mem.Allocator = undefined;

    // written by non-rt threads, read by rt mixer thread
    var mixerNotificationQueue: MpScRingFifo(MixerNotification, MAX_EVENTS) = undefined;
    var mixerResponseQueue: SpScRingFifo(MixerResponse, MAX_EVENTS) = undefined;

    // mutated on rt mixer thread, do not read from other threads
    var sources: Swapback(Source, MAX_SOURCES) = undefined;
    var sounds: Swapback(Sound, MAX_SOUNDS) = undefined;

    fn init(allocator: std.mem.Allocator) void {
        wavAllocator = allocator;
        sources = Swapback(Source, MAX_SOURCES).init();
        sounds = Swapback(Sound, MAX_SOUNDS).init();
        mixerNotificationQueue = MpScRingFifo(MixerNotification, MAX_EVENTS).init();
    }

    fn shutdown() void {
        for (0..sounds.len()) |i| {
            switch (sounds.get(i).data) {
                .wav => |wav| {
                    wav.free(wavAllocator);
                },
                .sine => {},
            }
        }
    }

    /// Processes events from the realtime mixer thread
    fn processEvents() void {
        while (mixerResponseQueue.realtimeRead()) |resp| {
            switch (resp) {
                .soundUnregistered => |sound| {
                    sound.free();
                },
                .sourceRemoved => |source| {
                    _ = source;
                },
                .sourceAdded => |source| {
                    _ = source;
                },
                .sourceCompleted => |source| {
                    _ = source;
                },
            }
        }
    }

    /// Synchronously load a sound, then notify mixer thread that sound is available.
    fn loadSound(path: []const u8) !Sound {
        const s = Sound.loadWav(wavAllocator, path) catch return error.BadSoundFile;
        mixerNotificationQueue.spinWrite(MixerNotification{ .registerSound = s }) catch |err| switch (err) {
            error.Overrun => {
                std.debug.print("Event queue overrun!\n", .{});
            },
        };
        return s;
    }

    fn addSine(hz: u32) Sound {
        const s = Sound.sine(44100, 1, hz);
        mixerNotificationQueue.spinWrite(MixerNotification{ .registerSound = s }) catch |err| switch (err) {
            error.Overrun => {
                std.debug.print("Event queue overrun!\n", .{});
            },
        };
        return s;
    }

    /// Notify mixer thread that sound should no longer be used.
    /// Cannot clean up sound resources until mixer thread notifies us that the sound is no longer in use.
    fn requestUnloadSound(id: u64) void {
        mixerNotificationQueue.spinWrite(MixerNotification{ .unregisterSound = id }) catch |err| switch (err) {
            error.Overrun => {
                std.debug.print("Event queue overrun!\n", .{});
            },
        };
    }

    fn requestPlaySound(params: SourceParams) u64 {
        var modifiedParams = params;
        modifiedParams.id = Source.idGen.fetchAdd(1, .Monotonic);
        mixerNotificationQueue.spinWrite(MixerNotification{ .playSound = modifiedParams }) catch |err| switch (err) {
            error.Overrun => {
                std.debug.print("Event queue overrun!\n", .{});
            },
        };
        return modifiedParams.id;
    }

    fn requestRemoveSource(id: u64) void {
        mixerNotificationQueue.spinWrite(MixerNotification{ .removeSource = id }) catch |err| switch (err) {
            error.Overrun => {
                std.debug.print("Event queue overrun!\n", .{});
            },
        };
    }

    inline fn rtPlaySound(params: SourceParams) void {
        if (findSound(params.soundId)) |i| {
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
                    // TODO error: too many sources!
                },
            };
        } else {
            // TODO error: no sound with index
        }
    }

    inline fn rtRemoveSource(id: u64) void {
        if (findSource(id)) |i| {
            const removedSource = sources.removeAt(i) catch |err| switch (err) {
                error.InvalidIndex => {
                    // TODO
                    unreachable;
                },
            };
            mixerResponseQueue.realtimeWrite(MixerResponse{ .sourceAdded = removedSource }) catch |err| switch (err) {
                error.Overrun => {
                    std.debug.print("Overrun on rtRemoveSource\n", .{});
                },
            };
        } else {
            // TODO error: source does not exist
        }
    }

    /// Make a sound available for use by sources.
    inline fn rtRegisterSound(s: Sound) void {
        sounds.add(s) catch |err| switch (err) {
            error.Overrun => {
                // TODO error: too many sounds!
            },
        };
    }

    /// Remove all sources that are using a sound.
    inline fn rtUnregisterSound(id: u64) void {
        if (findSound(id)) |i| {
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
            mixerResponseQueue.realtimeWrite(MixerResponse{ .soundUnregistered = unregisteredSound }) catch |err| switch (err) {
                error.Overrun => unreachable,
            };
        } else {
            // TODO error: sound does not exist
        }
    }

    inline fn findSound(id: u64) ?usize {
        for (0..sounds.len()) |i| {
            if (sounds.get(i).id == id) return i;
        }
        return null;
    }

    inline fn findSource(id: u64) ?usize {
        for (0..sources.len()) |i| {
            if (sources.get(i).id == id) return i;
        }
        return null;
    }

    fn render(target: []f32) void {
        {
            // TODO replace read loop with single range; will perform many fewer atomic operations when many events exist
            while (mixerNotificationQueue.realtimeRead()) |event| {
                switch (event) {
                    .playSound => |params| rtPlaySound(params),
                    .removeSource => |id| rtRemoveSource(id),
                    .registerSound => |s| rtRegisterSound(s),
                    .unregisterSound => |s| rtUnregisterSound(s),
                }
            }
        }
        {
            // remove sources for completed sounds
            var i: usize = 0;
            while (i != sources.len()) {
                if (sources.get(i).isComplete) {
                    const source = sources.removeAt(i) catch |err| switch (err) {
                        error.InvalidIndex => unreachable,
                    };
                    mixerResponseQueue.realtimeWrite(MixerResponse{ .sourceCompleted = source }) catch |err| switch (err) {
                        error.Overrun => unreachable,
                    };
                } else {
                    i += 1;
                }
            }
        }
        for (0..target.len / CHANNELS) |t| {
            var mix: f32 = 0;
            var value = [_]f32{0.0};
            for (0..sources.len()) |i| {
                sources.data[i].render(&value);
                mix += value[0];
            }
            if (mix > 1.0) mix = 1.0;
            if (mix < -1.0) mix = -1.0;
            target[t * 2 + 0] = mix;
            target[t * 2 + 1] = mix;
        }
    }
};

// RING BUFFERS

// Single-producer, single-consumer, null on underrun, alert on overrun
// Used to receive responses from the realtime mixer thread, e.g. sound is no longer in use & can be deallocated
fn SpScRingFifo(comptime T: anytype, comptime N: comptime_int) type {
    return struct {
        read: Atomic(usize),
        write: Atomic(usize),
        data: [N]T = undefined,

        fn init() @This() {
            return .{
                .read = Atomic(usize).init(0),
                .write = Atomic(usize).init(0),
            };
        }

        fn realtimeWrite(self: *@This(), obj: T) !void {
            const r = self.read.load(.Monotonic);
            const w = self.write.load(.Monotonic);
            const nextW = (w + 1) % N;
            if (nextW == r) return error.Overrun;
            self.data[w] = obj;
            self.write.store(nextW, .Release);
        }

        fn realtimeRead(self: *@This()) ?T {
            const w = self.write.load(.Acquire);
            const r = self.read.load(.Monotonic);
            if (r == w) return null; // Underrun
            const obj = self.data[r];
            self.read.store((r + 1) % N, .Release);
            return obj;
        }
    };
}

// Multiple-producer, single-consumer, null on underrun, alert on overrun
// Used to send notifications to the realtime mixer thread
fn MpScRingFifo(
    comptime Obj: anytype,
    comptime N: comptime_int,
) type {
    return struct {
        read: Atomic(usize),
        write: Atomic(usize),
        insert: Atomic(usize),
        data: [N]Obj = undefined,

        fn init() @This() {
            return .{
                .read = Atomic(usize).init(0),
                .write = Atomic(usize).init(0),
                .insert = Atomic(usize).init(0),
            };
        }

        /// Supports multiple, non-realtime producers.
        fn spinWrite(self: *@This(), obj: Obj) !void {
            var r: usize = undefined;
            var w: usize = undefined;
            var nextW: usize = undefined;
            overrunCheck: while (true) {
                r = self.read.load(.Monotonic);
                w = self.write.load(.Monotonic);
                nextW = (w + 1) % N;
                if (self.write.compareAndSwap(w, nextW, .SeqCst, .Monotonic) == null) { // advance write head
                    if (nextW == r) return error.Overrun;
                    break :overrunCheck;
                }
            }
            self.data[w] = obj;
            while (self.insert.compareAndSwap(w, nextW, .SeqCst, .Monotonic) != null) {} // advance insert head
        }

        // Supports single, realtime consumer.
        fn realtimeRead(self: *@This()) ?Obj {
            const i = self.insert.load(.Monotonic);
            const r = self.read.load(.SeqCst);
            if (i == r) {
                return null;
            }
            const obj = self.data[r];
            const nextR = (r + 1) % N;
            self.read.store(nextR, .SeqCst);
            return obj;
        }
    };
}

// BACKENDS

const AlsaBackend = struct {
    const RATE_HZ: c_uint = 44100;
    const SPACING_SEC: f64 = 1.0 / @as(f64, @floatFromInt(RATE_HZ));
    const CHANNELS: c_uint = 2;
    const BUF_FRAMES: c.snd_pcm_uframes_t = 4096;
    const MAX_SOUNDS: usize = 4;
    const MAX_VOICES: usize = 4;

    var mixerThread: std.Thread = undefined;
    var shutdownRequested = false;

    var player: *c.snd_pcm_t = undefined;
    var avail: c.snd_pcm_sframes_t = undefined;
    var buffer = [_]f32{0.0} ** (BUF_FRAMES * CHANNELS);

    fn init() !void {
        var hw: *c.snd_pcm_hw_params_t = undefined;
        var sw: *c.snd_pcm_sw_params_t = undefined;

        var rate: c_uint = RATE_HZ;
        var dir: c_int = 0;
        var err: c_int = 0;

        // TODO handle error codes
        _ = c.snd_pcm_open(&player, "default", .playback, .block);

        _ = c.snd_pcm_hw_params_malloc(&hw);
        _ = c.snd_pcm_hw_params_any(player, hw);
        _ = c.snd_pcm_hw_params_set_access(player, hw, .rw_interleaved);
        err = c.snd_pcm_hw_params_set_format(player, hw, .float_le);
        if (err != 0) {
            std.debug.print("unsupported sample format: {s}\n", .{c.snd_strerror(err)});
        }
        // _ = c.snd_pcm_hw_params_set_rate_resample(player, hw, 0); // TODO disable software resampling; implement my own? or use only sounds with the target rate.
        _ = c.snd_pcm_hw_params_set_rate_near(player, hw, &rate, &dir);
        if (rate != RATE_HZ) {
            std.debug.print("Rate {d} not supported, use {d} instead\n", .{ RATE_HZ, rate });
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

        mixerThread = try std.Thread.spawn(.{}, audio_thread_worker, .{});
        try mixerThread.setName("Mixer");
    }

    fn shutdown() void {
        shutdownRequested = true;
        mixerThread.join();
        _ = c.snd_pcm_close(player);
    }

    fn audio_thread_worker() void {
        var err: c_int = undefined;
        while (!shutdownRequested) { // TODO cheating? is this atomically updated?
            err = c.snd_pcm_wait(player, 1000);
            if (err < 0) {
                err = c.snd_pcm_recover(player, err, 0);
                if (err < 0) {
                    std.debug.print("Failed to recover from failed wait: {s}\n", .{c.snd_strerror(err)});
                    break;
                } else {
                    std.debug.print("Recovered from wait failure\n", .{});
                }
            }
            avail = c.snd_pcm_avail_update(player);
            if (avail < 0) {
                std.debug.print("avail_update: {s}\n", .{c.snd_strerror(@intCast(avail))});
                break;
            }
            avail = @min(avail, BUF_FRAMES);
            Middleware.render(buffer[0..@intCast(avail * CHANNELS)]);
            err = @intCast(c.snd_pcm_writei(player, @ptrCast(buffer[0..].ptr), @intCast(avail)));
            if (err < 0) {
                err = c.snd_pcm_recover(player, @intCast(err), 0);
                if (err < 0) {
                    std.debug.print("failed to recover from writei: {s}\n", .{c.snd_strerror(@intCast(err))});
                }
            }
            if (err != avail) {
                std.debug.print("render callback failed!\n", .{});
                break;
            }
        }
    }
};

fn Swapback(comptime T: anytype, comptime N: comptime_int) type {
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

        fn exportWhen(comptime condition: bool, comptime functions: type) type {
            return if (condition) functions else struct {};
        }

        pub usingnamespace exportWhen(std.meta.trait.isNumber(T), struct {
            // TODO export *ById fns when type has id member
        });
    };
}
