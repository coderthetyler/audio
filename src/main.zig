const std = @import("std");
const c = @import("alsa.zig");

// things an audio PAL can do:
// - list playback devices
// - change playback device
// - react to changes in devices
// - adjust playback volume
// - consume render callback

// sound card sends interrupt to CPU after each period
// typically, the period size is half the buffer size, i.e. double buffering
// sound cards have their own constraints on valid values for the period size & buffer size
// latency is determined by the buffer size & sample rate: samples / samples/sec = sec

// idea: budget memory allocations for sound

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();

    const Backend = AlsaBackend;

    try Backend.init(alloc);
    defer Backend.shutdown();

    Middleware.init();
    defer Middleware.shutdown();

    var buffer: [1024]u8 = undefined;
    const stdin = std.io.getStdIn();
    var stdinReader = stdin.reader();
    while (try stdinReader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        if (std.mem.eql(u8, line, "q")) {
            break;
        } else if (line.len >= 2 and std.mem.eql(u8, line[0..1], "l")) {
            const soundPath = line[2..];
            Backend.loadSound(soundPath) catch |err|
                switch (err) {
                error.TooManySounds => {
                    std.debug.print("Too many sounds: {s}\n", .{soundPath});
                    continue;
                },
                error.SoundAlreadyLoaded => {
                    std.debug.print("Sound already loaded: {s}\n", .{soundPath});
                    continue;
                },
                error.BadSoundFile => {
                    std.debug.print("Bad sound file: {s}\n", .{soundPath});
                    continue;
                },
            };
            std.debug.print("Load success: {s}\n", .{soundPath});
        } else if (line.len >= 2 and std.mem.eql(u8, line[0..1], "u")) {
            const soundPath = line[2..];
            Backend.unloadSound(soundPath) catch |err| switch (err) {
                error.SoundNotLoaded => {
                    std.debug.print("Sound not loaded: {s}\n", .{soundPath});
                    continue;
                },
            };
            std.debug.print("Unload success: {s}\n", .{soundPath});
            // TODO if sound unloaded, kill all its tracks
        } else if (line.len >= 2 and std.mem.eql(u8, line[0..1], "p")) {
            const soundPath = line[2..];
            const trackId = Backend.playSound(soundPath) catch |err| switch (err) {
                error.SoundNotLoaded => {
                    std.debug.print("Sound not loaded: {s}\n", .{soundPath});
                    continue;
                },
                error.TooManyTracks => {
                    std.debug.print("Too many tracks: {s}\n", .{soundPath});
                    continue;
                },
            };
            std.debug.print("Playing track {d}: {s}\n", .{ trackId, soundPath });
        } else if (line.len >= 2 and std.mem.eql(u8, line[0..1], "k")) {
            const idStr = line[2..];
            const id = std.fmt.parseInt(TrackId, idStr, 10) catch |err| switch (err) {
                error.Overflow => {
                    std.debug.print("Input too large: {s}\n", .{idStr});
                    continue;
                },
                error.InvalidCharacter => {
                    std.debug.print("Not an integer: {s}\n", .{idStr});
                    continue;
                },
            };
            Backend.killTrack(id);
            std.debug.print("Killed {d}\n", .{id});
        } else {
            std.debug.print("unknown cmd: {s}\n", .{line});
        }
    }
    std.debug.print("Goodbye\n", .{});
}

const SoundId = i32;
const TrackId = u64;
const TrackParams = struct {
    dbVolume: f32,
    position: [3]f32,
};

const AudioEngine = struct {
    fn init() void {}
    fn shutdown() void {}

    fn update(_: f64) void {} // TODO implement track state machine

    fn loadSound(_: [*:0]const u8) SoundId {}
    fn unloadSound(_: SoundId) void {}
    fn playSound(_: SoundId, _: TrackParams) TrackId {}

    fn pauseTrack(_: TrackId) void {}
    fn stopTrack(_: TrackId, _: f32) void {}
    fn setTrackVolume(_: TrackId, _: f32) void {}
    fn setTrackPosition(_: TrackId, _: [3]f32) void {}
};

const Track = struct {
    id: u64,
    sound: []i16,
    time: usize,
};

const Middleware = struct {
    const RATE_HZ: c_uint = 44100;
    const SPACING_SEC: f64 = 1.0 / @as(f64, @floatFromInt(RATE_HZ));
    const CHANNELS: c_uint = 2;

    var elapsed_time: f64 = 0.0;

    fn init() void {}
    fn shutdown() void {}

    fn render(buffer: []i16) void {
        for (0..buffer.len / CHANNELS) |i| {
            var mix: i16 = 0;
            for (&AlsaBackend.tracks) |*t| {
                if (t.time == t.sound.len) continue; // TODO reap finished sounds: try double-buffered track list?
                mix +|= t.sound[t.time];
                t.time += 1;
            }
            buffer[i * 2 + 0] = mix;
            buffer[i * 2 + 1] = mix;
        }
    }

    fn renderSines(buffer: [16]i16) void {
        for (0..buffer.len / CHANNELS) |i| {
            buffer[i * 2 + 0] = @intFromFloat(@as(f64, @floatFromInt(std.math.maxInt(i16))) * @sin(elapsed_time * 2 * std.math.pi * 300.0));
            buffer[i * 2 + 1] = @intFromFloat(@as(f64, @floatFromInt(std.math.maxInt(i16))) * @sin(elapsed_time * 2 * std.math.pi * 440.0));
            elapsed_time += SPACING_SEC;
        }
    }
};

const SoundLoadError = error{
    SoundAlreadyLoaded,
    TooManySounds,
    BadSoundFile,
};
const SoundUnloadError = error{
    SoundNotLoaded,
};
const TrackPlayError = error{
    SoundNotLoaded,
    TooManyTracks,
};

const AlsaBackend = struct {
    const RATE_HZ: c_uint = 44100;
    const SPACING_SEC: f64 = 1.0 / @as(f64, @floatFromInt(RATE_HZ));
    const CHANNELS: c_uint = 2;
    const BUF_FRAMES: c.snd_pcm_uframes_t = 4096;
    const MAX_SOUNDS: usize = 4;
    const MAX_TRACKS: usize = 4;

    var soundAllocator: std.mem.Allocator = undefined;
    var audioThread: std.Thread = undefined;
    var shutdownRequested = false;

    var player: *c.snd_pcm_t = undefined;
    var avail: c.snd_pcm_sframes_t = undefined;
    var buffer = [_]i16{0.0} ** (BUF_FRAMES * CHANNELS);

    var soundsCount: usize = 0;
    var sounds: [MAX_SOUNDS]WAVFile = undefined;

    var tracksCount: usize = 0;
    var tracks: [MAX_TRACKS]Track = undefined;
    var trackIdGen: TrackId = 0;

    fn playSound(path: []const u8) TrackPlayError!TrackId {
        if (tracksCount == MAX_TRACKS) {
            return error.TooManyTracks;
        }
        if (findSound(path)) |idx| {
            const id = trackIdGen;
            trackIdGen += 1;
            tracks[tracksCount] = .{
                .id = id,
                .sound = sounds[idx].buffer,
                .time = 0,
            };
            tracksCount += 1;
            return id;
        } else {
            return error.SoundNotLoaded;
        }
    }

    fn killTrack(id: TrackId) void {
        _ = id;
    }

    fn findSound(path: []const u8) ?usize {
        for (sounds[0..soundsCount], 0..) |*s, i| {
            if (std.mem.eql(u8, s.name, path)) {
                return i;
            }
        }
        return null;
    }

    fn loadSound(path: []const u8) SoundLoadError!void {
        if (findSound(path)) |_| {
            return error.SoundAlreadyLoaded;
        }
        // allocate, but not if too many
        if (soundsCount == sounds.len) {
            std.debug.print("Too many sounds loaded: {s}\n", .{path});
            return error.TooManySounds;
        }
        sounds[soundsCount] = WAVFile.load(soundAllocator, path) catch |err| {
            std.debug.print("Failed to load sound: {s}, {any}\n", .{ path, err });
            return error.BadSoundFile;
        };
        soundsCount += 1;
    }

    fn unloadSound(path: []const u8) SoundUnloadError!void {
        if (soundsCount == 0) return error.SoundNotLoaded;
        if (findSound(path)) |idx| {
            sounds[idx].free(soundAllocator);
            if (soundsCount != 1 and idx != soundsCount - 1) {
                sounds[idx] = sounds[soundsCount - 1];
            }
            soundsCount -= 1;
        } else {
            return error.SoundNotLoaded;
        }
    }

    fn init(alloc: std.mem.Allocator) !void {
        soundAllocator = alloc;

        var hw: *c.snd_pcm_hw_params_t = undefined;
        var sw: *c.snd_pcm_sw_params_t = undefined;

        var rate: c_uint = RATE_HZ;
        var dir: c_int = 0;

        // TODO handle error codes
        _ = c.snd_pcm_open(&player, "default", .playback, .block);

        _ = c.snd_pcm_hw_params_malloc(&hw);
        _ = c.snd_pcm_hw_params_any(player, hw);
        _ = c.snd_pcm_hw_params_set_access(player, hw, .rw_interleaved);
        _ = c.snd_pcm_hw_params_set_format(player, hw, .s16_le);
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

        audioThread = try std.Thread.spawn(.{}, audio_thread_worker, .{});
        try audioThread.setName("AudioThread");
    }

    fn shutdown() void {
        shutdownRequested = true;
        audioThread.join();
        _ = c.snd_pcm_close(player);
        for (0..soundsCount) |i| {
            sounds[i].free(soundAllocator);
        }
        soundsCount = 0;
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

pub const WAVFile = struct {
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

    name: []const u8,
    buffer: []i16,
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

        const copyName = try allocator.alloc(u8, path.len);
        errdefer allocator.free(copyName);
        @memcpy(copyName, path);

        return @This(){
            .name = copyName,
            .buffer = @as([*]i16, @ptrCast(@alignCast(data.ptr)))[0..@intCast(data.len * @sizeOf(u8) / @sizeOf(i16))],
            .sampleRateHz = header.sampleRate,
        };
    }

    pub inline fn at(self: @This(), index: anytype) i16 {
        return self.buffer[index % self.buffer.len];
    }

    pub fn free(self: @This(), allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        allocator.free(self.buffer);
    }
};
