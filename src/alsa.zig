pub const snd_pcm_t = opaque {};
/// Stream layout configuration.
/// Changes cannot be made while device is running.
pub const snd_pcm_hw_params_t = opaque {};
/// Driver configuration.
/// Changes can be made while device is running.
pub const snd_pcm_sw_params_t = opaque {};

/// Frame count.
pub const snd_pcm_uframes_t = c_ulong;
/// Frame count. Negative values are used as error codes.
pub const snd_pcm_sframes_t = c_long;

// Errors defined in errno.h.
// See alsa-lib/src/pcm/pcm.h for these error uses.
/// Underrun (playback) or overrun (capture).
pub const EPIPE: c_int = 32;
/// System has suspended ALSA drivers. Call #snd_pcm_recover().
pub const ESTRPIPE: c_int = 86;
/// Handshake between application & device was corrupted.
pub const EBADFD: c_int = 77;
/// Device was physically unplugged.
pub const ENOTTY: c_int = 25;
/// Device was physically unplugged.
pub const ENODEV: c_int = 19;
/// Interrupted system call.
pub const EINTR: c_int = 4;
/// No data is available.
/// Returned only when PCM is opened in non-blocking mode.
pub const EAGAIN: c_int = 11;

pub const snd_pcm_stream_t = enum(c_int) {
    playback = 0,
    capture,
};
/// ALSA does not define this type; provided here to make API more consistent
pub const snd_pcm_mode_t = enum(c_int) {
    block = 0,
    nonblock = 1,
    async_ = 2,
};
pub const snd_pcm_state_t = enum(c_int) {
    /// After #snd_pcm_open() is called; before #snd_pcm_hw_params() is called
    open = 0,
    /// After #snd_pcm_hw_params() is called; before #snd_pcm_prepare() is called
    setup,
    /// After #snd_pcm_prepare() is called; before device starts, either via #snd_pcm_start() or via writing/reading
    prepared,
    /// Device is processing samples.
    /// Call #snd_pcm_drop() or #snd_pcm_drain() to stop.
    running,
    /// Device hit underrun (playback) or overrun (capture).
    /// Call #snd_pcm_recover() to attempt to return to running state.
    xrun,
    /// Capture device is stopped but still has data available to be read.
    draining,
    /// After #snd_pcm_pause() is called.
    /// Not all hardware supports this.
    paused,
    /// Power manager suspended device.
    /// Call #snd_pcm_resume() to return to running state.
    /// Not all hardware supports this.
    suspended,
    /// Hardware is physically disconnected.
    disconnected,
};
pub const snd_pcm_access_t = enum(c_int) {
    /// mmap access with simple interleaved channels
    mmap_interleaved = 0,
    /// mmap access with simple non interleaved channels
    mmap_noninterleaved,
    /// mmap access with complex placement
    mmap_complex,
    /// snd_pcm_readi/snd_pcm_writei access
    rw_interleaved,
    /// snd_pcm_readn/snd_pcm_writen access
    rw_noninterleaved,
};
pub const snd_pcm_format_t = enum(c_int) {
    /// Unknown
    unknown = -1,
    /// Signed 8 bit
    s8 = 0,
    /// Unsigned 8 bit
    u8,
    /// Signed 16 bit Little Endian
    s16_le,
    /// Signed 16 bit Big Endian
    s16_be,
    /// Unsigned 16 bit Little Endian
    u16_le,
    /// Unsigned 16 bit Bit Endian
    u16_be,
    /// Signed 24 bit Little Endian using low three bytes in 32-bit word
    s24_le,
    /// Signed 24 bit Big Endian using low three bytes in 32-bit word
    s24_be,
    /// Unsigned 24 bit Little Endian using low three bytes in 32-bit word
    u24_le,
    /// Unsigned 24 bit Big Endian using low three bytes in 32-bit word
    u24_be,
    /// Signed 32 bit Little Endian
    s32_le,
    /// Signed 32 bit Big Endian
    s32_be,
    /// Unsigned 32 bit Little Endian
    u32_le,
    /// Unsigned 32 bit Big Endian
    u32_be,
    /// Float 32 bit Little Endian, Range -1.0 to 1.0
    float_le,
    /// Float 32 bit Big Endian, Range -1.0 to 1.0
    float_be,
    /// Float 64 bit Little Endian, Range -1.0 to 1.0
    float64_le,
    /// Float 64 bit Big Endian, Range -1.0 to 1.0
    float64_be,
};

// PCM state operations

/// Get current PCM state.
pub extern "asound" fn snd_pcm_state(
    pcm: *snd_pcm_t,
) snd_pcm_state_t;

/// Opens a PCM.
/// \state -> OPEN
/// \error -ENODEV Name does not identify a known ALSA device.
/// TODO other errors?
pub extern "asound" fn snd_pcm_open(
    ptr: **snd_pcm_t,
    name: [*:0]const u8,
    stream: snd_pcm_stream_t,
    mode: snd_pcm_mode_t,
) c_int;

/// Closes a PCM.
pub extern "asound" fn snd_pcm_close(
    pcm: *snd_pcm_t,
) c_int;

/// Prepares PCM for use.
/// \state SETUP -> PREPARED
pub extern "asound" fn snd_pcm_prepare(
    pcm: *snd_pcm_t,
) c_int;

/// Recover the PCM from an error or suspend.
/// Handles EINTR, EPIPE, and ESTRPIPE.
/// Returns 0 if error is handled; otherwise, returns the original error.
/// \state [SUSPEND | XRUN] -> RUNNING
/// \param err Error to handle.
/// \param silent If != 0, do not print error reason.
pub extern "asound" fn snd_pcm_recover(
    pcm: *snd_pcm_t,
    err: c_int,
    silent: c_int, // bool
) c_int;

/// Get the ASCII description for a given error code.
pub extern "asound" fn snd_strerror(
    errnum: c_int,
) [*:0]const u8;

/// Start a PCM.
/// \state PREPARED -> RUNNING
pub extern "asound" fn snd_pcm_start(
    pcm: *snd_pcm_t,
) c_int;

/// Stop a PCM. Pending samples in the ring buffer are ignored.
/// \state RUNNING -> SETUP
pub extern "asound" fn snd_pcm_drop(
    pcm: *snd_pcm_t,
) c_int;

/// Stop a PCM. Blocks for playback device to play remaining samples. Capture device drains until all samples are read.
/// \state RUNNING -> [DRAINING | SETUP]
pub extern "asound" fn snd_pcm_drain(
    pcm: *snd_pcm_t,
) c_int;

/// Pause or resume a PCM, if hardware supports it.
/// \state RUNNING -> [PAUSED | RUNNING]
/// \param enable 0 = Resume, 1 = Pause
pub extern "asound" fn snd_pcm_pause(
    pcm: *snd_pcm_t,
    enable: c_int,
) c_int;

/// Configure PCM hardware.
/// This selects a single configuration from the set of possible configurations described by the input. The hw_params struct is modified to have only this single configuration.
/// Parameters order: access, format, subformat, min channels, min rate, min period time, max buffer size, min tick time.
/// \state OPEN -> SETUP -> PREPARED
pub extern "asound" fn snd_pcm_hw_params(
    pcm: *snd_pcm_t,
    params: *snd_pcm_hw_params_t,
) c_int;

// Stream, i.e. "hardware", parameters

/// Allocate hardware parameter container.
/// Must subsequently call #snd_pcm_hw_params_free().
pub extern "asound" fn snd_pcm_hw_params_malloc(
    ptr: **snd_pcm_hw_params_t,
) c_int;

/// Free data allocated by #snd_pcm_hw_params_malloc().
pub extern "asound" fn snd_pcm_hw_params_free(
    ptr: *snd_pcm_hw_params_t,
) c_int;

/// Initialize parameter set with PCM hardware capabilities.
pub extern "asound" fn snd_pcm_hw_params_any(
    pcm: *snd_pcm_t,
    params: *snd_pcm_hw_params_t,
) c_int;

/// Choose a read/write access mode.
pub extern "asound" fn snd_pcm_hw_params_set_access(
    pcm: *snd_pcm_t,
    params: *snd_pcm_hw_params_t,
    access: snd_pcm_access_t,
) c_int;

/// Choose a sample binary format.
pub extern "asound" fn snd_pcm_hw_params_set_format(
    pcm: *snd_pcm_t,
    params: *snd_pcm_hw_params_t,
    format: snd_pcm_format_t,
) c_int;

/// Permit choosing only hardware-supported sample rates.
/// \param val 0 = Disable resampling; 1 = Enable resampling.
pub extern "asound" fn snd_pcm_hw_params_set_rate_resample(
    pcm: *snd_pcm_t,
    params: *snd_pcm_hw_params_t,
    val: c_uint, // bool
) c_int;

/// Choose a sampling rate nearest to some value.
/// \param val Rate in Hz; overwritten with nearest allowable rate.
/// \param dir Sub unit direction?
pub extern "asound" fn snd_pcm_hw_params_set_rate_near(
    pcm: *snd_pcm_t,
    params: *snd_pcm_hw_params_t,
    val: *c_uint,
    dir: *c_int,
) c_int;

/// Choose number of channels.
/// \param val Channels count, e.g. 2 = Stereo.
pub extern "asound" fn snd_pcm_hw_params_set_channels(
    pcm: *snd_pcm_t,
    params: *snd_pcm_hw_params_t,
    val: c_uint,
) c_int;

/// Check if hardware supports pause.
/// \param params Must contain a single configuration, see #snd_pcm_hw_params().
/// \return 0 = Does not support pause; 1 = Does support pause.
pub extern "asound" fn snd_pcm_hw_params_can_pause(
    params: *const snd_pcm_hw_params_t,
) c_int;

/// Check if hardware supports resume.
/// \param params Must contain a single configuration, see #snd_pcm_hw_params().
/// \return 0 = Does not support resume; 1 = Does support resume.
pub extern "asound" fn snd_pcm_hw_params_can_resume(
    params: *const snd_pcm_hw_params_t,
) c_int;

// Driver, i.e. "software", parameters

/// Allocate driver parameters struct using libc allocator.
pub extern "asound" fn snd_pcm_sw_params_malloc(
    ptr: **snd_pcm_sw_params_t,
) c_int;

/// Free driver parameters struct allocated using #snd_pcm_sw_params_malloc().
pub extern "asound" fn snd_pcm_sw_params_free(
    ptr: *snd_pcm_sw_params_t,
) c_int;

/// Size of #snd_pcm_sw_params_t in bytes.
/// This can be used in tandem with a Zig allocator to initialize #snd_pcm_sw_params_t.
pub extern "asound" fn snd_pcm_sw_params_sizeof() usize;

/// Initialize struct with current software configuration of PCM.
pub extern "asound" fn snd_pcm_sw_params_current(
    pcm: *snd_pcm_t,
    params: *snd_pcm_sw_params_t,
) c_int;

/// Set minimum number of available frames before #snd_pcm_wait() returns.
/// Sound card sends a hardware interrupt to transfer a 'period size' amount of frames. As such, any value other than a multiple of the period size will not yield expected results.
/// \param val Valid values are determined by the hardware?
pub extern "asound" fn snd_pcm_sw_params_set_avail_min(
    pcm: *snd_pcm_t,
    params: *snd_pcm_sw_params_t,
    val: snd_pcm_uframes_t,
) c_int;

/// For playback: start the device when available frames in the ring buffer exceed this threshold.
/// For capture: start the device when application tries to read at least as many frames as this threshold.
/// To disable automatic start, set the threshold to any value greater than the boundary.
/// \param val Start threshold in frames.
pub extern "asound" fn snd_pcm_sw_params_set_start_threshold(
    pcm: *snd_pcm_t,
    params: *snd_pcm_sw_params_t,
    val: snd_pcm_uframes_t,
) c_int;

/// Set number of frames prior to an underrun occurring to trigger automatic insertion of zeroes into the ring buffer ahead of the write head.
/// The number of silent frames inserted is determined by #snd_pcm_sw_params_set_silence_size().
/// Useful when application underrun cannot be avoided.
/// \param val Silence threshold in frames.
pub extern "asound" fn snd_pcm_sw_params_set_silence_threshold(
    pcm: *snd_pcm_t,
    params: *snd_pcm_sw_params_t,
    val: snd_pcm_uframes_t,
) c_int;

/// Set number of frames of silence to write when underrun is nearer than the #silence_threshold.
/// \param Silence size in frames. Set to 0 to disable. Set to #boundary to always fill unplayed samples with zeroes (must also set #silence_threshold to 0).
pub extern "asound" fn snd_pcm_sw_params_set_silence_size(
    pcm: *snd_pcm_t,
    params: *snd_pcm_sw_params_t,
    val: snd_pcm_uframes_t,
) c_int;

/// Write PCM driver settings to driver.
pub extern "asound" fn snd_pcm_sw_params(
    pcm: *snd_pcm_t,
    params: *snd_pcm_sw_params_t,
) c_int;

// IO & poll operations

pub extern "asound" fn snd_pcm_wait(
    pcm: *snd_pcm_t,
    timeout: c_int, // in milliseconds
) c_int;

/// Write interleaved frames to a PCM:
/// In blocking mode, waits until all frames are written. Return value != size only if a signal or underrun occurred.
/// In nonblocking mode, returns -EAGAIN if no data could be immediately written; otherwise, number of frames written.
/// \param buffer Contains frames to write.
/// \param size Number of frames to write.
/// \error -EBADFD   PCM not in #prepared or #running states.
/// \error -EPIPE    Underrun occurred.
/// \error -ESTRPIPE Suspend event occurred.
pub extern "asound" fn snd_pcm_writei(
    pcm: *snd_pcm_t,
    buffer: [*]const u8, // const void*
    size: snd_pcm_uframes_t,
) snd_pcm_sframes_t;

/// Write noninterleaved frames to a PCM.
/// \param bufs Array of frames, one for each channel
/// \param size Number of frames to write.
/// \error -EBADFD   PCM not in #prepared or #running states.
/// \error -EPIPE    Underrun occurred.
/// \error -ESTRPIPE Suspend event occurred.
pub extern "asound" fn snd_pcm_writen(
    pcm: *snd_pcm_t,
    bufs: [*][*]u8, // void **
    size: snd_pcm_uframes_t,
) snd_pcm_sframes_t;

/// Read interleaved frames from a PCM.
/// In blocking mode, waits until all requested frames are read. Returns a number of frames less than size only if a signal or underrun occurred.
/// In nonblocking mode, returns -EAGAIN if no frames were available to read.
/// \param buffer Where captured frames should be written.
/// \param size Number of frames to read.
/// \error -EBADFD   PCM not in #prepared or #running states.
/// \error -EPIPE    Overrun occurred.
/// \error -ESTRPIPE Suspend event occurred.
pub extern "asound" fn snd_pcm_readi(
    pcm: *snd_pcm_t,
    buffer: [*]u8, // void *
    size: snd_pcm_uframes_t,
) snd_pcm_sframes_t;

/// Read noninterleaved frames from a PCM.
/// In blocking mode, waits until all requested frames are read. Returns a number of frames less than size only if a signal or underrun occurred.
/// In nonblocking mode, returns -EAGAIN if no frames were available to read.
/// \param bufs Array of frames, one for each channel.
/// \param size Number of frames to write.
/// \error -EBADFD   PCM not in #prepared or #running states.
/// \error -EPIPE    Underrun occurred.
/// \error -ESTRPIPE Suspend event occurred.
pub extern "asound" fn snd_pcm_readn(
    pcm: *snd_pcm_t,
    bufs: [*][*]u8,
    size: snd_pcm_uframes_t,
) snd_pcm_sframes_t;

// Ring buffer pointer

/// Get the current read(capture)/write(playback) ring buffer point from the kernel driver.
/// The r/w pointer in the kernel driver is updated only when the sound card issues a hardware interrupt, so this gives a less accurate result than #snd_pcm_avail().
/// This does not require a kernel context switch.
/// \return Positive number of ready frames; negative error code otherwise
pub extern "asound" fn snd_pcm_avail_update(
    pcm: *snd_pcm_t,
) snd_pcm_sframes_t;

/// Reads the current r/w pointer from the hardware & calls #snd_pcm_avail_update().
/// Requires a kernel context switch.
/// \return Positive number of ready frames; negative error code otherwise
pub extern "asound" fn snd_pcm_avail(
    pcm: *snd_pcm_t,
) snd_pcm_sframes_t;

/// For playback: count of frames before next frames are sent to DAC.
/// For capture: count of frames before next frames are captured from ADC.
/// Requires a kernel context switch.
/// \param delayp Total I/O latency, measured in frames.
pub extern "asound" fn snd_pcm_delay(
    pcm: *snd_pcm_t,
    delayp: *snd_pcm_sframes_t,
) c_int;

/// Gets synchronized #snd_pcm_avail() and #snd_pcm_delay() values.
/// Requires only a single kernel context switch.
/// \param availp Number of available frames in the ring buffer.
/// \param delayp Total I/O latency, measured in frames.
pub extern "asound" fn snd_pcm_avail_delay(
    pcm: *snd_pcm_t,
    availp: *snd_pcm_sframes_t,
    delayp: *snd_pcm_sframes_t,
) c_int;

// Linking PCMs

/// Link two PCMs. The two will start, stop, and prepare in sync.
/// \error -ENOSYS PCMs do not support linking.
pub extern "asound" fn snd_pcm_link(
    pcm1: *snd_pcm_t,
    pcm2: *snd_pcm_t,
) c_int;

/// Unlink a PCM from a linked group.
/// \error -ENOSYS PCM does not support unlinking.
pub extern "asound" fn snd_pcm_unlink(
    pcm: *snd_pcm_t,
) c_int;
