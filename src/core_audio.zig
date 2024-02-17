pub const std = @import("std");

pub const OSStatus = i32;
pub const OSType = u32;
pub const AudioObjectID = u32;
pub const AudioDeviceID = AudioObjectID;
pub const AudioObjectPropertyListenerProc = *const fn (
    AudioObjectID, // inObjectID
    u32, // inNumberAddresses
    [*]const AudioObjectPropertyAddress, // inAddresses
    [*]u8, // inClientData
) OSStatus;
pub const AudioDeviceIOProc = *const fn (
    AudioObjectID, // inDevice
    *const AudioTimeStamp, // inNow
    *const AudioBufferList, // inInputData
    *AudioTimeStamp, // inInputTime
    *AudioBufferList, // outOutputData
    *const AudioTimeStamp, // inOutputTime
    ?[*]u8, // inClientData
) OSStatus;
pub const AudioDeviceIOProcID = AudioDeviceIOProc;
pub const AudioHardwareIOProcStreamUsage = extern struct {
    /// The IOProc whose stream usage is being specified.
    mIOProc: *anyopaque,
    /// The number of streams being specified.
    mNumberStreams: u32,
    /// An array of UInt32's whose length is specified by mNumberStreams. Each
    /// element of the array corresponds to a stream. A value of 0 means the
    /// stream is not to be enabled. Any other value means the stream is to be used.
    mStreamIsOn: [*]u32,
};
/// The SMPTE timecode is a standard used to assign a timecode to individual frames of media.
pub const SMPTETime = extern struct {
    const Type = enum(u32) {
        type24 = 0,
        type25 = 1,
        type30Drop = 2,
        type30 = 3,
        type2997 = 4,
        type2997Drop = 5,
        type60 = 6,
        type5994 = 7,
        type60Drop = 8,
        type5994Drop = 9,
        type50 = 10,
        type2398 = 11,
    };
    const Flags = enum(u32) {
        unknown = 0,
        /// The full time is valid.
        valid = 1 << 0,
        /// Time is running.
        running = 1 << 1,
    };
    /// The number of subframes in the full message.
    mSubframes: i16,
    /// The number of subframes per frame (typically 80).
    mSubframeDivisor: i16,
    /// The total number of messages received.
    mCounter: u32,
    /// The kind of SMPTE time using the SMPTE time type constants.
    mType: Type,
    /// A set of flags that indicate the SMPTE state.
    mFlags: Flags,
    /// The number of hours in the full message.
    mHours: i16,
    /// The number of minutes in the full message.
    mMinutes: i16,
    /// The number of seconds in the full message.
    mSeconds: i16,
    /// The number of frames in the full message.
    mFrames: i16,
};
/// Holds different representations of the same point in time.
pub const AudioTimeStamp = extern struct {
    const Flags = packed struct(u32) { // if all false, nothing is valid
        sampleTimeValid: bool, // 1 << 0
        hostTimeValid: bool, // 1 << 1
        rateScalarValid: bool, // 1 << 2
        wordClockTimeValid: bool, // 1 << 3
        SMPTETimeValid: bool, // 1 << 4
        _27: u27,
    };
    /// The absolute sample frame time.
    mSampleTime: f64,
    /// The host machine's time base, mach_absolute_time.
    mHostTime: u64,
    /// The ratio of actual host ticks per sample frame to the nominal host ticks per sample frame.
    mRateScalar: f64,
    /// The word clock time.
    mWordClockTime: u64,
    /// The SMPTE time.
    mSMPTETime: SMPTETime,
    /// A set of flags indicating which representations of the time are valid.
    mFlags: Flags,
    /// Pads the structure out to force an even 8 byte alignment.
    mReserved: u32,
};
/// Holds a buffer of audio data.
pub const AudioBuffer = extern struct {
    /// The number of interleaved channels in the buffer.
    mNumberChannels: u32,
    /// The number of bytes in the buffer pointed at by mData.
    mDataByteSize: u32,
    /// A pointer to the buffer of audio data.
    mData: ?[*]u8,
};
pub const AudioBufferList = extern struct {
    /// The number of AudioBuffers in the mBuffers array.
    mNumberBuffers: u32,
    /// A variable length array of AudioBuffers.
    mBuffers: [1]AudioBuffer, // TODO wtf?
};
/// Collects the three parts that identify a specific property together in a struct for easy transmission.
pub const AudioObjectPropertyAddress = extern struct {
    selector: u32,
    scope: u32, // e.g. kAudioObjectPropertyScopeGlobal
    element: u32, // e.g. kAudioObjectPropertyElementMain
};
pub const AudioFormatID = u32;
pub const AudioFormatFlags = u32;
/// Use to describe any constant bit rate (CBR) format that has channels that are the same size.
/// In all fields, a value of 0 indicates that the field is either unknown, not applicable or otherwise
/// is inapproprate for the format and should be ignored. Note that 0 is still a valid value for most
/// formats in the mFormatFlags field
pub const AudioStreamBasicDescription = extern struct {
    /// The number of sample frames per second of the data in the stream.
    mSampleRate: f64,
    /// The AudioFormatID indicating the general kind of data in the stream.
    mFormatID: AudioFormatID,
    /// The AudioFormatFlags for the format indicated by mFormatID.
    mFormatFlags: AudioFormatFlags,
    /// The number of bytes in a packet of data.
    mBytesPerPacket: u32,
    /// The number of sample frames in each packet of data.
    mFramesPerPacket: u32,
    /// The number of bytes in a single sample frame of data.
    mBytesPerFrame: u32,
    /// The number of channels in each frame of data.
    mChannelsPerFrame: u32,
    /// The number of bits of sample data for each channel in a frame of data.
    mBitsPerChannel: u32,
    mReserved: u32 = 0,

    pub fn lpcm(comptime SampleType: anytype, sampleRateHz: f64, channels: u32) @This() {
        var bytesPerSample = @as(u32, @sizeOf(SampleType));
        const formatFlags: u32 = comptime blk: {
            var formatFlags = kAudioFormatFlagIsPacked;
            switch (@typeInfo(SampleType)) {
                .Int => |int| {
                    if (int.signedness == .signed) {
                        formatFlags |= kAudioFormatFlagIsSignedInteger;
                    }
                },
                .Float => {
                    formatFlags |= kAudioFormatFlagIsFloat;
                },
                else => @compileError("samples must be integers or floats"),
            }
            break :blk formatFlags;
        };
        return @This(){
            .mSampleRate = sampleRateHz,
            .mFormatID = kAudioFormatLinearPCM,
            .mFormatFlags = formatFlags,
            .mFramesPerPacket = 1,
            .mChannelsPerFrame = channels,
            .mBitsPerChannel = bytesPerSample * 8,
            .mBytesPerFrame = bytesPerSample * channels * 1,
            .mBytesPerPacket = bytesPerSample * channels * 1 * 1,
        };
    }
};

//
//
// CORE AUDIO FUNCTIONS
//
//

/// Prints to standard out a textural description of the AudioObject
pub extern "CoreAudio" fn AudioObjectShow(AudioObjectID) void;
pub extern "CoreAudio" fn AudioObjectHasProperty(
    AudioObjectID, // inObjectID
    *const AudioObjectPropertyAddress, // inAddress
) bool;
pub extern "CoreAudio" fn AudioObjectIsPropertySettable(
    AudioObjectID, // inObjectID
    *const AudioObjectPropertyAddress, // inAddress
    *bool, // outIsSettable
) OSStatus;
pub extern "CoreAudio" fn AudioObjectGetPropertyDataSize(
    AudioObjectID, // inObjectID
    *const AudioObjectPropertyAddress, // inAddress
    u32, // inQualifierDataSize
    ?[*]u8, // inQualifierData
    *u32, // outDataSize
) OSStatus;
pub extern "CoreAudio" fn AudioObjectGetPropertyData(
    AudioObjectID, // inObjectID
    *const AudioObjectPropertyAddress, // inAddress
    u32, // inQualifierDataSize
    ?[*]u8, // inQualifierData
    *u32, // ioDataSize
    [*]u8, // outData
) OSStatus;
pub extern "CoreAudio" fn AudioObjectAddPropertyListener(
    AudioObjectID, // inObjectID
    *const AudioObjectPropertyAddress, // inAddress
    AudioObjectPropertyListenerProc, // inListener
    ?[*]u8, // inClientData
) OSStatus;
pub extern "CoreAudio" fn AudioObjectRemovePropertyListener(
    AudioObjectID, // inObjectID
    *const AudioObjectPropertyAddress, // inAddress
    AudioObjectPropertyListenerProc, // inListener
    ?[*]u8, // inClientData
) OSStatus;
/// Starts IO for the given AudioDeviceIOProcID.
pub extern "CoreAudio" fn AudioDeviceStart(
    AudioObjectID, // inDevice
    ?AudioDeviceIOProcID, // inProcID
) OSStatus;
/// Starts IO for the given AudioDeviceIOProcID and aligns the IO cycle of the AudioDevice with the given time.
pub extern "CoreAudio" fn AudioDeviceStartAtTime(
    AudioObjectID, // inDevice
    ?AudioDeviceIOProcID, // inProcID
    *AudioTimeStamp, // ioRequestedStartTime
    u32, // inFlags
) OSStatus;
/// Stops IO for the given AudioDeviceIOProcID.
pub extern "CoreAudio" fn AudioDeviceStop(
    AudioObjectID, // inDevice
    ?AudioDeviceIOProcID, // inProcID
) OSStatus;
/// Retrieves the current time from an AudioDevice. Note that the device has to be running.
pub extern "CoreAudio" fn AudioDeviceGetCurrentTime(
    AudioObjectID, // inDevice
    *AudioTimeStamp, // outTime
) OSStatus;
/// Translates the time in the AudioDevice's time base from one representation to another. Note that the device has to be running.
pub extern "CoreAudio" fn AudioDeviceTranslateTime(
    AudioObjectID, // inDevice
    *const AudioTimeStamp, // inTime,
    *AudioTimeStamp, // outTime
) OSStatus;
/// Query an AudioDevice to get a time equal to or later than the given time that is the best time to start IO.
pub extern "CoreAudio" fn AudioDeviceGetNearestStartTime(
    AudioObjectID, // inDevice
    *AudioTimeStamp, // ioRequestedStartTime
    u32, // inFlags
) OSStatus;

//
//
// CORE AUDIO CONSTANTS
//
//

pub const kAudioSystemObjectClassID = StringAs(u32, "asys");
/// The AudioObjectID that always refers to the one and only instance of the AudioSystemObject class.
pub const kAudioObjectSystemObject: AudioObjectID = 1;

pub const kAudioObjectClassID = StringAs(u32, "aobj");
pub const kAudioObjectUnknown: u32 = 0;

pub const kAudioDeviceClassID = StringAs(u32, "adev");
pub const kAudioDeviceUnknown = kAudioObjectUnknown;

//
//
// CORE AUDIO PROPERTY SCOPES
//
//

pub const kAudioObjectPropertyScopeGlobal = StringAs(u32, "glob");
pub const kAudioObjectPropertyScopeInput = StringAs(u32, "inpt");
pub const kAudioObjectPropertyScopeOutput = StringAs(u32, "outp");
pub const kAudioObjectPropertyScopePlayThrough = StringAs(u32, "ptru");

pub const kAudioDevicePropertyScopeOutput = kAudioObjectPropertyScopeOutput;

//
//
// CORE AUDIO PROPERTY SELECTORS
//
//

pub const kAudioHardwarePropertyDevices = StringAs(u32, "dev#");
pub const kAudioHardwarePropertyDefaultOutputDevice = StringAs(u32, "dOut");

pub const kAudioDevicePropertyDeviceName = StringAs(u32, "name");

/// An array of UInt32s whose values are the item IDs for the currently selected data sources.
pub const kAudioDevicePropertyDataSource = StringAs(u32, "ssrc");

//
//
// CORE AUDIO PROPERTY ELEMENTS
//
//

pub const kAudioObjectPropertyElementMain: u32 = 0;

//
//
// CORE AUDIO AUDIO FORMATS
//
//

pub const kAudioFormatLinearPCM = StringAs(AudioFormatID, "lpcm");

pub const kAudioFormatFlagIsFloat: AudioFormatFlags = 1 << 0;
pub const kAudioFormatFlagIsBigEndian: AudioFormatFlags = 1 << 1;
pub const kAudioFormatFlagIsSignedInteger: AudioFormatFlags = 1 << 2;
pub const kAudioFormatFlagIsPacked: AudioFormatFlags = 1 << 3;
pub const kAudioFormatFlagIsAlignedHigh: AudioFormatFlags = 1 << 4;
pub const kAudioFormatFlagIsNonInterleaved: AudioFormatFlags = 1 << 5;
pub const kAudioFormatFlagIsNonMixable: AudioFormatFlags = 1 << 6;
pub const kAudioFormatFlagsAreAllClear: AudioFormatFlags = 0x80000000;

//
//
// CORE AUDIO ERRORS
//
//

pub const CoreAudioErrors = error{
    /// The function call completed successfully.
    Success,
    /// The function call requires that the hardware be running but it isn't.
    HardwareNotRunning,
    /// The function call failed while doing something that doesn't provide any error messages.
    Unspecified,
    /// The AudioObject doesn't know about the property at the given address.
    UnknownProperty,
    /// An improperly sized buffer was provided when accessing the data of a property.
    BadPropertySize,
    /// The requested operation couldn't be completed.
    IllegalOperation,
    /// The AudioObjectID passed to the function doesn't map to a valid AudioObject.
    BadObject,
    /// The AudioObjectID passed to the function doesn't map to a valid AudioDevice.
    BadDevice,
    /// The AudioObjectID passed to the function doesn't map to a valid AudioStream.
    BadStream,
    /// The AudioObject doesn't support the requested operation.
    UnsupportedOperation,
    /// The AudioObject isn't ready to do the requested operation.
    NotReady,
    /// The AudioStream doesn't support the requested format.
    UnsupportedFormat,
    /// The requested operation can't be completed because the process doesn't have permission.
    PermissionsError,
    /// Unimplemented core routine.
    UnimplementedError,
    /// File not found.
    FileNotFound,
    /// File cannot be opened due to either file, directory, or sandbox permissions.
    FilePermissionError,
    /// File cannot be opened because too many files are already open.
    TooManyFilesOpen,
    /// File cannot be opened because the specified path is malformed.
    BadFilePath,
    /// Error in user parameter list.
    ParamError,
    /// Not enough room in heap zone.
    MemFullError,
};

pub fn CoreAudioError(status: OSStatus) !void {
    switch (status) {
        // CoreAudio/AudioHardwareBase.h
        0 => return,
        StringAs(OSStatus, "stop") => return CoreAudioErrors.HardwareNotRunning,
        StringAs(OSStatus, "what") => return CoreAudioErrors.Unspecified,
        StringAs(OSStatus, "who?") => return CoreAudioErrors.UnknownProperty,
        StringAs(OSStatus, "!siz") => return CoreAudioErrors.BadPropertySize,
        StringAs(OSStatus, "nope") => return CoreAudioErrors.IllegalOperation,
        StringAs(OSStatus, "!obj") => return CoreAudioErrors.BadObject,
        StringAs(OSStatus, "!dev") => return CoreAudioErrors.BadDevice,
        StringAs(OSStatus, "!str") => return CoreAudioErrors.BadStream,
        StringAs(OSStatus, "unop") => return CoreAudioErrors.UnsupportedOperation,
        StringAs(OSStatus, "nrdy") => return CoreAudioErrors.NotReady,
        StringAs(OSStatus, "!dat") => return CoreAudioErrors.UnsupportedFormat,
        StringAs(OSStatus, "!hog") => return CoreAudioErrors.PermissionsError,
        // CoreAudioTypes/CoreAudioBaseTypes.h
        -4 => return CoreAudioErrors.UnimplementedError,
        -43 => return CoreAudioErrors.FileNotFound,
        -54 => return CoreAudioErrors.FilePermissionError,
        -42 => return CoreAudioErrors.TooManyFilesOpen,
        StringAs(OSStatus, "!pth") => return CoreAudioErrors.BadFilePath,
        -50 => return CoreAudioErrors.ParamError,
        -108 => return CoreAudioErrors.MemFullError,
        else => unreachable,
    }
}

pub fn getPropertyDataBuffer(object: AudioObjectID, addr: *const AudioObjectPropertyAddress, buffer: []u8) ![]u8 {
    var length: u32 = @intCast(buffer.len);
    try CoreAudioError(AudioObjectGetPropertyData(object, addr, 0, null, &length, buffer.ptr));
    return buffer[0..length];
}

pub fn getPropertyDataStruct(comptime Type: anytype, object: AudioObjectID, addr: *const AudioObjectPropertyAddress) !Type {
    comptime std.debug.assert(@typeInfo(Type).Struct.layout != .Auto);
    var obj: [1]Type = undefined;
    var buffer = std.mem.sliceAsBytes(obj[0..]);
    var length: u32 = @intCast(buffer.len);
    try CoreAudioError(AudioObjectGetPropertyData(object, addr, 0, null, &length, buffer.ptr));
    return obj[0];
}

pub fn getPropertyDataSize(object: AudioObjectID, addr: *const AudioObjectPropertyAddress) !u32 {
    var dataSize: u32 = 0;
    try CoreAudioError(AudioObjectGetPropertyDataSize(object, addr, 0, null, &dataSize));
    return dataSize;
}

pub fn addPropertyListener(
    object: AudioObjectID,
    addr: *const AudioObjectPropertyAddress,
    callback: AudioObjectPropertyListenerProc,
) !void {
    try CoreAudioError(
        AudioObjectAddPropertyListener(
            object,
            addr,
            callback,
            null,
        ),
    );
}

pub fn removePropertyListener(
    object: AudioObjectID,
    addr: *const AudioObjectPropertyAddress,
    callback: AudioObjectPropertyListenerProc,
) !void {
    try CoreAudioError(
        AudioObjectRemovePropertyListener(
            object,
            addr,
            callback,
            null,
        ),
    );
}

pub inline fn StringAs(comptime As: type, comptime code: *const [4:0]u8) As {
    return @byteSwap(@as(*As, @alignCast(@constCast(@ptrCast(code)))).*);
}

pub const AudioSystemObject = struct {
    /// The current default output AudioDevice, if there is one.
    pub fn queryDefaultOutputDevice() !AudioDevice {
        const addr = AudioObjectPropertyAddress{
            .selector = kAudioHardwarePropertyDefaultOutputDevice,
            .scope = kAudioObjectPropertyScopeGlobal,
            .element = kAudioObjectPropertyElementMain,
        };
        return try getPropertyDataStruct(AudioDevice, kAudioObjectSystemObject, &addr);
    }

    /// All AudioDevices currently available to the system.
    pub fn queryDevicesAlloc(allocator: std.mem.Allocator) ![]AudioDevice {
        const addr = AudioObjectPropertyAddress{
            .selector = kAudioHardwarePropertyDevices,
            .scope = kAudioObjectPropertyScopeGlobal,
            .element = kAudioObjectPropertyElementMain,
        };
        var dataSize = try getPropertyDataSize(kAudioObjectSystemObject, &addr);
        const deviceCount = dataSize / @sizeOf(AudioDeviceID);
        const ids = try allocator.alloc(AudioDevice, deviceCount);
        _ = try getPropertyDataBuffer(kAudioObjectSystemObject, &addr, std.mem.sliceAsBytes(ids));
        return ids;
    }

    pub fn addDefaultOutputDeviceCallback(callback: AudioObjectPropertyListenerProc) !void {
        const addr = AudioObjectPropertyAddress{
            .selector = kAudioHardwarePropertyDefaultOutputDevice,
            .scope = kAudioObjectPropertyScopeGlobal,
            .element = kAudioObjectPropertyElementMain,
        };
        try addPropertyListener(kAudioObjectSystemObject, &addr, callback);
    }
};

pub const AudioDevice = extern struct {
    comptime {
        if (@sizeOf(AudioDevice) != @sizeOf(AudioDeviceID) or @alignOf(AudioDevice) != @alignOf(AudioDeviceID))
            @compileError("Implementation assumes AudioDevice struct is castable to AudioDeviceID");
    }
    id: AudioDeviceID,

    pub fn unknown() AudioDevice {
        return AudioDevice{ .id = kAudioDeviceUnknown };
    }

    pub inline fn isUnknown(self: @This()) bool {
        return self.id == kAudioDeviceUnknown;
    }

    pub fn queryDeviceName(self: @This(), buffer: []u8) ![]u8 {
        const addr = AudioObjectPropertyAddress{
            .selector = kAudioDevicePropertyDeviceName,
            .scope = kAudioDevicePropertyScopeOutput,
            .element = kAudioObjectPropertyElementMain,
        };
        return try getPropertyDataBuffer(self.id, &addr, buffer);
    }
};
