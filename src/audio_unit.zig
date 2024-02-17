const std = @import("std");
const CoreAudio = @import("core_audio.zig");

pub const OSStatus = CoreAudio.OSStatus;
pub const StringAs = CoreAudio.StringAs;
pub const AudioTimeStamp = CoreAudio.AudioTimeStamp;
pub const AudioBufferList = CoreAudio.AudioBufferList;
pub const AudioStreamBasicDescription = CoreAudio.AudioStreamBasicDescription;

pub const OSType = u32;
pub const AudioComponent = *opaque {};
pub const AudioComponentDescription = extern struct {
    /// A 4-char code identifying the generic type of an audio component.
    componentType: OSType,
    /// A 4-char code identifying the a specific individual component. type/subtype/manufacturer triples must be globally unique.
    componentSubtype: OSType,
    /// Vendor identification.
    componentManufacturer: OSType,
    /// Must be set to zero unless a known specific value is requested.
    componentFlags: u32,
    /// Must be set to zero unless a known specific value is requested.
    componentFlagsMask: u32,
};
pub const AudioComponentInstance = *opaque {};
pub const AudioUnitRenderActionFlags = packed struct(u32) {
    _2: u2 = 0,
    preRender: bool = false, // 1U << 2
    postRender: bool = false, // 1U << 3
    outputIsSilence: bool = false, // 1U << 4
    preflight: bool = false, // 1U << 5
    render: bool = false, // 1U << 6
    complete: bool = false, // 1U << 7
    postRenderError: bool = false, // 1U << 8
    doNotCheckRenderArgs: bool = false, // 1U << 9
    _22: u22 = 0,
};
pub const AudioUnitID = AudioComponentInstance;
pub const AURenderCallback = *const fn (
    [*]u8, // inRefCon
    *AudioUnitRenderActionFlags, // ioActionFlags
    *const AudioTimeStamp, // inTimeStamp
    u32, // inBusNumber
    u32, // inNumberFrames
    ?*AudioBufferList, // ioData
) OSStatus;
pub const AURenderCallbackStruct = extern struct {
    inputProc: ?AURenderCallback,
    inputProcRefCon: ?[*]u8,
};

//
//
// AUDIO UNIT TYPES, SUBTYPES, AND MANUFACTURERS
//
//

/// An output unit can be used standalone or as part of an AUGraph or AVAudioEngine. Apple provides
/// a number of output units that interface directly with an audio device.
pub const kAudioUnitType_Output = StringAs(u32, "auou");

/// (a.k.a. "AUHAL")  The audio unit that interfaces to any audio device. The user specifies which
/// audio device to track. Bus 0 is used to send audio output to the device; bus 1 is used to receive
/// audio input from the device. Available on macOS only.
pub const kAudioUnitSubType_HALOutput = StringAs(u32, "ahal");
/// A specialization of AUHAL that is used to track the user's selection of the default device as set
/// in the Sound Prefs. Available on macOS only.
pub const kAudioUnitSubType_DefaultOutput = StringAs(u32, "def ");
/// A specialization of AUHAL that is used to track the user's selection of the device to use for
/// sound effects, alerts and other UI sounds. Available on macOS only.
pub const kAudioUnitSubType_SystemOutput = StringAs(u32, "sys ");

/// The unique ID used to identifier audio units provided by Apple Inc.
pub const kAudioUnitManufacturer_Apple = StringAs(u32, "appl");

pub const kAudioOutputUnitProperty_CurrentDevice: AudioUnitPropertyID = 2000;

/// The context for audio unit characteristics that apply to the audio unit as a whole.
pub const kAudioUnitScope_Global: AudioUnitScope = 0;
/// The context for audio data coming into an audio unit.
pub const kAudioUnitScope_Input: AudioUnitScope = 1;
/// The context for audio data leaving an audio unit.
pub const kAudioUnitScope_Output: AudioUnitScope = 2;

/// # AudioStreamBasicDescription
/// An AudioStreamBasicDescription is used to specify the basic format for an audio data path. For instance,
/// 2 channels, 44.1KHz, Float32 linear pcm. The value can be both set and retrieved from an I/O element
pub const kAudioUnitProperty_StreamFormat: AudioUnitPropertyID = 8;

/// # AURenderCallbackStruct
/// This is used to provide the audio unit with input on the specified element (input bus) with audio data
/// from the provided callback.
pub const kAudioUnitProperty_SetRenderCallback: AudioUnitPropertyID = 23;

//
//
// AUDIO UNIT ERRORS
//
//
pub const AudioUnitErrors = error{
    InvalidProperty,
    InvalidParameter,
    InvalidElement,
    NoConnection,
    FailedInitialization,
    TooManyFramesToProcess,
    InvalidFile,
    UnknownFileType,
    FileNotSpecified,
    FormatNotSupported,
    Uninitialized,
    InvalidScope,
    PropertyNotWritable,
    CannotDoInCurrentContext,
    InvalidPropertyValue,
    PropertyNotInUse,
    Initialized,
    InvalidOfflineRender,
    Unauthorized,
    MIDIOutputBufferFull,
    InstanceTimedOut,
    InstanceInvalidated,
    RenderTimeout,
    ExtensionNotFound,
    InvalidParameterValue,
    InvalidFilePath,
    MissingKey,
    ComponentManagerNotSupported,
};

pub fn AudioUnitError(status: OSStatus) !void {
    switch (status) {
        0 => return,
        -10879 => return AudioUnitErrors.InvalidProperty,
        -10878 => return AudioUnitErrors.InvalidParameter,
        -10877 => return AudioUnitErrors.InvalidElement,
        -10876 => return AudioUnitErrors.NoConnection,
        -10875 => return AudioUnitErrors.FailedInitialization,
        -10874 => return AudioUnitErrors.TooManyFramesToProcess,
        -10871 => return AudioUnitErrors.InvalidFile,
        -10870 => return AudioUnitErrors.UnknownFileType,
        -10869 => return AudioUnitErrors.FileNotSpecified,
        -10868 => return AudioUnitErrors.FormatNotSupported,
        -10867 => return AudioUnitErrors.Uninitialized,
        -10866 => return AudioUnitErrors.InvalidScope,
        -10865 => return AudioUnitErrors.PropertyNotWritable,
        -10863 => return AudioUnitErrors.CannotDoInCurrentContext,
        -10851 => return AudioUnitErrors.InvalidPropertyValue,
        -10850 => return AudioUnitErrors.PropertyNotInUse,
        -10849 => return AudioUnitErrors.Initialized,
        -10848 => return AudioUnitErrors.InvalidOfflineRender,
        -10847 => return AudioUnitErrors.Unauthorized,
        -66753 => return AudioUnitErrors.MIDIOutputBufferFull,
        -66754 => return AudioUnitErrors.InstanceTimedOut,
        -66749 => return AudioUnitErrors.InstanceInvalidated,
        -66745 => return AudioUnitErrors.RenderTimeout,
        -66744 => return AudioUnitErrors.ExtensionNotFound,
        -66743 => return AudioUnitErrors.InvalidParameterValue,
        -66742 => return AudioUnitErrors.InvalidFilePath,
        -66741 => return AudioUnitErrors.MissingKey,
        -66740 => return AudioUnitErrors.ComponentManagerNotSupported,
        else => {
            std.debug.print("{d}\n", .{status});
            unreachable;
        },
    }
}

//
//
// AUDIO UNIT FUNCTIONS
//
//

pub const AudioUnitPropertyID = u32;
pub const AudioUnitScope = u32;
pub const AudioUnitElement = u32;

/// Finds an audio component.
pub extern "AudioUnit" fn AudioComponentFindNext(
    ?AudioComponent, // inComponent
    *const AudioComponentDescription, // inDesc
) ?AudioComponent;
/// Creates an audio component instance. This must be matched with a correspond call to #AudioComponentFindNext.
pub extern "AudioUnit" fn AudioComponentInstanceNew(
    AudioComponent, // inComponent
    *AudioComponentInstance, // outInstance
) OSStatus;
/// Disposes of an audio component instance. It will deallocate any resources that the instance was using.
pub extern "AudioUnit" fn AudioComponentInstanceDispose(
    AudioComponentInstance, // inInstance
) OSStatus;
/// Sets the value of a specified property. Property values for audio units are always passed by reference.
pub extern "AudioUnit" fn AudioUnitSetProperty(
    AudioUnitID, // inUnit
    AudioUnitPropertyID, // inID
    AudioUnitScope, // inScope
    AudioUnitElement, // inElement
    ?[*]const u8, // inData
    u32, // inDataSize
) OSStatus;
/// Retrieves the value of a specified property. Property values for audio units are always passed by reference.
pub extern "AudioUnit" fn AudioUnitGetProperty(
    AudioUnitID, // inUnit
    AudioUnitPropertyID, // inID
    AudioUnitScope, // inScope
    AudioUnitElement, // inElement
    [*]u8, // outData
    *u32, // ioDataSize
) OSStatus;
/// Initialize an audio unit. Major state of an audio unit (such as its I/O formats, memory allocations)
/// cannot be changed while an audio unit is initialized.
pub extern "AudioUnit" fn AudioUnitInitialize(
    AudioUnitID, // inUnit
) OSStatus;
/// Uninitialize an audio unit. Once an audio unit has been initialized, to change its state, the
/// audio unit should be uninitialized. The caller can then reconfigure the audio unit to match the
/// new environment (for instance, the sample rate to process audio is different than it was) and
/// then re-initialize the audio unit when those changes have been applied.
pub extern "AudioUnit" fn AudioUnitUninitialize(
    AudioUnitID, // inUnit
) OSStatus;
pub extern "AudioUnit" fn AudioOutputUnitStart(
    AudioUnitID, // ci
) OSStatus;
pub extern "AudioUnit" fn AudioOutputUnitStop(
    AudioUnitID, // ci
) OSStatus;

pub const AudioUnit = struct {
    id: AudioUnitID,

    pub fn newAUHAL() !AudioUnit {
        const desc = AudioComponentDescription{
            .componentType = kAudioUnitType_Output,
            .componentSubtype = kAudioUnitSubType_HALOutput,
            .componentManufacturer = kAudioUnitManufacturer_Apple,
            .componentFlags = 0,
            .componentFlagsMask = 0,
        };
        const component = AudioComponentFindNext(null, &desc) orelse return error.NoSuchAudioComponent;
        var id: AudioUnitID = undefined;
        try AudioUnitError(AudioComponentInstanceNew(component, &id));
        return AudioUnit{ .id = id };
    }

    /// Must be called exactly once for each AudioUnit when no longer needed.
    pub fn dispose(self: @This()) !void {
        try AudioUnitError(AudioComponentInstanceDispose(self.id));
    }

    /// Initialize the AudioUnit. It is ready to start.
    /// To make major changes to the AudioUnit configuration, first uninitialize it.
    pub fn initialize(self: @This()) !void {
        try AudioUnitError(AudioUnitInitialize(self.id));
    }

    /// Uninitialize the AudioUnit. After making major configuration changes, re-initialize it.
    pub fn uninitialize(self: @This()) !void {
        try AudioUnitError(AudioUnitUninitialize(self.id));
    }

    /// Set the AudioUnit's current output device.
    pub fn setOutputDevice(self: @This(), device: CoreAudio.AudioDevice) !void {
        try AudioUnitError(AudioUnitSetProperty(
            self.id,
            kAudioOutputUnitProperty_CurrentDevice,
            kAudioUnitScope_Global,
            0, // bus 0 is used to send output to AUHAL
            @ptrCast(&device),
            @sizeOf(@TypeOf(device)),
        ));
    }

    pub fn setInputStreamFormat(self: @This(), asbd: CoreAudio.AudioStreamBasicDescription) !void {
        try AudioUnitError(AudioUnitSetProperty(
            self.id,
            kAudioUnitProperty_StreamFormat,
            kAudioUnitScope_Input,
            0,
            @ptrCast(&asbd),
            @sizeOf(@TypeOf(asbd)),
        ));
    }

    pub fn setRenderCallback(self: @This(), func: ?AURenderCallback) !void {
        const callback = AURenderCallbackStruct{
            .inputProc = func,
            .inputProcRefCon = null,
        };
        try AudioUnitError(AudioUnitSetProperty(
            self.id,
            kAudioUnitProperty_SetRenderCallback,
            kAudioUnitScope_Global,
            0,
            @ptrCast(&callback),
            @sizeOf(AURenderCallbackStruct),
        ));
    }

    /// Start an I/O AudioUnit and, by extension, the processing graph it is connected to.
    pub fn start(self: @This()) !void {
        try AudioUnitError(AudioOutputUnitStart(self.id));
    }

    /// Stop an I/O AudioUnit and, by extension, the processing graph it is connected to.
    pub fn stop(self: @This()) !void {
        try AudioUnitError(AudioOutputUnitStop(self.id));
    }
};
