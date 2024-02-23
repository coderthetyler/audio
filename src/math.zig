const std = @import("std");

pub fn fft(inp: []Complex) void {
    fft_core(inp, -1);
}

pub fn ifft(inp: []Complex) void {
    fft_core(inp, 1);
}

/// radix-2 in-place iterative FFT.
/// works only for inputs of 2^x size, for integer x
fn fft_core(inp: []Complex, direction: f32) void {
    bitReversePermute(Complex, inp);
    var N: usize = 2;
    while (N <= inp.len) {
        var wInc = Complex.euler(direction * std.math.tau / @as(f32, @floatFromInt(N)));
        const halfN = @divExact(N, 2);
        var p: usize = 0;
        while (p < inp.len) { // p iterates over N/2 subproblems
            var w = Complex.of(1);
            for (0..halfN) |k| {
                const a = inp[p + k];
                const b = inp[p + k + halfN].times(w);
                inp[p + k] = a.plus(b);
                inp[p + k + halfN] = a.minus(b);
                w = w.times(wInc);
            }
            p += N;
        }
        N <<= 1;
    }
    if (direction == 1) { // inverse
        const fLen: f32 = @floatFromInt(inp.len);
        for (inp) |*i| {
            i.real /= fLen;
            i.imag /= fLen;
        }
    }
}

/// X_k = sum_{n=0}^{N-1} x_n e^{iTkn/N}, k in [0,N-1]
fn dft(inp: []Complex, out: []Complex) void {
    const N = inp.len;
    for (0..N) |k| {
        var Xk = Complex.zero();
        for (0..N) |n| {
            const angle = -1.0 * std.math.tau / @as(f32, @floatFromInt(N)) * @as(f32, @floatFromInt(n)) * @as(f32, @floatFromInt(k));
            Xk = Xk.plus(inp[n].times(Complex.euler(angle)));
        }
        out[k] = Xk;
    }
}

pub const Complex = struct {
    real: f32,
    imag: f32,

    pub inline fn of(real: f32) Complex {
        return Complex{ .real = real, .imag = 0 };
    }

    pub inline fn plus(self: @This(), that: @This()) Complex {
        return Complex{
            .real = self.real + that.real,
            .imag = self.imag + that.imag,
        };
    }

    pub inline fn minus(self: @This(), that: @This()) Complex {
        return Complex{
            .real = self.real - that.real,
            .imag = self.imag - that.imag,
        };
    }

    pub inline fn zero() Complex {
        return Complex{ .real = 0, .imag = 0 };
    }

    pub inline fn scale(self: @This(), factor: f32) Complex {
        return Complex{
            .real = self.real * factor,
            .imag = self.imag * factor,
        };
    }

    pub inline fn times(self: @This(), that: @This()) Complex {
        return Complex{
            .real = self.real * that.real - self.imag * that.imag,
            .imag = self.real * that.imag + self.imag * that.real,
        };
    }

    pub inline fn euler(angle: f32) Complex {
        return Complex{
            .real = @cos(angle),
            .imag = @sin(angle),
        };
    }

    pub inline fn magnitude(self: @This()) f32 {
        return @sqrt(self.real * self.real + self.imag * self.imag);
    }

    /// a value between -pi & pi (radians)
    pub inline fn wrapped_phase(self: @This()) f32 {
        return std.math.atan2(f32, self.imag, self.real);
    }

    pub inline fn conjugate(self: @This()) Complex {
        return Complex{
            .real = self.real,
            .imag = -self.imag,
        };
    }

    pub inline fn polar(mag: f32, angle: f32) Complex {
        return Complex{
            .real = mag * @cos(angle),
            .imag = mag * @sin(angle),
        };
    }

    pub inline fn round(self: @This(), factor: f32) Complex {
        return Complex{
            .real = @round(self.real * factor) / factor,
            .imag = @round(self.imag * factor) / factor,
        };
    }
};

test "fft == dft?" {
    inline for (1..10) |logN| {
        const N = comptime pow(2, logN);
        const F: f32 = 2;
        const S: f32 = 8;

        var dftInput: [N]Complex = undefined;
        var fftBuffer: [N]Complex = undefined;

        // sample sin(tau * F) at frequency S for N points
        for (0..N) |i| {
            const idx = @as(f32, @floatFromInt(i)) / S;
            const sample = @sin(std.math.tau * idx * F) + 0.5;
            dftInput[i] = Complex.of(sample);
            fftBuffer[i] = Complex.of(sample);
        }

        // compute dft & fft
        var dftOutput: [N]Complex = undefined;
        dft(&dftInput, &dftOutput);
        fft(&fftBuffer);

        // round for approximate comparison
        const factor: f32 = 10000.0 / @as(f32, @floatFromInt(N));
        for (0..N) |i| {
            dftOutput[i] = dftOutput[i].round(factor);
            fftBuffer[i] = fftBuffer[i].round(factor);
        }

        // compare outputs
        try std.testing.expectEqualSlices(Complex, &dftOutput, &fftBuffer);
    }
}

test "ifft(fft(f(t))) = f(t)" {
    const N = 1024;
    const F: f32 = 2;
    const S: f32 = 8;

    var fftBuffer: [N]Complex = undefined;
    var originalInput: [N]Complex = undefined;

    // sample sin(tau * F) at frequency S for N points
    for (0..N) |i| {
        const idx = @as(f32, @floatFromInt(i)) / S;
        const sample = @sin(std.math.tau * idx * F) + 0.5;
        fftBuffer[i] = Complex.of(sample);
        originalInput[i] = fftBuffer[i];
    }

    // fftBuffer = ifft(fft(fftBuffer))
    fft(&fftBuffer);
    ifft(&fftBuffer);

    // round results to a few decimal places to elide fp errors
    const factor: f32 = 10000.0;
    for (0..N) |i| {
        originalInput[i] = originalInput[i].round(factor);
        fftBuffer[i] = fftBuffer[i].round(factor);
    }

    // compare outputs
    try std.testing.expectEqualSlices(Complex, &originalInput, &fftBuffer);
}

inline fn log2(number: usize) u6 {
    var count: u6 = 0;
    while (number >= @as(usize, 1) << count) {
        count += 1;
    }
    return count - 1;
}

inline fn bitReverse(number: usize, bits: u6) usize {
    return @bitReverse(number) >> @intCast(@bitSizeOf(usize) - @as(usize, @intCast(bits)));
}

inline fn bitReversePermute(comptime T: anytype, inp: []T) void {
    const bits = log2(inp.len);
    for (0..inp.len) |i| {
        const j = bitReverse(i, bits);
        if (j > i) {
            const temp = inp[i];
            inp[i] = inp[j];
            inp[j] = temp;
        }
    }
}

// slow
inline fn pow(base: usize, power: usize) usize {
    var count: usize = 0;
    var result: usize = 1;
    while (count < power) {
        result *= base;
        count += 1;
    }
    return result;
}

test {
    const expectEqual = std.testing.expectEqual;
    const expectEqualSlices = std.testing.expectEqualSlices;

    try expectEqual(@as(usize, 1), pow(2, 0));
    try expectEqual(@as(usize, 2), pow(2, 1));
    try expectEqual(@as(usize, 4), pow(2, 2));
    try expectEqual(@as(usize, 8), pow(2, 3));
    try expectEqual(@as(usize, 16), pow(2, 4));

    try expectEqual(@as(u6, 0), log2(1));
    try expectEqual(@as(u6, 1), log2(2));
    try expectEqual(@as(u6, 2), log2(4));
    try expectEqual(@as(u6, 3), log2(8));
    try expectEqual(@as(u6, 4), log2(16));
    try expectEqual(@as(u6, 5), log2(32));

    const bits = log2(8);
    try expectEqual(@as(usize, 0), bitReverse(0, bits));
    try expectEqual(@as(usize, 4), bitReverse(1, bits));
    try expectEqual(@as(usize, 2), bitReverse(2, bits));
    try expectEqual(@as(usize, 6), bitReverse(3, bits));
    try expectEqual(@as(usize, 1), bitReverse(4, bits));
    try expectEqual(@as(usize, 5), bitReverse(5, bits));
    try expectEqual(@as(usize, 3), bitReverse(6, bits));
    try expectEqual(@as(usize, 7), bitReverse(7, bits));

    var arr = [_]u8{ 0, 1, 2, 3, 4, 5, 6, 7 };
    bitReversePermute(u8, &arr);
    try expectEqualSlices(u8, &[_]u8{ 0, 4, 2, 6, 1, 5, 3, 7 }, &arr);
}
