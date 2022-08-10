const std = @import("std");
const chunkMod = @import("chunk.zig");
const Chunk = chunkMod.Chunk;
const OpCode = chunkMod.OpCode;
const valueMod = @import("value.zig");

pub const stdout = std.io.getStdOut().writer();

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) !void {
    try stdout.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.count) {
        offset = try disassembleInstruction(chunk, offset);
    }
}

fn disassembleInstruction(chunk: *Chunk, offset: usize) !usize {
    try stdout.print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines.?[offset] == chunk.lines.?[offset - 1]) {
        try stdout.writeAll("   | ");
    } else {
        try stdout.print("{d:>4} ", .{chunk.lines.?[offset]});
    }
    const instruction = @intToEnum(OpCode, chunk.code.?[offset]);
    switch (instruction) {
        .op_return => return simpleInstruction("OP_RETURN", offset),
        .op_constant => return constantInstruction("OP_CONSTANT", chunk, offset),
    }
}

fn simpleInstruction(comptime name: []const u8, offset: usize) !usize {
    try stdout.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(comptime name: []const u8, chunk: *Chunk, offset: usize) !usize {
    const constant = chunk.code.?[offset + 1];
    try stdout.print("{s:<16} {d:4} '", .{ name, constant });
    try valueMod.printValue(stdout, chunk.constants.values.?[constant]);
    try stdout.writeAll("'\n");
    return offset + 2;
}
