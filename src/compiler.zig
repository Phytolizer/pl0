const std = @import("std");
const VM = @import("vm.zig").VM;
const scannerMod = @import("scanner.zig");
const Scanner = scannerMod.Scanner;
const Token = scannerMod.Token;
const TokenKind = scannerMod.TokenKind;
const chunkMod = @import("chunk.zig");
const Chunk = chunkMod.Chunk;
const OpCode = chunkMod.OpCode;
const Value = @import("value.zig").Value;
const common = @import("common.zig");
const debug = @import("debug.zig");

const Precedence = enum(u8) {
    prec_none,
    prec_assignment,
    prec_or,
    prec_and,
    prec_equality,
    prec_comparison,
    prec_term,
    prec_factor,
    prec_unary,
    prec_call,
    prec_primary,
};

const ParseError = std.mem.Allocator.Error || std.fmt.ParseFloatError;

const ParseFn = ?fn (parser: *Parser) ParseError!void;

const ParseRule = struct {
    prefix: ParseFn = null,
    infix: ParseFn = null,
    precedence: Precedence = .prec_none,
};

fn getRule(kind: TokenKind) ParseRule {
    return switch (kind) {
        .tk_left_paren => .{
            .prefix = Parser.grouping,
            .precedence = .prec_none,
        },
        .tk_minus => .{
            .prefix = Parser.unary,
            .infix = Parser.binary,
            .precedence = .prec_term,
        },
        .tk_plus => .{
            .infix = Parser.binary,
            .precedence = .prec_term,
        },
        .tk_slash => .{
            .infix = Parser.binary,
            .precedence = .prec_factor,
        },
        .tk_star => .{
            .infix = Parser.binary,
            .precedence = .prec_factor,
        },
        .tk_number => .{
            .prefix = Parser.number,
        },
        else => .{},
    };
}

const Parser = struct {
    scanner: *Scanner,
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,
    compiler: ?*Compiler,

    const Self = @This();

    pub fn init(scanner: *Scanner) Self {
        return Self{
            .scanner = scanner,
            .current = undefined,
            .previous = undefined,
            .hadError = false,
            .panicMode = false,
            .compiler = null,
        };
    }

    pub fn advance(self: *Self) void {
        self.previous = self.current;

        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.kind != .tk_error) {
                break;
            }

            self.errorAtCurrent(self.current.text);
        }
    }

    fn errorAtCurrent(self: *Self, message: []const u8) void {
        self.errorAt(&self.current, message);
    }

    pub fn emitError(self: *Self, message: []const u8) void {
        self.errorAt(&self.previous, message);
    }

    fn errorAt(self: *Self, token: *Token, message: []const u8) void {
        if (self.panicMode) {
            return;
        }
        self.panicMode = true;
        std.debug.print("[line {d}] Error", .{token.line});

        if (token.kind == .tk_eof) {
            std.debug.print(" at end", .{});
        } else if (token.kind == .tk_error) {
            // Nothing.
        } else {
            std.debug.print(" at '{s}'", .{token.text});
        }

        std.debug.print(": {s}\n", .{message});
        self.hadError = true;
    }

    pub fn consume(self: *Self, kind: TokenKind, comptime message: []const u8) void {
        if (self.current.kind == kind) {
            self.advance();
            return;
        }
        self.errorAtCurrent(message);
    }

    pub fn expression(self: *Self) ParseError!void {
        try self.parsePrecedence(.prec_assignment);
    }

    fn number(self: *Self) ParseError!void {
        const value = try std.fmt.parseFloat(f64, self.previous.text);
        try self.compiler.?.emitConstant(value);
    }

    fn grouping(self: *Self) ParseError!void {
        try self.expression();
        self.consume(.tk_right_paren, "Expect ')' after expression.");
    }

    fn unary(self: *Self) ParseError!void {
        const operatorKind = self.previous.kind;

        try self.parsePrecedence(.prec_unary);

        switch (operatorKind) {
            .tk_minus => try self.compiler.?.emitOp(.op_negate),
            else => unreachable,
        }
    }

    fn binary(self: *Self) ParseError!void {
        const operatorKind = self.previous.kind;
        const rule = getRule(operatorKind);
        try self.parsePrecedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

        switch (operatorKind) {
            .tk_plus => try self.compiler.?.emitOp(.op_add),
            .tk_minus => try self.compiler.?.emitOp(.op_subtract),
            .tk_star => try self.compiler.?.emitOp(.op_multiply),
            .tk_slash => try self.compiler.?.emitOp(.op_divide),
            else => unreachable,
        }
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) ParseError!void {
        self.advance();
        const prefixRule = getRule(self.previous.kind).prefix;
        if (prefixRule == null) {
            self.emitError("Expect expression.");
            return;
        }

        try prefixRule.?(self);

        while (@enumToInt(precedence) <= @enumToInt(getRule(self.current.kind).precedence)) {
            self.advance();
            const infixRule = getRule(self.previous.kind).infix;
            try infixRule.?(self);
        }
    }
};

const Compiler = struct {
    compilingChunk: *Chunk,
    parser: *Parser,

    const Self = @This();

    pub fn init(compilingChunk: *Chunk, parser: *Parser) Self {
        return Self{
            .compilingChunk = compilingChunk,
            .parser = parser,
        };
    }

    pub fn emitByte(self: *Self, byte: u8) !void {
        try self.compilingChunk.write(byte, self.parser.previous.line);
    }

    pub fn emitOp(self: *Self, op: OpCode) !void {
        try self.emitByte(@enumToInt(op));
    }

    pub fn currentChunk(self: *Self) *Chunk {
        return self.compilingChunk;
    }

    fn emitReturn(self: *Self) !void {
        try self.emitOp(.op_return);
    }

    pub fn end(self: *Self) !void {
        try self.emitReturn();
        if (common.debugPrintCode) {
            try debug.disassembleChunk(
                std.io.getStdOut().writer(),
                self.currentChunk(),
                "code",
            );
        }
    }

    pub fn emitBytes(self: *Self, b1: u8, b2: u8) !void {
        try self.emitByte(b1);
        try self.emitByte(b2);
    }

    pub fn emitConstant(self: *Self, value: Value) !void {
        try self.emitBytes(
            @enumToInt(OpCode.op_constant),
            try self.makeConstant(value),
        );
    }

    fn makeConstant(self: *Self, value: Value) !u8 {
        const constant = try self.currentChunk().addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            self.parser.emitError("Too many constants in one chunk.");
            return 0;
        }

        return @intCast(u8, constant);
    }
};

pub fn compile(source: []const u8, chunk: *Chunk) !bool {
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner);
    var compiler = Compiler.init(chunk, &parser);
    parser.compiler = &compiler;
    parser.advance();
    try parser.expression();
    parser.consume(.tk_eof, "Expect end of expression.");
    try compiler.end();
    return !parser.hadError;
}