const std = @import("std");
const debug = std.debug;
const io = std.io;
const mem = std.mem;

pub const Type = enum {
    Null,
    Boolean,
    Number, // TODO Separate integer type.
    String,
    Function,

    // Internal only:
    // TODO Separate this out into a different enum.
    Chunk,
};

const Value = union(Type) {
    Null: void,
    Boolean: bool,
    Number: f64,
    String: u32,
    Function: *Function,
    Chunk: *Chunk,
};

// TODO Move.
const StackFrame = struct {
    base: usize,
};

const Opcode = enum(u8) {
    Push, // ( -- const_tbl[n])
    Pop, // ( v0 v1 v2 .. vn-1 -- )
    Add, // ( a b -- a+b )
    Sub, // ( a b -- a-b )
    Mul, // ( a b -- a*b )
    Div, // ( a b -- a/b )
    Return,
};

const Token = enum {
    Eof,
    Name,
    NumberLiteral,
    StringLiteral,

    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,

    Let,
};

const TokenMapping = struct {
    char: u8,
    token: Token,
};

/// Maps single characters to their token ID.
const token_mapping = []TokenMapping{
    TokenMapping{ .char = '+', .token = Token.Plus },
    TokenMapping{ .char = '-', .token = Token.Minus },
    TokenMapping{ .char = '*', .token = Token.Star },
    TokenMapping{ .char = '/', .token = Token.Slash },
    TokenMapping{ .char = '(', .token = Token.LParen },
    TokenMapping{ .char = ')', .token = Token.RParen },
};

const KeywordMapping = struct {
    string: []const u8,
    token: Token,
};

/// Like token_mapping, but for full keywords.
const keyword_mapping = []KeywordMapping{KeywordMapping{ .string = "let", .token = Token.Let }};

fn isDigit(char: u8) bool {
    return char >= '0' and char <= '9';
}

fn isLetter(char: u8) bool {
    return (char >= 'A' and char <= 'Z') or (char >= 'a' and char <= 'z') or char == '_';
}

fn isAlphanumeric(char: u8) bool {
    return isDigit(char) or isLetter(char);
}

fn Compiler(comptime ReadError: type) type {
    return struct {
        const Error = ReadError || mem.Allocator.Error || error{
            InvalidChar,
            InvalidEscape,
            TooManyConstants,
            BadExpression,
        };

        input: *io.InStream(ReadError),
        current_token: Token,
        current_line: usize,
        buffer_char: u8,
        buffer: std.ArrayList(u8),
        number_value: f64,

        bytecode: std.ArrayList(u8),
        const_table: std.ArrayList(Value),

        fn init(allocator: *mem.Allocator, input: *io.InStream(ReadError)) !@This() {
            var compiler = @This(){
                .input = input,
                .current_token = Token.Eof,
                .current_line = 1,
                .buffer_char = 0,
                .buffer = std.ArrayList(u8).init(allocator),
                .number_value = 0.0,
                .bytecode = std.ArrayList(u8).init(allocator),
                .const_table = std.ArrayList(Value).init(allocator),
            };
            errdefer compiler.deinit();
            try compiler.readChar();
            try compiler.lex();
            return compiler;
        }

        fn deinit(self: *@This()) void {
            self.buffer.deinit();
            self.bytecode.deinit();
            self.const_table.deinit();
        }

        /// Reads a single character from the input and puts it in self.buffer_char, or it puts 0 if
        /// at the end of the input.
        fn readChar(self: *@This()) ReadError!void {
            var buffer: [1]u8 = undefined;
            const count = try self.input.read(buffer[0..]);
            self.buffer_char = if (count == 0) 0 else buffer[0];
        }

        fn lex(self: *@This()) Error!void {
            // Discard ignored stuff.
            discardWhitespace: while (true) {
                if (self.buffer_char == ' ' or self.buffer_char == '\t') {
                    // Plain whitespace.
                    try self.readChar();
                } else if (self.buffer_char == '\n') {
                    // Unix-style line breaks.
                    self.current_line += 1;
                    try self.readChar();
                } else if (self.buffer_char == '\r') {
                    // Windows and Mac Classic-style line breaks.
                    self.current_line += 1;
                    try self.readChar();
                    if (self.buffer_char == '\n') {
                        try self.readChar();
                    }
                } else if (self.buffer_char == '#') {
                    // Comments.
                    try self.readChar();
                    while (self.buffer_char != 0 and self.buffer_char != '\n' and self.buffer_char != '\r') {
                        try self.readChar();
                    }
                } else {
                    break :discardWhitespace;
                }
            }

            if (self.buffer_char == 0) {
                self.current_token = Token.Eof;
            } else if (isDigit(self.buffer_char)) {
                self.current_token = Token.NumberLiteral;
                self.number_value = @intToFloat(f64, self.buffer_char - '0');
                try self.readChar();
                while (isDigit(self.buffer_char) or self.buffer_char == '_') {
                    if (self.buffer_char != '_') {
                        self.number_value *= 10.0;
                        self.number_value += @intToFloat(f64, self.buffer_char - '0');
                    }
                    try self.readChar();
                }
                // TODO Radix indicator.
                if (self.buffer_char == '.') {
                    var mul: f64 = 0.1;
                    try self.readChar();
                    while (isDigit(self.buffer_char) or self.buffer_char == '_') {
                        if (self.buffer_char != '_') {
                            self.number_value += @intToFloat(f64, self.buffer_char - '0') * mul;
                            mul *= 0.1;
                        }
                        try self.readChar();
                    }
                }
            } else if (isLetter(self.buffer_char)) {
                self.current_token = Token.Name;
                self.buffer.len = 0;
                try self.buffer.append(self.buffer_char);
                try self.readChar();
                while (isAlphanumeric(self.buffer_char)) {
                    try self.buffer.append(self.buffer_char);
                    try self.readChar();
                }
            } else if (self.buffer_char == '"') {
                self.current_token = Token.StringLiteral;
                self.buffer.len = 0;
                try self.readChar();
                while (self.buffer_char != '"') {
                    if (self.buffer_char == '\\') {
                        try self.readChar();
                        const char = switch (self.buffer_char) {
                            // TODO Hex/Unicode escapes?
                            '0' => u8(0),
                            '"' => u8('"'),
                            't' => u8('\t'),
                            'n' => u8('\n'),
                            'r' => u8('\r'),
                            '\\' => u8('\\'),
                            else => return error.InvalidEscape,
                        };
                        try self.buffer.append(char);
                    } else {
                        try self.buffer.append(self.buffer_char);
                    }
                }
                try self.readChar();
            } else {
                for (token_mapping) |mapping| {
                    if (self.buffer_char == mapping.char) {
                        try self.readChar();
                        self.current_token = mapping.token;
                        return;
                    }
                }
                return error.InvalidChar;
            }
        }

        fn parseTerm(self: *@This()) Error!void {
            switch (self.current_token) {
                Token.NumberLiteral => {
                    const index = number: {
                        for (self.const_table.toSlice()) |constant, i| {
                            switch (constant) {
                                Type.Number => |number| {
                                    if (number == self.number_value) {
                                        break :number i;
                                    }
                                },

                                else => {},
                            }
                        }
                        try self.const_table.append(Value{ .Number = self.number_value });
                        break :number self.const_table.count() - 1;
                    };
                    if (index > 255) {
                        return error.TooManyConstants;
                    }
                    try self.bytecode.append(@enumToInt(Opcode.Push));
                    try self.bytecode.append(@intCast(u8, index));
                    try self.lex();
                },

                Token.StringLiteral => {
                    // TODO Get string literals working.
                    //const index = string: {
                    //    for (self.const_table.toSlice()) |constant, i| {
                    //        switch (constant) {
                    //            Type.String => |id| {
                    //                // TODO Guess we'd better intern this stuff at some point.
                    //                if (mem.eql(u8, id == self.???) {
                    //                    break :string i;
                    //                }
                    //            },

                    //            else => {},
                    //        }
                    //    }
                    //    try self.const_table.append(Value{ .Number//TODO TODO TODO TODO TODO TODO
                    //};
                    unreachable;
                },

                Token.LParen => {
                    try self.lex();
                    try self.parseExpr();
                    if (self.current_token != Token.RParen) {
                        return error.BadExpression;
                    }
                    try self.lex();
                },

                else => {
                    return error.BadExpression;
                },
            }
        }

        fn parseProduct(self: *@This()) Error!void {
            try self.parseTerm();
            while (self.current_token == Token.Star or self.current_token == Token.Slash) {
                const token = self.current_token;
                try self.lex();
                try self.parseTerm();
                const opcode = if (token == Token.Star) Opcode.Mul else Opcode.Div;
                try self.bytecode.append(@enumToInt(opcode));
            }
        }

        fn parseSum(self: *@This()) Error!void {
            try self.parseProduct();
            while (self.current_token == Token.Plus or self.current_token == Token.Minus) {
                const token = self.current_token;
                try self.lex();
                try self.parseProduct();
                const opcode = if (token == Token.Plus) Opcode.Add else Opcode.Sub;
                try self.bytecode.append(@enumToInt(opcode));
            }
        }

        fn parseExpr(self: *@This()) Error!void {
            try self.parseSum();
        }
    };
}

// TODO Set up a function to finalize all GC objects.
const GCHeader = struct {
    next: ?*GCHeader,
    obj_type: Type,
};

const String = struct {
    // TODO Mark?
    string: []u8,

    /// This is a dummy function. `std.HashMap` requires a function that hashes its keys, but
    /// because the keys are already hashed, this just passes them straight through.
    fn hash(id: u32) u32 {
        return id;
    }

    /// Same with this thing.
    fn eql(a: u32, b: u32) bool {
        return a == b;
    }
};

const Chunk = struct {
    header: GCHeader,
    bytecode: []u8,
    const_table: []Value,
};

const Function = struct {
    header: GCHeader,
    chunk: *Chunk,
    entry_point: usize,
    // TODO upvalues: []Upvalue,
};

pub const State = struct {
    const StringTable = std.HashMap(u32, String, String.hash, String.eql);

    allocator: *mem.Allocator,
    stack: std.ArrayList(Value),
    stack_frames: std.ArrayList(StackFrame),
    string_table: StringTable,
    first_obj: ?*GCHeader,

    pub fn init(allocator: *mem.Allocator) !State {
        var state = State{
            .allocator = allocator,
            .stack = std.ArrayList(Value).init(allocator),
            .stack_frames = std.ArrayList(StackFrame).init(allocator),
            .string_table = StringTable.init(allocator),
            .first_obj = null,
        };
        try state.stack_frames.append(StackFrame{ .base = 0 });
        return state;
    }

    fn deinit(self: *State) void {
        // TODO
        //var obj = self.first_obj;
        //while (obj != null) : (obj = obj.?.next) {
        //    obj.?.finalize();
        //}
        self.stack.deinit();
        self.stack_frames.deinit();
    }

    fn _currentFrame(self: *State) *StackFrame {
        return &self.stack_frames.toSlice()[self.stack_frames.count() - 1];
    }

    fn stackSize(self: *State) usize {
        return self.stack.count() - self._currentFrame().base;
    }

    fn pushNull(self: *State) !void {
        return self.stack.append(Value{ .Null = {} });
    }

    fn pushBoolean(self: *State, value: bool) !void {
        return self.stack.append(Value{ .Boolean = value });
    }

    fn pushNumber(self: *State, value: f64) !void {
        return self.stack.append(Value{ .Number = value });
    }

    fn normalizeIndex(self: *State, index: isize) isize {
        return if (index < 0) @intCast(isize, self.stackSize()) + index else index;
    }

    fn _getStackValue(self: *State, index: isize) *Value {
        var absIndex = self.normalizeIndex(index);
        if (absIndex < 0 or @intCast(usize, absIndex) >= self.stackSize()) {
            debug.panic("Nano stack index out of bounds");
        }
        const i = self._currentFrame().base + @intCast(usize, absIndex);
        return &self.stack.toSlice()[i];
    }

    //fn duplicate(self: *State, index: isize) !void {
    //    if (_getStackValue
    //}

    fn getType(self: *State, index: isize) Type {
        return Type(self._getStackValue(index).*);
    }

    fn asBoolean(self: *State, index: isize) bool {
        return switch (self._getStackValue(index).*) {
            Type.Null => false,
            Type.Boolean => |val| val,
            else => true,
        };
    }

    fn getNumber(self: *State, index: isize) ?f64 {
        return switch (self._getStackValue(index).*) {
            Type.Number => |num| num,
            else => null,
        };
    }

    fn pushString(self: *State, string: []const u8) !void {
        const key = std.hash.Fnv1a_32.hash(string);
        var handle = try self.string_table.getOrPut(key);
        if (!handle.found_existing) {
            handle.kv.value = String{ .string = try mem.dupe(self.allocator, u8, string) };
        }
        try self.stack.append(Value{ .String = key });
    }

    fn getString(self: *State, index: isize) ?[]const u8 {
        return switch (self._getStackValue(index).*) {
            Type.String => |id| self.string_table.get(id).?.value.string,
            else => null,
        };
    }

    fn compile(self: *State, comptime ReadError: type, input: *io.InStream(ReadError)) Compiler(ReadError).Error!void {
        // Create the compiler.
        var compiler = try Compiler(ReadError).init(self.allocator, input);
        errdefer compiler.deinit();

        // Run the compiler.
        try compiler.parseExpr();
        try compiler.bytecode.append(@enumToInt(Opcode.Return));

        // Convert the compiled junk into a Chunk object.
        var chunk = try self.allocator.create(Chunk);
        chunk.* = Chunk{
            .header = GCHeader{
                .next = self.first_obj,
                .obj_type = Type.Chunk,
            },
            .bytecode = compiler.bytecode.toOwnedSlice(),
            .const_table = compiler.const_table.toOwnedSlice(),
        };
        self.first_obj = &chunk.header;
        compiler.buffer.deinit();

        var function = try self.allocator.create(Function);
        function.* = Function{
            .header = GCHeader{
                .next = self.first_obj,
                .obj_type = Type.Function,
            },
            .chunk = chunk,
            .entry_point = 0,
        };
        self.first_obj = &function.header;

        try self.stack.append(Value{ .Function = function });
    }

    fn call(self: *State) !void { // TODO Arguments.
        debug.assert(self.stackSize() >= 1);
        try self.stack_frames.append(StackFrame{ .base = self.stack.count() });
        const value = &self.stack.toSlice()[self.stack.count() - 1];
        if (value.* != Type.Function) {
            return error.TypeMismatch;
        }
        const chunk = value.Function.chunk;

        var ip = value.Function.entry_point;
        interpret: while (true) : (ip += 1) {
            switch (@intToEnum(Opcode, chunk.bytecode[ip])) {
                Opcode.Push => {
                    ip += 1;
                    try self.stack.append(chunk.const_table[chunk.bytecode[ip]]);
                },

                Opcode.Pop => {
                    ip += 1;
                    var count = chunk.bytecode[ip];
                    while (count > 0) : (count -= 1) {
                        _ = self.stack.pop();
                    }
                },

                Opcode.Add => {
                    if (self.stack.at(self.stack.count() - 1) != Type.Number or self.stack.at(self.stack.count() - 2) != Type.Number) {
                        return error.TypeMismatch;
                    }
                    self.stack.toSlice()[self.stack.count() - 2].Number += self.stack.at(self.stack.count() - 1).Number;
                    _ = self.stack.pop();
                },

                Opcode.Sub => {
                    if (self.stack.at(self.stack.count() - 1) != Type.Number or self.stack.at(self.stack.count() - 2) != Type.Number) {
                        return error.TypeMismatch;
                    }
                    self.stack.toSlice()[self.stack.count() - 2].Number -= self.stack.at(self.stack.count() - 1).Number;
                    _ = self.stack.pop();
                },

                Opcode.Mul => {
                    if (self.stack.at(self.stack.count() - 1) != Type.Number or self.stack.at(self.stack.count() - 2) != Type.Number) {
                        return error.TypeMismatch;
                    }
                    self.stack.toSlice()[self.stack.count() - 2].Number *= self.stack.at(self.stack.count() - 1).Number;
                    _ = self.stack.pop();
                },

                Opcode.Div => {
                    if (self.stack.at(self.stack.count() - 1) != Type.Number or self.stack.at(self.stack.count() - 2) != Type.Number) {
                        return error.TypeMismatch;
                    }
                    self.stack.toSlice()[self.stack.count() - 2].Number /= self.stack.at(self.stack.count() - 1).Number;
                    _ = self.stack.pop();
                },

                Opcode.Return => {
                    break :interpret;
                },
            }
        }

        debug.assert(self.stack.count() > self._currentFrame().base);
        self.stack.toSlice()[self._currentFrame().base - 1] = self.stack.at(self.stack.count() - 1);
        self.stack.len = self.stack_frames.pop().base;
    }
};

test "Pretty Much Everything" {
    var direct_allocator = std.heap.DirectAllocator.init();
    const allocator = &direct_allocator.allocator;

    var state = State.init(allocator) catch unreachable;
    defer state.deinit();
    debug.assert(state.stackSize() == 0);

    state.pushNull() catch unreachable;
    debug.assert(state.stackSize() == 1);
    debug.assert(state.getType(0) == Type.Null);
    debug.assert(state.getType(-1) == Type.Null);

    state.pushBoolean(true) catch unreachable;
    debug.assert(state.stackSize() == 2);
    debug.assert(state.getType(1) == Type.Boolean);
    debug.assert(state.asBoolean(-1));

    state.pushNumber(3.14159) catch unreachable;
    debug.assert(state.stackSize() == 3);
    debug.assert(state.getType(2) == Type.Number);
    debug.assert(state.getNumber(-1).? == 3.14159);

    var file = std.os.File.openRead("test.nano") catch unreachable;
    defer file.close();
    var stream = file.inStream();
    state.compile(std.os.File.InStream.Error, &stream.stream) catch unreachable;
    debug.assert(state.stackSize() == 4);
    debug.assert(state.getType(3) == Type.Function);

    state.call() catch unreachable;
    debug.assert(state.stackSize() == 4);
    debug.assert(state.getType(3) == Type.Number);
    debug.warn("Result = {}\n", state.getNumber(-1).?);

    state.pushString("Hello, world!") catch unreachable;
    debug.assert(state.stackSize() == 5);
    debug.assert(state.getType(4) == Type.String);
    debug.assert(mem.eql(u8, state.getString(-1).?, "Hello, world!"));
    state.pushString("Hello, world!") catch unreachable;
    state.pushString("Hello, world!") catch unreachable;
    debug.assert(state.string_table.size == 1);
}
