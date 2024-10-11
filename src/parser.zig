const std = @import("std");
const lexer = @import("lexer.zig");
const model = @import("model_data.zig");
const print = std.debug.print;

const TokenType = lexer.TokenType;
const Token = lexer.Token;
const ModelData = model.ModelData;

fn printIndent(indent: u32) void {
    var tabsToPrint = indent;
    while (tabsToPrint > 0) : (tabsToPrint -= 1) {
        std.debug.print("| ", .{});
    }
}

fn isNextTokenOneOfTypes(
    comptime accepted_types: []const TokenType,
    tokens: []const Token,
    position: usize,
) bool {
    if (checkEof(tokens, position)) {
        return false;
    }

    inline for (accepted_types) |op| {
        if (op == tokens[position].tag) {
            return true;
        }
    }

    return false;
}

pub const ParseDiagnostics = struct {
    error_token: ?Token = null,
    error_expected: ?[]const u8 = null,
};

pub const ParseOptions = struct {
    allocator: std.mem.Allocator,
    diags: *ParseDiagnostics,
    model_data: ModelData,
};

pub const ParseError = error{
    UnexpectedToken,
    ExpectedExpression,
    UnexpectedEOF,
    DomainNotFound,
    OutOfMemory,
};

pub const ASTNode_Program = struct {
    nogood: []const *ASTNode_Nogood,

    fn print(self: ASTNode_Program) void {
        std.debug.print("Program:\n", .{});
        for (self.nogood) |nogood| {
            nogood.print(1);
        }
    }
};

pub const ASTNode_Nogood = struct {
    set_expr: []const *ASTNode_SetExpr,

    fn print(self: ASTNode_Nogood, indent: u32) void {
        printIndent(indent);
        std.debug.print("Nogood:\n", .{});
        for (self.set_expr) |set_expr| {
            set_expr.print(indent + 1);
        }
    }

    fn parse(
        options: ParseOptions,
        tokens: []const Token,
        position: usize,
    ) !?ParseResult(ASTNode_Nogood) {
        if (checkEof(tokens, position)) {
            return null;
        }

        var current_position = position;

        const result = try ASTNode_SetExpr.parse(
            options,
            tokens,
            current_position,
        ) orelse {
            return null;
        };

        var set_expr_list = std.ArrayList(*ASTNode_SetExpr).init(options.allocator);

        const set_expr = try options.allocator.create(ASTNode_SetExpr);
        set_expr.* = result.node;

        try set_expr_list.append(set_expr);
        current_position += result.advance;

        while (checkTokenTag(tokens[position], .OR)) {
            current_position += 1;

            const next_result = try ASTNode_SetExpr.parse(
                options,
                tokens,
                current_position,
            ) orelse {
                options.diags.error_token = tokens[current_position];
                return ParseError.ExpectedExpression;
            };

            const _set_expr = try options.allocator.create(ASTNode_SetExpr);
            _set_expr.* = next_result.node;

            try set_expr_list.append(_set_expr);
            current_position += next_result.advance;
        }

        return ParseResult(ASTNode_Nogood){
            .advance = current_position - position,
            .node = .{
                .set_expr = try set_expr_list.toOwnedSlice(),
            },
        };
    }
};

pub const ASTNode_SetExpr = struct {
    expr: *ASTNode_ShiftExpr,
    args: ?[]const *ASTNode_Numeric,
    used: bool,

    fn print(self: ASTNode_SetExpr, indent: u32) void {
        if (self.used) {
            printIndent(indent);
            std.debug.print("SetExpr:\n", .{});

            self.expr.print(indent + 1);

            if (self.args) |args| {
                for (args) |arg| {
                    printIndent(indent + 1);
                    std.debug.print("Arg:\n", .{});
                    arg.print(indent + 2);
                }
            }
        } else {
            self.expr.print(indent);
        }
    }

    fn parse(
        options: ParseOptions,
        tokens: []const Token,
        position: usize,
    ) !?ParseResult(ASTNode_SetExpr) {
        var current_position = position;
        var _used = false;

        const shift_result = try ASTNode_ShiftExpr.parse(
            options,
            tokens,
            position,
        ) orelse {
            options.diags.error_token = tokens[current_position];
            options.diags.error_expected = "atom";
            return ParseError.UnexpectedToken;
        };
        current_position += shift_result.advance;

        var args_list: ?std.ArrayList(*ASTNode_Numeric) = null;

        if (isNextTokenOneOfTypes(&.{.IN}, tokens, current_position)) {
            current_position += 1;
            _used = true;

            args_list = std.ArrayList(*ASTNode_Numeric).init(options.allocator);

            if (!checkTokenTag(tokens[current_position], .INT)) {
                options.diags.error_token = tokens[current_position];
                options.diags.error_expected = "number";
                return ParseError.UnexpectedToken;
            }
            current_position += 1;

            if (!checkTokenTag(tokens[current_position], .LPAREN)) {
                options.diags.error_token = tokens[current_position];
                options.diags.error_expected = "parenthesis";
                return ParseError.UnexpectedToken;
            }
            current_position += 1;

            var first_arg = true;
            while (first_arg or checkTokenTag(tokens[current_position], .COMMA)) {
                if (first_arg) {
                    first_arg = false;
                } else {
                    current_position += 1;
                }

                if (try ASTNode_Numeric.parse(options, tokens, current_position)) |result| {
                    current_position += result.advance;
                    const numeric_expr = try options.allocator.create(ASTNode_Numeric);
                    numeric_expr.* = result.node;
                    try args_list.?.append(numeric_expr);
                } else {
                    options.diags.error_token = tokens[current_position];
                    options.diags.error_expected = "numeric argument";
                    return ParseError.UnexpectedToken;
                }
            }

            if (!checkTokenTag(tokens[current_position], .RPAREN)) {
                options.diags.error_token = tokens[current_position];
                options.diags.error_expected = "closing parenthesis";
                return ParseError.UnexpectedToken;
            }
            current_position += 1;
        }

        const shift_expr = try options.allocator.create(ASTNode_ShiftExpr);
        shift_expr.* = shift_result.node;

        return ParseResult(ASTNode_SetExpr){
            .advance = current_position - position,
            .node = .{
                .expr = shift_expr,
                .args = if (args_list != null)
                    try args_list.?.toOwnedSlice()
                else
                    null,
                .used = _used,
            },
        };
    }
};

pub const ShiftType = enum { shift, or_expr };

pub const ASTNode_ShiftExpr = union(ShiftType) {
    shift: struct {
        lhs: *ASTNode_ShiftExpr,
        rhs: *ASTNode_ShiftExpr,
    },
    or_expr: *ASTNode_OrExpr,

    fn print(self: ASTNode_ShiftExpr, indent: u32) void {
        switch (self) {
            .or_expr => |node| node.print(indent),
            .shift => |node| {
                printIndent(indent);
                std.debug.print("ShiftExpr:\n", .{});
                printIndent(indent + 1);
                std.debug.print("expr:\n", .{});
                node.lhs.print(indent + 2);

                printIndent(indent + 1);
                std.debug.print("op: +\n", .{});

                printIndent(indent + 1);
                std.debug.print("expr:\n", .{});
                node.rhs.print(indent + 2);
            },
        }
    }

    fn parse(
        options: ParseOptions,
        tokens: []const Token,
        position: usize,
    ) ParseError!?ParseResult(ASTNode_ShiftExpr) {
        var current_position = position;
        if (checkTokenTag(tokens[current_position], .SHIFT)) {
            current_position += 1;

            if (!checkTokenTag(tokens[current_position], .LPAREN)) {
                options.diags.error_token = tokens[current_position];
                options.diags.error_expected = "parenthesis";
                return ParseError.UnexpectedToken;
            }
            current_position += 1;

            const lhs = try options.allocator.create(ASTNode_ShiftExpr);
            if (try ASTNode_ShiftExpr.parse(
                options,
                tokens,
                current_position,
            )) |result| {
                current_position += result.advance;
                lhs.* = result.node;
            }

            if (!checkTokenTag(tokens[current_position], .COMMA)) {
                options.diags.error_token = tokens[current_position];
                options.diags.error_expected = "comma";
                return ParseError.UnexpectedToken;
            }
            current_position += 1;

            const rhs = try options.allocator.create(ASTNode_ShiftExpr);
            if (try ASTNode_ShiftExpr.parse(
                options,
                tokens,
                current_position,
            )) |result| {
                current_position += result.advance;
                rhs.* = result.node;
            }

            if (!checkTokenTag(tokens[current_position], .RPAREN)) {
                options.diags.error_token = tokens[current_position];
                options.diags.error_expected = "closing parenthesis";
                return ParseError.UnexpectedToken;
            }
            current_position += 1;

            return ParseResult(ASTNode_ShiftExpr){
                .advance = current_position - position,
                .node = .{
                    .shift = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    },
                },
            };
        }

        if (try ASTNode_OrExpr.parse(
            options,
            tokens,
            current_position,
        )) |result| {
            current_position += result.advance;
            const expr = try options.allocator.create(ASTNode_OrExpr);
            expr.* = result.node;

            return ParseResult(ASTNode_ShiftExpr){
                .advance = current_position - position,
                .node = .{
                    .or_expr = expr,
                },
            };
        }

        return null;
    }
};

pub fn OpItem(comptime ExprNode: type) type {
    return struct {
        op: TokenType,
        expr: *ExprNode,
    };
}

pub fn BinaryOpNode(
    comptime SubExprNode: type,
    comptime name: []const u8,
    comptime ops: []const TokenType,
) type {
    return struct {
        expr: *SubExprNode,
        ops: []OpItem(SubExprNode),

        const Self = @This();
        pub const SubExpr = SubExprNode;

        fn print(self: Self, indent: u32) void {
            if (self.ops.len > 0) {
                printIndent(indent);
                std.debug.print("{s}:\n", .{name});

                printIndent(indent + 1);
                std.debug.print("expr:\n", .{});
                self.expr.print(indent + 2);

                for (self.ops) |op_item| {
                    printIndent(indent + 1);
                    std.debug.print("op: {}\n", .{op_item.op});

                    printIndent(indent + 1);
                    std.debug.print("expr:\n", .{});
                    op_item.expr.print(indent + 2);
                }
            } else {
                self.expr.print(indent);
            }
        }

        fn parse(
            options: ParseOptions,
            tokens: []const Token,
            position: usize,
        ) !?ParseResult(Self) {
            var current_position = position;

            const expr_result = try SubExprNode.parse(
                options,
                tokens,
                current_position,
            ) orelse {
                return null;
            };
            current_position += expr_result.advance;

            var ops_list = std.ArrayList(OpItem(SubExprNode)).init(options.allocator);

            while (isNextTokenOneOfTypes(ops, tokens, current_position)) {
                const op = tokens[current_position].tag;
                current_position += 1;

                const _expr_result = try SubExprNode.parse(
                    options,
                    tokens,
                    current_position,
                ) orelse {
                    options.diags.error_token = tokens[current_position];
                    options.diags.error_expected = "expression";
                    return ParseError.UnexpectedToken;
                };
                current_position += _expr_result.advance;

                const _expr = try options.allocator.create(SubExprNode);
                _expr.* = _expr_result.node;

                try ops_list.append(.{ .op = op, .expr = _expr });
            }

            const expr = try options.allocator.create(SubExprNode);
            expr.* = expr_result.node;

            return ParseResult(Self){
                .advance = current_position - position,
                .node = .{
                    .expr = expr,
                    .ops = try ops_list.toOwnedSlice(),
                },
            };
        }
    };
}

pub const ASTNode_OrExpr = BinaryOpNode(
    ASTNode_AndExpr,
    "OrExpr",
    &.{.OR},
);
pub const ASTNode_AndExpr = BinaryOpNode(
    ASTNode_EqualityExpr,
    "AndExpr",
    &.{.AND},
);
pub const ASTNode_EqualityExpr = BinaryOpNode(
    ASTNode_ComparisonExpr,
    "EqualityExpr",
    &.{ .EQ, .NEQ },
);
pub const ASTNode_ComparisonExpr = BinaryOpNode(
    ASTNode_TermExpr,
    "ComparisonExpr",
    &.{ .LEQ, .GEQ, .LT, .GT },
);
pub const ASTNode_TermExpr = BinaryOpNode(
    ASTNode_FactorExpr,
    "TermExpr",
    &.{ .PLUS, .MINUS },
);
pub const ASTNode_FactorExpr = BinaryOpNode(
    ASTNode_UnaryExpr,
    "FactorExpr",
    &.{ .MULT, .DIV, .MOD },
);

pub const UnaryType = enum { atom, unary };

pub const ASTNode_UnaryExpr = union(UnaryType) {
    atom: *ASTNode_AtomExpr,
    unary: struct {
        op: TokenType,
        expr: *ASTNode_UnaryExpr,
    },

    fn print(self: ASTNode_UnaryExpr, indent: u32) void {
        switch (self) {
            .atom => |node| node.print(indent),
            .unary => |node| {
                printIndent(indent);
                std.debug.print("UnaryExpr:\n", .{});
                printIndent(indent + 1);
                std.debug.print("op: {}\n", .{node.op});
                node.expr.print(indent + 1);
            },
        }
    }

    fn parse(
        options: ParseOptions,
        tokens: []const Token,
        position: usize,
    ) ParseError!?ParseResult(ASTNode_UnaryExpr) {
        var current_position = position;
        if (isNextTokenOneOfTypes(&.{ .NOT, .MINUS }, tokens, current_position)) {
            const op: TokenType = tokens[current_position].tag;
            current_position += 1;
            const result = try ASTNode_UnaryExpr.parse(
                options,
                tokens,
                current_position,
            ) orelse {
                options.diags.error_token = tokens[current_position];
                return ParseError.ExpectedExpression;
            };
            current_position += result.advance;

            const expr = try options.allocator.create(ASTNode_UnaryExpr);
            expr.* = result.node;

            return ParseResult(ASTNode_UnaryExpr){
                .advance = current_position - position,
                .node = .{
                    .unary = .{
                        .op = op,
                        .expr = expr,
                    },
                },
            };
        }

        if (try ASTNode_AtomExpr.parse(
            options,
            tokens,
            current_position,
        )) |result| {
            current_position += result.advance;
            const expr = try options.allocator.create(ASTNode_AtomExpr);
            expr.* = result.node;

            return ParseResult(ASTNode_UnaryExpr){
                .advance = current_position - position,
                .node = .{
                    .atom = expr,
                },
            };
        }

        return null;
    }
};

pub const AtomType = enum { id, true, false, numeric, expr };

pub const ASTNode_AtomExpr = union(AtomType) {
    id: *ASTNode_Identifier,
    true: void,
    false: void,
    numeric: *ASTNode_Numeric,
    expr: *ASTNode_SetExpr,

    fn print(self: ASTNode_AtomExpr, indent: u32) void {
        printIndent(indent);
        std.debug.print("Atom:\n", .{});
        switch (self) {
            .id => |node| {
                node.print(indent + 1);
            },
            .true => {
                printIndent(indent + 1);
                std.debug.print("true\n", .{});
            },
            .false => {
                printIndent(indent + 1);
                std.debug.print("false\n", .{});
            },
            .numeric => |node| {
                node.print(indent + 1);
            },
            .expr => |node| {
                node.print(indent + 1);
            },
        }
    }

    fn parse(
        options: ParseOptions,
        tokens: []const Token,
        position: usize,
    ) ParseError!?ParseResult(ASTNode_AtomExpr) {
        var current_position = position;
        if (try ASTNode_Identifier.parse(options, tokens, current_position)) |result| {
            current_position += result.advance;
            const expr = try options.allocator.create(ASTNode_Identifier);
            expr.* = result.node;

            return ParseResult(ASTNode_AtomExpr){
                .advance = current_position - position,
                .node = .{
                    .id = expr,
                },
            };
        }
        if (checkTokenTag(tokens[current_position], .TRUE)) {
            current_position += 1;
            return ParseResult(ASTNode_AtomExpr){
                .advance = current_position - position,
                .node = .{
                    .true = undefined,
                },
            };
        }
        if (checkTokenTag(tokens[current_position], .FALSE)) {
            current_position += 1;
            return ParseResult(ASTNode_AtomExpr){
                .advance = current_position - position,
                .node = .{
                    .false = undefined,
                },
            };
        }
        if (try ASTNode_Numeric.parse(options, tokens, current_position)) |result| {
            current_position += result.advance;

            const expr = try options.allocator.create(ASTNode_Numeric);
            expr.* = result.node;

            return ParseResult(ASTNode_AtomExpr){
                .advance = current_position - position,
                .node = .{
                    .numeric = expr,
                },
            };
        }
        if (checkTokenTag(tokens[current_position], .LPAREN)) {
            current_position += 1;

            const result = try ASTNode_SetExpr.parse(
                options,
                tokens,
                current_position,
            ) orelse {
                options.diags.error_token = tokens[current_position];
                return ParseError.ExpectedExpression;
            };
            current_position += result.advance;

            if (!checkTokenTag(tokens[current_position], .RPAREN)) {
                options.diags.error_token = tokens[current_position];
                options.diags.error_expected = "closing parenthesis";
                return ParseError.UnexpectedToken;
            }
            current_position += 1;

            //~ojf: Need to allocate here to break circular dependency
            const expr = try options.allocator.create(ASTNode_SetExpr);

            expr.* = result.node;
            return ParseResult(ASTNode_AtomExpr){
                .advance = current_position - position,
                .node = .{
                    .expr = expr,
                },
            };
        }

        return null;
    }
};

pub const NumericType = enum { number, range };

pub const ASTNode_Numeric = union(NumericType) {
    number: i64,
    range: struct {
        start: i64,
        end: i64,
    },

    fn print(self: ASTNode_Numeric, indent: u32) void {
        printIndent(indent);
        switch (self) {
            .number => |number| {
                std.debug.print("{}\n", .{number});
            },
            .range => |range| {
                std.debug.print("{}-{}\n", .{ range.start, range.end });
            },
        }
    }

    fn parse(
        options: ParseOptions,
        tokens: []const Token,
        position: usize,
    ) !?ParseResult(ASTNode_Numeric) {
        var current_position = position;

        var first_negative = false;
        if (checkTokenTag(tokens[current_position], .MINUS)) {
            current_position += 1;
            first_negative = true;
        }

        if (!checkTokenTag(tokens[current_position], .NUMBER)) {
            return null;
        }

        const first_number = tokens[current_position].number.?;
        current_position += 1;

        var node: ASTNode_Numeric = undefined;
        if (isNextTokenOneOfTypes(&.{.RANGE}, tokens, current_position)) {
            current_position += 1;

            var second_negative = false;
            if (checkTokenTag(tokens[current_position], .MINUS)) {
                current_position += 1;
                second_negative = true;
            }

            if (!checkTokenTag(tokens[current_position], .NUMBER)) {
                options.diags.error_token = tokens[current_position];
                options.diags.error_expected = "number";
                return ParseError.UnexpectedToken;
            }

            const second_number = tokens[current_position].number.?;
            current_position += 1;

            node = ASTNode_Numeric{ .range = .{
                .start = @as(i64, (if (first_negative) -1 else 1)) * first_number,
                .end = @as(i64, (if (second_negative) -1 else 1)) * second_number,
            } };
        } else {
            node = ASTNode_Numeric{
                .number = @as(i64, (if (first_negative) -1 else 1)) * first_number,
            };
        }

        return ParseResult(ASTNode_Numeric){
            .advance = current_position - position,
            .node = node,
        };
    }
};

pub const ASTNode_Identifier = struct {
    domain: model.DomainType,
    name: []const u8,
    dimensions: []const i64,

    fn print(self: ASTNode_Identifier, indent: u32) void {
        printIndent(indent);
        std.debug.print("Identifier: {s}\n", .{self.name});
    }

    fn parse(
        options: ParseOptions,
        tokens: []const Token,
        position: usize,
    ) !?ParseResult(ASTNode_Identifier) {
        if (!isNextTokenOneOfTypes(&.{.ID}, tokens, position)) {
            return null;
        }

        const token = tokens[position];
        const identifier = token.lexeme;
        std.debug.print("Identifier: {s}\n", .{identifier});
        for (options.model_data.finds) |find| {
            if (identifier.len >= find.len and
                std.mem.eql(u8, find, identifier[0..find.len]))
            {
                const domain = options.model_data.domain_types.get(find) orelse {
                    options.diags.error_token = token;
                    return ParseError.DomainNotFound;
                };

                var iter = std.mem.split(u8, identifier[find.len + 1 ..], "_");

                if (domain == .function) {
                    _ = iter.next().?;
                }

                var dimensions_list = std.ArrayList(i64).init(options.allocator);
                while (iter.next()) |dim| {
                    try dimensions_list.append(std.fmt.parseInt(i64, dim, 10) catch {
                        @panic("Failed to parse dimension!");
                    });
                }

                return .{
                    .advance = 1,
                    .node = .{
                        .domain = domain,
                        .name = find,
                        .dimensions = try dimensions_list.toOwnedSlice(),
                    },
                };
            }
        }

        return null;
    }
};

fn ParseResult(comptime ASTNode: type) type {
    return struct {
        advance: usize,
        node: ASTNode,
    };
}

inline fn checkEof(tokens: []const Token, position: usize) bool {
    return tokens.len == position;
}

inline fn checkTokenTag(token: Token, tag: TokenType) bool {
    return token.tag == tag;
}

pub fn parse(options: ParseOptions, tokens: []const Token) !*ASTNode_Program {
    var nogood_list = std.ArrayList(*ASTNode_Nogood).init(options.allocator);
    var position: usize = 0;

    while (try ASTNode_Nogood.parse(options, tokens, position)) |result| {
        position += result.advance;

        const expr = try options.allocator.create(ASTNode_Nogood);
        expr.* = result.node;

        try nogood_list.append(expr);
    }

    if (position != tokens.len) {
        return ParseError.UnexpectedEOF;
    }

    const program = try options.allocator.create(ASTNode_Program);
    program.* = .{
        .nogood = try nogood_list.toOwnedSlice(),
    };

    return program;
}

pub fn printParseTree(program: *ASTNode_Program) void {
    program.print();
}
