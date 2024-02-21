const std = @import("std");
const print = std.debug.print;

const json_input = @embedFile("./test.json");
const conjure_json_input = @embedFile("./conjure.json");
const learnts_input = @embedFile("./test.learnts");

pub const log_level: std.log.Level = .debug;

// -- Lexing --

const TokenType = enum {
    LPAREN,
    RPAREN,
    PLUS,
    DEQ,
    EQ,
    NEQ,
    LEQ,
    GEQ,
    LT,
    GT,
    MINUS,
    NOT,
    TRUE,
    FALSE,
    SHIFT,
    RANGE,
    IN,
    INT,
    COMMA,
    OR,
    AT,
    MULT,
    DIV,
    MOD,
    NUMBER,
    LETTER,
    ID,
    NEWLINE,
};

const Token = struct {
    tag: TokenType,
    lexeme: []const u8,
    number: ?i64,

    line: usize,
    col: usize,
};

inline fn isAlpha(char: u8) bool {
    return ((char >= 'a' and char <= 'z') or
        (char >= 'A' and char <= 'Z') or
        (char == '_'));
}

inline fn isDigit(char: u8) bool {
    return char >= '0' and char <= '9';
}

fn lex(allocator: std.mem.Allocator, input: []const u8) []Token {
    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(allocator);

    var i: usize = 0;
    var line: usize = 0;
    var col: usize = 0;
    while (i < input.len) : (i += 1) {
        var start_index = i;
        var token_number: ?i64 = null;

        var token_type: TokenType = switch (input[i]) {
            '(' => .LPAREN,
            ')' => .RPAREN,
            '+' => .PLUS,
            '-' => .MINUS,
            '*' => .MULT,
            '/' => .DIV,
            '%' => .MOD,
            '@' => .AT,
            ',' => .COMMA,
            '\n' => t: {
                line += 1;
                col = 0;
                break :t .NEWLINE;
            },
            '.' => t: {
                if (i + 1 != input.len and input[i + 1] == '.') {
                    i += 1;
                    break :t .RANGE;
                }
            },
            '\r' => t: {
                if (i + 1 != input.len and input[i + 1] == '\n') {
                    i += 1;
                    break :t .RANGE;
                }
            },
            '\\' => t: {
                if (i + 1 != input.len and input[i + 1] == '/') {
                    i += 1;
                    break :t .OR;
                }

                @panic("Unrecognized token!");
            },
            '=' => t: {
                if (i + 1 != input.len and input[i + 1] == '=') {
                    i += 1;
                    break :t .DEQ;
                }

                break :t .EQ;
            },
            '!' => t: {
                if (i + 1 != input.len and input[i + 1] == '=') {
                    i += 1;
                    break :t .NEQ;
                }

                break :t .NOT;
            },
            '<' => t: {
                if (i + 1 != input.len and input[i + 1] == '=') {
                    i += 1;
                    break :t .LEQ;
                }

                break :t .LT;
            },
            '>' => t: {
                if (i + 1 != input.len and input[i + 1] == '=') {
                    i += 1;
                    break :t .GEQ;
                }

                break :t .GT;
            },
            ' ' => {
                col += 1;
                continue;
            },
            '\t' => {
                col += 4;
                continue;
            },
            else => |c| t: {
                if (isDigit(c)) {
                    while (i + 1 < input.len and isDigit(input[i + 1])) {
                        i += 1;
                    }

                    token_number = std.fmt.parseInt(
                        i64,
                        input[start_index .. i + 1],
                        10,
                    ) catch {
                        @panic("Failed to parse number!");
                    };

                    break :t .NUMBER;
                } else if (isAlpha(c)) {
                    while (i + 1 < input.len and
                        (isAlpha(input[i + 1]) or isDigit(input[i + 1])))
                    {
                        i += 1;
                    }

                    // TODO(ojf): If we start adding more keywords make this a hash table
                    const ident = input[start_index .. i + 1];
                    if (std.mem.eql(u8, ident, "in")) {
                        break :t .IN;
                    }
                    if (std.mem.eql(u8, ident, "int")) {
                        break :t .INT;
                    }
                    if (std.mem.eql(u8, ident, "true")) {
                        break :t .TRUE;
                    }
                    if (std.mem.eql(u8, ident, "false")) {
                        break :t .FALSE;
                    }
                    if (std.mem.eql(u8, ident, "shift")) {
                        break :t .SHIFT;
                    }

                    break :t .ID;
                }
            },
        };

        var token = Token{
            .tag = token_type,
            .lexeme = input[start_index .. i + 1],
            .number = token_number,
            .line = line,
            .col = col,
        };

        col += i + 1 - start_index;

        tokens.append(token) catch {
            @panic("Failed to add token to stream!");
        };
    }

    return tokens.items;
}

// --- Parsing ---

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

const ParseDiagnostics = struct {
    error_token: ?Token = null,
    error_expected: ?[]const u8 = null,
};

const ParseOptions = struct {
    allocator: std.mem.Allocator,
    diags: *ParseDiagnostics,
    conjure: ConjureData,
};

const ParseError = error{
    UnexpectedToken,
    ExpectedExpression,
    UnexpectedEOF,
    DomainNotFound,
    OutOfMemory,
};

const ASTNode_Program = struct {
    nogood: []const ASTNode_Nogood,

    fn print(self: ASTNode_Program) void {
        std.debug.print("Program:\n", .{});
        for (self.nogood) |nogood| {
            nogood.print(1);
        }
    }
};

const ASTNode_Nogood = struct {
    set_expr: []const ASTNode_SetExpr,

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

        var set_expr_list = std.ArrayList(ASTNode_SetExpr).init(options.allocator);

        try set_expr_list.append(result.node);
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

            try set_expr_list.append(next_result.node);
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

const ASTNode_SetExpr = struct {
    expr: ASTNode_EqualityExpr,
    args: ?[]const ASTNode_Numeric,

    fn print(self: ASTNode_SetExpr, indent: u32) void {
        printIndent(indent);
        std.debug.print("SetExpr:\n", .{});

        printIndent(indent + 1);
        std.debug.print("Expr:\n", .{});
        self.expr.print(indent + 2);

        if (self.args) |args| {
            for (args) |arg| {
                printIndent(indent + 2);
                std.debug.print("Arg:\n", .{});
                arg.print(indent + 3);
            }
        }
    }

    fn parse(
        options: ParseOptions,
        tokens: []const Token,
        position: usize,
    ) !?ParseResult(ASTNode_SetExpr) {
        var current_position = position;

        const equality_result = try ASTNode_EqualityExpr.parse(
            options,
            tokens,
            position,
        ) orelse {
            options.diags.error_token = tokens[current_position];
            options.diags.error_expected = "atom";
            return ParseError.UnexpectedToken;
        };
        current_position += equality_result.advance;

        var args_list: ?std.ArrayList(ASTNode_Numeric) = null;

        if (isNextTokenOneOfTypes(&.{.IN}, tokens, current_position)) {
            current_position += 1;

            args_list = std.ArrayList(ASTNode_Numeric).init(options.allocator);

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
                    try args_list.?.append(result.node);
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

        return ParseResult(ASTNode_SetExpr){
            .advance = current_position - position,
            .node = .{
                .expr = equality_result.node,
                .args = if (args_list != null)
                    try args_list.?.toOwnedSlice()
                else
                    null,
            },
        };
    }
};

fn OpItem(comptime ExprNode: type) type {
    return struct {
        op: TokenType,
        expr: ExprNode,
    };
}

fn BinaryOpNode(
    comptime SubExprNode: type,
    comptime name: []const u8,
    comptime ops: []const TokenType,
) type {
    return struct {
        expr: SubExprNode,
        ops: []OpItem(SubExprNode),

        const Self = @This();
        const SubExpr = SubExprNode;

        fn print(self: Self, indent: u32) void {
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

                try ops_list.append(.{ .op = op, .expr = _expr_result.node });
            }

            return ParseResult(Self){
                .advance = current_position - position,
                .node = .{
                    .expr = expr_result.node,
                    .ops = try ops_list.toOwnedSlice(),
                },
            };
        }
    };
}

const ASTNode_EqualityExpr = BinaryOpNode(
    ASTNode_ComparisonExpr,
    "EqualityExpr",
    &.{ .EQ, .NEQ },
);
const ASTNode_ComparisonExpr = BinaryOpNode(
    ASTNode_TermExpr,
    "ComparisonExpr",
    &.{ .LEQ, .GEQ, .LT, .GT },
);
const ASTNode_TermExpr = BinaryOpNode(
    ASTNode_FactorExpr,
    "TermExpr",
    &.{ .PLUS, .MINUS },
);
const ASTNode_FactorExpr = BinaryOpNode(
    ASTNode_AtomExpr,
    "FactorExpr",
    &.{ .MULT, .DIV, .MOD },
);

const AtomType = enum { id, true, false, numeric, expr };

const ASTNode_AtomExpr = union(AtomType) {
    id: ASTNode_Identifier,
    true: void,
    false: void,
    numeric: ASTNode_Numeric,
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
            return ParseResult(ASTNode_AtomExpr){
                .advance = current_position - position,
                .node = .{
                    .id = result.node,
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
            return ParseResult(ASTNode_AtomExpr){
                .advance = current_position - position,
                .node = .{
                    .numeric = result.node,
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

const NumericType = enum { number, range };

const ASTNode_Numeric = union(NumericType) {
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

        var first_number = tokens[current_position].number.?;
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

            var second_number = tokens[current_position].number.?;
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

const ASTNode_Identifier = struct {
    domain: DomainType,
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
        for (options.conjure.finds) |find| {
            if (identifier.len >= find.len and
                std.mem.eql(u8, find, identifier[0..find.len]))
            {
                const domain = options.conjure.domains.get(find) orelse {
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

fn parse(options: ParseOptions, tokens: []const Token) !ASTNode_Program {
    var nogood_list = std.ArrayList(ASTNode_Nogood).init(options.allocator);
    var position: usize = 0;

    while (try ASTNode_Nogood.parse(options, tokens, position)) |result| {
        position += result.advance;

        try nogood_list.append(result.node);
    }

    if (position != tokens.len) {
        return ParseError.UnexpectedEOF;
    }

    return .{
        .nogood = try nogood_list.toOwnedSlice(),
    };
}

// --- Translation ---
const DomainType = enum {
    matrix,
    function,
};

fn parseDomainType(string: []const u8) DomainType {
    if (std.mem.eql(u8, string, "DomainMatrix")) {
        return .matrix;
    }
    if (std.mem.eql(u8, string, "DomainFunction")) {
        return .function;
    }
    @panic("Failed to parse domain type!");
}

fn convertProgram(
    allocator: std.mem.Allocator,
    program: ASTNode_Program,
) ![]const u8 {
    var ret = try allocator.alloc(u8, 0);
    for (program.nogood) |nogood| {
        const nogood_str = try convertNogood(allocator, nogood) orelse {
            continue;
        };

        const new_ret = try std.mem.concat(
            allocator,
            u8,
            &.{ ret, nogood_str },
        );
        allocator.free(nogood_str);
        allocator.free(ret);
        ret = new_ret;
    }
    return ret;
}

fn convertNogood(
    allocator: std.mem.Allocator,
    nogood: ASTNode_Nogood,
) !?[]const u8 {
    if (nogood.set_expr.len == 0) {
        return null;
    }

    var ret = try convertSetExpr(allocator, nogood.set_expr[0]);

    for (nogood.set_expr[1..]) |set_expr| {
        const set_expr_str = try convertSetExpr(allocator, set_expr);
        const new_ret = try std.mem.concat(
            allocator,
            u8,
            &.{ ret, " \\/ ", set_expr_str },
        );
        allocator.free(set_expr_str);
        allocator.free(ret);
        ret = new_ret;
    }

    return ret;
}

fn convertSetExpr(
    allocator: std.mem.Allocator,
    set_expr: ASTNode_SetExpr,
) anyerror![]const u8 {
    var ret = try convertExpr(allocator, ASTNode_EqualityExpr, set_expr.expr);

    if (set_expr.args) |args| {
        var new_ret = try std.mem.concat(
            allocator,
            u8,
            &.{ ret, " in int(" },
        );
        allocator.free(ret);
        ret = new_ret;

        if (args.len > 0) {
            const numeric_str = try convertNumeric(allocator, args[0]);
            new_ret = try std.mem.concat(
                allocator,
                u8,
                &.{ ret, numeric_str },
            );
            allocator.free(ret);
            allocator.free(numeric_str);

            ret = new_ret;
        }

        if (args.len > 1) {
            for (args[1..]) |arg| {
                const numeric_str = try convertNumeric(allocator, arg);
                new_ret = try std.mem.concat(
                    allocator,
                    u8,
                    &.{ ret, ",", numeric_str },
                );
                allocator.free(ret);
                allocator.free(numeric_str);

                ret = new_ret;
            }
        }

        new_ret = try std.mem.concat(
            allocator,
            u8,
            &.{ ret, ")" },
        );
        allocator.free(ret);
        ret = new_ret;
    }

    return ret;
}

fn convertNumeric(
    allocator: std.mem.Allocator,
    numeric: ASTNode_Numeric,
) ![]const u8 {
    switch (numeric) {
        .number => |n| {
            return std.fmt.allocPrint(allocator, "{}", .{n});
        },
        .range => |r| {
            return std.fmt.allocPrint(allocator, "{}..{}", .{ r.start, r.end });
        },
    }
}

fn convertExpr(
    allocator: std.mem.Allocator,
    comptime ExprType: type,
    expr: ExprType,
) ![]const u8 {
    var ret: []const u8 = try (if (ExprType.SubExpr == ASTNode_AtomExpr)
        convertAtom(allocator, expr.expr)
    else
        convertExpr(allocator, ExprType.SubExpr, expr.expr));

    for (expr.ops) |op| {
        const op_str = switch (op.op) {
            .EQ => " = ",
            .NEQ => " != ",
            .LEQ => " <= ",
            .GEQ => " >= ",
            .LT => " < ",
            .GT => " > ",
            .PLUS => " + ",
            .MINUS => " - ",
            .MULT => " * ",
            .DIV => " / ",
            .MOD => " % ",
            else => @panic("Unrecongnized operation!"),
        };

        const expr_str: []const u8 = try (if (ExprType.SubExpr == ASTNode_AtomExpr)
            convertAtom(allocator, op.expr)
        else
            convertExpr(allocator, ExprType.SubExpr, op.expr));
        const new_ret = try std.mem.concat(
            allocator,
            u8,
            &.{ ret, op_str, expr_str },
        );
        allocator.free(ret);
        allocator.free(expr_str);

        ret = new_ret;
    }

    return ret;
}

fn convertAtom(
    allocator: std.mem.Allocator,
    atom: ASTNode_AtomExpr,
) ![]const u8 {
    return switch (atom) {
        .true => try std.fmt.allocPrint(allocator, "true", .{}),
        .false => try std.fmt.allocPrint(allocator, "false", .{}),
        .numeric => |node| try convertNumeric(allocator, node),
        .expr => |node| s: {
            const expr_str = try convertSetExpr(allocator, node.*);
            defer allocator.free(expr_str);

            break :s std.fmt.allocPrint(allocator, "({s})", .{expr_str});
        },
        .id => |identifier| s: {
            var ret = switch (identifier.domain) {
                .function => try std.fmt.allocPrint(allocator, "{s}_Function{}D[", .{ identifier.name, identifier.dimensions.len }),
                .matrix => try std.fmt.allocPrint(allocator, "{s}[", .{identifier.name}),
            };

            var firstDim = true;
            for (identifier.dimensions) |dim| {
                const connector: []const u8 = if (firstDim) "" else ",";
                firstDim = false;

                const new_ret = try std.fmt.allocPrint(allocator, "{s}{s}{}", .{ ret, connector, dim });
                allocator.free(ret);
                ret = new_ret;
            }
            const new_ret = try std.fmt.allocPrint(allocator, "{s}]", .{ret});
            allocator.free(ret);
            ret = new_ret;

            break :s ret;
        },
    };
}

const ConjureData = struct {
    finds: []const []const u8,
    domains: std.StringArrayHashMap(DomainType),

    /// Parse JSON from conjure
    fn parseLeaky(allocator: std.mem.Allocator, input: []const u8) !ConjureData {
        var ret = ConjureData{
            .finds = undefined,
            .domains = std.StringArrayHashMap(DomainType).init(allocator),
        };

        const conjure = try std.json.parseFromSliceLeaky(
            std.json.Value,
            allocator,
            input,
            .{},
        );

        const finds_json_array: std.json.Value = conjure.object.get("finds") orelse {
            @panic("Could not get finds!");
        };
        var finds_arraylist = std.ArrayList([]const u8).init(allocator);

        for (finds_json_array.array.items) |find| {
            try finds_arraylist.append(find.object.get("Name").?.string);
        }
        ret.finds = try finds_arraylist.toOwnedSlice();

        const original_domains_array: std.json.Value = conjure.object.get(
            "originalDomains",
        ) orelse {
            @panic("Could not get domains array!");
        };

        for (original_domains_array.array.items) |item| {
            const domain_name = item
                .array.items[0]
                .object.get("Name").?.string;

            for (ret.finds) |find| {
                if (std.mem.eql(u8, find, domain_name)) {
                    const domain = parseDomainType(
                        item.array.items[1]
                            .object.keys()[0],
                    );

                    try ret.domains.put(domain_name, domain);
                }
            }
        }

        return ret;
    }
};

pub fn parseAndConvert(
    allocator: std.mem.Allocator,
    conjure_data: ConjureData,
    input: []const u8,
) ![]const u8 {
    var tokens = lex(allocator, input);
    var diags = ParseDiagnostics{};
    var parseOptions = ParseOptions{
        .allocator = allocator,
        .conjure = conjure_data,
        .diags = &diags,
    };

    const parse_result = parse(parseOptions, tokens) catch |err| {
        print("Encountered an error while parsing:\n\n", .{});
        print("{s}\n", .{input});

        var charsToPrint = diags.error_token.?.col;
        while (charsToPrint != 0) : (charsToPrint -= 1) {
            print("~", .{});
        }
        print("^\n", .{});

        switch (err) {
            ParseError.UnexpectedEOF => {
                print("Reached EOF while parsing!\n", .{});
            },
            ParseError.UnexpectedToken => {
                print("Expected {s}, found '{s}'\n", .{ diags.error_expected.?, diags.error_token.?.lexeme });
            },
            ParseError.ExpectedExpression => {
                print("Expected an expression, found '{s}'\n", .{diags.error_token.?.lexeme});
            },
            ParseError.DomainNotFound => {
                print("Could not parse identifier, no domain found.\n", .{});
            },
            else => return err,
        }

        std.os.exit(1);
    };

    return try convertProgram(
        allocator,
        parse_result,
    );
}

pub fn main() !void {
    // Global Arena
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const input = try std.json.parseFromSliceLeaky(
        std.json.Value,
        allocator,
        json_input,
        .{},
    );

    const conjure_data = try ConjureData.parseLeaky(allocator, conjure_json_input);

    var converted_map = std.AutoArrayHashMap(i64, []const u8)
        .init(allocator);

    // Parser Arena
    var parser_arena = std.heap.ArenaAllocator.init(allocator);
    defer parser_arena.deinit();

    const parser_allocator = parser_arena.allocator();

    // Convert stuff
    for (input.object.values(), input.object.keys(), 0..) |in, key, line| {
        if (line % 10000 == 0) {
            _ = parser_arena.reset(.retain_capacity);
        }
        const obj = in.object;
        if (obj.get("representation") != null) {
            const string = obj.get("name").?.string;
            const representation = obj.get("representation").?.string;

            const converted = try parseAndConvert(
                parser_allocator,
                conjure_data,
                string,
            );
            defer allocator.free(converted);

            var pos_val: []const u8 = undefined;
            var neg_val: []const u8 = undefined;
            var pos_op: []const u8 = "=";
            var neg_op: []const u8 = "=";

            if (std.mem.eql(u8, "2vals", representation)) {
                neg_val = obj.get("val1").?.string;
                pos_val = obj.get("val2").?.string;
            } else if (std.mem.eql(u8, "order", representation)) {
                pos_val = obj.get("value").?.string;
                neg_val = pos_val;
                pos_op = "<=";
                neg_op = ">";
            } else {
                pos_val = obj.get("value").?.string;
                neg_val = pos_val;
                pos_op = "=";
                neg_op = "!=";
            }

            const positive = try std.fmt.allocPrint(
                allocator,
                "{s}{s}{s}",
                .{ converted, pos_op, pos_val },
            );
            const negative = try std.fmt.allocPrint(
                allocator,
                "{s}{s}{s}",
                .{ converted, neg_op, neg_val },
            );
            allocator.free(converted);

            const int_key = try std.fmt.parseInt(i64, key, 10);

            try converted_map.put(int_key, positive);
            try converted_map.put(-1 * int_key, negative);
            // print("{}: {s}\n", .{ int_key, positive });
            // print("-{}: {s}\n", .{ int_key, negative });
        }
    }

    // Replace satvars
    var stdout = std.io.getStdOut().writer();
    var lines = std.mem.split(u8, learnts_input, "\n");
    _ = lines.next().?; // Skip header
    while (lines.next()) |line| {
        var cells = std.mem.split(u8, line, ", ");
        var size_str = cells.next() orelse {
            continue;
        };
        var size = std.fmt.parseInt(usize, size_str, 10) catch {
            continue;
        };
        var converted_literals = try std.ArrayList([]const u8)
            .initCapacity(allocator, size);

        var clause = cells.next() orelse {
            continue;
        };
        var literals = std.mem.split(u8, clause, " ");

        while (literals.next()) |literal| {
            const literal_int = try std.fmt.parseInt(i64, literal, 10);
            const converted = converted_map.get(literal_int) orelse {
                continue;
            };
            try converted_literals.append(converted);
        }

        if (converted_literals.items.len == size) {
            const joined = try std.mem.join(
                allocator,
                " \\/ ",
                converted_literals.items,
            );
            defer allocator.free(joined);
            try stdout.print("{s},\n", .{joined});
        }
    }
}

test "Test the program" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const test_string = "(car_Function1D_00018 in int (2, 3, 4..10)) + 10";

    const tokens = lex(allocator, test_string);
    var diags = ParseDiagnostics{};

    const conjure_data = try ConjureData.parseLeaky(allocator, conjure_json_input);

    const parseOptions = ParseOptions{
        .allocator = allocator,
        .conjure = conjure_data,
        .diags = &diags,
    };
    const parse_result = parse(parseOptions, tokens) catch |err| {
        print("Encountered an error while parsing:\n\n", .{});
        print("{s}\n", .{test_string});

        var charsToPrint = diags.error_token.?.col;
        while (charsToPrint != 0) : (charsToPrint -= 1) {
            print("~", .{});
        }
        print("^\n", .{});

        switch (err) {
            ParseError.UnexpectedEOF => {
                print("Reached EOF while parsing!\n", .{});
            },
            ParseError.UnexpectedToken => {
                print("Expected {s}, found '{s}'\n", .{ diags.error_expected.?, diags.error_token.?.lexeme });
            },
            ParseError.ExpectedExpression => {
                print("Expected an expression, found '{s}'\n", .{diags.error_token.?.lexeme});
            },
            ParseError.DomainNotFound => {
                print("Could not parse identifier, no domain found.\n", .{});
            },
            ParseError.OutOfMemory => {
                print("Out of memory!\n", .{});
            },
        }

        std.os.exit(1);
    };

    parse_result.print();

    const converted = try convertProgram(
        std.testing.allocator,
        parse_result,
    );
    defer std.testing.allocator.free(converted);

    print("\nCONVERTED: {s}\n", .{converted});
}
