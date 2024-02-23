const std = @import("std");
const print = std.debug.print;

pub const TokenType = enum {
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

pub const Token = struct {
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

pub fn lex(allocator: std.mem.Allocator, input: []const u8) []Token {
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
