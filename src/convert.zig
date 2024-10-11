const std = @import("std");
const parser = @import("parser.zig");

pub fn convertProgram(
    allocator: std.mem.Allocator,
    program: *parser.ASTNode_Program,
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
    nogood: *parser.ASTNode_Nogood,
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
    set_expr: *parser.ASTNode_SetExpr,
) anyerror![]const u8 {
    var ret = try convertShift(allocator, set_expr.expr);

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

fn convertShift(
    allocator: std.mem.Allocator,
    shift: *parser.ASTNode_ShiftExpr,
) ![]const u8 {
    return switch (shift.*) {
        .or_expr => |or_expr| convertExpr(allocator, parser.ASTNode_OrExpr, or_expr),
        .shift => |s| b: {
            const lhs_str = try convertShift(allocator, s.lhs);
            const rhs_str = try convertShift(allocator, s.rhs);
            defer allocator.free(lhs_str);
            defer allocator.free(rhs_str);

            break :b std.fmt.allocPrint(allocator, "{s} + {s}", .{ lhs_str, rhs_str });
        },
    };
}

fn convertNumeric(
    allocator: std.mem.Allocator,
    numeric: *parser.ASTNode_Numeric,
) ![]const u8 {
    switch (numeric.*) {
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
    expr: *ExprType,
) ![]const u8 {
    var ret: []const u8 = try (if (ExprType.SubExpr == parser.ASTNode_UnaryExpr)
        convertUnary(allocator, expr.expr)
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
            .OR => " \\/ ",
            .AND => " /\\ ",
            else => @panic("Unrecognized operation!"),
        };

        const expr_str: []const u8 = try (if (ExprType.SubExpr == parser.ASTNode_UnaryExpr)
            convertUnary(allocator, op.expr)
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

fn convertUnary(
    allocator: std.mem.Allocator,
    unary: *parser.ASTNode_UnaryExpr,
) ![]const u8 {
    return switch (unary.*) {
        .atom => |atom| convertAtom(allocator, atom),
        .unary => |u| b: {
            const unary_str = try convertUnary(allocator, u.expr);
            defer allocator.free(unary_str);

            var op_str = "!";
            if (u.op == .MINUS) {
                op_str = "-";
            }

            break :b std.fmt.allocPrint(allocator, "{s}{s}", .{ op_str, unary_str });
        },
    };
}

fn convertAtom(
    allocator: std.mem.Allocator,
    atom: *parser.ASTNode_AtomExpr,
) ![]const u8 {
    return switch (atom.*) {
        .true => try std.fmt.allocPrint(allocator, "true", .{}),
        .false => try std.fmt.allocPrint(allocator, "false", .{}),
        .numeric => |node| try convertNumeric(allocator, node),
        .expr => |node| s: {
            const expr_str = try convertSetExpr(allocator, node);
            defer allocator.free(expr_str);

            break :s std.fmt.allocPrint(allocator, "({s})", .{expr_str});
        },
        .id => |identifier| s: {
            var ret = switch (identifier.domain) {
                .function => try std.fmt.allocPrint(allocator, "{s}_Function{}D[", .{ identifier.name, identifier.dimensions.len }),
                .matrix => try std.fmt.allocPrint(allocator, "{s}[", .{identifier.name}),
                .integer => try std.fmt.allocPrint(allocator, "{s}", .{identifier.name}),
            };

            var firstDim = true;
            for (identifier.dimensions) |dim| {
                const connector: []const u8 = if (firstDim) "" else ", ";
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
