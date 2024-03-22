const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const model = @import("model_data.zig");
const print = std.debug.print;

const TokenType = lexer.TokenType;
const Token = lexer.Token;

const ParseDiagnostics = parser.ParseDiagnostics;
const ParseOptions = parser.ParseOptions;
const ParseError = parser.ParseError;
const DomainType = model.DomainType;
const ModelData = model.ModelData;

pub const log_level: std.log.Level = .debug;

pub const Delimiter = struct {
    lower: i64,
    higher: i64,
    higher_inclusive: bool,

    fn print(self: Delimiter) void {
        if (self.higher_inclusive) {
            std.debug.print("[{d: >5}, {d: >5}]", .{ self.lower, self.higher });
        } else {
            std.debug.print("[{d: >5}, {d: >5})", .{ self.lower, self.higher });
        }
    }

    fn contains(self: Delimiter, val: Value) bool {
        switch (val) {
            .all => {
                return true;
            },
            .val => |v| {
                if (self.higher_inclusive) {
                    return v >= self.lower and v <= self.higher;
                } else {
                    return v >= self.lower and v < self.higher;
                }
            },
        }
    }
};

pub const BinType = enum { hash, count };

pub const Bin = union(BinType) {
    hash: std.AutoArrayHashMap(Delimiter, *Bin),
    count: u32,

    pub fn print(self: Bin, print_percent: bool) void {
        switch (self) {
            .hash => |h| {
                if (print_percent) {
                    std.debug.print("              ", .{});
                    for (h.get(h.keys()[0]).?.hash.keys()) |k| {
                        k.print();
                        std.debug.print("    ", .{});
                    }
                    std.debug.print("\n", .{});
                }

                for (h.keys()) |k| {
                    if (print_percent) {
                        k.print();
                    }
                    h.get(k).?.print(false);
                    if (print_percent) {
                        std.debug.print("\n", .{});
                    }
                }
            },
            .count => |c| {
                std.debug.print(" {d: ^13}    ", .{c});
            },
        }
    }

    pub fn toCSV(self: Bin, allocator: std.mem.Allocator, line: bool) ![]u8 {
        switch (self) {
            .hash => |h| {
                var list = std.ArrayList(u8).init(allocator);
                for (h.keys()) |k| {
                    const str: []u8 = try h.get(k).?.toCSV(allocator, false);
                    if (line) {
                        try list.appendSlice(str[0 .. str.len - 2]);
                        try list.append('\n');
                    } else {
                        try list.appendSlice(str);
                    }
                }
                return try list.toOwnedSlice();
            },
            .count => |c| {
                return try std.fmt.allocPrint(allocator, "{d}, ", .{c});
            },
        }
    }
};

var delimiter_finder: std.ArrayList(std.AutoArrayHashMap(Value, []Delimiter)) = undefined;

pub fn createBin(allocator: std.mem.Allocator, d: model.Domain) !*Bin {
    const bins = try allocator.create(Bin);
    var list = std.ArrayList(model.Range).init(allocator);
    for (d.dimensions) |dim| {
        try list.append(dim);
    }
    // try list.append(d.domain);

    try createDelimiterHelper(allocator, list.items);
    return createBinHelper(allocator, bins, try list.toOwnedSlice());
}

fn createBinHelper(allocator: std.mem.Allocator, bin: *Bin, ranges: []model.Range) !*Bin {
    if (ranges.len == 0) {
        bin.* = .{
            .count = 0,
        };
        return bin;
    }

    bin.* = .{
        .hash = std.AutoArrayHashMap(Delimiter, *Bin).init(allocator),
    };
    const inc = @divFloor(ranges[0].upper - ranges[0].lower, 10);
    var l = ranges[0].lower;
    var u = l + inc;
    for (0..10) |i| {
        const del: Delimiter = .{
            .lower = l,
            .higher = u,
            .higher_inclusive = (i == 9),
        };
        const b1: *Bin = try allocator.create(Bin);
        try bin.hash.put(del, try createBinHelper(allocator, b1, ranges[1..]));

        if (i == 8) {
            l = u;
            u = ranges[0].upper;
        } else {
            l = u;
            u += inc;
        }
    }

    return bin;
}

fn createDelimiterHelper(allocator: std.mem.Allocator, rs: []model.Range) !void {
    delimiter_finder = std.ArrayList(std.AutoArrayHashMap(Value, []Delimiter)).init(allocator);

    for (rs) |r| {
        const inc = @divFloor(r.upper - r.lower, 10);
        var l = r.lower;
        var u = l + inc;
        var possible_delimiters = std.ArrayList(Delimiter).init(allocator);
        for (0..10) |i| {
            const del: Delimiter = .{
                .lower = l,
                .higher = u,
                .higher_inclusive = (i == 9),
            };

            try possible_delimiters.append(del);

            if (i == 8) {
                l = u;
                u = r.upper;
            } else {
                l = u;
                u += inc;
            }
        }

        var delimiters = std.AutoArrayHashMap(Value, []Delimiter).init(allocator);
        var i = r.lower;
        while (i <= r.upper) : (i += 1) {
            const v: Value = .{
                .val = i,
            };
            var fitting_delimiters = std.ArrayList(Delimiter).init(allocator);
            for (possible_delimiters.items) |del| {
                if (del.contains(v)) {
                    try fitting_delimiters.append(del);
                }
            }

            try delimiters.put(v, try fitting_delimiters.toOwnedSlice());
        }

        try delimiters.put(Value.all, try possible_delimiters.toOwnedSlice());
        try delimiter_finder.append(delimiters);
    }
}

pub const ValueType = enum { val, all };
pub const Value = union(ValueType) {
    val: isize,
    all: void,
};

pub fn increment(bin: *Bin, vals: []Value) void {
    incrementHelper(bin, vals, delimiter_finder.items);
}

fn incrementHelper(bin: *Bin, vals: []Value, finder: []std.AutoArrayHashMap(Value, []Delimiter)) void {
    if (vals.len == 0) {
        bin.count += 1;
        return;
    }

    const delimiters: []Delimiter = finder[0].get(vals[0]).?;
    for (delimiters) |del| {
        incrementHelper(bin.hash.get(del).?, vals[1..], finder[1..]);
    }
}

pub const BinReturn = struct {
    dimensions: []i64,
    val: ?isize,
    op: ?lexer.TokenType,

    fn print(self: BinReturn) void {
        std.debug.print("{d}", .{self.dimensions.len});
        std.debug.print("dimensions: ", .{});
        for (self.dimensions) |d| {
            std.debug.print("{}, ", .{d});
        }
        std.debug.print("\n", .{});
        if (self.val) |v| {
            std.debug.print("val: {d}\n", .{v});
        } else {
            std.debug.print("val: {s}\n", .{"null"});
        }
        if (self.op) |o| {
            std.debug.print("op: {}\n", .{o});
        } else {
            std.debug.print("op: {s}\n", .{"null"});
        }
    }
};

const BinErrors = error{
    OutOfMemory,
};

// TODO(oyy): The way this is implemented cannot handle more than one find variable.
pub fn binProgram(allocator: std.mem.Allocator, program: *parser.ASTNode_Program) BinErrors![]const BinReturn {
    var return_list = std.ArrayList(BinReturn).init(allocator);
    for (program.nogood) |nogood| {
        var nogood_values = try binNogood(allocator, nogood);
        try return_list.appendSlice(try nogood_values.toOwnedSlice());
        // for (nogood_values.items) |n| {
        //     if (n.dimensions.len == 0) {
        //         continue;
        //     }
        //     var increment_list = std.ArrayList(Value).init(allocator);
        //     for (n.dimensions) |d| {
        //         try increment_list.append(.{ .val = d });
        //     }

        //     const domain = model_data.domains[0].domain;
        //     if (n.val) |v| {
        //         switch (n.op.?) {
        //             .EQ => {
        //                 try increment_list.append(.{ .val = v });
        //             },
        //             .NEQ => {
        //                 var i = domain.lower;
        //                 while (i <= domain.upper) : (i += 1) {
        //                     if (i != v) {
        //                         try increment_list.append(.{ .val = i });
        //                         increment( increment_list.items);
        //                         _ = increment_list.pop();
        //                     }
        //                 }
        //                 increment_list.deinit();
        //                 continue;
        //             },
        //             .LT => {
        //                 var i = domain.lower;
        //                 while (i < v) : (i += 1) {
        //                     try increment_list.append(.{ .val = i });
        //                     increment( increment_list.items);
        //                     _ = increment_list.pop();
        //                 }
        //                 increment_list.deinit();
        //                 continue;
        //             },
        //             .LEQ => {
        //                 var i = domain.lower;
        //                 while (i <= v) : (i += 1) {
        //                     try increment_list.append(.{ .val = i });
        //                     increment( increment_list.items);
        //                     _ = increment_list.pop();
        //                 }
        //                 increment_list.deinit();
        //                 continue;
        //             },
        //             .GT => {
        //                 var i = v + 1;
        //                 while (i <= domain.upper) : (i += 1) {
        //                     try increment_list.append(.{ .val = i });
        //                     increment( increment_list.items);
        //                     _ = increment_list.pop();
        //                 }
        //                 increment_list.deinit();
        //                 continue;
        //             },
        //             .GEQ => {
        //                 var i = v;
        //                 while (i <= domain.upper) : (i += 1) {
        //                     try increment_list.append(.{ .val = i });
        //                     increment( increment_list.items);
        //                     _ = increment_list.pop();
        //                 }
        //                 increment_list.deinit();
        //                 continue;
        //             },
        //             else => {
        //                 print("This case should never be reached.", .{});
        //             },
        //         }
        //     } else {
        //         // try increment_list.append(Value.all);
        //     }

        //     increment( try increment_list.toOwnedSlice());
        // }
    }
    return try return_list.toOwnedSlice();
}

fn binNogood(allocator: std.mem.Allocator, nogood: *parser.ASTNode_Nogood) BinErrors!std.ArrayList(BinReturn) {
    var return_list = std.ArrayList(BinReturn).init(allocator);
    for (nogood.set_expr) |set| {
        var bin_values = try binSetExpr(allocator, set);
        try return_list.appendSlice(try bin_values.toOwnedSlice());
    }

    return return_list;
}

fn binSetExpr(allocator: std.mem.Allocator, set: *parser.ASTNode_SetExpr) BinErrors!std.ArrayList(BinReturn) {
    if (set.args) |nums| {
        var shift_values = try binShiftExpr(allocator, set.expr);
        defer shift_values.deinit();
        var return_list = std.ArrayList(BinReturn).init(allocator);
        // NOTE: This should always be length one. It should directly go to an identifier.
        // Also the optional field should be null.
        std.testing.expect(shift_values.items.len == 1) catch {
            print("This should have never happened. Located in binSetExpr.", .{});
        };
        const shift_val = shift_values.items[0];
        for (nums) |num| {
            switch (num.*) {
                .number => |n| {
                    try return_list.append(.{
                        .dimensions = shift_val.dimensions,
                        .val = n,
                        .op = .EQ,
                    });
                },
                .range => |range| {
                    var i = range.start;
                    while (i <= range.end) : (i += 1) {
                        try return_list.append(.{
                            .dimensions = shift_val.dimensions,
                            .val = i,
                            .op = .EQ,
                        });
                    }
                },
            }
        }

        return return_list;
    } else {
        return try binShiftExpr(allocator, set.expr);
    }
}

fn binShiftExpr(allocator: std.mem.Allocator, shift: *parser.ASTNode_ShiftExpr) BinErrors!std.ArrayList(BinReturn) {
    switch (shift.*) {
        .shift => |s| {
            var lhs_bins = try binShiftExpr(allocator, s.lhs);
            var rhs_bins = try binShiftExpr(allocator, s.rhs);
            var combined = std.ArrayList(BinReturn).init(allocator);
            try combined.appendSlice(try lhs_bins.toOwnedSlice());
            try combined.appendSlice(try rhs_bins.toOwnedSlice());

            return combined;
        },
        .or_expr => |or_expr| {
            return try binOrExpr(allocator, or_expr);
        },
    }
}

fn binOrExpr(allocator: std.mem.Allocator, or_expr: *parser.ASTNode_OrExpr) BinErrors!std.ArrayList(BinReturn) {
    if (or_expr.ops.len > 0) {
        var return_list = std.ArrayList(BinReturn).init(allocator);
        var expr = try binAndExpr(allocator, or_expr.expr);
        try return_list.appendSlice(try expr.toOwnedSlice());

        for (or_expr.ops) |op| {
            var _expr = try binAndExpr(allocator, op.expr);
            try return_list.appendSlice(try _expr.toOwnedSlice());
        }

        return return_list;
    } else {
        return try binAndExpr(allocator, or_expr.expr);
    }
}

fn binAndExpr(allocator: std.mem.Allocator, and_expr: *parser.ASTNode_AndExpr) BinErrors!std.ArrayList(BinReturn) {
    if (and_expr.ops.len > 0) {
        var return_list = std.ArrayList(BinReturn).init(allocator);
        var expr = try binEqualityExpr(allocator, and_expr.expr);
        try return_list.appendSlice(try expr.toOwnedSlice());

        for (and_expr.ops) |op| {
            var _expr = try binEqualityExpr(allocator, op.expr);
            try return_list.appendSlice(try _expr.toOwnedSlice());
        }

        return return_list;
    } else {
        return try binEqualityExpr(allocator, and_expr.expr);
    }
}

fn binEqualityExpr(allocator: std.mem.Allocator, eq_expr: *parser.ASTNode_EqualityExpr) BinErrors!std.ArrayList(BinReturn) {
    if (eq_expr.ops.len > 0) {
        // NOTE: EqualityExpr Operator length should always be 1.
        //       In most programming languages you cannot chain equality operators.
        std.testing.expect(eq_expr.ops.len == 1) catch {
            print("This should have never happened. Located in binEqualityExpr.", .{});
        };
        var return_list = std.ArrayList(BinReturn).init(allocator);
        const op = eq_expr.ops[0];

        var lhs = try binComparisonExpr(allocator, eq_expr.expr);
        var rhs = try binComparisonExpr(allocator, op.expr);

        //~oyy: If they are both singular we can get information about the value compared.
        //      Otherwise, it is impossible to get information about the restricted domain.
        // if (lhs.len == 1 and rhs.len == 1) {
        //     // Should be able to check if two numbers are equal to each other but that would be stupid in this case.
        //     if (lhs[0].val) |v| {
        //         var dim_list = std.ArrayList(i64).init(allocator);
        //         try dim_list.appendSlice(rhs[0].dimensions);
        //         var ret = [_]BinReturn{.{
        //             .dimensions = try dim_list.toOwnedSlice(),
        //             .val = v,
        //             .op = op.op,
        //         }};
        //         return &ret;
        //     } else if (rhs[0].val) |v| {
        //         var dim_list = std.ArrayList(i64).init(allocator);
        //         try dim_list.appendSlice(lhs[0].dimensions);
        //         var ret = [_]BinReturn{.{
        //             .dimensions = try dim_list.toOwnedSlice(),
        //             .val = v,
        //             .op = op.op,
        //         }};
        //         return &ret;
        //     }
        // }

        try return_list.appendSlice(try lhs.toOwnedSlice());
        try return_list.appendSlice(try rhs.toOwnedSlice());

        return return_list;
    } else {
        return try binComparisonExpr(allocator, eq_expr.expr);
    }
}

fn binComparisonExpr(allocator: std.mem.Allocator, cmp_expr: *parser.ASTNode_ComparisonExpr) BinErrors!std.ArrayList(BinReturn) {
    if (cmp_expr.ops.len > 0) {
        // NOTE: ComparisonExpr Operator length should always be 1.
        //       In most programming languages you cannot chain comparison operators.
        std.testing.expect(cmp_expr.ops.len == 1) catch {
            print("This should have never happened. Located in binEqualityExpr.", .{});
        };
        var return_list = std.ArrayList(BinReturn).init(allocator);
        const op = cmp_expr.ops[0];
        var lhs = try binTermExpr(allocator, cmp_expr.expr);
        var rhs = try binTermExpr(allocator, op.expr);

        // if (lhs.len == 1 and rhs.len == 1) {
        //     if (lhs[0].val) |v| {
        //         var op_new: lexer.TokenType = undefined;
        //         if (op.op == .LT) {
        //             op_new = .GT;
        //         } else if (op.op == .LEQ) {
        //             op_new = .GEQ;
        //         } else if (op.op == .GT) {
        //             op_new = .LT;
        //         } else {
        //             op_new = .LEQ;
        //         }
        //         var dim_list = std.ArrayList(i64).init(allocator);
        //         try dim_list.appendSlice(rhs[0].dimensions);
        //         var ret = [_]BinReturn{.{
        //             .dimensions = try dim_list.toOwnedSlice(),
        //             .val = v,
        //             .op = op.op,
        //         }};
        //         return &ret;
        //     } else if (rhs[0].val) |v| {
        //         var dim_list = std.ArrayList(i64).init(allocator);
        //         try dim_list.appendSlice(lhs[0].dimensions);
        //         var ret = [_]BinReturn{.{
        //             .dimensions = try dim_list.toOwnedSlice(),
        //             .val = v,
        //             .op = op.op,
        //         }};
        //         return &ret;
        //     }
        // }

        try return_list.appendSlice(try lhs.toOwnedSlice());
        try return_list.appendSlice(try rhs.toOwnedSlice());

        return return_list;
    } else {
        return try binTermExpr(allocator, cmp_expr.expr);
    }
}

fn binTermExpr(allocator: std.mem.Allocator, term_expr: *parser.ASTNode_TermExpr) BinErrors!std.ArrayList(BinReturn) {
    if (term_expr.ops.len > 0) {
        var return_list = std.ArrayList(BinReturn).init(allocator);
        var expr = try binFactorExpr(allocator, term_expr.expr);
        try return_list.appendSlice(try expr.toOwnedSlice());

        for (term_expr.ops) |op| {
            var _expr = try binFactorExpr(allocator, op.expr);
            try return_list.appendSlice(try _expr.toOwnedSlice());
        }

        return return_list;
    } else {
        return try binFactorExpr(allocator, term_expr.expr);
    }
}

fn binFactorExpr(allocator: std.mem.Allocator, factor_expr: *parser.ASTNode_FactorExpr) BinErrors!std.ArrayList(BinReturn) {
    if (factor_expr.ops.len > 0) {
        var return_list = std.ArrayList(BinReturn).init(allocator);
        var expr = try binUnaryExpr(allocator, factor_expr.expr);
        try return_list.appendSlice(try expr.toOwnedSlice());

        for (factor_expr.ops) |op| {
            var _expr = try binUnaryExpr(allocator, op.expr);
            try return_list.appendSlice(try _expr.toOwnedSlice());
        }

        return return_list;
    } else {
        return try binUnaryExpr(allocator, factor_expr.expr);
    }
}

fn binUnaryExpr(allocator: std.mem.Allocator, unary_expr: *parser.ASTNode_UnaryExpr) BinErrors!std.ArrayList(BinReturn) {
    switch (unary_expr.*) {
        .atom => |atom| {
            return try binAtomExpr(allocator, atom);
        },
        .unary => |expr| {
            var next_unary = try binUnaryExpr(allocator, expr.expr);

            if (next_unary.items.len == 1 and expr.op == .MINUS) {
                if (next_unary.items[0].val) |v| {
                    next_unary.items[0].val = -v;
                    var return_list = std.ArrayList(BinReturn).init(allocator);
                    try return_list.appendSlice(try next_unary.toOwnedSlice());
                    return return_list;
                }
            }

            return next_unary;
        },
    }
}

fn binAtomExpr(allocator: std.mem.Allocator, atom_expr: *parser.ASTNode_AtomExpr) BinErrors!std.ArrayList(BinReturn) {
    switch (atom_expr.*) {
        .true => {
            return std.ArrayList(BinReturn).init(allocator);
        },
        .false => {
            return std.ArrayList(BinReturn).init(allocator);
        },
        .expr => |set_expr| {
            return try binSetExpr(allocator, set_expr);
        },
        .numeric => |num| {
            switch (num.*) {
                .number => |number| {
                    var return_list = std.ArrayList(BinReturn).init(allocator);
                    try return_list.append(.{
                        .dimensions = &[_]i64{},
                        .val = number,
                        .op = null,
                    });
                    return return_list;
                },
                .range => |range| {
                    var return_list = std.ArrayList(BinReturn).init(allocator);
                    var i = range.start;
                    while (i <= range.end) : (i += 1) {
                        try return_list.append(
                            .{
                                .dimensions = &[_]i64{},
                                .val = i,
                                .op = null,
                            },
                        );
                    }
                    return return_list;
                },
            }
        },
        .id => |id| {
            var dim_list = std.ArrayList(i64).init(allocator);
            try dim_list.appendSlice(id.dimensions);
            var return_list = std.ArrayList(BinReturn).init(allocator);
            try return_list.append(.{
                .dimensions = try dim_list.toOwnedSlice(),
                .val = null,
                .op = null,
            });
            return return_list;
        },
    }
}
