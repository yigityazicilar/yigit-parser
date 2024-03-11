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

const DelimiterType = enum { range, op };
pub const Op = enum { LEQ, GEQ };

pub const Delimiter = union(DelimiterType) {
    op: struct { op: Op, value: isize },
    range: struct { lower: isize, higher: isize, higher_inclusive: bool },

    fn print(self: Delimiter) void {
        switch (self) {
            .op => |op| {
                var op_string = if (op.op == Op.LEQ) "less than" else "greater than";
                std.debug.print("Delimiter: {s} or equal to {d}\n", .{ op_string, op.value });
            },
            .range => |r| {
                if (r.higher_inclusive) {
                    std.debug.print("Delimiter: Between [{d}, {d}]\n", .{ r.lower, r.higher });
                } else {
                    std.debug.print("Delimiter: Between [{d}, {d})\n", .{ r.lower, r.higher });
                }
            },
        }
    }

    fn contains(self: Delimiter, val: Value) bool {
        switch (val) {
            .all => {
                return true;
            },
            .val => |v| {
                switch (self) {
                    .op => |op| {
                        if (op.op == .LEQ) {
                            return v <= op.value;
                        } else {
                            return v >= op.value;
                        }
                    },
                    .range => |r| {
                        if (r.higher_inclusive) {
                            return v >= r.lower and v <= r.higher;
                        } else {
                            return v >= r.lower and v < r.higher;
                        }
                    },
                }
            },
        }
    }
};

pub const BinType = enum { hash, count };

pub const Bin = union(BinType) {
    hash: std.AutoArrayHashMap(Delimiter, *Bin),
    count: u32,
};

var delimiter_finder: std.ArrayList(std.AutoArrayHashMap(Value, []Delimiter)) = undefined;

pub fn createBin(allocator: std.mem.Allocator, d: model.Domain) !*Bin {
    const bins = try allocator.create(Bin);
    var list = std.ArrayList(model.Range).init(allocator);
    for (d.dimensions) |dim| {
        try list.append(dim);
    }
    try list.append(d.domain);

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
        const range: Delimiter = .{
            .range = .{
                .lower = l,
                .higher = u,
                .higher_inclusive = (i == 9),
            },
        };
        const b1: *Bin = try allocator.create(Bin);
        try bin.hash.put(range, try createBinHelper(allocator, b1, ranges[1..]));

        const less_than: Delimiter = .{
            .op = .{
                .op = .LEQ,
                .value = u,
            },
        };
        const b2: *Bin = try allocator.create(Bin);
        try bin.hash.put(less_than, try createBinHelper(allocator, b2, ranges[1..]));

        const greater_than: Delimiter = .{
            .op = .{
                .op = .GEQ,
                .value = l,
            },
        };
        const b3: *Bin = try allocator.create(Bin);
        try bin.hash.put(greater_than, try createBinHelper(allocator, b3, ranges[1..]));

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
            const range: Delimiter = .{
                .range = .{
                    .lower = l,
                    .higher = u,
                    .higher_inclusive = (i == 9),
                },
            };
            const less_than: Delimiter = .{
                .op = .{
                    .op = .LEQ,
                    .value = u,
                },
            };
            const greater_than: Delimiter = .{
                .op = .{
                    .op = .GEQ,
                    .value = l,
                },
            };

            try possible_delimiters.appendSlice(&.{ range, less_than, greater_than });

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

const BinReturn = struct {
    dimensions: []isize,
    val: ?isize,
    op: ?lexer.TokenType,
};

pub fn binProgram(allocator: std.mem.Allocator, bin: *Bin, program: *parser.ASTNode_Program) void {
    for (program.nogood) |nogood| {
        const nogood_values = binNogood(allocator, bin, nogood);
        for (nogood_values) |n| {
            var increment_list = std.ArrayList(Value).init(allocator);
            for (n.dimensions) |d| {
                try increment_list.append(.{ .val = d });
            }

            // TODO(oyy): Check the operator here and assign values depending on that.
            if (n.val) |v| {
                try increment_list.append(.{ .val = v });
            } else {
                try increment_list.append(Value.all);
            }

            increment(bin, try increment_list.toOwnedSlice());
        }
    }
}

fn binNogood(allocator: std.mem.Allocator, bin: *Bin, nogood: *parser.ASTNode_Nogood) []BinReturn {
    const return_list = std.ArrayList(Value).init(allocator);
    for (nogood.set_expr) |set| {
        const bin_values = binSetExpr(allocator, bin, set);
        return_list.appendSlice(bin_values);
    }

    return try return_list.toOwnedSlice();
}

fn binSetExpr(allocator: std.mem.Allocator, bin: *Bin, set: *parser.ASTNode_SetExpr) []BinReturn {
    if (set.args) |nums| {
        const shift_values = binShiftExpr(allocator, bin, set.expr);
        var return_list = std.ArrayList(BinReturn).init();
        // NOTE: This should always be length one. It should directly go to an identifier.
        // Also the optional field should be null.
        std.testing.expect(shift_values.len == 1) orelse {
            print("This should have never happened. Located in binSetExpr.");
        };
        const shift_val = shift_values[0];
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

        return try return_list.toOwnedSlice();
    } else {
        return binShiftExpr(allocator, bin, set.expr);
    }
}

fn binShiftExpr(allocator: std.mem.Allocator, bin: *Bin, shift: *parser.ASTNode_ShiftExpr) []BinReturn {
    switch (shift.*) {
        .shift => |s| {
            var lhs_bins = binShiftExpr(allocator, bin, s.lhs);
            var rhs_bins = binShiftExpr(allocator, bin, s.rhs);
            var combined = std.ArrayList(BinReturn).init(allocator);
            try combined.appendSlice(lhs_bins);
            try combined.appendSlice(rhs_bins);

            return try combined.toOwnedSlice();
        },
        .or_expr => |or_expr| {
            return binOrExpr(allocator, bin, or_expr);
        },
    }
}

fn binOrExpr(allocator: std.mem.Allocator, bin: *Bin, or_expr: *parser.ASTNode_OrExpr) []BinReturn {
    if (or_expr.ops.len > 0) {
        var return_list = std.ArrayList(BinReturn).init();
        try return_list.appendSlice(binAndExpr(allocator, bin, or_expr.expr));

        for (or_expr.ops) |op| {
            try return_list.appendSlice(binAndExpr(allocator, bin, op.expr));
        }

        return try return_list.toOwnedSlice();
    } else {
        return binAndExpr(allocator, bin, or_expr.expr);
    }
}

fn binAndExpr(allocator: std.mem.Allocator, bin: *Bin, and_expr: *parser.ASTNode_AndExpr) []BinReturn {
    if (and_expr.ops.len > 0) {
        var return_list = std.ArrayList(BinReturn).init();
        try return_list.appendSlice(binEqualityExpr(allocator, bin, and_expr.expr));

        for (and_expr.ops) |op| {
            try return_list.appendSlice(binEqualityExpr(allocator, bin, op.expr));
        }

        return try return_list.toOwnedSlice();
    } else {
        return binEqualityExpr(allocator, bin, and_expr.expr);
    }
}

fn binEqualityExpr(allocator: std.mem.Allocator, bin: *Bin, eq_expr: *parser.ASTNode_EqualityExpr) []BinReturn {
    if (eq_expr.ops.len > 0) {
        // NOTE: EqualityExpr Operator length should always be 1.
        //       In most programming languages you cannot chain equality operators.
        std.testing.expect(eq_expr.ops.len == 1) orelse {
            print("This should have never happened. Located in binEqualityExpr.");
        };
        var return_list = std.ArrayList(BinReturn).init();
        const op = eq_expr.ops[0];

        const lhs: []BinReturn = binComparisonExpr(allocator, bin, eq_expr.expr);
        const rhs: []BinReturn = binComparisonExpr(allocator, bin, op.expr);
        //~oyy: If they are both singular we can get information about the value compared.
        //      Otherwise, it is impossible to get information about the restricted domain.
        if (lhs.len == 1 and rhs.len == 1) {
            // Should be able to check if two numbers are equal to each other but that would be stupid in this case.
            // TODO(oyy): Need to decide on what to do for not equals. Should increment everything that is not those bins.
            if (lhs[0].val) |v| {
                return &.{.{
                    .dimensions = rhs[0].dimensions,
                    .val = v,
                    .op = op.op,
                }};
            } else if (rhs[0].val) |v| {
                return &.{.{
                    .dimensions = lhs[0].dimensions,
                    .val = v,
                    .op = op.op,
                }};
            } else {
                return &.{ lhs[0], rhs[0] };
            }
        } else {
            try return_list.appendSlice(lhs);
            try return_list.appendSlice(rhs);

            return try return_list.toOwnedSlice();
        }
    } else {
        return binComparisonExpr(allocator, bin, eq_expr.expr);
    }
}

fn binComparisonExpr(allocator: std.mem.Allocator, bin: *Bin, cmp_expr: *parser.ASTNode_ComparisonExpr) []BinReturn {
    if (cmp_expr.ops.len > 0) {
        // NOTE: ComparisonExpr Operator length should always be 1.
        //       In most programming languages you cannot chain comparison operators.
        std.testing.expect(cmp_expr.ops.len == 1) orelse {
            print("This should have never happened. Located in binEqualityExpr.");
        };
        var return_list = std.ArrayList(BinReturn).init();
        const op = cmp_expr.ops[0];
        const lhs = binTermExpr(allocator, bin, cmp_expr.expr);
        const rhs = binTermExpr(allocator, bin, op.expr);

        if (lhs.len == 1 and rhs.len == 1) {
            if (lhs[0].val) |v| {
                return &.{.{
                    .dimensions = rhs[0].dimensions,
                    .val = v,
                    .op = op.op,
                }};
            } else if (rhs[0].val) |v| {
                return &.{.{
                    .dimensions = lhs[0].dimensions,
                    .val = v,
                    .op = op.op,
                }};
            } else {
                return &.{ lhs[0], rhs[0] };
            }
        } else {
            try return_list.appendSlice(lhs);
            try return_list.appendSlice(rhs);

            return try return_list.toOwnedSlice();
        }
    } else {
        return binTermExpr(allocator, bin, cmp_expr.expr);
    }
}

fn binTermExpr(allocator: std.mem.Allocator, bin: *Bin, term_expr: *parser.ASTNode_TermExpr) []BinReturn {
    if (term_expr.ops.len > 0) {
        var return_list = std.ArrayList(BinReturn).init();
        try return_list.appendSlice(binFactorExpr(allocator, bin, term_expr.expr));

        for (term_expr.ops) |op| {
            try return_list.appendSlice(binFactorExpr(allocator, bin, op.expr));
        }

        return try return_list.toOwnedSlice();
    } else {
        return binFactorExpr(allocator, bin, term_expr.expr);
    }
}

fn binFactorExpr(allocator: std.mem.Allocator, bin: *Bin, factor_expr: *parser.ASTNode_FactorExpr) []BinReturn {
    if (factor_expr.ops.len > 0) {
        var return_list = std.ArrayList(BinReturn).init();
        try return_list.appendSlice(binUnaryExpr(allocator, bin, factor_expr.expr));

        for (factor_expr.ops) |op| {
            try return_list.appendSlice(binUnaryExpr(allocator, bin, op.expr));
        }

        return try return_list.toOwnedSlice();
    } else {
        return binUnaryExpr(allocator, bin, factor_expr.expr);
    }
}

fn binUnaryExpr(allocator: std.mem.Allocator, bin: *Bin, unary_expr: *parser.ASTNode_UnaryExpr) []BinReturn {
    switch (unary_expr.*) {
        .atom => |atom| {
            return binAtomExpr(allocator, bin, atom);
        },
        .unary => |expr| {
            const next_unary = binUnaryExpr(allocator, bin, expr.expr);
            if (next_unary.len == 1 and expr.op == .MINUS) {
                if (next_unary[0].val) |v| {
                    next_unary[0].val = -v;
                    return &.{next_unary[0]};
                }
            }
            return next_unary;
        },
    }
}

fn binAtomExpr(allocator: std.mem.Allocator, bin: *Bin, atom_expr: *parser.ASTNode_AtomExpr) []BinReturn {
    switch (atom_expr.*) {
        .true => {
            return &.{};
        },
        .false => {
            return &.{};
        },
        .expr => |set_expr| {
            return binSetExpr(allocator, bin, set_expr);
        },
        .numeric => |num| {
            switch (num.*) {
                .number => |number| {
                    return &.{
                        .{
                            .dimensions = &.{},
                            .val = number,
                        },
                    };
                },
                .range => |range| {
                    var return_list = std.ArrayList(BinReturn).init();
                    var i = range.start;
                    while (i <= range.end) : (i += 1) {
                        try return_list.append(
                            .{
                                .dimensions = &.{},
                                .val = i,
                            },
                        );
                    }
                    return try return_list.toOwnedSlice();
                },
            }
        },
        .id => |id| {
            return &.{
                .{
                    .dimensions = id.dimensions,
                },
            };
        },
    }
}
