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

            try possible_delimiters.append(range);
            try possible_delimiters.append(less_than);
            try possible_delimiters.append(greater_than);

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
