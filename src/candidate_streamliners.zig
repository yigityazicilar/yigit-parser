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
const Op = enum { LEQ, G };

pub const Delimiter = union(DelimiterType) {
    op: struct { op: Op, value: isize },
    range: struct { lower: isize, higher: isize },
};

pub const BinType = enum { hash, count };

pub const Bin = union(BinType) {
    hash: std.AutoArrayHashMap(Delimiter, *Bin),
    count: u32,
};

pub fn createBin(allocator: std.mem.Allocator, d: model.Domain) *Bin {
    const domain_bin = try allocator.create(Bin);
    const list = std.ArrayList(model.Range).init(allocator);
    for (d.dimensions) |dim| {
        try list.append(dim);
    }

    try list.append(d.domain);

    return createBinHelper(allocator, domain_bin, list.items);
}

fn createBinHelper(allocator: std.mem.Allocator, bin: *Bin, ranges: []model.Range) *Bin {
    if (ranges.len == 0) {
        bin.count = 0;
        return bin;
    }

    bin.hash = std.AutoArrayHashMap(Delimiter, *Bin).init(allocator);
    const increment: u32 = @divFloor(ranges[0].upper - ranges[0].lower, 10);
    var l = ranges[0].lower;
    var u = l + increment;
    for (0..10) |i| {
        const d1: Delimiter = .{
            .range = .{
                .lower = l,
                .higher = u,
            },
        };
        const b1: *Bin = try allocator.create(Bin);
        try bin.hash.put(d1, createBinHelper(allocator, b1, ranges[1..]));

        const d2: Delimiter = .{
            .op = .{
                .op = .LEQ,
                .value = u,
            },
        };
        const b2: *Bin = try allocator.create(Bin);
        try bin.hash.put(d2, createBinHelper(allocator, b2, ranges[1..]));

        const d3: Delimiter = .{
            .op = .{
                .op = .G,
                .value = l,
            },
        };
        const b3: *Bin = try allocator.create(Bin);
        try bin.hash.put(d3, createBinHelper(allocator, b3, ranges[1..]));

        if (i == 8) {
            l = u;
            u = ranges[0].upper;
        } else {
            l = u;
            u += increment;
        }
    }

    return bin;
}
