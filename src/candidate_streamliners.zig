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
const Op = enum { lower_eq, higher };
pub const Delimiter = union(DelimiterType) {
    op: struct { op: Op, value: u32 },
    range: struct { lower: u8, higher: u8 },
};

pub const BinType = enum { hash, count };

pub const Bin = union(BinType) {
    hash: std.AutoArrayHashMap(Delimiter, Bin),
    count: u32,
};

pub fn newBin(allocator: std.mem.Allocator, d: model.Domain) Bin {
    const domain_bin: Bin = .{
        .hash = std.AutoArrayHashMap(Delimiter, Bin).init(allocator),
    };

    const domain_size: u32 = @divFloor(d.domain.upper - d.domain.lower, 10);
    for (0..10) |i| {
        print("{} ", .{i});
    }
    print("{}\n", .{domain_size});
    return domain_bin;
}
