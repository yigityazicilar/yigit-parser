const std = @import("std");
const print = std.debug.print;

const ArgumentsStruct = struct {
    encodings_file: ?[]const u8 = null,
    conjure_json_file: ?[]const u8 = null,
    learnts_file: ?[]const u8 = null,
    finds_file: ?[]const u8 = null,
    output_file: ?[]const u8 = null,
    bin_file: ?[]const u8 = null,
};

const usageMessage: []const u8 = "Usage: process-nogoods -e <encodings-file> -c <conjure-json-file> -l <learnts-file> -f <finds-file> [-o <output-file>] [-b <bin-file>]\n";
inline fn streql(s1: []const u8, s2: []const u8) bool {
    return std.mem.eql(u8, s1, s2);
}

pub fn parseArguments(
    allocator: std.mem.Allocator,
    args: *std.process.ArgIterator,
) !ArgumentsStruct {
    var ret: ArgumentsStruct = .{};

    while (args.next()) |arg| {
        if (streql(arg, "--encodings") or streql(arg, "-e")) {
            ret.encodings_file = args.next() orelse {
                print("{s}process-nogoods: Error: argument -e/--encodings requires a value\n", .{usageMessage});
                std.os.exit(1);
            };
        } else if (streql(arg, "--conjure-json") or streql(arg, "-c")) {
            ret.conjure_json_file = args.next() orelse {
                print("{s}process-nogoods: Error: argument -c/--conjure-json requires a value\n", .{usageMessage});
                std.os.exit(1);
            };
        } else if (streql(arg, "--learnts") or streql(arg, "-l")) {
            ret.learnts_file = args.next() orelse {
                print("{s}process-nogoods: Error: argument -l/--learnts requires a value\n", .{usageMessage});
                std.os.exit(1);
            };
        } else if (streql(arg, "--finds") or streql(arg, "-f")) {
            ret.finds_file = args.next() orelse {
                print("{s}process-nogoods: Error: argument -f/--finds requires a value\n", .{usageMessage});
                std.os.exit(1);
            };
        } else if (streql(arg, "--output") or streql(arg, "-o")) {
            ret.output_file = args.next() orelse {
                print("{s}process-nogoods: Error: argument -o/--output requires a value\n", .{usageMessage});
                std.os.exit(1);
            };
        } else if (streql(arg, "--bin-output") or streql(arg, "-b")) {
            ret.bin_file = args.next() orelse {
                print("{s}process-nogoods: Error: argument -b/--bin-output requires a value\n", .{usageMessage});
                std.os.exit(1);
            };
        } else {
            print("{s}process-nogoods: Error: unknown argument: {s}\n", .{ usageMessage, arg });
            std.os.exit(1);
        }
    }

    var missing_args = std.ArrayList([]const u8).init(allocator);
    defer missing_args.deinit();
    if (ret.encodings_file == null) {
        try missing_args.append("-e/--encodings");
    }
    if (ret.conjure_json_file == null) {
        try missing_args.append("-c/--conjure-json");
    }
    if (ret.learnts_file == null) {
        try missing_args.append("-l/--learnts");
    }
    if (ret.finds_file == null) {
        try missing_args.append("-f/--finds");
    }
    if (ret.output_file == null) {
        ret.output_file = "output.eprime";
    }
    if (ret.bin_file == null) {
        ret.bin_file = "output.bin";
    }

    if (missing_args.items.len > 0) {
        var errMessage: []const u8 = "process-nogoods: Error: the following arguments are required: ";
        var missing_args_str = try std.mem.join(
            allocator,
            ", ",
            missing_args.items,
        );
        defer allocator.free(missing_args_str);

        print("{s}{s}{s}\n", .{ usageMessage, errMessage, missing_args_str });
        std.os.exit(1);
    }

    return ret;
}
