const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const arguments = @import("args.zig");
const convert = @import("convert.zig");
const model = @import("model_data.zig");
const candidate_streamliners = @import("candidate_streamliners.zig");
const print = std.debug.print;

const TokenType = lexer.TokenType;
const Token = lexer.Token;

const ParseDiagnostics = parser.ParseDiagnostics;
const ParseOptions = parser.ParseOptions;
const ParseError = parser.ParseError;
const ModelData = model.ModelData;

pub const log_level: std.log.Level = .debug;

const ParseAndConvertResult = struct {
    parse_result: *parser.ASTNode_Program,
    convert_result: []const u8,
};

pub fn parseAndConvert(
    allocator: std.mem.Allocator,
    model_data: ModelData,
    input: []const u8,
) !ParseAndConvertResult {
    var tokens = lexer.lex(allocator, input);
    var diags = ParseDiagnostics{};
    var parseOptions = ParseOptions{
        .allocator = allocator,
        .model_data = model_data,
        .diags = &diags,
    };

    const parse_result = parser.parse(parseOptions, tokens) catch |err| {
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

    return .{
        .parse_result = parse_result,
        .convert_result = try convert.convertProgram(allocator, parse_result),
    };
}

fn readFile(
    allocator: std.mem.Allocator,
    filename: []const u8,
) ![]const u8 {
    return std.fs.cwd().readFileAlloc(allocator, filename, 64 * 1000 * 1000 * 1000) catch {
        print("Unable to read the file from the following path: {s}\n", .{filename});
        std.os.exit(1);
    };
}

pub fn main() !void {
    // Global Arena
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    // Argument Parsing
    var _args = std.process.args();
    _ = _args.next().?; // Skip the program name

    const args = try arguments.parseArguments(allocator, &_args);

    const json_input = try readFile(allocator, args.encodings_file.?);
    var eprime_file = try readFile(allocator, args.conjure_json_file.?);
    const learnts_input = try readFile(allocator, args.learnts_file.?);
    const finds_input = try readFile(allocator, args.finds_file.?);
    defer allocator.free(json_input);
    defer allocator.free(eprime_file);
    defer allocator.free(learnts_input);
    defer allocator.free(finds_input);

    const conjure_json_input = in: {
        var split = std.mem.splitSequence(u8, eprime_file, "$ Conjure's");
        _ = split.next(); // Skipping the Essence Prime
        const conjure_json_dirty = split.next() orelse {
            @panic("Failed to get conjure JSON!");
        };
        var conjure_json_input_list = try std.ArrayList(u8).initCapacity(allocator, conjure_json_dirty.len);
        var dirty_lines = std.mem.splitScalar(u8, conjure_json_dirty, '\n');

        while (dirty_lines.next()) |dirty_line| {
            //~ ojf: Remove preceding '$ '
            if (dirty_line.len > 2) {
                try conjure_json_input_list.appendSlice(dirty_line[2..]);
            }
        }
        break :in try conjure_json_input_list.toOwnedSlice();
    };

    const input = try std.json.parseFromSliceLeaky(
        std.json.Value,
        allocator,
        json_input,
        .{},
    );

    const model_data = try ModelData.parseLeaky(allocator, conjure_json_input, finds_input);
    _ = candidate_streamliners.createBin(allocator, model_data.domains[0]);

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
                model_data,
                string,
            );
            defer allocator.free(converted.convert_result);

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
                .{ converted.convert_result, pos_op, pos_val },
            );
            const negative = try std.fmt.allocPrint(
                allocator,
                "{s}{s}{s}",
                .{ converted.convert_result, neg_op, neg_val },
            );

            const int_key = try std.fmt.parseInt(i64, key, 10);

            try converted_map.put(int_key, positive);
            try converted_map.put(-1 * int_key, negative);
        }
    }

    // Replace satvar
    //~ oyy: create the whole output path.
    if (std.fs.path.dirname(args.output_file.?)) |dirname| {
        std.fs.cwd().makePath(dirname) catch {
            print("Could not create the output directories.", .{});
        };
    }
    var output_file = try std.fs.cwd().createFile(args.output_file.?, .{});
    defer output_file.close();

    var file_writer = output_file.writer();
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
            try file_writer.print("{s},\n", .{joined});
        }
    }
}

test "Test the program" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const test_string = "shift((car_Function1D_00018 in int (4, 3, 5..10)), 9)";

    const tokens = lexer.lex(allocator, test_string);
    var diags = ParseDiagnostics{};

    var eprime_file = try readFile(allocator, "model.eprime");
    const finds_input = try readFile(allocator, "finds.json");
    defer allocator.free(eprime_file);
    defer allocator.free(finds_input);

    const conjure_json_input = in: {
        var split = std.mem.splitSequence(u8, eprime_file, "$ Conjure's");
        _ = split.next(); // Skipping the Essence Prime
        const conjure_json_dirty = split.next() orelse {
            @panic("Failed to get conjure JSON!");
        };
        var conjure_json_input_list = try std.ArrayList(u8).initCapacity(allocator, conjure_json_dirty.len);
        var dirty_lines = std.mem.splitScalar(u8, conjure_json_dirty, '\n');

        while (dirty_lines.next()) |dirty_line| {
            //~ ojf: Remove preceding '$ '
            if (dirty_line.len > 2) {
                try conjure_json_input_list.appendSlice(dirty_line[2..]);
            }
        }
        break :in try conjure_json_input_list.toOwnedSlice();
    };

    const model_data = try ModelData.parseLeaky(allocator, conjure_json_input, finds_input);

    const parseOptions = ParseOptions{
        .allocator = allocator,
        .model_data = model_data,
        .diags = &diags,
    };
    const parse_result = parser.parse(parseOptions, tokens) catch |err| {
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

    parser.printParseTree(parse_result);

    const converted = try convert.convertProgram(
        std.testing.allocator,
        parse_result,
    );
    defer std.testing.allocator.free(converted);

    print("\nCONVERTED: {s}\n", .{converted});
}
