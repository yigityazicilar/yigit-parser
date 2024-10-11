const std = @import("std");
const print = std.debug.print;

pub const ModelData = struct {
    finds: []const []const u8,
    domain_types: std.StringArrayHashMap(DomainType),
    domains: []Domain,

    pub fn parseLeaky(
        allocator: std.mem.Allocator,
        model_json: []const u8,
        finds_json: []const u8,
    ) !ModelData {
        var ret = ModelData{
            .finds = undefined,
            .domain_types = std.StringArrayHashMap(DomainType).init(allocator),
            .domains = undefined,
        };

        const conjure_json = try std.json.parseFromSliceLeaky(
            std.json.Value,
            allocator,
            model_json,
            .{},
        );

        const finds_json_array: std.json.Value = conjure_json.object.get("finds") orelse {
            @panic("Could not get finds!");
        };
        var finds_arraylist = std.ArrayList([]const u8).init(allocator);

        for (finds_json_array.array.items) |find| {
            try finds_arraylist.append(find.object.get("Name").?.string);
        }
        ret.finds = try finds_arraylist.toOwnedSlice();

        const original_domains_array: std.json.Value = conjure_json.object.get(
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

                    try ret.domain_types.put(domain_name, domain);
                }
            }
        }

        const domains = try std.json.parseFromSliceLeaky(
            []Domain,
            allocator,
            finds_json,
            .{},
        );

        for (domains) |d| {
            std.testing.expect(ret.domain_types.contains(d.name)) catch {
                print("The find variable {s} cannot be found inside the model.\n", .{d.name});
                std.process.exit(1);
            };
        }

        ret.domains = domains;

        return ret;
    }

    fn parseDomainType(string: []const u8) DomainType {
        if (std.mem.eql(u8, string, "DomainMatrix")) {
            return .matrix;
        }
        if (std.mem.eql(u8, string, "DomainFunction")) {
            return .function;
        }
        if (std.mem.eql(u8, string, "DomainInt")) {
            return .integer;
        }
        std.debug.print("Failed to parse domain type: {s}\n", .{string});
        @panic("Failed to parse domain type!");
    }
};

pub const DomainType = enum { matrix, function, integer };

pub const Domain = struct {
    name: []const u8,
    domain: Range,
    dimensions: ?[]Range,
    varType: ?[]const u8,
};

pub const Range = struct { lower: ?isize, upper: ?isize, varType: []const u8 };

fn printDomain(d: Domain) void {
    print("{{\n", .{});
    print("\tname: {s}\n", .{d.name});
    print("\tdomain: {}..{}\n", .{
        d.domain.lower,
        d.domain.upper,
    });
    print("\tdimensions: [\n", .{});
    for (d.dimensions) |dim| {
        print("\t\t{}..{},\n", .{ dim.lower, dim.upper });
    }
    print("\t]\n}}\n", .{});
}
