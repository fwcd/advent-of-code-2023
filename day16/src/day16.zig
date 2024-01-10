const std = @import("std");

const allocator = std.heap.page_allocator;
const stdout = std.io.getStdOut().writer();

const String = []u8;
const Matrix = std.ArrayList(String);
const Vec2 = struct { x: i32, y: i32 };

fn Set(comptime T: type) type {
    return std.AutoHashMap(T, void);
}

fn setsEqual(comptime T: type, a: Set(T), b: Set(T)) bool {
    var aIter = a.keyIterator();
    while (aIter.next()) |x| {
        if (!b.contains(x.*)) {
            return false;
        }
    }
    var bIter = b.keyIterator();
    while (bIter.next()) |x| {
        if (!a.contains(x.*)) {
            return false;
        }
    }
    return true;
}

fn step(current: Set(Vec2), matrix: Matrix) Set(Vec2) {
    _ = matrix;
    _ = current;
    var next = Set(Vec2).init(allocator);

    // TODO

    return next;
}

fn countEnergized(matrix: Matrix) !u32 {
    var visited = Set(Vec2).init(allocator);
    defer visited.deinit();

    var current = Set(Vec2).init(allocator);
    try current.put(.{ .x = 0, .y = 0 }, {});

    var next = Set(Vec2).init(allocator);
    while (!setsEqual(Vec2, current, next)) {
        try stdout.print("Looping @ {d}\n", .{current.count()});
        current.deinit();
        current = next;
        next = step(current, matrix);
        var nextIter = next.keyIterator();
        while (nextIter.next()) |pos| {
            try visited.put(pos.*, {});
        }
    }
    next.deinit();
    current.deinit();

    return visited.count();
}

pub fn main() !u8 {
    var args = try std.process.argsAlloc(allocator);
    if (args.len <= 1) {
        try stdout.print("Usage: {s} <path to input>\n", .{args[0]});
        return 1;
    }

    var matrix = Matrix.init(allocator);
    defer matrix.deinit();

    var buffer: [1024]u8 = undefined;
    var file = try std.fs.cwd().openFile(args[1], .{});
    var bufReader = std.io.bufferedReader(file.reader());
    var reader = bufReader.reader();
    defer file.close();

    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |bufLine| {
        const line = try allocator.dupeZ(u8, bufLine);
        try matrix.append(line);
    }

    defer {
        for (matrix.items) |line| {
            allocator.free(line);
        }
    }

    for (matrix.items) |line| {
        try stdout.print("{s}\n", .{line});
    }

    try stdout.print("Part 1: {d}\n", .{try countEnergized(matrix)});

    return 0;
}
