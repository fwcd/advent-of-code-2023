const std = @import("std");

const allocator = std.heap.page_allocator;
const stdout = std.io.getStdOut().writer();

const String = []u8;
const Matrix = std.ArrayList(String);
const Vec2 = struct { x: i32, y: i32 };
const Beam = struct { pos: Vec2, dir: Vec2 };

fn Set(comptime T: type) type {
    return std.AutoHashMap(T, void);
}

fn setEquals(comptime T: type, a: Set(T), b: Set(T)) bool {
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

fn vec2Add(a: Vec2, b: Vec2) Vec2 {
    return .{ .x = a.x + b.x, .y = a.y + b.y };
}

fn mirrorForward(v: Vec2) Vec2 {
    return .{ .x = -v.y, .y = -v.x };
}

fn mirrorBack(v: Vec2) Vec2 {
    return .{ .x = v.y, .y = v.x };
}

fn advance(beam: Beam, dir: Vec2) Beam {
    return .{ .pos = vec2Add(beam.pos, dir), .dir = dir };
}

fn advanceInBounds(beam: Beam, dir: Vec2, matrix: Matrix) ?Beam {
    const nextBeam = advance(beam, dir);
    const pos = nextBeam.pos;
    if (pos.y >= 0 and pos.y < matrix.items.len and pos.x >= 0 and pos.x < matrix.items[0].len) {
        return nextBeam;
    } else {
        return null;
    }
}

fn matrixAt(pos: Vec2, matrix: Matrix) *u8 {
    return &matrix.items[@intCast(pos.y)][@intCast(pos.x)];
}

fn step(current: Set(Beam), matrix: Matrix) !Set(Beam) {
    var next = Set(Beam).init(allocator);

    var currentIter = current.keyIterator();
    while (currentIter.next()) |beam| {
        const field: u8 = matrixAt(beam.pos, matrix).*;
        if (field == '-' and beam.dir.x == 0) {
            inline for ([_]i32{ 1, -1 }) |dx| {
                if (advanceInBounds(beam.*, .{ .x = dx, .y = 0 }, matrix)) |nextBeam| {
                    try next.put(nextBeam, {});
                }
            }
        } else if (field == '|' and beam.dir.y == 0) {
            inline for ([_]i32{ 1, -1 }) |dy| {
                if (advanceInBounds(beam.*, .{ .x = 0, .y = dy }, matrix)) |nextBeam| {
                    try next.put(nextBeam, {});
                }
            }
        } else {
            const dir = switch (field) {
                '/' => mirrorForward(beam.dir),
                '\\' => mirrorBack(beam.dir),
                else => beam.dir,
            };
            if (advanceInBounds(beam.*, dir, matrix)) |nextBeam| {
                try next.put(nextBeam, {});
            }
        }
    }

    return next;
}

fn countEnergized(matrix: Matrix) !u32 {
    const startBeam = .{ .pos = .{ .x = 0, .y = 0 }, .dir = .{ .x = 1, .y = 0 } };

    var visited = Set(Vec2).init(allocator);
    defer visited.deinit();
    try visited.put(startBeam.pos, {});

    var last = Set(Beam).init(allocator);
    defer last.deinit();

    var current = Set(Beam).init(allocator);
    defer current.deinit();
    try current.put(startBeam, {});

    var maxIter: i32 = 2000;
    while (!setEquals(Beam, last, current) and maxIter >= 0) {
        last.deinit();
        last = current;
        current = try step(current, matrix);
        // std.log.info("Total: {d}", .{current.count()});
        var nextIter = current.keyIterator();
        while (nextIter.next()) |beam| {
            try visited.put(beam.pos, {});
            // std.log.info("  ({d}, {d}) > ({d}, {d})", .{ beam.pos.x, beam.pos.y, beam.dir.x, beam.dir.y });
        }
        maxIter -= 1;
    }

    // DEBUG
    var visitedIter = visited.keyIterator();
    while (visitedIter.next()) |pos| {
        matrixAt(pos.*, matrix).* = '#';
    }
    for (matrix.items) |row| {
        std.log.info("{s}", .{row});
    }

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
