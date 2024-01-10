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

fn countEnergized(matrix: Matrix, start: Beam) !u32 {
    var visited = Set(Vec2).init(allocator);
    defer visited.deinit();
    try visited.put(start.pos, {});

    var last = Set(Beam).init(allocator);
    defer last.deinit();

    var current = Set(Beam).init(allocator);
    defer current.deinit();
    try current.put(start, {});

    // Found through trial 'n error. Not elegant, there are likely more clever
    // stop conditions (a foolproof one would be to store/check the history of
    // beam sets, not sure if that's faster though. Since the period could be
    // longer 1, in our setEquals(last, current) check likely isn't even
    // useful).
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

    const part1 = try countEnergized(matrix, .{ .pos = .{ .x = 0, .y = 0 }, .dir = .{ .x = 1, .y = 0 } });
    try stdout.print("Part 1: {d}\n", .{part1});

    var starts = std.ArrayList(Beam).init(allocator);
    defer starts.deinit();

    const width = matrix.items[0].len;
    const height = matrix.items.len;

    for (0..width) |x| {
        try starts.append(.{ .pos = .{ .x = @intCast(x), .y = 0 }, .dir = .{ .x = 0, .y = 1 } });
        try starts.append(.{ .pos = .{ .x = @intCast(x), .y = @intCast(height - 1) }, .dir = .{ .x = 0, .y = -1 } });
    }

    for (0..height) |y| {
        try starts.append(.{ .pos = .{ .x = 0, .y = @intCast(y) }, .dir = .{ .x = 1, .y = 0 } });
        try starts.append(.{ .pos = .{ .x = @intCast(width - 1), .y = @intCast(y) }, .dir = .{ .x = -1, .y = 0 } });
    }

    var part2: u32 = 0;
    for (starts.items) |start| {
        std.log.info("Starting at ({d}, {d})", .{ start.pos.x, start.pos.y });
        part2 = @max(part2, try countEnergized(matrix, start));
    }
    try stdout.print("Part 2: {d}\n", .{part2});

    return 0;
}
