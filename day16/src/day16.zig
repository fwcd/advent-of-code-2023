const std = @import("std");

const allocator = std.heap.page_allocator;

const Map = std.AutoHashMap;
const List = std.ArrayList;
const String = []u8;
const Matrix = List(String);
const Vec2 = struct { x: i32, y: i32 };
const Beam = struct { pos: Vec2, dir: Vec2 };

fn Set(comptime T: type) type {
    return Map(T, void);
}

fn mapContainsAll(comptime K: type, comptime V: type, a: Map(K, V), b: Map(K, V)) bool {
    var aIter = a.iterator();
    while (aIter.next()) |e| {
        if (!b.contains(e.key_ptr.*) or b.get(e.key_ptr.*) != e.value_ptr.*) {
            return false;
        }
    }
    return true;
}

fn mapEquals(comptime K: type, comptime V: type, a: Map(K, V), b: Map(K, V)) bool {
    return mapContainsAll(K, V, a, b) and mapContainsAll(K, V, b, a);
}

fn mapsContain(comptime K: type, comptime V: type, sets: List(Map(K, V)), expected: Map(K, V)) bool {
    for (sets.items) |set| {
        if (mapEquals(K, V, set, expected)) {
            return true;
        }
    }
    return false;
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

    var history = List(Set(Beam)).init(allocator);
    defer {
        var i: usize = 0;
        while (i < history.items.len) : (i += 1) {
            history.items[i].deinit();
        }
        history.deinit();
    }

    var current = Set(Beam).init(allocator);
    defer current.deinit();
    try current.put(start, {});

    while (!mapsContain(Beam, void, history, current)) {
        try history.append(current);
        current = try step(current, matrix);
        var currentIter = current.keyIterator();
        while (currentIter.next()) |beam| {
            try visited.put(beam.pos, {});
        }
    }

    return visited.count();
}

pub fn main() !u8 {
    var args = try std.process.argsAlloc(allocator);
    if (args.len <= 1) {
        try std.io.getStdErr().writer().print("Usage: {s} <path to input>\n", .{args[0]});
        return 1;
    }

    var matrix = Matrix.init(allocator);
    defer {
        for (matrix.items) |line| {
            allocator.free(line);
        }
        matrix.deinit();
    }

    var buffer: [1024]u8 = undefined;
    var file = try std.fs.cwd().openFile(args[1], .{});
    var bufReader = std.io.bufferedReader(file.reader());
    var reader = bufReader.reader();
    defer file.close();

    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |bufLine| {
        const line = try allocator.dupeZ(u8, bufLine);
        try matrix.append(line);
    }

    for (matrix.items) |line| {
        std.log.debug("{s}", .{line});
    }

    const part1 = try countEnergized(matrix, .{ .pos = .{ .x = 0, .y = 0 }, .dir = .{ .x = 1, .y = 0 } });
    std.log.info("Part 1: {d}", .{part1});

    var starts = List(Beam).init(allocator);
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
    std.log.info("Part 2: {d}", .{part2});

    return 0;
}
