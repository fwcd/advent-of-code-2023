const std = @import("std");

const allocator = std.heap.page_allocator;
const stdout = std.io.getStdOut().writer();

pub fn main() !u8 {
    var args = try std.process.argsAlloc(allocator);
    if (args.len <= 1) {
        try stdout.print("Usage: {s} <path to input>\n", .{args[0]});
        return 1;
    }

    var buf: [1024]u8 = undefined;
    var file = try std.fs.cwd().openFile(args[1], .{});
    var bufReader = std.io.bufferedReader(file.reader());
    var reader = bufReader.reader();
    defer file.close();

    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try stdout.print("{s}\n", .{line});
    }

    try stdout.print("Part 1: {d}\n", .{0});

    return 0;
}
