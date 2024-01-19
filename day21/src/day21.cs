using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

int Mod(int n, int m) => (n % m + m) % m;

bool IsWall(long cell) => cell < 0;

long Get(List<List<long>> maze, int i, int j) =>
  maze[Mod(i, maze.Count)][Mod(j, maze[0].Count)];

IEnumerable<long> Neighbors(List<List<long>> maze, int i, int j) =>
  Enumerable.Range(-1, 3)
    .SelectMany(d => d == 0 ? new List<long>() : new List<long> {
      Get(maze, i + d, j),
      Get(maze, i, j + d),
    });

List<List<long>> Step(List<List<long>> maze) =>
  maze
    .Select((row, i) => row
      .Select((cell, j) => IsWall(cell)
        ? cell
        : Math.Sign(Neighbors(maze, i, j).Where(n => !IsWall(n)).Sum()))
      .ToList())
    .ToList();

List<List<long>> StepN(List<List<long>> maze, int n) =>
  n <= 0 ? maze : StepN(Step(maze), n - 1);

if (args.Length == 0)
{
  Console.WriteLine("Usage: day21 <path to input>");
  return 1;
}

List<List<long>> maze = File.ReadAllText(args[0])
  .Trim()
  .Split('\n')
  .Select(row => row
    .Select(cell => cell == 'S' ? 1L : cell == '#' ? -1L : 0L)
    .ToList())
  .ToList();

long part1 = StepN(maze, 64)
  .Select(row => row.Where(cell => !IsWall(cell)).Sum())
  .Sum();

Console.WriteLine($"Part 1: {part1}");

// Console.WriteLine($"{string.Join("\n", StepN(maze, 6).Select(row => string.Join(", ", row.Select(c => IsWall(c) ? " " : c.ToString()))))}");

return 0;
