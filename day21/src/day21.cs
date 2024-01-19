using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

bool IsPopulated(char cell) => cell == 'S' || cell == 'O';

bool IsFree(char cell) => cell == '.';

bool InBounds(List<List<char>> maze, int i, int j) =>
  i >= 0 && i < maze.Count && j >= 0 && j < maze[0].Count;

char Get(List<List<char>> maze, int i, int j) =>
  InBounds(maze, i, j) ? maze[i][j] : ' ';

IEnumerable<char> Neighbors(List<List<char>> maze, int i, int j) =>
  Enumerable.Range(-1, 3)
    .SelectMany(d => d == 0 ? new List<char>() : new List<char> {
      Get(maze, i + d, j),
      Get(maze, i, j + d),
    });

List<List<char>> Step(List<List<char>> maze) =>
  maze
    .Select((row, i) => row
      .Select((cell, j) => IsFree(cell) && Neighbors(maze, i, j).Any(IsPopulated)
        ? 'O'
        : cell == 'S' ? '.' : cell)
      .ToList())
    .ToList();

if (args.Length == 0)
{
  Console.WriteLine("Usage: day21 <path to input>");
  return 1;
}

List<List<char>> maze = File.ReadAllText(args[0])
  .Trim()
  .Split('\n')
  .Select(row => row.ToList())
  .ToList();

foreach (var neighbor in Neighbors(maze, 1, 1)) {
  Console.WriteLine($"Neighbor: {neighbor}");
}

Console.WriteLine($"{string.Join("\n", Step(maze).Select(l => string.Join(", ", l)))}");

return 0;
