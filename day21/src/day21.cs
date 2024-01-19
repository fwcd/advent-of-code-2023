using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

bool IsPopulated(char cell) => cell == 'S' || cell == 'O';

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
      .Select((cell, j) => cell == '.' && Neighbors(maze, i, j).Any(IsPopulated)
        ? 'O'
        : cell == '#' ? '#' : '.')
      .ToList())
    .ToList();

List<List<char>> StepN(List<List<char>> maze, int n) =>
  n <= 0 ? maze : StepN(Step(maze), n - 1);

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

int part1 = StepN(maze, 64)
  .Select(row => row.Count(cell => cell == 'O'))
  .Sum();

Console.WriteLine($"Part 1: {part1}");

return 0;
