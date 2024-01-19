using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

if (args.Length == 0)
{
  Console.WriteLine("Usage: day21 <path to input>");
  return 1;
}

List<List<char>> maze = File.ReadAllText(args[0])
  .Trim()
  .Split('\n')
  .Select(l => l.ToList())
  .ToList();

Console.WriteLine($"{string.Join("\n", maze.Select(l => string.Join(", ", l)))}");

return 0;
