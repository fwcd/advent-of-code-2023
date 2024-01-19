using System;
using System.IO;

if (args.Length == 0)
{
  Console.WriteLine("Usage: day21 <path to input>");
  return 1;
}

string[] maze = File.ReadAllText(args[0]).Trim().Split('\n');
Console.WriteLine($"{string.Join(", ", maze)}");

return 0;
