import std.array, std.file, std.stdio;

void main(string[] args)
{
  string[] input = readText(args[1]).split(",");
  writefln("Got %(%s,%)", input);
}
