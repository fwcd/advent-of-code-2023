import std.array, std.algorithm, std.conv, std.file, std.stdio, std.string;

int hash(string s)
{
  int value = 0;
  foreach (dchar c; s) {
    value = ((value + c.to!int) * 17) % 256;
  }
  return value;
}

void main(string[] args)
{
  string[] input = readText(args[1]).strip.split(",");
  writefln("Got %(%s,%)", input);
  writefln("Got %(%d,%)", input.map!(hash));
}
