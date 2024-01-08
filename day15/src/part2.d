import std.array, std.conv, std.file, std.stdio;

int hash(string s)
{
  int value = 0;
  foreach (dchar c; s) {
    value = ((value + to!int(c)) * 17) % 256;
  }
  return value;
}

void main(string[] args)
{
  string[] input = readText(args[1]).split(",");
  writefln("Got %(%s,%)", input);
}
