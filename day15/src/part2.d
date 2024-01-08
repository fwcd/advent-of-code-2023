import std.stdio, std.file;

void main(string[] args)
{
  string input = readText(args[1]);
  writefln("Got %s", input);
}
