import std.array, std.algorithm, std.conv, std.file, std.range, std.stdio, std.string, std.typecons;

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
  Tuple!(string, "key", int, "focalLength")[][256] boxes;
  int[string] indices;

  foreach (string lens; input) {
    if (lens[$-1] == '-') {
      string key = lens[0..$-1];
      if (key in indices) {
        int i = indices[key];
        indices.remove(key);
        boxes[i] = boxes[i].filter!(e => e.key != key).array;
      }
    } else {
      string[] components = lens.split("=");
      string key = components[0];
      int focalLength = components[1].to!int;

      int i = hash(key);
      int j = boxes[i].countUntil!(e => e.key == key).to!int;
      if (j < boxes[i].length) {
        // Lens is already in box, update focal length
        boxes[i][j].focalLength = focalLength;
      } else {
        // Insert lens
        boxes[i] ~= tuple!("key", "focalLength")(key, focalLength);
        indices[key] = i;
      }
    }
  }

  int focusPower = boxes[]
    .enumerate
    .map!(ibox => ibox[1].enumerate.map!(ie => (ibox[0] + 1) * (ie[0] + 1) * ie[1].focalLength).sum)
    .sum
    .to!int;

  writefln("%d", focusPower);
}
