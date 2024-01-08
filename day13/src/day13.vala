int main(string[] args) {
  if (args.length == 1) {
    print(@"Usage: $(args[0]) <path to input>\n");
    return 1;
  }

  try {
    var file = File.new_for_path(args[1]);
    var dis = new DataInputStream(file.read());
    string line;
    while ((line = dis.read_line()) != null) {
      print("%s\n", line);
    }
  } catch (Error e) {
    error("%s", e.message);
  }

  return 0;
}
