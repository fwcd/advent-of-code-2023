string[] transpose(string[] matrix) {
  string[] t = {};
  for (int i = 0; i < matrix[0].length; i++) {
    var col = new StringBuilder();
    foreach (string row in matrix) {
      col.append_unichar(row[i]);
    }
    t += col.str;
  }
  return t;
}

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
