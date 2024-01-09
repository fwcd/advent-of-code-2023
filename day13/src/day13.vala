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

bool isSymmetryAxis(string[] matrix, int i) {
  for (int k = 0; k < matrix.length; k++) {
    int j1 = i - k;
    int j2 = i + k + 1;
    if (j1 < 0 || j1 >= matrix.length || j2 < 0 || j2 >= matrix.length) {
      return true;
    }
    if (matrix[j1] != matrix[j2]) {
      print(@"$j1 -> $(matrix[j1]) != $(matrix[j2]) <- $j2 so not at $i\n");
      return false;
    }
  }
  return true;
}

int symmetryAxis(string[] matrix) {
  for (int i = 0; i < matrix.length - 1; i++) {
    if (matrix[i] == matrix[i + 1] && isSymmetryAxis(matrix, i)) {
      return i;
    }
  }
  return -1;
}

int symmetryScore(string[] matrix) {
  int hAxis = symmetryAxis(matrix);
  int vAxis = symmetryAxis(transpose(matrix));
  return 100 * (hAxis + 1) + (vAxis + 1);
}

int main(string[] args) {
  if (args.length == 1) {
    print(@"Usage: $(args[0]) <path to input>\n");
    return 1;
  }

  int part1 = 0;

  void process(string[] matrix) {
    part1 += symmetryScore(matrix);
  }

  try {
    var file = File.new_for_path(args[1]);
    var dis = new DataInputStream(file.read());
    string line;
    string[] matrix = {};
    while ((line = dis.read_line()) != null) {
      if (line.length > 0) {
        matrix += line;
      } else {
        process(matrix);
        matrix = {};
      }
    }
    if (matrix.length > 0) {
      process(matrix);
    }
  } catch (Error e) {
    error("%s", e.message);
  }

  print(@"Part 1: $part1\n");

  return 0;
}
