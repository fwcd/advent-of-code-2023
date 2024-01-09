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

int symmetryScore(int h, int v) {
  return 100 * (h + 1) + (v + 1);
}

int part1SymmetryScore(string[] matrix) {
  int h = symmetryAxis(matrix);
  int v = symmetryAxis(transpose(matrix));
  return symmetryScore(h, v);
}

int part2SymmetryScore(string[] matrix) {
  int h = symmetryAxis(matrix);
  int v = symmetryAxis(transpose(matrix));
  for (int i = 0; i < matrix.length; i++) {
    string tmp = matrix[i];
    for (int j = 0; j < matrix[i].length; j++) {
      matrix[i] = @"$(tmp[:j])$(tmp[j] == '#' ? '.' : '#')$(tmp[j + 1:])";
      int h2 = symmetryAxis(matrix);
      int v2 = symmetryAxis(transpose(matrix));
      if (h2 != h && h2 >= 0) {
        print(@"h: $h -> $h2 -> score: $(symmetryScore(h2, -1))\n");
        foreach (string line in matrix) {
          print(@"$line\n");
        }
        return symmetryScore(h2, -1);
      }
      if (v2 != v && v2 >= 0) {
        print(@"v: $v -> $v2 -> score: $(symmetryScore(-1, v2))\n");
        foreach (string line in matrix) {
          print(@"$line\n");
        }
        return symmetryScore(-1, v2);
      }
    }
    matrix[i] = tmp;
  }
  return -1;
}

int main(string[] args) {
  if (args.length == 1) {
    print(@"Usage: $(args[0]) <path to input>\n");
    return 1;
  }

  int part1 = 0;
  int part2 = 0;

  void process(string[] matrix) {
    part1 += part1SymmetryScore(matrix);
    part2 += part2SymmetryScore(matrix);
    print(@"$part2\n");
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
  print(@"Part 2: $part2\n");

  return 0;
}
