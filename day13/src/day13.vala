string[] transpose(string[] matrix) {
  string[] t = {};
  for (int j = 0; j < matrix[0].length; j++) {
    var col = new StringBuilder();
    foreach (string row in matrix) {
      col.append_unichar(row[j]);
    }
    t += col.str;
  }
  return t;
}

int hammingDistance(string a, string b) {
  if (a.length != b.length) {
    return -1;
  }
  int distance = 0;
  for (int i = 0; i < a.length; i++) {
    if (a[i] != b[i]) {
      distance++;
    }
  }
  return distance;
}

int symmetryCost(string[] matrix, int i) {
  int cost = 0;
  for (int k = 0; k < matrix.length; k++) {
    int j1 = i - k;
    int j2 = i + k + 1;
    if (j1 < 0 || j1 >= matrix.length || j2 < 0 || j2 >= matrix.length) {
      break;
    }
    cost += hammingDistance(matrix[j1], matrix[j2]);
  }
  return cost;
}

int symmetryAxis(string[] matrix, int expectedCost) {
  for (int i = 0; i < matrix.length - 1; i++) {
    if (symmetryCost(matrix, i) == expectedCost) {
      return i;
    }
  }
  return -1;
}

int symmetryScore(string[] matrix, int expectedCost) {
  int h = symmetryAxis(matrix, expectedCost);
  int v = symmetryAxis(transpose(matrix), expectedCost);
  return 100 * (h + 1) + (v + 1);
}

int main(string[] args) {
  if (args.length == 1) {
    print(@"Usage: $(args[0]) <path to input>\n");
    return 1;
  }

  int part1 = 0;
  int part2 = 0;

  void process(string[] matrix) {
    part1 += symmetryScore(matrix, 0);
    part2 += symmetryScore(matrix, 1);
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
