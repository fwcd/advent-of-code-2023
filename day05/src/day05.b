/*
 * An implementation of day 5 in the B programming language using Smith's
 * B compiler (https://cpjsmith.uk/b).
 * 
 * Initial idea: Store the list of intervals in the target domain (location) and
 * cut them down until we've reached the start domain (seeds). For now we
 * should be good by just applying the mappings though, perhaps part 2 will
 * require something more fancy?
 */

#define LINE_BUFSIZE 128
#define SEEDS_BUFSIZE 64
#define MAP_COUNT 7
#define SEEDS_SKIP_CHARS 7
#define MAP_SKIP_LINES 2

read_line(out_str, out_size, out_eof) {
  extrn getchar;
  auto i, c;

  i = 0;

  while (1) {
    c = getchar();
    if (c == '*n' | c == '*e' | i >= (out_size - 1)) {
      *out_eof = c != '*n';
      /* NOTE: B uses '*e' (EOT, U+0004) as string terminators instead of the "modern" NUL. */
      out_str[i] = '*e';
      return (i);
    }
    out_str[i] = c;
    i++;
  }
}

print_array(buf, length) {
  extrn printf;
  auto i;

  i = 0;

  printf("[");
  while (i < length) {
    printf("%d", buf[i]);
    if (i < length - 1) {
      printf(", ");
    }
    i++;
  }
  printf("]");
}

parse_integer(str, length, out_integer) {
  auto i, c, result;

  i = 0;
  result = 0;

  while (1) {
    c = str[i];
    if (c < '0' | c > '9') {
      *out_integer = result;
      return (i);
    }
    result = result * 10 + (c - '0');
    i++;
  }
}

parse_integers(str, length, out_count, out_buf, out_size) {
  extrn parse_integer, printf;
  auto i, j, di;
  
  i = 0;
  j = 0;
  di = 0;

  while (i < length && j < out_size) {
    di = parse_integer(str + i, length - i, out_buf + j) + 1 /* space */;
    if (di == 0) {
      goto end;
    }
    i =+ di;
    j++;
  }

  end:
  *out_count = j;

  return (i);
}

main() {
  extrn printf, read_line, print_array, parse_integers, line, seeds, seed_count;
  auto length, eof;

  length = read_line(line, LINE_BUFSIZE, &eof);
  parse_integers(line + SEEDS_SKIP_CHARS, length - SEEDS_SKIP_CHARS, &seed_count, seeds, SEEDS_BUFSIZE);

  printf("Got: ");
  print_array(seeds, seed_count);
  printf("*n");
}

line[LINE_BUFSIZE];

seeds[SEEDS_BUFSIZE];
seed_count;

maps[MAP_COUNT];
map_lengths[MAP_COUNT];
