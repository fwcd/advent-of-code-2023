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

read_line(out_str, out_size) {
  extrn getchar;
  auto i, c;

  i = 0;

  while (1) {
    c = getchar();
    if (c == '*n' | c == '*e' | i >= (out_size - 1)) {
      goto end;
    }
    out_str[i] = c;
    i++;
  }

  /* NOTE: B uses '*e' (EOT, U+0004) as string terminators instead of the "modern" NUL. */
  end:
  out_str[i] = '*e';

  return (i);
}

parse_integer(str, length) {
  auto i, n;

  i = 0;

  return (n);
}

parse_seeds(str, length, out_buf, out_size) {
  auto i;

  /* Skip the initial 'seeds: ' */
  i = 7;
}

main() {
  extrn printf, read_line, line;
  auto i, length;

  length = read_line(line, LINE_BUFSIZE);
  printf("Got line of length %d: %s*n", length, line);
}

line[LINE_BUFSIZE];

seeds[SEEDS_BUFSIZE];
seed_count;

maps[MAP_COUNT];
map_lengths[MAP_COUNT];
