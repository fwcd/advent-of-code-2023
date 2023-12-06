/*
 * An implementation of day 5 in the B programming language using Smith's
 * B compiler (https://cpjsmith.uk/b).
 * 
 * Initial idea: Store the list of intervals in the target domain (location) and
 * cut them down until we've reached the start domain (seeds). For now we
 * should be good by just applying the mappings though, perhaps part 2 will
 * require something more fancy?
 */

/* Buffer sizes. */
#define LINE_SIZE 128
#define SEEDS_SIZE 64
#define MAPS_SIZE 256
#define MAP_LENGTHS_SIZE 16

/* Parse states. */
#define PARSE_STATE_SEEDS 1
#define PARSE_STATE_DELIMITER 2
#define PARSE_STATE_MAP_HEADER 3
#define PARSE_STATE_MAP 4

/* Magic parse constants. */
#define SEEDS_SKIP_CHARS 7

read_line(str, size, out_eof) {
  extrn getchar;
  auto i, c;

  i = 0;

  while (1) {
    c = getchar();
    if (c == '*n' | c == '*e' | i >= (size - 1)) {
      *out_eof = c != '*n';
      /* NOTE: B uses '*e' (EOT, U+0004) as string terminators instead of the "modern" NUL. */
      str[i] = '*e';
      return (i);
    }
    str[i++] = c;
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

parse_integers(str, length, out_count, buf, buf_size) {
  extrn parse_integer, printf;
  auto i, j, di;
  
  i = 0;
  j = 0;
  di = 0;

  while (i < length && j < buf_size) {
    di = parse_integer(str + i, length - i, buf + j) + 1 /* space */;
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

line[LINE_SIZE];

read_input(seeds, seeds_size, out_seed_count, maps, maps_size, map_lengths, map_lengths_size, out_map_count) {
  extrn printf, exit, line, read_line, parse_integers;
  auto length, eof, state, map_index, map_length;

  state = PARSE_STATE_SEEDS;
  map_index = 0;
  map_length = 0;

  while (1) {
    length = read_line(line, LINE_SIZE, &eof);

    switch (state) {
    case PARSE_STATE_SEEDS:
      parse_integers(line + SEEDS_SKIP_CHARS, length - SEEDS_SKIP_CHARS, out_seed_count, seeds, seeds_size);
      state = PARSE_STATE_DELIMITER;
      goto switch_end;
    case PARSE_STATE_DELIMITER:
      state = PARSE_STATE_MAP_HEADER;
      goto switch_end;
    case PARSE_STATE_MAP_HEADER:
      state = PARSE_STATE_MAP;
      goto switch_end;
    case PARSE_STATE_MAP:
      if (length > 0) {
        parse_integers(line, length, map_lengths + map_index, maps, maps_size);
        map_length++;
      } else {
        state = PARSE_STATE_MAP_HEADER;
      }
      if (length == 0 | eof) {
        if (map_index >= map_lengths_size) {
          printf("Map index out of bounds!*n");
          exit(1);
        }
        map_lengths[map_index++] = map_length;
      }
      goto switch_end;
    }

    switch_end:
    if (eof) {
      goto while_end;
    }
  }

  while_end:
  *out_map_count = map_index;
}

seeds[SEEDS_SIZE];
seed_count;

maps[MAPS_SIZE];
map_lengths[MAP_LENGTHS_SIZE];
map_count;

main() {
  extrn printf, print_array, read_input, line, seeds, seed_count, maps, map_lengths, map_count;

  read_input(seeds, SEEDS_SIZE, &seed_count, maps, MAPS_SIZE, map_lengths, MAP_LENGTHS_SIZE, &map_count);

  printf("Parsed %d maps*n", map_count);
  printf("Seeds: ");
  print_array(seeds, seed_count);
  printf("*n");
}
