/*
 * An implementation of day 5 in the B programming language using Smith's
 * B compiler (https://cpjsmith.uk/b).
 * 
 * Initial idea: Store the list of intervals in the target domain (location) and
 * cut them down until we've reached the start domain (seeds). For now we
 * should be good by just applying the mappings though, perhaps part 2 will
 * require something more fancy?
 */

#define DEBUG_LOGGING

/* Buffer sizes. */
#define LINE_SIZE 256
#define SEEDS_SIZE 64
#define MAP_DATA_SIZE 1024
#define MAP_LENGTHS_SIZE 16

/* Parse states. */
#define PARSE_STATE_SEEDS 1
#define PARSE_STATE_DELIMITER 2
#define PARSE_STATE_MAP_HEADER 3
#define PARSE_STATE_MAP 4

/* Magic parse constants. */
#define SEEDS_SKIP_CHARS 7

/* Domain-specific stuff. */
#define MAP_ENTRY_LENGTH 3

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
  extrn printf;
  auto i, c, result;

  i = 0;
  result = 0;

  while (i < length) {
    c = str[i];
    if (c < '0' | c > '9') {
      break;
    }
    result = result * 10 + (c - '0');
    i++;
  }

  *out_integer = result;

  return (i);
}

parse_spaces(str, length) {
  auto i, c;

  i = 0;

  while (i < length) {
    c = str[i];
    if (c != ' ') {
      break;
    }
    i++;
  }

  return (i);
}

parse_integers(str, length, out_count, buf, buf_size) {
  extrn parse_integer, parse_spaces, printf;
  auto i, j, di;
  
  i = 0;
  j = 0;
  di = 0;

  while (i < length & j < buf_size) {
    di = parse_integer(str + i, length - i, buf + j);
    if (di == 0) {
      break;
    }
    i =+ di;
    i =+ parse_spaces(str + i, length - i);
    j++;
  }

  *out_count = j;

  return (i);
}

line[LINE_SIZE];

read_input(seeds, seeds_size, out_seed_count, map_data, map_data_size, map_lengths, map_lengths_size, out_map_count) {
  extrn printf, exit, line, read_line, parse_integers;
  auto state, next_state, line_number, length, eof, map_index, map_length, map_data_offset, entry_length;

  state = PARSE_STATE_SEEDS;
  next_state = state;
  line_number = 1;

  map_index = 0;
  map_length = 0;
  map_data_offset = 0;

  while (1) {
    length = read_line(line, LINE_SIZE, &eof);

    switch (state) {
    case PARSE_STATE_SEEDS:
      parse_integers(line + SEEDS_SKIP_CHARS, length - SEEDS_SKIP_CHARS, out_seed_count, seeds, seeds_size);
      next_state = PARSE_STATE_DELIMITER;
      break;
    case PARSE_STATE_DELIMITER:
      next_state = PARSE_STATE_MAP_HEADER;
      break;
    case PARSE_STATE_MAP_HEADER:
      next_state = PARSE_STATE_MAP;
      break;
    case PARSE_STATE_MAP:
      if (length > 0) {
        parse_integers(line, length, &entry_length, map_data + map_data_offset, map_data_size - map_data_offset);
        if (entry_length != MAP_ENTRY_LENGTH) {
          printf("Line %d: Map %d entry %d has length %d (expected %d)!*n", line_number, map_index, map_length, entry_length, MAP_ENTRY_LENGTH);
          exit(1);
        }
        map_length++;
        map_data_offset =+ entry_length;
      }
      if ((length == 0) | eof) {
        if (map_index >= map_lengths_size) {
          printf("Line %d: Map index out of bounds, maximum size is %d!*n", line_number, map_lengths_size);
          exit(1);
        }
        map_lengths[map_index++] = map_length;
        map_length = 0;
        next_state = PARSE_STATE_MAP_HEADER;
      }
      break;
    }

    if (eof) {
      break;
    }

    #ifdef DEBUG_LOGGING
    printf("Line %d: State %d -> %d, map index %d, entry index %d*n", line_number, state, next_state, map_index, map_length);
    #endif

    line_number++;
    state = next_state;
  }

  *out_map_count = map_index;
}

apply_map(value, map_data, map_length) {
  auto entry_index, dest_range_start, source_range_start, range_length, source_range_end;

  entry_index = 0;

  while (entry_index < map_length) {
    dest_range_start   = map_data[entry_index * MAP_ENTRY_LENGTH];
    source_range_start = map_data[entry_index * MAP_ENTRY_LENGTH + 1];
    range_length       = map_data[entry_index * MAP_ENTRY_LENGTH + 2];

    source_range_end = source_range_start + range_length;
    if ((value >= source_range_start) & (value < source_range_end)) {
      return (dest_range_start + value - source_range_start);
    }

    entry_index++;
  }

  return (value);
}

seed_to_location(seed, map_data, map_lengths, map_count) {
  extrn printf, apply_map;
  auto value, map_index, map_length, map_data_offset;

  value = seed;
  map_index = 0;
  map_data_offset = 0;

  while (map_index < map_count) {
    map_length = map_lengths[map_index];
    value = apply_map(value, map_data + map_data_offset, map_length);
    map_data_offset =+ map_length * MAP_ENTRY_LENGTH;
    map_index++;
  }

  return (value);
}

seeds[SEEDS_SIZE];
seed_count;

/*
 * Map data is stored as a flat buffer using to the following format:
 * 
 * | 50   | 98   | 2  | 52   | 50  | 48  | 0   | 15   | 37  | ... |
 * | MAP_ENTRY_LENGTH | MAP_ENTRY_LENGTH | MAP_ENTRY_LENGTH | ... |
 * | map_length[0] = 2                   | map_length[1] = 3  ... |
 */
map_data[MAP_DATA_SIZE];
map_lengths[MAP_LENGTHS_SIZE];
map_count;

compute_part1() {
  extrn printf, seed_to_location, seeds, seed_count, map_data, map_lengths, map_count;
  auto seed_index, seed, min_location, location;

  seed_index = 0;
  min_location = 100000000000;

  while (seed_index < seed_count) {
    seed = seeds[seed_index];
    location = seed_to_location(seed, map_data, map_lengths, map_count);
    if (location < min_location) {
      min_location = location;
    }
    seed_index++;
  }

  return (min_location);
}

main() {
  extrn printf, print_array, read_input, compute_part1, line, seeds, seed_count, map_data, map_lengths, map_count;
  auto i, j, k, part1, part2;

  read_input(seeds, SEEDS_SIZE, &seed_count, map_data, MAP_DATA_SIZE, map_lengths, MAP_LENGTHS_SIZE, &map_count);

  #ifdef DEBUG_LOGGING
  printf("Parsed %d maps*n", map_count);

  printf("Seeds: ");
  print_array(seeds, seed_count);
  printf("*n");

  printf("Maps:*n");
  i = 0;
  k = 0;
  while (i < map_count) {
    j = 0;
    while (j < map_lengths[i]) {
      print_array(map_data + k, MAP_ENTRY_LENGTH);
      printf("*n");
      j++;
      k =+ MAP_ENTRY_LENGTH;
    }
    printf("*n");
    i++;
  }
  #endif

  part1 = compute_part1();
  printf("Part 1: %d*n", part1);
}
