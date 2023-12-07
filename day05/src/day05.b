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
#define MAP_DATA_SIZE 2048
#define MAP_LENGTHS_SIZE 16
#define RANGES_SIZE 512

/* Parse states. */
#define PARSE_STATE_SEEDS 1
#define PARSE_STATE_DELIMITER 2
#define PARSE_STATE_MAP_HEADER 3
#define PARSE_STATE_MAP 4

/* Magic parse constants. */
#define SEEDS_SKIP_CHARS 7

/* Domain-specific stuff. */
#define MAP_ENTRY_LENGTH 3
#define RANGE_SIZE 2

/* Generic stuff. */
#define MAX_INT 10000000000

/* Read until the next newline or EOT and return the length of the line. */
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

/* Print an array to stdout. */
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

/* Parse an integer from the given string. */
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

/* Parse as many spaces as possible from the given string (and return the count). */
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

/* Parse a space-separated list of integers from the given string. */
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

/* Swap the values at the given indices in the given array. */
swap(buf, i, j) {
  auto tmp;

  tmp = buf[i];
  buf[i] = buf[j];
  buf[j] = tmp;
}

/* Sort by start of source range using insertion sort. */
sort_map(map_data, map_length) {
  extrn printf, print_array;
  auto i, j, k, previous_index, current_index;

  #ifdef DEBUG_LOGGING
  print_array(map_data, map_length * MAP_ENTRY_LENGTH);
  printf("*n");
  #endif

  i = 1;

  while (i < map_length) {
    j = i;
    while (j > 0) {
      previous_index = (j - 1) * MAP_ENTRY_LENGTH + 1;
      current_index  = j       * MAP_ENTRY_LENGTH + 1;

      if (map_data[(j - 1) * MAP_ENTRY_LENGTH + 1] <= map_data[j * MAP_ENTRY_LENGTH + 1]) {
        break;
      }

      #ifdef DEBUG_LOGGING
      printf("Swapping %d (%d) with %d (%d)*n", j - 1, map_data[(j - 1) * MAP_ENTRY_LENGTH + 1], j, map_data[j * MAP_ENTRY_LENGTH + 1]);
      #endif

      k = 0;
      while (k < MAP_ENTRY_LENGTH) {
        swap(map_data, (j - 1) * MAP_ENTRY_LENGTH + k, j * MAP_ENTRY_LENGTH + k);
        k++;
      }
      j--;
    }
    i++;
  }

  #ifdef DEBUG_LOGGING
  print_array(map_data, map_length * MAP_ENTRY_LENGTH);
  printf("*n");
  #endif
}

/*
 * Append the given range to the given map. Note that this assumes that map_data can
 * freely be extended, which is generally only the case right during the postprocessing
 * step (after parsing a map), since we would otherwise overwrite the next map in the buffer.
 */
append_range_to_map(dest_range_start, src_range_start, range_length, map_data, map_data_size, inout_map_length) {
  #ifdef DEBUG_LOGGING
  printf("Appending*n");
  print_array(map_data, *inout_map_length * MAP_ENTRY_LENGTH);
  printf("*n");
  #endif

  if (*inout_map_length * MAP_ENTRY_LENGTH >= map_data_size) {
    printf("Cannot append range [dest: %d, src: %d, length: %d] to map since map data size (%d) is too small!*n", dest_range_start, src_range_start, range_length, map_data_size);
    exit(1);
  }
  map_data[*inout_map_length * MAP_ENTRY_LENGTH]     = dest_range_start;
  map_data[*inout_map_length * MAP_ENTRY_LENGTH + 1] = src_range_start;
  map_data[*inout_map_length * MAP_ENTRY_LENGTH + 2] = range_length;
  ++*inout_map_length;

  #ifdef DEBUG_LOGGING
  print_array(map_data, *inout_map_length * MAP_ENTRY_LENGTH);
  printf("*n");
  #endif
}

/* Append the gaps between ranges to make sure we cover the entire domain. This assumes the map is sorted by src range start. */
fill_gaps_in_map(map_data, map_data_size, inout_map_length) {
  extrn exit, printf;
  auto offset, entry_index, base_map_length, src_range_start, range_length;

  offset = 0;
  entry_index = 0;
  base_map_length = *inout_map_length;

  while (entry_index < base_map_length) {
    src_range_start  = map_data[entry_index * MAP_ENTRY_LENGTH + 1];
    range_length     = map_data[entry_index * MAP_ENTRY_LENGTH + 2];

    if (offset < src_range_start) {
      /* We have a gap: [offset, src_range_start] */
      append_range_to_map(offset, offset, src_range_start - offset, map_data, map_data_size, inout_map_length);
    }

    offset = src_range_start + range_length;
    entry_index++;
  }

  append_range_to_map(offset, offset, MAX_INT - offset, map_data, map_data_size, inout_map_length);
}

/* Sort and append gaps as ranges to the map. */
postprocess_map(map_data, map_data_size, inout_map_length) {
  sort_map(map_data, *inout_map_length);
  fill_gaps_in_map(map_data, map_data_size, inout_map_length);
}

line[LINE_SIZE];

/* Read, parse and postprocess the input seeds and maps. */
read_input(seeds, seeds_size, out_seed_count, map_data, map_data_size, map_lengths, map_lengths_size, out_map_count) {
  extrn printf, exit, line, read_line, parse_integers;
  auto state, next_state, line_number, length, eof, map_index, map_length, map_data_offset, map_data_start_offset, entry_length;

  state = PARSE_STATE_SEEDS;
  next_state = state;
  line_number = 1;

  map_index = 0;
  map_length = 0;
  map_data_offset = 0;
  map_data_start_offset = 0;

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
        map_data_offset = map_data_start_offset + map_length * MAP_ENTRY_LENGTH;
        parse_integers(line, length, &entry_length, map_data + map_data_offset, map_data_size - map_data_offset);
        if (entry_length != MAP_ENTRY_LENGTH) {
          printf("Line %d: Map %d entry %d has length %d (expected %d)!*n", line_number, map_index, map_length, entry_length, MAP_ENTRY_LENGTH);
          exit(1);
        }
        map_length++;
      }
      if ((length == 0) | eof) {
        if (map_index >= map_lengths_size) {
          printf("Line %d: Map index out of bounds, maximum size is %d!*n", line_number, map_lengths_size);
          exit(1);
        }
        postprocess_map(map_data + map_data_start_offset, map_data_size - map_data_start_offset, &map_length);
        map_lengths[map_index] = map_length;
        map_data_start_offset =+ map_length * MAP_ENTRY_LENGTH;
        map_index++;
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

/* Apply the given map to a single value. */
map_value(value, map_data, map_length) {
  auto entry_index, dest_range_start, src_range_start, range_length, src_range_end;

  entry_index = 0;

  while (entry_index < map_length) {
    dest_range_start = map_data[entry_index * MAP_ENTRY_LENGTH];
    src_range_start  = map_data[entry_index * MAP_ENTRY_LENGTH + 1];
    range_length     = map_data[entry_index * MAP_ENTRY_LENGTH + 2];

    src_range_end = src_range_start + range_length;
    if ((value >= src_range_start) & (value < src_range_end)) {
      return (dest_range_start + value - src_range_start);
    }

    entry_index++;
  }

  return (value);
}

/* Find the location for a given seed. */
seed_to_location(seed, map_data, map_lengths, map_count) {
  extrn printf, map_value;
  auto value, map_index, map_length, map_data_offset;

  value = seed;
  map_index = 0;
  map_data_offset = 0;

  while (map_index < map_count) {
    map_length = map_lengths[map_index];
    value = map_value(value, map_data + map_data_offset, map_length);
    map_data_offset =+ map_length * MAP_ENTRY_LENGTH;
    map_index++;
  }

  return (value);
}

/* Returns the minimum of the given two values. */
min(x, y) {
  return (x < y ? x : y);
}

/* Returns the maximum of the given two values. */
max(x, y) {
  return (x > y ? x : y);
}

/* Copies a slice of an array into another. */
memcpy(dest, src, n) {
  auto i;

  i = 0;

  while (i < n) {
    dest[i] = src[i];
    i++;
  }
}

/* Test whether the given value is in the given range. */
range_contains(value, start, end) {
  return (value >= start & value < end);
}

/* Test whether the given two ranges intersect and where. */
range_intersect(start1, end1, start2, end2, out_intersects, out_start, out_end) {
  if (end2 <= start1 | end1 <= start2) {
    *out_intersects = 0;
  } else {
    *out_intersects = 1;
    *out_start = max(start1, start2);
    *out_end = min(end1, end2);
  }
}

/*
 * Apply the given map to a range.
 *
 * Applying a map to a range can result in multiple disjoint ranges
 * in the destination space therefore we pass in a buffer that we
 * write these ranges to.
 */
map_range(range_start, range_length, intersect_ranges, intersect_ranges_size, out_intersect_range_count, map_data, map_length) {
  extrn printf, exit;
  auto range_end, entry_index, dest_range_start, src_range_start, range_length, src_range_end, intersect_start, intersect_end, intersects, intersect_index;

  range_end = range_start + range_length;
  entry_index = 0;
  intersect_index = 0;

  while (entry_index < map_length) {
    dest_range_start = map_data[entry_index * MAP_ENTRY_LENGTH];
    src_range_start  = map_data[entry_index * MAP_ENTRY_LENGTH + 1];
    range_length     = map_data[entry_index * MAP_ENTRY_LENGTH + 2];

    src_range_end = src_range_start + range_length;
    range_intersect(range_start, range_end, src_range_start, src_range_end, &intersects, &intersect_start, &intersect_end);
    if (intersects) {
      if ((intersect_index + 1) * RANGE_SIZE > intersect_ranges_size) {
        printf("Intersect range buffer of size %d is too small.*n", intersect_ranges_size);
        exit(1);
      }
      intersect_ranges[intersect_index * RANGE_SIZE]     = intersect_start;
      intersect_ranges[intersect_index * RANGE_SIZE + 1] = intersect_end;
      intersect_index++;
    }

    entry_index++;
  }

  *out_intersect_range_count = intersect_index;
}

/* Apply the given map to multiple ranges. */
map_ranges(src_ranges, src_range_count, intersect_ranges, intersect_ranges_size, out_intersect_range_count, map_data, map_length) {
  auto src_range_index, total_intersect_range_count, current_intersect_range_count, range_start, range_length, intersect_ranges_offset;
  
  src_range_index = 0;
  total_intersect_range_count = 0;
  current_intersect_range_count = 0;

  while (src_range_index < src_range_count) {
    range_start  = src_ranges[src_range_index * RANGE_SIZE];
    range_length = src_ranges[src_range_index * RANGE_SIZE + 1];

    intersect_ranges_offset = total_intersect_range_count * RANGE_SIZE;
    map_range(range_start, range_length, intersect_ranges + intersect_ranges_offset, intersect_ranges_size - intersect_ranges_offset, &current_intersect_range_count, map_data, map_length);

    total_intersect_range_count =+ current_intersect_range_count;
    src_range_index++;
  }

  *out_intersect_range_count = total_intersect_range_count;
}

/* Apply all maps sequentially (seeds to locations) to multiple ranges. */
ranges_to_locations(src_ranges, inout_src_range_count, dest_ranges, dest_ranges_size, inout_dest_range_count, map_data, map_lengths, map_count) {
  auto map_index, map_length, map_data_offset;

  map_index = 0;
  map_data_offset = 0;

  while (map_index < map_count) {
    map_length = map_lengths[map_index];

    #ifdef DEBUG_LOGGING
    print_array(src_ranges, *inout_src_range_count);
    printf(" -> ");
    #endif

    map_ranges(src_ranges, *inout_src_range_count, dest_ranges, dest_ranges_size, inout_dest_range_count, map_data, map_length);

    memcpy(src_ranges, dest_ranges, *inout_dest_range_count);
    *inout_src_range_count = *inout_dest_range_count;

    #ifdef DEBUG_LOGGING
    print_array(dest_ranges, *inout_dest_range_count);
    printf("*n");
    #endif

    map_data_offset =+ map_length * MAP_ENTRY_LENGTH;
    map_index++;
  }
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
  min_location = MAX_INT;

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

/*
 * Ranges are stored as a flat buffer using to the following format:
 * 
 * | start | length | start | length | ... |
 */

src_ranges[RANGES_SIZE];
src_range_count;

dest_ranges[RANGES_SIZE];
dest_range_count;

compute_part2() {
  extrn src_range_count, ranges_to_locations;
  auto min_location, range_index;

  min_location = MAX_INT;
  range_index = 0;

  memcpy(src_ranges, seeds, seed_count);
  src_range_count = seed_count;
  dest_range_count = 0;

  ranges_to_locations(src_ranges, &src_range_count, dest_ranges, RANGES_SIZE, &dest_range_count, map_data, map_lengths, map_count);

  /* TODO */
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

  part2 = compute_part2();
  printf("Part 2: %d*n", part2);
}
