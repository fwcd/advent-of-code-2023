#include <stdbool.h>
#include <stdio.h>
#include <string.h>

const char *DIGITS[] = {"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};

int scan_part1_digit(const char *s, int length) {
  if (length > 0) {
    char c = *s;
    if (c >= '0' && c <= '9') {
      return c - '0';
    }
  }
  return -1;
}

bool is_part2_digit(const char *s, int length, int n) {
  const char *digit = DIGITS[n - 1];
  int digit_length = strlen(digit);
  if (digit_length <= length) {
    for (int i = 0; i < digit_length; i++) {
      if (s[i] != digit[i]) {
        return false;
      }
    }
    return true;
  }
  return false;
}

int scan_part2_digit(const char *s, int length) {
  int n = scan_part1_digit(s, length);
  if (n > 0) {
    return n;
  }
  for (n = 1; n <= sizeof(DIGITS) / sizeof(const char *); n++) {
    if (is_part2_digit(s, length, n)) {
      return n;
    }
  }
  return -1;
}

int find_number(const char *line, int length, int (*scan_digit)(const char *, int)) {
  int first = -1;
  int last = -1;
  for (int i = 0; i < length; i++) {
    char n = scan_digit(line + i, length - i);
    if (n > 0) {
      last = n;
      if (first < 0) {
        first = last;
      }
    }
  }
  return first * 10 + last;
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "Usage: %s <input>\n", argv[0]);
    return 1;
  }

  FILE *file = fopen(argv[1], "r");
  if (file == NULL) {
    fprintf(stderr, "Could not open input!\n");
    return 1;
  }

  char *line = NULL;
  size_t length = 0;
  size_t size = 0;

  int part1 = 0;
  int part2 = 0;

  while ((length = getline(&line, &size, file)) != -1) {
    if (length > 0) {
      part1 += find_number(line, length, scan_part1_digit);
      part2 += find_number(line, length, scan_part2_digit);
    }
  }

  fclose(file);

  printf("Part 1: %d\n", part1);
  printf("Part 2: %d\n", part2);

  return 0;
}
