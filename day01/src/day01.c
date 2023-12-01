#include <stdio.h>

int main(int argc, char **argv) {
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

  int sum = 0;

  while ((length = getline(&line, &size, file)) != -1) {
    if (length > 0) {
      int first = -1;
      int last = -1;
      for (int i = 0; i < length; i++) {
        char c = line[i];
        if (c >= '0' && c <= '9') {
          last = c - '0';
          if (first < 0) {
            first = last;
          }
        }
      }
      sum += first * 10 + last;
    }
  }

  fclose(file);

  printf("Part 1: %d\n", sum);

  return 0;
}
