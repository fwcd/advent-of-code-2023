#define LINE_BUFSIZE 128

getline(buf, size) {
  extrn getchar;
  auto i, c;

  i = 0;

  while (1) {
    c = getchar();
    if (c == '*n' | c == '*e' | i >= (size - 1)) {
      goto end;
    }
    buf[i] = c;
    i++;
  }

  /* NOTE: B uses '*e' (EOT, U+0004) as string terminators instead of the 'modern' NUL. */
  end:
  buf[i] = '*e';

  return (i);
}

main() {
  extrn printf, getline, line;
  auto i, n;

  n = getline(line, LINE_BUFSIZE);
  printf("Got line of length %d: %s*n", n, line);
}

line[LINE_BUFSIZE];
