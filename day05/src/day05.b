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
  extrn printf, getline, buf;
  auto i, n;

  i = 0;
  while (i < 128) {
    buf[i++] = 0;
  }

  n = getline(buf, 128);
  printf("Got line of length %d: %s*n", n, buf);
}

buf[128];
