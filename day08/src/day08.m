#import <Foundation/Foundation.h>

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "Usage: %s <input>\n", argv[0]);
    return 1;
  }
  NSLog(@"Hello world!");
  return 0;
}
