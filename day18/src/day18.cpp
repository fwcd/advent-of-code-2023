#include <fstream>
#include <iostream>

int main(int argc, char *argv[]) {
  if (argc == 1) {
    std::cerr << "Usage: " << argv[0] << " <path to input>" << std::endl;
    return 1;
  }

  std::ifstream file;
  file.open(argv[1]);

  // TODO

  return 0;
}
