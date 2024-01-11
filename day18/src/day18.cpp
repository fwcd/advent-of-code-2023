#include <fstream>
#include <ios>
#include <iostream>
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

struct Vec2 {
  int x;
  int y;

  Vec2(int x = 0, int y = 0) : x(x), y(y) {}

  Vec2 operator+(Vec2 rhs) const {
    return {x + rhs.x, y + rhs.y};
  }

  Vec2 operator*(int factor) {
    return {x * factor, y * factor};
  }
};

struct Inst {
  Vec2 vec;
  std::string color;
};

std::ostream &operator<<(std::ostream &os, const Vec2 &vec) {
  os << '(' << vec.x << ", " << vec.y << ')';
  return os;
}

std::ostream &operator<<(std::ostream &os, const Inst &inst) {
  os << inst.vec << " (#" << inst.color << ')';
  return os;
}

Vec2 parseDir(char c) {
  switch (c) {
  case 'L': return {-1, 0};
  case 'R': return {1, 0};
  case 'U': return {0, -1};
  case 'D': return {0, 1};
  default: throw std::runtime_error("Could not parse direction");
  }
}

template<char D>
std::istream &skip(std::istream &is) {
  // https://stackoverflow.com/a/14139975
  char parsed;
  if (is >> parsed && parsed != D) {
    is.setstate(std::ios_base::failbit);
  }
  return is;
}

Inst parseInst(const std::string &raw) {
  std::istringstream iss(raw);
  std::string rawDir, rawLength, rawColor;
  std::getline(iss, rawDir, ' ');
  std::getline(iss, rawLength, ' ');
  std::getline(iss, rawColor, ' ');
  return {
    .vec = parseDir(rawDir[0]) * std::stoi(rawLength),
    .color = rawColor.substr(2, 6),
  };
}

int main(int argc, char *argv[]) {
  if (argc == 1) {
    std::cerr << "Usage: " << argv[0] << " <path to input>" << std::endl;
    return 1;
  }

  std::ifstream file;
  file.open(argv[1]);

  std::vector<Inst> insts;

  for (std::string line; std::getline(file, line);) {
    insts.push_back(parseInst(line));
  }

  for (const Inst &inst : insts) {
    std::cout << inst << std::endl;
  }

  return 0;
}
