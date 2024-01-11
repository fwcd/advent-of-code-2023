#include <algorithm>
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

  Vec2 operator+(Vec2 rhs) const {
    return {x + rhs.x, y + rhs.y};
  }

  Vec2 operator-(Vec2 rhs) const {
    return {x - rhs.x, y - rhs.y};
  }

  Vec2 operator*(int factor) const {
    return {x * factor, y * factor};
  }

  void operator+=(Vec2 rhs) {
    x += rhs.x;
    y += rhs.y;
  }

  void operator-=(Vec2 rhs) {
    x -= rhs.x;
    y -= rhs.y;
  }

  Vec2 max(Vec2 rhs) const {
    return {std::max(x, rhs.x), std::max(y, rhs.y)};
  }

  Vec2 min(Vec2 rhs) const {
    return {std::min(x, rhs.x), std::min(y, rhs.y)};
  }
};

struct Rect {
  Vec2 topLeft;
  Vec2 bottomRight;
};

struct Inst {
  Vec2 vec;
  std::string color;
};

Rect boundingBox(const std::vector<Inst> &insts, Vec2 start = {0, 0}) {
  Rect bb = { .topLeft = start, .bottomRight = start };
  Vec2 current = start;
  for (const Inst &inst : insts) {
    bb.topLeft = bb.topLeft.max(current);
    bb.bottomRight = bb.topLeft.min(current);
    current += inst.vec;
  }
  return bb;
}

std::vector<std::string> digTerrain(const std::vector<Inst> &insts) {
  Rect bb = boundingBox(insts);
  std::vector<std::string> terrain;

  for (const Inst &inst : insts) {
    // TODO
  }

  return terrain;
}

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
