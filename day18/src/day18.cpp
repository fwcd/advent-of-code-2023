#include <algorithm>
#include <fstream>
#include <ios>
#include <iostream>
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

template <typename T>
int signum(T value) {
  return value > 0 ? 1 : value < 0 ? -1 : 0;
}

struct Vec2 {
  int x;
  int y;

  auto operator<=>(const Vec2 &rhs) const = default;

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

  Vec2 signum() const {
    return {::signum(x), ::signum(y)};
  }
};

struct Rect {
  Vec2 topLeft;
  Vec2 bottomRight;

  Vec2 size() {
    return bottomRight - topLeft + Vec2({1, 1});
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

Rect boundingBox(const std::vector<Inst> &insts, Vec2 start = {0, 0}) {
  Rect bb = { .topLeft = start, .bottomRight = start };
  Vec2 pos = start;
  for (const Inst &inst : insts) {
    bb.topLeft = bb.topLeft.min(pos);
    bb.bottomRight = bb.bottomRight.max(pos);
    pos += inst.vec;
  }
  return bb;
}

std::vector<std::string> digTerrain(const std::vector<Inst> &insts, Vec2 start = {0, 0}) {
  Rect bb = boundingBox(insts);
  std::vector<std::string> terrain;

  for (int i = 0; i < bb.size().y; i++) {
    terrain.push_back(std::string(bb.size().x, ' '));
  }

  Vec2 pos = start;
  for (const Inst &inst : insts) {
    Vec2 next = pos + inst.vec;
    Vec2 step = inst.vec.signum();
    while (pos != next) {
      terrain[pos.y][pos.x] = '#';
      pos += step;
    }
  }

  return terrain;
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

  std::vector<std::string> terrain = digTerrain(insts);

  for (const std::string &row : terrain) {
    std::cout << row << std::endl;
  }

  return 0;
}
