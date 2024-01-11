#include <algorithm>
#include <array>
#include <cstddef>
#include <deque>
#include <fstream>
#include <functional>
#include <ios>
#include <iostream>
#include <ostream>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <string>
#include <unordered_set>
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

  Vec2 operator/(int divisor) const {
    return {x / divisor, y / divisor};
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

  std::array<Vec2, 4> neighbors() const {
    return {
      Vec2({x - 1, y}),
      Vec2({x + 1, y}),
      Vec2({x, y - 1}),
      Vec2({x, y + 1}),
    };
  }

  static Vec2 parseDir(char c) {
    switch (c) {
    case 'L': return {-1, 0};
    case 'R': return {1, 0};
    case 'U': return {0, -1};
    case 'D': return {0, 1};
    default: throw std::runtime_error("Could not parse direction");
    }
  }
};

std::ostream &operator<<(std::ostream &os, const Vec2 &vec) {
  os << '(' << vec.x << ", " << vec.y << ')';
  return os;
}

template <>
struct std::hash<Vec2> {
  std::size_t operator()(const Vec2 &vec) const {
    return std::hash<int>()(vec.x) ^ std::hash<int>()(vec.y);
  }
};

struct Rect {
  Vec2 topLeft;
  Vec2 bottomRight;

  Vec2 center() const {
    return (topLeft + bottomRight) / 2;
  }

  Vec2 size() const {
    return bottomRight - topLeft + Vec2({1, 1});
  }
};

struct Inst {
  const Vec2 dir1;
  const Vec2 dir2;

  static Inst parse(const std::string &raw) {
    std::istringstream iss(raw);
    std::string rawDir1, rawLength1, rawHex;
    std::getline(iss, rawDir1, ' ');
    std::getline(iss, rawLength1, ' ');
    std::getline(iss, rawHex, ' ');

    Vec2 dir1 = Vec2::parseDir(rawDir1[0]) * std::stoi(rawLength1);

    int length2 = std::stoi(rawHex.substr(2, 5), nullptr, 16);
    char rawDir2;
    switch (rawHex[7] - '0') {
    case 0: rawDir2 = 'R'; break;
    case 1: rawDir2 = 'D'; break;
    case 2: rawDir2 = 'L'; break;
    case 3: rawDir2 = 'U'; break;
    }

    Vec2 dir2 = Vec2::parseDir(rawDir2) * length2;

    return { .dir1 = dir1, .dir2 = dir2 };
  }
};

std::ostream &operator<<(std::ostream &os, const Inst &inst) {
  os << inst.dir1 << ", " << inst.dir2;
  return os;
}

struct Polygon {
  std::vector<Vec2> vertices;
};

std::ostream &operator<<(std::ostream &os, const Polygon &poly) {
  int count = poly.vertices.size();
  for (int i = 0; i < count; i++) {
    os << poly.vertices[i];
    if (i < count - 1) {
      os << ", ";
    }
  }
  return os;
}

int main(int argc, char *argv[]) {
  if (argc == 1) {
    std::cerr << "Usage: " << argv[0] << " <path to input>" << std::endl;
    return 1;
  }

  std::ifstream file;
  file.open(argv[1]);

  Vec2 pos1 {0, 0},
       pos2 {0, 0};
  Polygon poly1, poly2;

  for (std::string line; std::getline(file, line);) {
    Inst inst = Inst::parse(line);
    poly1.vertices.push_back(pos1);
    poly2.vertices.push_back(pos2);
    pos1 += inst.dir1;
    pos2 += inst.dir2;
  }

  std::cout << poly1 << std::endl;
  std::cout << poly2 << std::endl;

  return 0;
}
