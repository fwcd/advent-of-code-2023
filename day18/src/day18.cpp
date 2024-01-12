#include <algorithm>
#include <array>
#include <cstddef>
#include <deque>
#include <fstream>
#include <functional>
#include <ios>
#include <iostream>
#include <iterator>
#include <optional>
#include <ostream>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <valarray>
#include <vector>

template <typename T>
T signum(T value) {
  return value > 0 ? 1 : value < 0 ? -1 : 0;
}

struct Vec2 {
  long long x;
  long long y;

  constexpr Vec2(long long x, long long y) : x(x), y(y) {}

  auto operator<=>(const Vec2 &rhs) const = default;

  Vec2 operator+(Vec2 rhs) const {
    return {x + rhs.x, y + rhs.y};
  }

  Vec2 operator-(Vec2 rhs) const {
    return {x - rhs.x, y - rhs.y};
  }

  Vec2 operator*(long long factor) const {
    return {x * factor, y * factor};
  }

  Vec2 operator/(long long divisor) const {
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

  long long cross(Vec2 rhs) const {
    return x * rhs.y - y * rhs.x;
  }

  Vec2 signum() const {
    return {::signum(x), ::signum(y)};
  }

  bool isAxisAlignedWith(Vec2 rhs) const {
    return (x == rhs.x && y == 0 && rhs.y == 0)
        || (y == rhs.y && x == 0 && rhs.x == 0);
  }

  bool isCollinearWithPoints(Vec2 other1, Vec2 other2) const {
    return (x == other1.x && x == other2.x)
        || (y == other1.y && y == other2.y);
  }

  std::array<Vec2, 4> neighbors() const {
    return {
      Vec2(x - 1, y),
      Vec2(x + 1, y),
      Vec2(x, y - 1),
      Vec2(x, y + 1),
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

struct Inst {
  std::array<Vec2, 2> dirs;

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

    return {{dir1, dir2}};
  }
};

std::ostream &operator<<(std::ostream &os, const Inst &inst) {
  os << inst.dirs[0] << ", " << inst.dirs[1];
  return os;
}

struct Polygon {
  std::vector<Vec2> vertices;

  /// Computes the area of the polygon via the shoelace formula.
  long long area() const {
    long long doubleArea = 0;
    for (int i0 = 0; i0 < vertices.size(); i0++) {
      int i1 = (i0 + 1) % vertices.size();
      doubleArea += vertices[i0].cross(vertices[i1]);
    }
    return doubleArea / 2;
  }
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

  std::array<Vec2, 2> positions {Vec2 {0, 0}, Vec2 {0, 0}};
  std::array<Polygon, 2> polygons {Polygon {}, Polygon {}};

  for (std::string line; std::getline(file, line);) {
    for (int i = 0; i < 2; i++) {
      Inst inst = Inst::parse(line);
      polygons[i].vertices.push_back(positions[i]);
      positions[i] += inst.dirs[i];
    }
  }

  for (int i = 0; i < 2; i++) {
    std::cout << "Part " << (i + 1) << ": " << polygons[i].area() << std::endl;
  }

  return 0;
}
