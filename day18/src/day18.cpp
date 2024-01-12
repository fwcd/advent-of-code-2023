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

  static const Vec2 ZERO;
  static const Vec2 RIGHT;
  static const Vec2 DOWN;
  static const Vec2 LEFT;
  static const Vec2 UP;

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

  bool isAxisAligned() const {
    return x == 0 || y == 0;
  }

  long long manhattan() const {
    return std::abs(x) + std::abs(y);
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
    case 'L': return Vec2::LEFT;
    case 'R': return Vec2::RIGHT;
    case 'U': return Vec2::UP;
    case 'D': return Vec2::DOWN;
    default: throw std::runtime_error("Could not parse direction");
    }
  }
};

constexpr Vec2 Vec2::ZERO = {0, 0};
constexpr Vec2 Vec2::RIGHT = {1, 0};
constexpr Vec2 Vec2::DOWN = {0, 1};
constexpr Vec2 Vec2::LEFT = {-1, 0};
constexpr Vec2 Vec2::UP = {0, -1};

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

constexpr int PARTS = 2;

struct Inst {
  std::array<Vec2, PARTS> dirs;

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

  /// Computes the area of the polygon via the shoelace triangle formula.
  /// See https://en.wikipedia.org/wiki/Shoelace_formula#Triangle_formula
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

  std::vector<Inst> insts;

  {
    std::ifstream file;
    file.open(argv[1]);

    for (std::string line; std::getline(file, line);) {
      insts.push_back(Inst::parse(line));
    }
  }

  std::array<Vec2, PARTS> positions {Vec2 {0, 0}, Vec2 {0, 0}};
  std::array<Polygon, PARTS> polygons {Polygon {}, Polygon {}};

  // Since we want to include the boundary too, we'll have to add some extra
  // area to account for that.
  //
  // Essentially we have:        but we want:
  //   +---+---+                  ##########
  //   | ##### |                  ##########
  //   + ##### +                  ##########
  //      ...                        ...
  //
  // i.e. we pretend that every vertex is in the center of each integer grid
  // cell, but we want the area to include the full grid cells.

  std::array<long long, PARTS> quadrupleExtraArea {0, 0};

  for (int i = 0; i < insts.size(); i++) {
    Inst inst = insts[i];
    Inst next = insts[(i + 1) % insts.size()];

    std::cout << inst << std::endl;

    for (int part = 0; part < PARTS; part++) {
      Vec2 dir = inst.dirs[part];
      polygons[part].vertices.push_back(positions[part]);
      positions[part] += dir;
      
      Vec2 step = dir.signum();
      quadrupleExtraArea[part] += (dir - step).manhattan() * 2;

      // The cross product is > 0 if we turn right, in which case we need to
      // fill in 3 quarters of a grid cell, < 0 if we turn left, in which case
      // we have to fill in 1 quarter etc or 0 if we move ahead, in which case
      // we have to fill in 2 quarters (i.e. half the cell).
      int turn = signum(dir.cross(next.dirs[part]));
      quadrupleExtraArea[part] += 2 + turn;
    }
  }

  for (int part = 0; part < PARTS; part++) {
    long long area = polygons[part].area() + quadrupleExtraArea[part] / 4;
    std::cout << "Part " << (part + 1) << ": " << area << std::endl;
  }

  return 0;
}
