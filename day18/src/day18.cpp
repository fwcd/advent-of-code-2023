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

  bool isCollinear(Vec2 other1, Vec2 other2) const {
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

struct Triangle {
  Vec2 a, b, c;

  bool contains(Vec2 p) const {
    // Compute barycentric coordinates
    Triangle baryA {p, b, c};
    Triangle baryB {a, p, c};
    Triangle baryC {a, b, p};
    int lamA = baryA.signedDoubleArea();
    int lamB = baryB.signedDoubleArea();
    int lamC = baryC.signedDoubleArea();
    std::cout << "  Testing " << p << ": " << lamA << " and " << lamB << " and " << lamC << std::endl;
    // If all barycentric coordinates are positive, we definitely contain the point
    // If one of the coordinates is zero, the point is on the boundary
    return lamA >= 0 && lamB >= 0 && lamC >= 0;
  }

  bool isFacingUp() const {
    // True iff the triangle is wound counter-clockwise
    return signedDoubleArea() > 0;
  }

  long long signedDoubleArea() const {
    return (a - b).cross(c - b);
  }
};

std::ostream &operator<<(std::ostream &os, const Triangle &tri) {
  os << tri.a << ", " << tri.b << ", " << tri.c;
  return os;
}

struct Polygon {
  std::vector<Vec2> vertices;

  /// Triangulates the polygon using ear clipping in O(|vertices|^2).
  std::vector<Triangle> triangulate() const {
    std::vector<Triangle> triangles;
    std::vector<Vec2> remaining = vertices;

    std::reverse(remaining.begin(), remaining.end());

    // Perform ear clipping
    while (remaining.size() > 3) {
      // Merge collinear vertices
      for (int i0 = 0; i0 < remaining.size(); ) {
        int i1 = (i0 + 1) % remaining.size();
        int i2 = (i0 + 2) % remaining.size();
        if (remaining[i0].isCollinear(remaining[i1], remaining[i2])) {
          remaining.erase(remaining.begin() + i1);
        } else {
          i0++;
        }
      }

      std::cout << "Remaining: ";
      for (Vec2 v : remaining) {
        std::cout << v << ",";
      }
      std::cout << std::endl;

      // Find convex ear
      for (int i0 = 0; i0 < remaining.size(); i0++) {
        int i1 = (i0 + 1) % remaining.size();
        int i2 = (i0 + 2) % remaining.size();
        Vec2 p0 = remaining[i0];
        Vec2 p1 = remaining[i1];
        Vec2 p2 = remaining[i2];
        Triangle tri {p0, p1, p2};
        bool isConvex = tri.isFacingUp();

        Vec2 d0 = p0 - p1;
        Vec2 d2 = p2 - p1;
        std::cout << "  ==> " << p0 << ", " << p1 << ", " << p2 << " -\td0: " << d0 << ",\td2: " << d2 << ",\td0 x d2: " << d0.cross(d2) << ",\td2 x d0: " << d2.cross(d0) << ", convex: " << isConvex << std::endl;

        if (isConvex) {
          // Check if lies within the polygon by testing if every vertex is in the triangle
          bool intersectsPolygon = false;
          for (int j = 0; j < remaining.size(); j++) {
            if (j != i0 && j != i1 && j != i2 && tri.contains(remaining[j])) {
              intersectsPolygon = true;
              break;
            }
          }

          if (!intersectsPolygon) {
            // Clip it!
            triangles.push_back({p0, p1, p2});
            remaining.erase(remaining.begin() + i1);
            std::cout << "Snip snap" << std::endl;
            goto continueOuter;
          }
        }
      }

      throw new std::runtime_error("No convex ear found!");

      continueOuter:;
    }

    triangles.push_back({remaining[0], remaining[1], remaining[2]});

    return triangles;
  }

  /// Computes the area of the polygon via triangulation.
  long long area() const {
    long long doubleArea = 0;
    std::vector<Triangle> tris = triangulate();
    for (const Triangle &tri : tris) {
      doubleArea += std::abs(tri.signedDoubleArea());
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

  std::cout << "Part 1: " << poly1.area() << std::endl;
  // std::cout << "Part 2: " << poly2.area() << std::endl;

  return 0;
}
