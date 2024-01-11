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

struct Plan {
  const std::vector<Inst> insts;
  const Vec2 start;

  Rect boundingBox() {
    Rect bb = { .topLeft = start, .bottomRight = start };
    Vec2 pos = start;
    for (const Inst &inst : insts) {
      bb.topLeft = bb.topLeft.min(pos);
      bb.bottomRight = bb.bottomRight.max(pos);
      pos += inst.dir1;
    }
    return bb;
  }
};

struct Terrain {
  const Plan plan;
  const Rect bounds;
  std::vector<std::string> fields;

  Terrain(Plan plan) : plan(plan), bounds(plan.boundingBox()) {
    for (int i = 0; i < bounds.size().y; i++) {
      fields.push_back(std::string(bounds.size().x, ' '));
    }

    Vec2 pos = plan.start;
    for (const Inst &inst : plan.insts) {
      Vec2 next = pos + inst.dir1;
      Vec2 step = inst.dir1.signum();
      while (pos != next) {
        (*this)[pos] = '#';
        pos += step;
      }
    }
  }

  char &operator[](Vec2 pos) {
    Vec2 rel = pos - bounds.topLeft;
    return fields[rel.y][rel.x];
  }

  bool inBounds(Vec2 pos) {
    return pos.x >= bounds.topLeft.x && pos.x <= bounds.bottomRight.x && pos.y >= bounds.topLeft.y && pos.y <= bounds.bottomRight.y;
  }

  void fill() {
    // Very crude heuristic that will probably fail in the general case, but
    // works for our inputs.
    Vec2 inner = plan.start + (bounds.center() - plan.start).signum();
    std::unordered_set<Vec2> visited;
    std::deque<Vec2> queue;

    queue.push_back(inner);

    while (!queue.empty()) {
      Vec2 pos = queue.front();
      queue.pop_front();
      if (!visited.contains(pos)) {
        visited.insert(pos);
        (*this)[pos] = '#';
        for (Vec2 neighbor : pos.neighbors()) {
          if (inBounds(neighbor) && (*this)[neighbor] == ' ') {
            queue.push_back(neighbor);
          }
        }
      }
    }
  }

  int area() const {
    int total = 0;

    for (const std::string &row : fields) {
      for (char c : row) {
        if (c == '#') {
          total++;
        }
      }
    }

    return total;
  }
};

std::ostream &operator<<(std::ostream &os, const Terrain &terrain) {
  for (const std::string &row : terrain.fields) {
    os << row << std::endl;
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

  std::vector<Inst> insts;

  for (std::string line; std::getline(file, line);) {
    insts.push_back(Inst::parse(line));
  }

  Plan plan {insts, {0, 0}};
  Terrain terrain {plan};

  terrain.fill();
  std::cout << "Part 1: " << terrain.area() << std::endl;

  return 0;
}
