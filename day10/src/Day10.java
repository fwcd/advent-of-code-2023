import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;

public class Day10 {
  private static record Vec2(int x, int y) {
    public boolean isZero() { return x == 0 && y == 0; }

    public Vec2 rotate90() { return new Vec2(-y, x); }
    
    public Vec2 negate() { return new Vec2(-x, -y); }

    public Vec2 plus(Vec2 rhs) { return new Vec2(x + rhs.x, y + rhs.y); }

    public Vec2 minus(Vec2 rhs) { return new Vec2(x - rhs.x, y - rhs.y); }

    public Vec2 offsetX(int dx) { return new Vec2(x + dx, y); }

    public Vec2 offsetY(int dy) { return new Vec2(x, y + dy); }

    public Stream<Vec2> cardinalNeighbors() { return Stream.of(offsetY(-1), offsetY(1), offsetX(-1), offsetX(1)); }
  }

  public static record Maze(List<String> lines) {
    public boolean isInBounds(Vec2 pos) {
      return pos.x >= 0 && pos.x < lines.get(0).length() && pos.y >= 0 && pos.y < lines.size();
    }

    public char get(Vec2 pos) {
      return lines.get(pos.y).charAt(pos.x);
    }

    public Stream<Vec2> getPipeNeighbors(Vec2 pos) {
      return switch (get(pos)) {
        case '|' -> Stream.of(pos.offsetY(-1), pos.offsetY(1));
        case '-' -> Stream.of(pos.offsetX(-1), pos.offsetX(1));
        case 'L' -> Stream.of(pos.offsetY(-1), pos.offsetX(1));
        case 'J' -> Stream.of(pos.offsetY(-1), pos.offsetX(-1));
        case '7' -> Stream.of(pos.offsetY(1), pos.offsetX(-1));
        case 'F' -> Stream.of(pos.offsetY(1), pos.offsetX(1));
        case 'S' -> pos.cardinalNeighbors().filter(n -> isInBounds(n) && getPipeNeighbors(n).anyMatch(pn -> pn.equals(pos)));
        default -> Stream.empty();
      };
    }

    public Stream<Vec2> getInnerNeighbors(Vec2 pos) {
      return switch (get(pos)) {
        case '.' -> pos.cardinalNeighbors().filter(n -> isInBounds(pos) && get(pos) == '.');
        default -> Stream.empty();
      };
    }

    public Stream<Vec2> getNeighborsInBounds(Vec2 pos) {
      return Stream.concat(getPipeNeighbors(pos), getInnerNeighbors(pos)).filter(this::isInBounds);
    }

    public Optional<Vec2> locate(char value) {
      for (int y = 0; y < lines.size(); y++) {
        String line = lines.get(y);
        for (int x = 0; x < line.length(); x++) {
          if (line.charAt(x) == value) {
            return Optional.of(new Vec2(x, y));
          }
        }
      }
      return Optional.empty();
    }

    private static record BfsNode(Vec2 position, Vec2 tangent, int distance, boolean isInner) {}

    private static record BfsResult(Set<Vec2> visited, int maxDistance, int innerCount) {}

    public BfsResult bfs(Vec2 start) {
      Deque<BfsNode> queue = new ArrayDeque<>(List.of(new BfsNode(start, new Vec2(0, 0), 0, /* isInner = */ false)));
      Set<Vec2> visited = new HashSet<>();
      int maxDistance = 0;
      int innerCount = 0;

      while (!queue.isEmpty()) {
        BfsNode node = queue.poll();
        if (!visited.contains(node.position)) {
          visited.add(node.position);
          maxDistance = Math.max(node.distance, maxDistance);

          if (node.isInner) {
            innerCount++;
          } else if (!node.tangent.isZero()) {
            Vec2 inwardNormal = node.tangent.rotate90();
            queue.offer(new BfsNode(node.position.plus(inwardNormal), new Vec2(0, 0), node.distance + 1, /* isInner = */ true));
          }

          int i = 0;
          for (Vec2 neighbor : (Iterable<Vec2>) getNeighborsInBounds(node.position)::iterator) {
            Vec2 tangent = neighbor.minus(node.position);
            if (i % 2 != 0) {
              tangent = tangent.negate();
            }
            queue.offer(new BfsNode(neighbor, tangent, node.distance + 1, node.isInner));
            i++;
          }
        }
      }

      return new BfsResult(visited, maxDistance, innerCount);
    }
  }

  public static void main(String[] args) throws IOException {
    if (args.length == 0) {
      System.err.println("Usage: day10 <input>");
      System.exit(1);
    }

    var maze = new Maze(Files.readAllLines(Paths.get(args[0])));
    var bfsResult = maze.bfs(maze.locate('S').orElseThrow(() -> new NoSuchElementException("No start found")));

    System.out.println("Part 1: " + bfsResult.maxDistance);
    System.out.println("Part 2: " + bfsResult.innerCount);
  }
}
