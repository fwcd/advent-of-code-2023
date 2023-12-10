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
    public Vec2 rotate90() { return new Vec2(-y, x); }
    
    public Vec2 scale(int factor) { return new Vec2(x * factor, y * factor); }

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
      return isInBounds(pos) ? lines.get(pos.y).charAt(pos.x) : ' ';
    }

    public Stream<Vec2> getPipeNeighbors(Vec2 pos) {
      return switch (get(pos)) {
        case '|' -> Stream.of(pos.offsetY(-1), pos.offsetY(1));
        case '-' -> Stream.of(pos.offsetX(-1), pos.offsetX(1));
        case 'L' -> Stream.of(pos.offsetY(-1), pos.offsetX(1));
        case 'J' -> Stream.of(pos.offsetY(-1), pos.offsetX(-1));
        case '7' -> Stream.of(pos.offsetY(1), pos.offsetX(-1));
        case 'F' -> Stream.of(pos.offsetY(1), pos.offsetX(1));
        case 'S' -> pos.cardinalNeighbors().filter(n -> getPipeNeighbors(n).anyMatch(pn -> pn.equals(pos)));
        default -> Stream.empty();
      };
    }

    public Stream<Vec2> getInnerNeighbors(Vec2 pos) {
      return switch (get(pos)) {
        case '.' -> pos.cardinalNeighbors().filter(n -> get(n) == '.');
        default -> Stream.empty();
      };
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

    private static enum BfsNodeType { START, PIPE, INNER }

    private static record BfsNode(Vec2 position, Vec2 tangent, int tangentSign, int distance, BfsNodeType type) {}

    private static record BfsResult(Set<Vec2> visited, int maxDistance, int innerCount) {}

    public BfsResult bfs(Vec2 start) {
      Deque<BfsNode> queue = new ArrayDeque<>(List.of(new BfsNode(start, new Vec2(0, 0), 1, 0, BfsNodeType.START)));
      Set<Vec2> visited = new HashSet<>();
      int maxDistance = 0;
      int innerCount = 0;

      while (!queue.isEmpty()) {
        BfsNode node = queue.poll();
        if (!visited.contains(node.position)) {
          visited.add(node.position);
          maxDistance = Math.max(node.distance, maxDistance);

          switch (node.type) {
          case BfsNodeType.INNER:
            innerCount++;
            break;
          case BfsNodeType.PIPE:
            Vec2 inwardNormal = node.tangent.rotate90();
            Vec2 neighbor = node.position.plus(inwardNormal);
            if (get(neighbor) == '.') {
              queue.offer(new BfsNode(neighbor, new Vec2(0, 0), node.tangentSign, node.distance + 1, BfsNodeType.INNER));
            }
            break;
          default:
            break;
          }

          int i = 0;
          Stream<Vec2> neighbors = (
            node.type == BfsNodeType.INNER
              ? getInnerNeighbors(node.position)
              : getPipeNeighbors(node.position)
          ).filter(this::isInBounds);

          for (Vec2 neighbor : (Iterable<Vec2>) neighbors::iterator) {
            int tangentSign = node.tangentSign;
            if (node.type == BfsNodeType.START && i % 2 == 0) {
              tangentSign *= -1;
            }
            Vec2 tangent = neighbor.minus(node.position).scale(tangentSign);
            BfsNodeType type = node.type == BfsNodeType.START ? BfsNodeType.PIPE : node.type;
            queue.offer(new BfsNode(neighbor, tangent, tangentSign, node.distance + 1, type));
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
