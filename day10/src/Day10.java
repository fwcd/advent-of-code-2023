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

    public Vec2 offsetX(int dx) { return new Vec2(x + dx, y); }

    public Vec2 offsetY(int dy) { return new Vec2(x, y + dy); }

    public Stream<Vec2> cardinalNeighbors() { return Stream.of(offsetY(-1), offsetY(1), offsetX(-1), offsetX(1)); }
  }

  public static record Maze(List<String> lines) {
    public char get(Vec2 pos) {
      return lines.get(pos.y).charAt(pos.x);
    }

    public List<Vec2> getPipeNeighbors(Vec2 pos) {
      switch (get(pos)) {
      case '|': return List.of(pos.offsetY(-1), pos.offsetY(1));
      case '-': return List.of(pos.offsetX(-1), pos.offsetX(1));
      case 'L': return List.of(pos.offsetY(-1), pos.offsetX(1));
      case 'J': return List.of(pos.offsetY(-1), pos.offsetX(-1));
      case '7': return List.of(pos.offsetY(1), pos.offsetX(-1));
      case 'F': return List.of(pos.offsetY(1), pos.offsetX(1));
      case 'S':
        return pos.cardinalNeighbors()
          .filter(n -> getPipeNeighbors(n).contains(pos))
          .toList();
      default: return List.of();
      }
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

    private static record BfsNode(Vec2 position, int distance) {}

    private static record BfsResult(Set<Vec2> visited, int maxDistance) {}

    public BfsResult bfs(Vec2 start) {
      Deque<BfsNode> queue = new ArrayDeque<>(List.of(new BfsNode(start, 0)));
      Set<Vec2> visited = new HashSet<>();
      int maxDistance = 0;

      while (!queue.isEmpty()) {
        BfsNode node = queue.poll();
        if (!visited.contains(node.position)) {
          visited.add(node.position);
          maxDistance = Math.max(node.distance, maxDistance);
          for (Vec2 neighbor : getPipeNeighbors(node.position)) {
            queue.offer(new BfsNode(neighbor, node.distance + 1));
          }
        }
      }

      return new BfsResult(visited, maxDistance);
    }
  }

  public static void main(String[] args) throws IOException {
    if (args.length == 0) {
      System.err.println("Usage: day10 <input>");
      System.exit(1);
    }

    var maze = new Maze(Files.readAllLines(Paths.get(args[0])));
    var bfsResult = maze.bfs(maze.locate('S').orElseThrow(() -> new NoSuchElementException("No start found")));

    int part1 = bfsResult.maxDistance;
    System.out.println("Part 1: " + part1);
  }
}
