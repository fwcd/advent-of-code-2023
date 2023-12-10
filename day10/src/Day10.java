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
  private static record Position(int x, int y) {
    public Position offsetX(int dx) { return new Position(x + dx, y); }

    public Position offsetY(int dy) { return new Position(x, y + dy); }
  }

  public static record Maze(List<String> lines) {
    public char get(Position pos) {
      return lines.get(pos.y).charAt(pos.x);
    }

    public List<Position> getNeighbors(Position pos) {
      switch (get(pos)) {
      case '|': return List.of(pos.offsetY(-1), pos.offsetY(1));
      case '-': return List.of(pos.offsetX(-1), pos.offsetX(1));
      case 'L': return List.of(pos.offsetY(-1), pos.offsetX(1));
      case 'J': return List.of(pos.offsetY(-1), pos.offsetX(-1));
      case '7': return List.of(pos.offsetY(1), pos.offsetX(-1));
      case 'F': return List.of(pos.offsetY(1), pos.offsetX(1));
      case 'S':
        return Stream.of(pos.offsetY(-1), pos.offsetY(1), pos.offsetX(-1), pos.offsetX(1))
          .filter(n -> getNeighbors(n).contains(pos))
          .toList();
      default: return List.of();
      }
    }

    public Optional<Position> locate(char value) {
      for (int y = 0; y < lines.size(); y++) {
        String line = lines.get(y);
        for (int x = 0; x < line.length(); x++) {
          if (line.charAt(x) == value) {
            return Optional.of(new Position(x, y));
          }
        }
      }
      return Optional.empty();
    }

    private static record BfsNode(Position position, int distance) {}

    public int bfsMaxDistance(Position start) {
      Deque<BfsNode> queue = new ArrayDeque<>(List.of(new BfsNode(start, 0)));
      Set<Position> visited = new HashSet<>();
      int maxDistance = 0;

      while (!queue.isEmpty()) {
        BfsNode node = queue.poll();
        if (!visited.contains(node.position)) {
          visited.add(node.position);
          maxDistance = Math.max(node.distance, maxDistance);
          for (Position neighbor : getNeighbors(node.position)) {
            queue.offer(new BfsNode(neighbor, node.distance + 1));
          }
        }
      }

      return maxDistance;
    }
  }

  public static void main(String[] args) throws IOException {
    if (args.length == 0) {
      System.err.println("Usage: day10 <input>");
      System.exit(1);
    }

    Maze maze = new Maze(Files.readAllLines(Paths.get(args[0])));
    int part1 = maze.bfsMaxDistance(maze.locate('S').orElseThrow(() -> new NoSuchElementException("No start found")));

    System.out.println("Part 1: " + part1);
  }
}
