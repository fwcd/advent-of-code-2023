import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Day10 {
  private static record Vec2(int x, int y) {
    public Vec2 rotateCCW() { return new Vec2(-y, x); }

    public Vec2 rotateCW() { return new Vec2(y, -x); }
    
    public Vec2 scale(int factor) { return new Vec2(x * factor, y * factor); }

    public Vec2 plus(Vec2 rhs) { return new Vec2(x + rhs.x, y + rhs.y); }

    public Vec2 minus(Vec2 rhs) { return new Vec2(x - rhs.x, y - rhs.y); }

    public Vec2 offsetX(int dx) { return new Vec2(x + dx, y); }

    public Vec2 offsetY(int dy) { return new Vec2(x, y + dy); }

    public Stream<Vec2> cardinalNeighbors() { return Stream.of(offsetY(-1), offsetY(1), offsetX(-1), offsetX(1)); }
  }

  public static record Maze(List<String> lines) {
    public int getHeight() { return lines.size(); }

    public int getWidth() { return lines.get(0).length(); }

    public boolean isInBounds(Vec2 pos) {
      return pos.x >= 0 && pos.x < getWidth() && pos.y >= 0 && pos.y < getHeight();
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

    public Optional<Vec2> locate(char value) {
      for (int y = 0; y < getHeight(); y++) {
        String line = lines.get(y);
        for (int x = 0; x < getWidth(); x++) {
          if (line.charAt(x) == value) {
            return Optional.of(new Vec2(x, y));
          }
        }
      }
      return Optional.empty();
    }

    public static record PipeSegment(Vec2 position, Vec2 tangent) {}

    public static record Pipe(List<PipeSegment> segments, Set<Vec2> positions) {}

    public Pipe dfsPipe(Vec2 start) {
      Set<Vec2> visited = new HashSet<>();
      List<PipeSegment> segments = new ArrayList<>();
      Deque<PipeSegment> stack = new ArrayDeque<>(List.of(new PipeSegment(start, new Vec2(0, 0))));

      while (!stack.isEmpty()) {
        PipeSegment segment = stack.pop();
        if (!visited.contains(segment.position)) {
          visited.add(segment.position);
          segments.add(segment);
          for (Vec2 neighbor : (Iterable<Vec2>) getPipeNeighbors(segment.position)::iterator) {
            stack.push(new PipeSegment(neighbor, neighbor.minus(segment.position)));
          }
        }
      }

      return new Pipe(segments, visited);
    }

    private Set<Vec2> dfsInner(Vec2 start, Pipe pipe) {
      if (pipe.positions.contains(start)) {
        return Set.of();
      }

      Set<Vec2> visited = new HashSet<>();
      Deque<Vec2> stack = new ArrayDeque<>(List.of(start));

      while (!stack.isEmpty()) {
        Vec2 position = stack.pop();
        if (!visited.contains(position)) {
          visited.add(position);
          for (Vec2 neighbor : (Iterable<Vec2>) position.cardinalNeighbors()::iterator) {
            if (!pipe.positions.contains(neighbor)) {
              if (!isInBounds(neighbor)) {
                return Set.of();
              }
              stack.push(neighbor);
            }
          }
        }
      }

      return visited;
    }

    public Set<Vec2> dfsAllInner(Pipe pipe, int sign) {
      Set<Vec2> visited = new HashSet<>();

      for (PipeSegment segment : pipe.segments) {
        Vec2 normal = segment.tangent.rotateCCW().scale(sign);
        visited.addAll(dfsInner(segment.position.plus(normal), pipe));
      }
      
      return visited;
    }

    public String toString(Set<Vec2> markedPositions) {
      return IntStream.range(0, getHeight())
        .mapToObj(y -> IntStream.range(0, getWidth())
          .mapToObj(x -> new Vec2(x, y))
          .map(pos -> markedPositions.contains(pos) ? "\033[31;1;4mX\033[0m" : Character.toString(get(pos)))
          .collect(Collectors.joining()))
        .collect(Collectors.joining("\n"));
    }

    @Override
    public String toString() {
      return toString(Set.of());
    }
  }

  public static void main(String[] args) throws IOException {
    if (args.length == 0) {
      System.err.println("Usage: day10 <input>");
      System.exit(1);
    }

    var maze = new Maze(Files.readAllLines(Paths.get(args[0])));
    var pipe = maze.dfsPipe(maze.locate('S').orElseThrow(() -> new NoSuchElementException("No start found")));

    System.out.println("Part 1: " + pipe.segments.size() / 2);

    for (int sign : List.of(1, -1)) {
      var inner = maze.dfsAllInner(pipe, sign);
      System.out.println(maze.toString(inner));
      System.out.println("Part 2 candidate: " + inner.size());
    }
  }
}
