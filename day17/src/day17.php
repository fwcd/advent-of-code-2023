<?php

class Vec2 {
  public int $x;
  public int $y;

  function __construct(int $x = 0, int $y = 0) {
    $this->x = $x;
    $this->y = $y;
  }

  function __toString(): string {
    return "($this->x, $this->y)";
  }

  function inBounds(int $width, int $height) {
    return $this->x >= 0 && $this->x < $width && $this->y >= 0 && $this->y < $height;
  }

  function add(Vec2 $rhs): Vec2 {
    return new Vec2($this->x + $rhs->x, $this->y + $rhs->y);
  }

  function sub(Vec2 $rhs): Vec2 {
    return new Vec2($this->x - $rhs->x, $this->y - $rhs->y);
  }

  function turnLeft(): Vec2 {
    return new Vec2($this->y, -$this->x);
  }

  function turnRight(): Vec2 {
    return new Vec2(-$this->y, $this->x);
  }

  function arrow(): string {
    if ($this == new Vec2(1, 0)) {
      return '>';
    } elseif ($this == new Vec2(-1, 0)) {
      return '<';
    } elseif ($this == new Vec2(0, 1)) {
      return 'v';
    } elseif ($this == new Vec2(0, -1)) {
      return '^';
    } else {
      return '?';
    }
  }
}

class Node {
  public Vec2 $pos;
  public Vec2 $dir;
  public array $path;
  public int $total;
  public int $straightLeft;

  function __construct(Vec2 $pos, Vec2 $dir, array $path, int $total, int $straightLeft) {
    $this->pos = $pos;
    $this->dir = $dir;
    $this->path = $path;
    $this->total = $total;
    $this->straightLeft = $straightLeft;
  }
}

function shortestPath(array $matrix, int $maxStraight = 3): Node {
  $visited = [];
  $queue = new \SplPriorityQueue();
  foreach ([new Vec2(1, 0), new Vec2(0, 1)] as $start) {
    $queue->insert(new Node($start, $start, [], intval($matrix[$start->y][$start->x]), $maxStraight - 1), 0);
  }

  $width = strlen($matrix[0]);
  $height = count($matrix);
  $dest = new Vec2($width - 1, $height - 1);

  while ($queue->valid()) {
    $node = $queue->extract();
    if (!array_key_exists((string) $node->pos, $visited)) {
      if ($node->pos == $dest) {
        return $node;
      }
      $visited[(string) $node->pos] = true;
      $dirs = [$node->dir->turnLeft(), $node->dir->turnRight()];
      if ($node->straightLeft > 0) {
        array_push($dirs, $node->dir);
      }
      echo "$node->pos ($node->straightLeft left)" . PHP_EOL;
      foreach ($dirs as $dir) {
        $pos = $node->pos->add($dir);
        if (!array_key_exists((string) $pos, $visited) && $pos->inBounds($width, $height)) {
          $total = $node->total + intval($matrix[$pos->y][$pos->x]);
          $path = [...$node->path, $node];
          $straightLeft = (($dir == $node->dir) ? $node->straightLeft : $maxStraight) - 1;
          $next = new Node($pos, $dir, $path, $total, $straightLeft);
          // FIXME: A* seems to yield a different length on demo2 (362) with the
          // heuristic than without (363), why? Isn't the heuristic monotonic?
          $diff = $dest->sub($pos);
          $cost = $total;
          $c = $total - $node->total;
          $h = $cost - $total;
          // $cost = $total;
          $queue->insert($next, -$cost);
          echo "  {$dir->arrow()} $pos (cost $cost = $node->total + $c + $h)" . PHP_EOL;
        } else {
          echo "  {$dir->arrow()} $pos <SKIPPED>" . PHP_EOL;
        }
      }
    }
  }

  throw new Exception("Destination not found!");
}

function printUsage() {
  echo "Usage: day17 [--dump] <path to input>" . PHP_EOL;
}

$filePath = null;
$dump = false;

foreach (array_slice($argv, 1) as $arg) {
  if ($arg == '--dump') {
    $dump = true;
  } else if ($arg == '--help') {
    printUsage();
    exit(0);
  } else if (preg_match('/--\w+/', $arg)) {
    echo "Unrecognized argument: $arg" . PHP_EOL;
    exit(1);
  } else {
    $filePath = $arg;
  }
}

if ($filePath == null) {
  printUsage();
  exit(1);
}

$raw = trim(file_get_contents($filePath));
$input = preg_split('/\R/', $raw);

$destNode = shortestPath($input);
echo "Part 1: $destNode->total" . PHP_EOL;

if ($dump) {
  $formatted = $input;

  foreach ([...$destNode->path, $destNode] as $node) {
    $pos = $node->pos;
    $dir = $node->dir;
    $formatted[$pos->y][$pos->x] = $dir->arrow();
  }

  echo join(PHP_EOL, $formatted) . PHP_EOL;
}
