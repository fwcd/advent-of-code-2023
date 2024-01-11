<?php

if ($argc <= 1) {
  echo "Usage: day17 <path to input>" . PHP_EOL;
  exit(1);
}

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
    $queue->insert(new Node($start, $start, [], 0, $maxStraight - 1), 0);
  }

  $width = strlen($matrix[0]);
  $height = count($matrix);
  $dest = new Vec2($width - 1, $height - 1);

  while ($queue->valid()) {
    $node = $queue->extract();
    if ($node->pos == $dest) {
      return $node;
    }
    $visited[(string) $node->pos] = true;
    $dirs = [$node->dir->turnLeft(), $node->dir->turnRight()];
    if ($node->straightLeft > 0) {
      array_push($dirs, $node->dir);
    }
    echo "$node->pos" . PHP_EOL;
    foreach ($dirs as $dir) {
      $pos = $node->pos->add($dir);
      if (!array_key_exists((string) $pos, $visited) && $pos->inBounds($width, $height)) {
        $path = [...$node->path, $node];
        $cost = intval($matrix[$pos->y][$pos->x]);
        $total = $node->total + $cost;
        $straightLeft = (($dir == $node->dir) ? $node->straightLeft : $maxStraight) - 1;
        $next = new Node($pos, $dir, $path, $total, $straightLeft);
        // DEBUG
        echo "  {$dir->arrow()} $pos (cost $cost)" . PHP_EOL;
        $queue->insert($next, -$total);
      }
    }
  }

  throw new Exception("Destination not found!");
}

$raw = trim(file_get_contents($argv[1]));
$input = preg_split('/\R/', $raw);

$destNode = shortestPath($input);
echo "Part 1: $destNode->total" . PHP_EOL;

foreach ([...$destNode->path, $destNode] as $node) {
  $pos = $node->pos;
  $dir = $node->dir;
  $input[$pos->y][$pos->x] = $dir->arrow();
}

echo join(PHP_EOL, $input) . PHP_EOL;
