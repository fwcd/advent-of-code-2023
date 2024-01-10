<?php

if ($argc <= 1) {
  echo "Usage: day17 <path to input>" . PHP_EOL;
  exit(1);
}

$raw = file_get_contents($argv[1]);
$input = preg_split('/\R/', $raw);
var_dump($input);
