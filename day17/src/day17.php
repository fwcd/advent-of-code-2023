<?php

if ($argc <= 1) {
  echo "Usage: day17 <path to input>" . PHP_EOL;
  exit(1);
}

$input = $argv[1];
echo "Got $input" . PHP_EOL;
