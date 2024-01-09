package main

import (
	"fmt"
	"os"
	"strings"
)

func matricesEqual(a []string, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i, _ := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func transpose(matrix []string) []string {
	t := make([]string, len(matrix))
	for j, _ := range matrix[0] {
		col := ""
		for _, row := range matrix {
			col += string(row[j])
		}
		t[j] = col
	}
	return t
}

func reverse(s string) string {
	r := ""
	for i, _ := range s {
		r += string(s[len(s)-1-i])
	}
	return r
}

func flip(matrix []string) []string {
	flipped := make([]string, len(matrix))
	for i, row := range matrix {
		flipped[i] = reverse(row)
	}
	return flipped
}

func tiltRowWest(row string) string {
	tilted := ""
	floor := ""
	for _, c := range row {
		switch c {
		case '.':
			floor += string(c)
		case 'O':
			tilted += string(c)
		case '#':
			tilted += floor + string(c)
			floor = ""
		default:
			tilted += string(c)
		}
	}
	tilted += floor
	return tilted
}

func tiltWest(matrix []string) []string {
	tilted := make([]string, len(matrix))
	for i, row := range matrix {
		tilted[i] = tiltRowWest(row)
	}
	return tilted
}

func tiltEast(matrix []string) []string {
	return flip(tiltWest(flip(matrix)))
}

func tiltNorth(matrix []string) []string {
	return transpose(tiltWest(transpose(matrix)))
}

func tiltSouth(matrix []string) []string {
	return transpose(tiltEast(transpose(matrix)))
}

func tiltCycle(matrix []string) []string {
	return tiltEast(tiltSouth(tiltWest(tiltNorth(matrix))))
}

func tiltCycles(matrix []string, n int) []string {
	cycled := matrix
	visited := make([][]string, 0)
	for i := 1; i < n; i++ {
		cycled = tiltCycle(cycled)
		for _, previous := range visited {
			if matricesEqual(previous, cycled) {
				fmt.Println("Period has length", len(visited))
				return visited[n%len(visited)]
			}
		}
		visited = append(visited, cycled)
	}
	return cycled
}

func rowLoadWest(row string) int {
	load := 0
	for i, c := range row {
		if c == 'O' {
			load += len(row) - i
		}
	}
	return load
}

func totalLoadWest(matrix []string) int {
	load := 0
	for _, row := range matrix {
		load += rowLoadWest(row)
	}
	return load
}

func totalLoadNorth(matrix []string) int {
	return totalLoadWest(transpose(matrix))
}

func main() {
	args := os.Args
	if len(args) <= 1 {
		fmt.Println("Usage:", args[0], "<path to input>")
		os.Exit(1)
	}

	data, err := os.ReadFile(args[1])
	if err != nil {
		panic(err)
	}

	input := strings.Split(string(data), "\n")
	for input[len(input)-1] == "" {
		input = input[:len(input)-1]
	}

	fmt.Println("Part 1:", totalLoadNorth(tiltNorth(input)))
	fmt.Println("Part 2:", totalLoadNorth(tiltCycles(input, 1000000000)))
}
