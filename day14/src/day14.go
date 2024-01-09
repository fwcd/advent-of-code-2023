package main

import (
	"fmt"
	"os"
	"strings"
)

func transpose(matrix []string) []string {
	t := make([]string, len(matrix))
	for j := 0; j < len(matrix[0]); j++ {
		col := ""
		for _, row := range matrix {
			col += string(row[j])
		}
		t[j] = col
	}
	return t
}

func tilt(row string) string {
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

func tiltMatrix(matrix []string) []string {
	tilted := make([]string, len(matrix))
	for i, row := range matrix {
		tilted[i] = tilt(row)
	}
	return tilted
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

	fmt.Println(input)
	fmt.Println(transpose(input))
	fmt.Println(tiltMatrix(transpose(input)))
}
