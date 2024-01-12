import Foundation

let args = CommandLine.arguments
if args.count == 1 {
  print("Usage: \(args[0]) <path to input>")
  exit(1)
}

let rawInput = try String(contentsOfFile: args[1])
print(rawInput)
