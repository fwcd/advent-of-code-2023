import Foundation

let args = CommandLine.arguments
if args.count == 1 {
  print("Usage: \(args[0]) <path to input>")
  exit(1)
}

print("Hello world!")
