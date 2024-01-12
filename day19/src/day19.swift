import Foundation
import RegexBuilder

let args = CommandLine.arguments
if args.count == 1 {
  print("Usage: \(args[0]) <path to input>")
  exit(1)
}

enum Operator: String {
  case lessThan = "<"
  case greaterThan = ">"
}

extension Operator {
  static let pattern = try! Regex<String>("[<>]")

  func apply(_ lhs: Int, _ rhs: Int) -> Bool {
    switch self {
    case .lessThan: return lhs < rhs
    case .greaterThan: return lhs > rhs
    }
  }
}

struct Condition {
  let category: String
  let `operator`: Operator
}

extension Condition {
  private static let categoryRef = Reference(String.self)
  private static let operatorRef = Reference(Operator.self)
  static let pattern = Regex {
    Capture(as: categoryRef) {
      try! Regex<String>("[a-z]")
    }
    Capture(as: operatorRef) {
      Operator.pattern
    } transform: { rawOperator in
      Operator(rawValue: String(rawOperator))!
    }
  }

  init?(rawValue: String) {
    guard let match = try? Self.pattern.wholeMatch(in: rawValue) else { return nil }
    self.init(
      category: match[Self.categoryRef],
      operator: match[Self.operatorRef]
    )
  }
}

enum Output {
  case reject
  case accept
  case workflow(String)
}

extension Output {
  static let pattern = try! Regex<String>("\\w+")

  init(rawValue: String) {
    switch rawValue {
    case "R": self = .reject
    case "A": self = .accept
    case _: self = .workflow(rawValue)
    }
  }
}

struct Rule {
  let condition: Condition?
  let output: Output
}

extension Rule {
  private static let conditionRef = Reference(Condition?.self)
  private static let outputRef = Reference(Output.self)
  static let pattern = Regex {
    Optionally {
      Capture(as: conditionRef) {
        Condition.pattern
      } transform: { rawCondition in
        Condition(rawValue: String(rawCondition))
      }
    }
    ":"
    Capture(as: outputRef) {
      Output.pattern
    } transform: { rawOutput in
      Output(rawValue: rawOutput)
    }
  }

  init?(rawValue: String) {
    guard let match = try? Self.pattern.wholeMatch(in: rawValue) else { return nil }
    self.init(
      condition: match[Self.conditionRef],
      output: match[Self.outputRef]
    )
  }
}

typealias Part = [String: Int]

let rawInput = try String(contentsOfFile: args[1])
print(rawInput)
