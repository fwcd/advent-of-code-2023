import Foundation
import RegexBuilder

let args = CommandLine.arguments
if args.count == 1 {
  print("Usage: \(args[0]) <path to input>")
  exit(1)
}

enum ParseError: Error {
  case noMatch(String)
}

enum Operator: String {
  case lessThan = "<"
  case greaterThan = ">"
}

extension Operator {
  static let pattern = /[<>]/

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
      /[a-z]/
    } transform: {
      String($0)
    }
    Capture(as: operatorRef) {
      Operator.pattern
    } transform: { rawOperator in
      Operator(rawValue: String(rawOperator))!
    }
  }

  init(rawValue: Substring) throws {
    guard let match = try Self.pattern.wholeMatch(in: rawValue) else { throw ParseError.noMatch("Could not match condition") }
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
  static let pattern = /\w+/

  init(rawValue: Substring) {
    switch rawValue {
    case "R": self = .reject
    case "A": self = .accept
    case _: self = .workflow(String(rawValue))
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
        try Condition(rawValue: rawCondition)
      }
      ":"
    }
    Capture(as: outputRef) {
      Output.pattern
    } transform: { rawOutput in
      Output(rawValue: rawOutput)
    }
  }

  init(rawValue: Substring) throws {
    guard let match = try Self.pattern.wholeMatch(in: rawValue) else { throw ParseError.noMatch("Could not match rule") }
    self.init(
      condition: match[Self.conditionRef],
      output: match[Self.outputRef]
    )
  }
}

struct Workflow {
  let name: String
  let rules: [Rule]
}

extension Workflow {
  private static let nameRef = Reference(String.self)
  private static let rulesRef = Reference([Rule].self)

  static let pattern = Regex {
    Capture(as: nameRef) {
      /\w+/
    } transform: {
      String($0)
    }
    "{"
    Capture(as: rulesRef) {
      /[^}]+/
    } transform: { rawRules in
      try rawRules.split(separator: ",").map { try Rule(rawValue: $0) }
    }
    "}"
  }

  init(rawValue: Substring) throws {
    guard let match = try Self.pattern.wholeMatch(in: rawValue) else { throw ParseError.noMatch("Could not match workflow") }
    self.init(
      name: match[Self.nameRef],
      rules: match[Self.rulesRef]
    )
  }
}

struct Part {
  let values: [String: Int]
}

extension Part {
  private static let valuesRef = Reference([String: Int].self)

  static let pattern = Regex {
    "{"
    Capture(as: valuesRef) {
      /[^}]+/
    } transform: { rawRules in
      Dictionary(uniqueKeysWithValues: rawRules.split(separator: ",").map {
        let split = $0.split(separator: "=")
        return (String(split[0]), Int(split[1])!)
      })
    }
    "}"
  }

  init(rawValue: Substring) throws {
    guard let match = try Self.pattern.wholeMatch(in: rawValue) else { throw ParseError.noMatch("Could not match part") }
    self.init(
      values: match[Self.valuesRef]
    )
  }
}

struct Input {
  let workflows: [Workflow]
  let parts: [Part]
}

extension Input {
  init(rawValue: Substring) throws {
    let chunks = rawValue.split(separator: "\n\n").map { $0.split(separator: "\n") }
    self.init(
      workflows: try chunks[0].map { try Workflow(rawValue: $0) },
      parts: try chunks[1].map { try Part(rawValue: $0) }
    )
  }
}

let rawInput = try String(contentsOfFile: args[1])
let input = try Input(rawValue: rawInput[...])
print(input)
