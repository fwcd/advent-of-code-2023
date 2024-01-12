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
  static let pattern = try! Regex("[<>]", as: String.self)

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
      try! Regex("[a-z]", as: String.self)
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
  static let pattern = try! Regex("\\w+", as: String.self)

  init(rawValue: String) {
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
    Capture(as: conditionRef) {
      Optionally {
        Condition.pattern
      }
    } transform: { rawCondition in
      Condition(rawValue: String(rawCondition))
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

struct Workflow {
  let name: String
  let rules: [Rule]
}

extension Workflow {
  private static let nameRef = Reference(String.self)
  private static let rulesRef = Reference([Rule].self)

  static let pattern = Regex {
    Capture(as: nameRef) {
      try! Regex("\\w+", as: String.self)
    }
    "{"
    Capture(as: rulesRef) {
      try! Regex("[^}]+", as: String.self)
    } transform: { rawRules in
      rawRules.split(separator: ",").map { Rule(rawValue: String($0))! }
    }
    "}"
  }

  init?(rawValue: String) {
    guard let match = try? Self.pattern.wholeMatch(in: rawValue) else { return nil }
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
      try! Regex("[^}]+", as: String.self)
    } transform: { rawRules in
      Dictionary(uniqueKeysWithValues: rawRules.split(separator: ",").map {
        let split = $0.split(separator: "=")
        return (String(split[0]), Int(split[1])!)
      })
    }
    "}"
  }

  init?(rawValue: String) {
    guard let match = try? Self.pattern.wholeMatch(in: rawValue) else { return nil }
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
  init(rawValue: String) {
    let chunks = rawValue.split(separator: "\n\n").map { $0.split(separator: "\n") }
    self.init(
      workflows: chunks[0].map { Workflow(rawValue: String($0))! },
      parts: chunks[1].map { Part(rawValue: String($0))! }
    )
  }
}

let rawInput = try String(contentsOfFile: args[1])
let input = Input(rawValue: rawInput)
print(input)
