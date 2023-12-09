#import <Foundation/Foundation.h>

@interface Node : NSObject

@property(nonatomic, retain) NSString *name;
@property(nonatomic, retain) NSString *left;
@property(nonatomic, retain) NSString *right;

@end

@interface Input : NSObject 

@property(nonatomic, retain) NSString *instructions;
@property(nonatomic, retain) NSDictionary<NSString *, Node *> *nodes;

@end

@implementation Node

- (id)initWithRawInput:(NSString *)input {
  NSRegularExpression *regex = [[NSRegularExpression alloc] initWithPattern:@"(\\w+) = \\((\\w+), (\\w+)\\)" options:0 error:nil];
  NSTextCheckingResult *match = [regex firstMatchInString:input options:0 range:NSMakeRange(0, input.length)];

  self.name = [input substringWithRange:[match rangeAtIndex:1]];
  self.left = [input substringWithRange:[match rangeAtIndex:2]];
  self.right = [input substringWithRange:[match rangeAtIndex:3]];

  return self;
}

@end

@implementation Input

- (id)initWithRawInput:(NSString *)input {
  NSArray<NSString *> *lines = [input componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
  self.instructions = lines[0];

  NSMutableDictionary<NSString *, Node *> *nodes = [[NSMutableDictionary alloc] init];
  for (NSString *line in [lines subarrayWithRange:NSMakeRange(2, lines.count - 2)]) {
    Node *node = [[Node alloc] initWithRawInput:line];
    nodes[node.name] = node;
  }
  self.nodes = nodes;

  return self;
}

- (NSMutableArray<NSString *> *)namesOfNodesWithSuffix:(NSString *)suffix {
  NSMutableArray<NSString *> *result = [[NSMutableArray alloc] init];

  for (NSString *name in self.nodes.keyEnumerator) {
    if ([name hasSuffix:suffix]) {
      [result addObject:name];
    }
  }

  return result;
}

- (NSString *)stepOnceFrom:(NSString *)name withInstructionIndex:(int)i {
  Node *node = self.nodes[name];
  switch ([self.instructions characterAtIndex:i]) {
  case 'L': return node.left;
  case 'R': return node.right;
  default: return nil;
  }
}

- (NSString *)step:(int)times timesFrom:(NSString *)name withInstructionIndex:(int)i {
  NSString *current = name;
  for (int j = 0; j < (times % self.instructions.length); j++) {
    current = [self stepOnceFrom:current withInstructionIndex:(i + j) % self.instructions.length];
  }
  return current;
}

- (int)naiveStepsFromSuffix:(NSString *)startSuffix toSuffix:(NSString *)goalSuffix withStepSize:(int)stepSize fromInstructionIndex:(int)startInstructionIndex {
  int steps = 0;
  NSMutableArray<NSString *> *current = [self namesOfNodesWithSuffix:startSuffix];

  BOOL (^reachedGoal)() = ^{
    for (NSString *name in current) {
      if (![name hasSuffix:goalSuffix]) {
        return NO;
      }
    }
    return YES;
  };

  while (!reachedGoal()) {
    for (int i = 0; i < current.count; i++) {
      current[i] = [self step:stepSize timesFrom:current[i] withInstructionIndex:(startInstructionIndex + steps) % self.instructions.length];
    }
    steps += stepSize;
  }

  return steps;
}

- (int)stepsFromSuffix:(NSString *)startSuffix toSuffix:(NSString *)goalSuffix {
  int baseSteps = 0;
  int cycleLength = 1;

  // Idea: Advance each node to a goal (e.g. ...Z) and keep it 'intact', i.e. on a goal node.
  // 
  //              ...A  ...A  ...A ...
  //                 |     |     |
  //              ...Z     ? \   ?
  //                 |     |  |
  //  cycle length n |     |  |
  //                 |     |  |
  //              ...Z     ?  | step in multiples of n
  //                 |     |  |
  //                 |     |  |
  //                 |     |  |
  //              ...Z  ...Z /

  for (NSString *name in [self namesOfNodesWithSuffix:startSuffix]) {
    NSString *base = [self step:baseSteps timesFrom:name withInstructionIndex:0];
    baseSteps += [self naiveStepsFromSuffix:base toSuffix:goalSuffix withStepSize:cycleLength fromInstructionIndex:baseSteps];

    NSString *cycleBase = [self step:(baseSteps + 1) timesFrom:name withInstructionIndex:0];
    cycleLength = 1 + [self naiveStepsFromSuffix:cycleBase toSuffix:goalSuffix withStepSize:cycleLength fromInstructionIndex:baseSteps + 1];
    baseSteps += cycleLength;
  }

  return baseSteps - cycleLength;
}

@end

int main(void) {
  NSArray<NSString *> *arguments = NSProcessInfo.processInfo.arguments;
  if (arguments.count < 2) {
    fprintf(stderr, "Usage: %s <input>\n", arguments[0].UTF8String);
    return 1;
  }

  NSString *inputPath = arguments[1];
  NSString *rawInput = [NSString stringWithContentsOfFile:inputPath encoding:NSUTF8StringEncoding error:nil];
  Input *input = [[Input alloc] initWithRawInput:rawInput];

  int part1 = [input naiveStepsFromSuffix:@"AAA" toSuffix:@"ZZZ" withStepSize:1 fromInstructionIndex:0];
  NSLog(@"Part 1: %d", part1);

  int part2 = [input stepsFromSuffix:@"A" toSuffix:@"Z"];
  NSLog(@"Part 2: %d", part2);

  return 0;
}
