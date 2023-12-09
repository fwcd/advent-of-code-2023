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

- (NSMutableSet<NSString *> *)namesOfNodesWithSuffix:(NSString *)suffix {
  NSMutableSet<NSString *> *result = [[NSMutableSet alloc] init];

  for (NSString *name in self.nodes.keyEnumerator) {
    if ([name hasSuffix:suffix]) {
      [result addObject:name];
    }
  }

  return result;
}

- (NSString *)nextNodeFrom:(NSString *)name forInstruction:(char)instruction {
  Node *node = self.nodes[name];
  switch (instruction) {
  case 'L': return node.left;
  case 'R': return node.right;
  default: return nil;
  }
}

- (NSString *)nodeAfterCycleFrom:(NSString *)name {
  NSString *current = name;
  for (int i = 0; i < self.instructions.length; i++) {
    current = [self nextNodeFrom:current forInstruction:[self.instructions characterAtIndex:i]];
  }
  return current;
}

- (NSMutableDictionary<NSString *, NSString *> *)cycleMapping {
  NSMutableDictionary<NSString *, NSString *> *result = [[NSMutableDictionary alloc] init];

  for (NSString *name in self.nodes.keyEnumerator) {
    result[name] = [self nodeAfterCycleFrom:name];
  }

  return result;
}

- (int) stepsFromSuffix:(NSString *)startSuffix toSuffix:(NSString *)goalSuffix {
  int steps = 0;
  NSMutableSet<NSString *> *current = [self namesOfNodesWithSuffix:startSuffix];

  NSLog(@"%@", self.cycleMapping);

  BOOL (^reachedGoal)() = ^{
    for (NSString *name in current) {
      if (![name hasSuffix:goalSuffix]) {
        return NO;
      }
    }
    return YES;
  };

  while (!reachedGoal()) {
    // We mutate the existing set instead of assigning a new one since the block
    // captures this particular instance and workarounds generally seem to be
    // more complex.
    NSSet<NSString *> *currentCopy = [[NSSet alloc] initWithSet:current];
    [current removeAllObjects];
    for (NSString *name in currentCopy) {
      char instruction = [self.instructions characterAtIndex:(steps % self.instructions.length)];
      [current addObject:[self nextNodeFrom:name forInstruction:instruction]];
    }
    steps++;
  }

  return steps;
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

  int part1 = [input stepsFromSuffix:@"AAA" toSuffix:@"ZZZ"];
  NSLog(@"Part 1: %d", part1);

  int part2 = [input stepsFromSuffix:@"A" toSuffix:@"Z"];
  NSLog(@"Part 2: %d", part2);

  return 0;
}
