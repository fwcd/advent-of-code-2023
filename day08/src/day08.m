#import <Foundation/Foundation.h>

@interface Node : NSObject

@property(nonatomic, retain) NSString *name;
@property(nonatomic, retain) NSString *left;
@property(nonatomic, retain) NSString *right;

- (id) initWithRawInput:(NSString *)input;

@end

@interface Input : NSObject 

@property(nonatomic, retain) NSString *instructions;
@property(nonatomic, retain) NSDictionary<NSString *, Node *> *nodes;

- (id) initWithRawInput:(NSString *)input;

@end

@implementation Node

- (id) initWithRawInput:(NSString *)input {
  NSRegularExpression *regex = [[NSRegularExpression alloc] initWithPattern:@"(\\w+) = \\((\\w+), (\\w+)\\)" options:0 error:nil];
  NSTextCheckingResult *match = [regex firstMatchInString:input options:0 range:NSMakeRange(0, input.length)];

  self.name = [input substringWithRange:[match rangeAtIndex:1]];
  self.left = [input substringWithRange:[match rangeAtIndex:2]];
  self.right = [input substringWithRange:[match rangeAtIndex:3]];

  return self;
}

@end

@implementation Input

- (id) initWithRawInput:(NSString *)input {
  NSArray<NSString *> *lines = [input componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
  self.instructions = lines[0];

  NSMutableDictionary<NSString *, Node *> *nodes;
  for (NSString *line in [lines subarrayWithRange:NSMakeRange(2, lines.count - 2)]) {
    Node *node = [[Node alloc] initWithRawInput:line];
    nodes[node.name] = node;
  }
  self.nodes = nodes;

  return self;
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

  return 0;
}
