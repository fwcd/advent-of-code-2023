; Constants.
@.usage_str = private unnamed_addr constant [27 x i8] c"Usage: %s <path to input>\0A\00"
@.part1_str = private unnamed_addr constant [9 x i8] c"Part 1: \00"
@.part2_str = private unnamed_addr constant [9 x i8] c"Part 2: \00"

; External declarations
declare i32 @printf(ptr, ...)
declare ptr @fopen(ptr, ptr)

; Main
define i32 @main(i32 %argc, ptr %argv) {
  %no_args = icmp eq i32 %argc, 1
  br i1 %no_args, label %print_usage, label %end

print_usage:
  %exe_name = load ptr, ptr %argv
  call i32 (ptr, ...) @printf(ptr @.usage_str, ptr %exe_name)
  br label %end

end:
  ret i32 0
}