; Constants.
@.usage_fmt = private unnamed_addr constant [27 x i8] c"Usage: %s <path to input>\0A\00"
@.part1_fmt = private unnamed_addr constant [12 x i8] c"Part 1: %d\0A\00"
@.part2_fmt = private unnamed_addr constant [12 x i8] c"Part 2: %d\0A\00"

; External declarations
declare i32 @printf(ptr, ...)
declare ptr @fopen(ptr, ptr)

; Main
define i32 @main(i32 %argc, ptr %argv) {
  %no_args = icmp eq i32 %argc, 1
  br i1 %no_args, label %print_usage, label %print_parts

print_usage:
  %exe_name = load ptr, ptr %argv
  call i32 (ptr, ...) @printf(ptr @.usage_fmt, ptr %exe_name)
  ret i32 1

print_parts:
  call i32 (ptr, ...) @printf(ptr @.part1_fmt, i32 0)
  call i32 (ptr, ...) @printf(ptr @.part2_fmt, i32 0)
  br label %end

end:
  ret i32 0
}