; Constants.
@.row_prefix_fmt  = private unnamed_addr constant [13 x i8] c" %*[a-zA-Z]:\00"
@.integer_fmt     = private unnamed_addr constant [ 3 x i8] c"%d\00"
@.usage_fmt       = private unnamed_addr constant [27 x i8] c"Usage: %s <path to input>\0A\00"
@.part1_fmt       = private unnamed_addr constant [12 x i8] c"Part 1: %d\0A\00"
@.part2_fmt       = private unnamed_addr constant [12 x i8] c"Part 2: %d\0A\00"
@.file_mode       = private unnamed_addr constant [ 2 x i8] c"r\00"
@.debug_fmt       = private unnamed_addr constant [11 x i8] c"DEBUG: %d\0A\00"
@.arr_element_fmt = private unnamed_addr constant [ 4 x i8] c"%d \00"
@.newline         = private unnamed_addr constant [ 2 x i8] c"\0A\00"

; External declarations
declare i32 @printf(ptr, ...)
declare ptr @fopen(ptr, ptr)
declare i32 @fscanf(ptr, ptr, ...)
declare i32 @fclose(ptr)

; NOTE: We (unsafely) assume that the buffer is large enough and are using
; `fscanf`, even though it is generally discouraged for most applications. Since
; we control both the input and the implementation, we can use it safely
; provided we are careful enough.
define i32 @parse_line(ptr %file, ptr %buf) {
  call i32 (ptr, ptr, ...) @fscanf(ptr %file, ptr @.row_prefix_fmt)
  %i_ptr = alloca i32
  store i32 0, ptr %i_ptr
  br label %parse_int

parse_int:
  %i = load i32, ptr %i_ptr
  %current_ptr = getelementptr ptr, ptr %buf, i32 %i

  %n = call i32 (ptr, ptr, ...) @fscanf(ptr %file, ptr @.integer_fmt, ptr %current_ptr)

  %next_i = add i32 %i, 1
  store i32 %next_i, ptr %i_ptr
  %reached_end = icmp sle i32 %n, 0
  br i1 %reached_end, label %end, label %parse_int

end:
  %i.2 = load i32, ptr %i_ptr
  %i.3 = sub i32 %i.2, 1
  ret i32 %i.3
}

define void @print_array(ptr %array, i32 %count) {
  %i_ptr = alloca i32
  store i32 0, ptr %i_ptr
  br label %print_element

print_element:
  %i = load i32, ptr %i_ptr
  %element_ptr = getelementptr ptr, ptr %array, i32 %i
  %value = load i32, ptr %element_ptr

  call i32 (ptr, ...) @printf(ptr @.arr_element_fmt, i32 %value)

  %next_i = add i32 %i, 1
  store i32 %next_i, ptr %i_ptr
  %reached_end = icmp sge i32 %next_i, %count
  br i1 %reached_end, label %end, label %print_element

end:
  call i32 (ptr, ...) @printf(ptr @.newline)
  ret void
}

define i32 @main(i32 %argc, ptr %argv) {
  %no_args = icmp eq i32 %argc, 1
  br i1 %no_args, label %print_usage, label %read_input

print_usage:
  %exe_name = load ptr, ptr %argv
  call i32 (ptr, ...) @printf(ptr @.usage_fmt, ptr %exe_name)
  ret i32 1

read_input:
  %input_path_ptr = getelementptr ptr, ptr %argv, i64 1
  %input_path = load ptr, ptr %input_path_ptr
  %file = call ptr (ptr, ptr) @fopen(ptr %input_path, ptr @.file_mode)

  %times = alloca i32, i32 16
  %time_count = call i32 (ptr, ptr) @parse_line(ptr %file, ptr %times)
  call void (ptr, i32) @print_array(ptr %times, i32 %time_count)

  %distances = alloca i32, i32 16
  %distance_count = call i32 (ptr, ptr) @parse_line(ptr %file, ptr %distances)
  call void (ptr, i32) @print_array(ptr %distances, i32 %distance_count)

  call i32 (ptr) @fclose(ptr %file)
  br label %print_parts

print_parts:
  call i32 (ptr, ...) @printf(ptr @.part1_fmt, i32 0)
  call i32 (ptr, ...) @printf(ptr @.part2_fmt, i32 0)
  br label %end

end:
  ret i32 0
}
