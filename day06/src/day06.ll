; Constants.
@.row_prefix_fmt      = private unnamed_addr constant [13 x i8] c" %*[a-zA-Z]:\00"
@.integer_fmt         = private unnamed_addr constant [ 3 x i8] c"%d\00"
@.usage_fmt           = private unnamed_addr constant [27 x i8] c"Usage: %s <path to input>\0A\00"
@.part1_fmt           = private unnamed_addr constant [12 x i8] c"Part 1: %d\0A\00"
@.part2_fmt           = private unnamed_addr constant [12 x i8] c"Part 2: %d\0A\00"
@.file_mode           = private unnamed_addr constant [ 2 x i8] c"r\00"
@.debug_fmt           = private unnamed_addr constant [11 x i8] c"DEBUG: %d\0A\00"
@.arr_element_fmt     = private unnamed_addr constant [ 4 x i8] c"%d \00"
@.newline             = private unnamed_addr constant [ 2 x i8] c"\0A\00"
@.counts_mismatch_err = private unnamed_addr constant [55 x i8] c"Time count of %d does not match distance count of %d!\0A\00"

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

define i32 @compute_distance(i32 %total_time, i32 %hold_time) {
  %travel_time = sub i32 %total_time, %hold_time
  %distance = mul i32 %travel_time, %hold_time ; aka. speed
  ret i32 %distance
}

define i32 @count_successful_races(i32 %total_time, i32 %record_distance) {
  %hold_time_ptr = alloca i32
  %count_ptr = alloca i32
  br label %count_race

count_race:
  %hold_time = load i32, ptr %hold_time_ptr

  %distance = call i32 (i32, i32) @compute_distance(i32 %total_time, i32 %hold_time)

  %beats_record = icmp sgt i32 %distance, %record_distance
  %count_delta = sext i1 %beats_record to i32
  %count = load i32, ptr %count_ptr
  %next_count = add i32 %count, %count_delta
  store i32 %next_count, ptr %count_ptr

  %next_hold_time = add i32 %hold_time, 1
  store i32 %next_hold_time, ptr %hold_time_ptr
  %reached_end = icmp sge i32 %next_hold_time, %total_time
  br i1 %reached_end, label %end, label %count_race

end:
  %final_count = load i32, ptr %count_ptr
  ret i32 %final_count
}

define i32 @compute_part1(ptr %times, ptr %distances, i32 %count) {
  %i_ptr = alloca i32
  %part1_ptr = alloca i32
  store i32 1, ptr %part1_ptr
  br label %count_race

count_race:
  %i = load i32, ptr %i_ptr

  %total_time_ptr = getelementptr ptr, ptr %times, i32 %i
  %total_time = load i32, ptr %total_time_ptr

  %record_distance_ptr = getelementptr ptr, ptr %distances, i32 %i
  %record_distance = load i32, ptr %record_distance_ptr

  %races = call i32 (i32, i32) @count_successful_races(i32 %total_time, i32 %record_distance)

  %part1 = load i32, ptr %part1_ptr
  %next_part1 = mul i32 %part1, %races
  store i32 %next_part1, ptr %part1_ptr

  %next_i = add i32 %i, 1
  store i32 %next_i, ptr %i_ptr
  %reached_end = icmp sge i32 %next_i, %count
  br i1 %reached_end, label %end, label %count_race

end:
  %final_part1 = load i32, ptr %part1_ptr
  ret i32 %final_part1
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

  %distances = alloca i32, i32 16
  %distance_count = call i32 (ptr, ptr) @parse_line(ptr %file, ptr %distances)

  call i32 (ptr) @fclose(ptr %file)
  br label %print_inputs

print_inputs:
  call void (ptr, i32) @print_array(ptr %times, i32 %time_count)
  call void (ptr, i32) @print_array(ptr %distances, i32 %distance_count)
  br label %check_counts

check_counts:
  %counts_equal = icmp eq i32 %time_count, %distance_count
  br i1 %counts_equal, label %compute_parts, label %emit_counts_mismatch

emit_counts_mismatch:
  call i32 (ptr, ...) @printf(ptr @.counts_mismatch_err, i32 %time_count, i32 %distance_count)
  ret i32 1

compute_parts:
  %part1 = call i32 (ptr, ptr, i32) @compute_part1(ptr %times, ptr %distances, i32 %time_count)
  br label %print_parts

print_parts:
  call i32 (ptr, ...) @printf(ptr @.part1_fmt, i32 %part1)
  call i32 (ptr, ...) @printf(ptr @.part2_fmt, i32 0)
  br label %end

end:
  ret i32 0
}
