; Constants.
@.row_prefix_fmt      = private unnamed_addr constant [13 x i8] c" %*[a-zA-Z]:\00"
@.integer_fmt         = private unnamed_addr constant [ 4 x i8] c"%ld\00"
@.usage_fmt           = private unnamed_addr constant [27 x i8] c"Usage: %s <path to input>\0A\00"
@.part1_fmt           = private unnamed_addr constant [13 x i8] c"Part 1: %ld\0A\00"
@.part2_fmt           = private unnamed_addr constant [13 x i8] c"Part 2: %ld\0A\00"
@.file_mode           = private unnamed_addr constant [ 2 x i8] c"r\00"
@.debug_fmt           = private unnamed_addr constant [12 x i8] c"DEBUG: %ld\0A\00"
@.arr_element_fmt     = private unnamed_addr constant [ 5 x i8] c"%ld \00"
@.newline             = private unnamed_addr constant [ 2 x i8] c"\0A\00"
@.counts_mismatch_err = private unnamed_addr constant [57 x i8] c"Time count of %ld does not match distance count of %ld!\0A\00"

; External declarations
declare i32 @printf(ptr, ...)
declare ptr @fopen(ptr, ptr)
declare i32 @fscanf(ptr, ptr, ...)
declare i32 @fclose(ptr)
declare double @log10(double)
declare double @ceil(double)
declare double @pow(double, double)

; NOTE: We (unsafely) assume that the buffer is large enough and are using
; `fscanf`, even though it is generally discouraged for most applications. Since
; we control both the input and the implementation, we can use it safely
; provided we are careful enough.
define i64 @parse_line(ptr %file, ptr %buf) {
  call i32 (ptr, ptr, ...) @fscanf(ptr %file, ptr @.row_prefix_fmt)
  %i_ptr = alloca i64
  store i64 0, ptr %i_ptr
  br label %parse_int

parse_int:
  %i = load i64, ptr %i_ptr
  %current_ptr = getelementptr ptr, ptr %buf, i64 %i

  %n_i32 = call i32 (ptr, ptr, ...) @fscanf(ptr %file, ptr @.integer_fmt, ptr %current_ptr)
  %n = sext i32 %n_i32 to i64

  %next_i = add i64 %i, 1
  store i64 %next_i, ptr %i_ptr
  %reached_end = icmp sle i64 %n, 0
  br i1 %reached_end, label %end, label %parse_int

end:
  %i.2 = load i64, ptr %i_ptr
  %i.3 = sub i64 %i.2, 1
  ret i64 %i.3
}

define i64 @compute_distance(i64 %total_time, i64 %hold_time) {
  %travel_time = sub i64 %total_time, %hold_time
  %distance = mul i64 %travel_time, %hold_time ; aka. speed
  ret i64 %distance
}

define i64 @count_successful_races(i64 %total_time, i64 %record_distance) {
  %hold_time_ptr = alloca i64
  store i64 0, ptr %hold_time_ptr

  %count_ptr = alloca i64
  store i64 0, ptr %count_ptr

  br label %count_race

count_race:
  %hold_time = load i64, ptr %hold_time_ptr

  %distance = call i64 (i64, i64) @compute_distance(i64 %total_time, i64 %hold_time)

  %beats_record = icmp sgt i64 %distance, %record_distance
  %count_delta = select i1 %beats_record, i64 1, i64 0
  %count = load i64, ptr %count_ptr
  %next_count = add i64 %count, %count_delta
  store i64 %next_count, ptr %count_ptr

  %next_hold_time = add i64 %hold_time, 1
  store i64 %next_hold_time, ptr %hold_time_ptr
  %reached_end = icmp sge i64 %next_hold_time, %total_time
  br i1 %reached_end, label %end, label %count_race

end:
  %final_count = load i64, ptr %count_ptr
  ret i64 %final_count
}

define i64 @compute_part1(ptr %times, ptr %distances, i64 %count) {
  %i_ptr = alloca i64
  store i64 0, ptr %i_ptr

  %part1_ptr = alloca i64
  store i64 1, ptr %part1_ptr

  br label %count_races

count_races:
  %i = load i64, ptr %i_ptr

  %total_time_ptr = getelementptr ptr, ptr %times, i64 %i
  %total_time = load i64, ptr %total_time_ptr

  %record_distance_ptr = getelementptr ptr, ptr %distances, i64 %i
  %record_distance = load i64, ptr %record_distance_ptr

  %races = call i64 (i64, i64) @count_successful_races(i64 %total_time, i64 %record_distance)

  %part1 = load i64, ptr %part1_ptr
  %next_part1 = mul i64 %part1, %races
  store i64 %next_part1, ptr %part1_ptr

  %next_i = add i64 %i, 1
  store i64 %next_i, ptr %i_ptr
  %reached_end = icmp sge i64 %next_i, %count
  br i1 %reached_end, label %end, label %count_races

end:
  %final_part1 = load i64, ptr %part1_ptr
  ret i64 %final_part1
}

define i64 @concat_integers(ptr %array, i64 %count) {
  %i_ptr = alloca i31
  store i64 0, ptr %i_ptr

  %result_ptr = alloca i64
  store i64 0, ptr %result_ptr

  br label %append_integer

append_integer:
  %i = load i64, ptr %i_ptr

  %value_ptr = getelementptr ptr, ptr %array, i64 %i
  %value = load i64, ptr %value_ptr

  %value_double = sitofp i64 %value to double
  %value_log = call double (double) @log10(double %value_double)
  %value_log_ceil = call double (double) @ceil(double %value_log)
  %power_of_ten = call double (double, double) @pow(double 10.0, double %value_log_ceil)
  %power_of_ten_int = fptosi double %power_of_ten to i64

  %result = load i64, ptr %result_ptr
  %next_result_upper = mul i64 %result, %power_of_ten_int
  %next_result = add i64 %next_result_upper, %value
  store i64 %next_result, ptr %result_ptr

  %next_i = add i64 %i, 1
  store i64 %next_i, ptr %i_ptr
  %reached_end = icmp sge i64 %next_i, %count
  br i1 %reached_end, label %end, label %append_integer

end:
  %final_result = load i64, ptr %result_ptr
  ret i64 %final_result
}

define i64 @compute_part2(ptr %times, ptr %distances, i64 %count) {
  %total_time = call i64 (ptr, i64) @concat_integers(ptr %times, i64 %count)
  %record_distance = call i64 (ptr, i64) @concat_integers(ptr %distances, i64 %count)
  call i32 (ptr, ...) @printf(ptr @.debug_fmt, i64 %total_time)
  call i32 (ptr, ...) @printf(ptr @.debug_fmt, i64 %record_distance)
  ret i64 0 ; TODO
}

define void @print_array(ptr %array, i64 %count) {
  %i_ptr = alloca i64
  store i64 0, ptr %i_ptr
  br label %print_element

print_element:
  %i = load i64, ptr %i_ptr
  %element_ptr = getelementptr ptr, ptr %array, i64 %i
  %value = load i64, ptr %element_ptr

  call i32 (ptr, ...) @printf(ptr @.arr_element_fmt, i64 %value)

  %next_i = add i64 %i, 1
  store i64 %next_i, ptr %i_ptr
  %reached_end = icmp sge i64 %next_i, %count
  br i1 %reached_end, label %end, label %print_element

end:
  call i32 (ptr, ...) @printf(ptr @.newline)
  ret void
}

define i64 @main(i64 %argc, ptr %argv) {
  %no_args = icmp eq i64 %argc, 1
  br i1 %no_args, label %print_usage, label %read_input

print_usage:
  %exe_name = load ptr, ptr %argv
  call i32 (ptr, ...) @printf(ptr @.usage_fmt, ptr %exe_name)
  ret i64 1

read_input:
  %input_path_ptr = getelementptr ptr, ptr %argv, i64 1
  %input_path = load ptr, ptr %input_path_ptr
  %file = call ptr (ptr, ptr) @fopen(ptr %input_path, ptr @.file_mode)

  %times = alloca i64, i64 16
  %time_count = call i64 (ptr, ptr) @parse_line(ptr %file, ptr %times)

  %distances = alloca i64, i64 16
  %distance_count = call i64 (ptr, ptr) @parse_line(ptr %file, ptr %distances)

  call i32 (ptr) @fclose(ptr %file)
  br label %print_inputs

print_inputs:
  call void (ptr, i64) @print_array(ptr %times, i64 %time_count)
  call void (ptr, i64) @print_array(ptr %distances, i64 %distance_count)
  br label %check_counts

check_counts:
  %counts_equal = icmp eq i64 %time_count, %distance_count
  br i1 %counts_equal, label %compute_parts, label %emit_counts_mismatch

emit_counts_mismatch:
  call i32 (ptr, ...) @printf(ptr @.counts_mismatch_err, i64 %time_count, i64 %distance_count)
  ret i64 1

compute_parts:
  %part1 = call i64 (ptr, ptr, i64) @compute_part1(ptr %times, ptr %distances, i64 %time_count)
  %part2 = call i64 (ptr, ptr, i64) @compute_part2(ptr %times, ptr %distances, i64 %time_count)
  br label %print_parts

print_parts:
  call i32 (ptr, ...) @printf(ptr @.part1_fmt, i64 %part1)
  call i32 (ptr, ...) @printf(ptr @.part2_fmt, i64 0)
  br label %end

end:
  ret i64 0
}
