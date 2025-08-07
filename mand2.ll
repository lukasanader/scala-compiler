declare i32 @printf(i8*, ...)

@.str_nl = private constant [2 x i8] c"\0A\00"
@.str_star = private constant [2 x i8] c"*\00"
@.str_space = private constant [2 x i8] c" \00"
@.str_int = private constant [3 x i8] c"%d\00"
@.str_c = private constant [3 x i8] c"%c\00"

define void @new_line() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_nl, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_star() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_star, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_space() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_space, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_int(i32 %x) {
  %t0 = getelementptr [3 x i8], [3 x i8]* @.str_int, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
  ret void
}

define void @print_char(i32 %x) {
  %t0 = getelementptr [3 x i8], [3 x i8]* @.str_c, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
  ret void
}

define void @skip() #0 {
  ret void
}

; END OF BUILT-IN FUNCTIONS (prelude)
@Ymin = global double -1.3
@Ymax = global double 1.3
@Ystep = global double 0.05
@Xmin = global double -2.1
@Xmax = global double 1.1
@Xstep = global double 0.02
@Maxiters = global i32 1000
define void @m_iter (i32 %m, double %x, double %y, double %zr, double %zi) {
   %tmp_1 = load i32, i32* @Maxiters
   %tmp_0 = icmp sle i32  %tmp_1, %m
   br i1 %tmp_0, label %if_branch_16, label %else_branch_17

if_branch_16:
   call void @print_char (i32 32)
   ret void

else_branch_17:
   %tmp_4 = fmul double  %zi, %zi
   %tmp_5 = fmul double  %zr, %zr
   %tmp_3 = fadd double  %tmp_4, %tmp_5
   %tmp_2 = fcmp ole double  4.0, %tmp_3
   br i1 %tmp_2, label %if_branch_18, label %else_branch_19

if_branch_18:
   %tmp_7 = srem i32  %m, 10
   %tmp_6 = add i32  48, %tmp_7
   call void @print_char (i32 %tmp_6)
   ret void

else_branch_19:
   %tmp_8 = add i32  %m, 1
   %tmp_11 = fmul double  %zr, %zr
   %tmp_12 = fmul double  %zi, %zi
   %tmp_10 = fsub double  %tmp_11, %tmp_12
   %tmp_9 = fadd double  %x, %tmp_10
   %tmp_15 = fmul double  %zr, %zi
   %tmp_14 = fmul double  2.0, %tmp_15
   %tmp_13 = fadd double  %tmp_14, %y
   call void @m_iter (i32 %tmp_8, double %x, double %y, double %tmp_9, double %tmp_13)
   ret void
}

define void @x_iter (double %x, double %y) {
   %tmp_21 = load double, double* @Xmax
   %tmp_20 = fcmp ole double  %x, %tmp_21
   br i1 %tmp_20, label %if_branch_24, label %else_branch_25

if_branch_24:
   call void @m_iter (i32 0, double %x, double %y, double 0.0, double 0.0)
   %tmp_23 = load double, double* @Xstep
   %tmp_22 = fadd double  %x, %tmp_23
   call void @x_iter (double %tmp_22, double %y)
   ret void

else_branch_25:
   call void @skip ()
   ret void
}

define void @y_iter (double %y) {
   %tmp_27 = load double, double* @Ymax
   %tmp_26 = fcmp ole double  %y, %tmp_27
   br i1 %tmp_26, label %if_branch_31, label %else_branch_32

if_branch_31:
   %tmp_28 = load double, double* @Xmin
   call void @x_iter (double %tmp_28, double %y)
   call void @print_char (i32 10)
   %tmp_30 = load double, double* @Ystep
   %tmp_29 = fadd double  %y, %tmp_30
   call void @y_iter (double %tmp_29)
   ret void

else_branch_32:
   call void @skip ()
   ret void
}

define i32 @main() {
   %tmp_33 = load double, double* @Ymin
   call void @y_iter (double %tmp_33)
   ret i32 0
}
