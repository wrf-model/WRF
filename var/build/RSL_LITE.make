# RSL_LITE

RSL_LITE_OBJS = c_code.o buf_for_proc.o rsl_malloc.o rsl_bcast.o \
   task_for_point.o period.o swap.o cycle.o f_pack.o f_xpose.o

librsl_lite.a : $(RSL_LITE_OBJS)
	$(RM) librsl_lite.a
	$(AR) librsl_lite.a $(RSL_LITE_OBJS)
