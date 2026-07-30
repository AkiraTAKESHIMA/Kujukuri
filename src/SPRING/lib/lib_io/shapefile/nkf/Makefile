TOPSRCDIR = ../../../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES:
.SUFFIXES: .f90 .F90 .c
%.o: %.f90
	$(FC) $(FCFLAGS) -c $<
%.o: %.c
	$(CC) $(CCFLAGS) -c $<
%.mod: %.f90 %.o
	@:


OBJS = \
	libnkf.o

.PHONY: all
all:
	$(MAKE) $(OBJS)
	$(INSTALL) $(OBJS) ../..

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod


