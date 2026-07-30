TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES:
.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(LIBDIR_CONST)
%.mod: %.f90 %.o
	@:

TARGET = lib_proj.a

OBJ_F = \
  lib_proj_sinusoidal.o\
  lib_proj_lambertconformalconic.o

OBJ_ALL = $(OBJ_F)

MOD_ALL = $(OBJ_ALL:.o=.mod)

.PHONY: all
all: $(TARGET)

$(TARGET): $(OBJ_ALL)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod $(TARGET)
