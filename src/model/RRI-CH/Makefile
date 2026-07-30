TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(MODDIR)
%.mod: %.f90 %.o
	@:

OBJS = \
def_const.o\
def_type.o\
mod_param.o\
mod_util.o\
mod_mesh.o\
mod_set.o\
mod_river.o\
main.o

TARGET = main

all:
	$(MAKE) $(OBJS)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJS) $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET)


