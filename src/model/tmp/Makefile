TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(SPRINGMODDIR)
%.mod: %.f90 %.o
	@:

OBJS = \
def_const.o\
def_type.o\
mod_mesh.o\
mod_util.o\
mod_config.o\
mod_rk45.o\
mod_river.o\
mod_forcing.o\
mod_driver.o\
main.o

TARGET = main.exe

all:
	$(MAKE) $(OBJS)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJS) $(SPRINGLIB)
	$(FC) $(FCFLAGS) -o $@ $^

clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET)


