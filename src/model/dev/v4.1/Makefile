TOPSRCDIR = ../../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(SPRINGMODDIR)
%.mod: %.f90 %.o
	@:

OBJS = \
def_const.o\
def_static.o\
def_runge.o\
mod_base.o\
mod_section.o\
mod_forcing.o\
mod_river.o\
mod_slope.o\
mod_rivslo.o\
mod_driver.o\
mod_config.o\
main.o

TARGET = main.exe

all:
	$(MAKE) $(OBJS)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJS) $(SPRINGLIB)
	$(FC) $(FCFLAGS) -o $@ $^

clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET)


