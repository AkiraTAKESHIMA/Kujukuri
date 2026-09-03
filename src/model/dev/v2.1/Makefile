TOPSRCDIR = ../../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(SPRINGMODDIR)
%.mod: %.f90 %.o
	@:

OBJS = \
mod_globals.o\
mod_sub.o\
mod_runge.o\
mod_dam.o\
mod_break.o\
mod_tecout.o\
mod_evp.o\
mod_config.o\
mod_section.o\
mod_river.o\
mod_slope.o\
mod_gw.o\
mod_rivslo.o\
mod_infilt.o\
mod_budget.o\
mod_dt_check.o\
mod_tsas.o\
main.o

TARGET = main.exe

all:
	$(MAKE) $(OBJS)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJS) $(SPRINGLIB)
	$(FC) $(FCFLAGS) -o $@ $^

clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET)


