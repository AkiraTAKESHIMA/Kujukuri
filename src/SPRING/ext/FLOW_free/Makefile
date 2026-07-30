TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES:
.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(MODDIR) -I$(CMNDIR1)


MODS = \
	def_consts.o\
	mod_utils.o

BASE1 = make_gcmmap
BASE2 = modify_hires
BASE3 = calc_uparea
BASE4 = const_network
BASE5 = define_catchment
BASE6 = visual_check
BASE7 = gcm_rivermap
BASE8 = set_map
BASE9 = calc_inpmat

TARGET1 = $(BASE1).exe
TARGET2 = $(BASE2).exe
TARGET3 = $(BASE3).exe
TARGET4 = $(BASE4).exe
TARGET5 = $(BASE5).exe
TARGET6 = $(BASE6).exe
TARGET7 = $(BASE7).exe
TARGET8 = $(BASE8).exe
TARGET9 = $(BASE9).exe

TARGETS = \
	$(TARGET1)\
	$(TARGET2)\
	$(TARGET3)\
	$(TARGET4)\
	$(TARGET5)\
	$(TARGET6)\
	$(TARGET7)\
	$(TARGET8)\
	$(TARGET9)

.PHONY: all
all:
	make $(MODS)
	make $(TARGETS)

.PHONY: mods
mods:
	make $(MODS)

.PHONY: clean
clean:
	rm -f *~ *.o *.mod *.exe


$(TARGET1): $(BASE1).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET2): $(BASE2).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET3): $(BASE3).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET4): $(BASE4).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET5): $(BASE5).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET6): $(BASE6).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET7): $(BASE7).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET8): $(BASE8).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET9): $(BASE9).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

