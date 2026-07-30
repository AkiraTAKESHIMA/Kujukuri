TOPSRCDIR = ../../..
include $(TOPSRCDIR)/Mkinclude

.PHONY: all
all:
	$(MAKE) -C nkf
	$(MAKE) -C io

.PHONY: clean
clean:
	$(MAKE) -C nkf clean
	$(MAKE) -C io clean
