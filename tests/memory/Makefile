#
# Compile tests.
#
DESTDIR		= /usr/local
LDESTDIR	= $(DESTDIR)/lib/pdp11
CC		= $(DESTDIR)/bin/pdp11-pcc
AS		= $(DESTDIR)/bin/pdp11-asm
LD		= $(DESTDIR)/bin/pdp11-ld
DISASM		= $(DESTDIR)/bin/pdp11-disasm
GHDL            = ghdl
CFLAGS		= -O
CPPFLAGS	=
LDFLAGS		= -a0
GHDLFLAGS       = -fexplicit --ieee=synopsys --workdir=../../verif

SOURCES		= memory.bin

all:		input.oct

install:

clean:
		rm -f *~ *.o *.log *.vcd.gz *.vcd memory.oct core

run:		input.oct
		cp -p $< memory.oct
		$(GHDL) -r $(GHDLFLAGS) bksim --vcdgz=output.vcd.gz --stop-time=260ms --ieee-asserts=disable 2>&1 | tee run.log
#		../../verif/bksim --vcdgz=output.vcd.gz --stop-time=260ms --ieee-asserts=disable 2>&1 | tee run.log
		@rm memory.oct

input.oct:      $(SOURCES)
		$(DISASM) -b -a0 $< > $@
