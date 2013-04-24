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

SOURCES		= instructions.S

all:		input.oct

install:

clean:
		rm -f *~ *.o *.log *.vcd.gz *.vcd core

run:		input.oct
		cp -p $< memory.oct
		$(GHDL) -r $(GHDLFLAGS) bksim --vcdgz=output.vcd.gz --stop-time=1200us --ieee-asserts=disable 2>&1 | tee run.log
#		../../verif/bksim --vcdgz=output.vcd.gz --stop-time=1200us --ieee-asserts=disable 2>&1 | tee run.log
		@rm memory.oct

input.oct:      $(SOURCES)
		$(CC) $(CPPFLAGS) -c $< -o input.o
		$(LD) $(LDFLAGS) input.o -o input.bin
		$(DISASM) -a0 input.bin > $@
		@rm input.bin input.o

.s.oct:
		$(AS) $(ASFLAGS) $< -o $*.o
		$(LD) $(LDFLAGS) $*.o -o $*.bin
		$(DISASM) -a0 $*.bin > $@
		@rm $*.bin
