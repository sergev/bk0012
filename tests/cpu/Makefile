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

TESTS		= instructions.oct

all:		$(TESTS)

install:

clean:
		rm -f *~ *.o *.oct *.log core

run:		$(TESTS)
		cp -p $< memory.oct
		$(GHDL) -r $(GHDLFLAGS) bksim --stop-time=1ms --ieee-asserts=disable 2>&1 | tee run.log
		@rm memory.oct

.SUFFIXES:      .oct

.S.oct:
		$(CC) $(CPPFLAGS) -c $< -o $*.o
		$(LD) $(LDFLAGS) $*.o -o $*.bin
		$(DISASM) -a0 $*.bin > $@
		@rm $*.bin

.s.oct:
		$(AS) $(ASFLAGS) $< -o $*.o
		$(LD) $(LDFLAGS) $*.o -o $*.bin
		$(DISASM) -a0 $*.bin > $@
		@rm $*.bin
