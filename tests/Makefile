all:            po1.oct po4.oct

po1.oct:        po1.bin
		pdp11-disasm -b -a0 $< > $@

po4.oct:        po4.bin
		pdp11-disasm -b -a0 $< > $@
