PROG            = bksim
OBJ             = resetgen.o \
                  bus_monitor.o \
                  memory.o \
                  bksim.o \
                  icache_stub.o \
                  dcache_stub.o \
                  ibuffer.o \
                  gpr.o \
                  alu.o \
                  dummy_debugger.o \
                  moonshiner11.o

GHDL            = ghdl
GHDLFLAGS       = -fexplicit --ieee=synopsys
#MODELSIM        = /usr/local/modelsim
MODELSIM        = /mips/proj/tools/questa/10.2_1/questasim/linux

vpath %.vhd ../rtl

all:            $(PROG)

clean:
	rm -rf *.o *.lst *.vcd work-obj93.cf work transcript vsim.wlf INCA_libs ncsim.key ncvhdl.log ncelab.log ncsim.log $(PROG)

run:
	$(GHDL) -r $(GHDLFLAGS) $(PROG) --vcd=$(PROG).vcd --stop-time=140ms --ieee-asserts=disable 2>&1 | tee run.log

runrun:
	$(GHDL) -r $(GHDLFLAGS) $(PROG) --ieee-asserts=disable

$(PROG):        $(OBJ)
	$(GHDL) -e $(GHDLFLAGS) $@

view:
	gtkwave $(PROG).vcd databus.gtkw

# Using Mentor ModelSim
modelsim:
	$(MODELSIM)/vlib work
	$(MODELSIM)/vcom ../rtl/def.vhd
	$(MODELSIM)/vcom ../rtl/iface.vhd
	$(MODELSIM)/vcom ../rtl/alu.vhd
	$(MODELSIM)/vcom ../rtl/config.vhd
	$(MODELSIM)/vcom ../rtl/dcache_stub.vhd
	$(MODELSIM)/vcom ../rtl/gpr.vhd
	$(MODELSIM)/vcom dummy_debugger.vhd
	$(MODELSIM)/vcom ../rtl/ibuffer.vhd
	$(MODELSIM)/vcom ../rtl/icache_stub.vhd
	$(MODELSIM)/vcom ../rtl/moonshiner11.vhd
	$(MODELSIM)/vcom ../rtl/resetgen.vhd
	$(MODELSIM)/vcom textio.vhd
	$(MODELSIM)/vcom pdp11_disasm.vhd
	$(MODELSIM)/vcom bksim.vhd
	$(MODELSIM)/vcom bus_monitor.vhd
	$(MODELSIM)/vcom memory.vhd
	$(MODELSIM)/vsim -c -do 'run 100000us; quit' bksim

# Using Cadence UIS
ius:
	ncvhdl -v93 ../rtl/def.vhd
	ncvhdl -v93 ../rtl/iface.vhd
	ncvhdl -v93 ../rtl/alu.vhd
	ncvhdl -v93 ../rtl/config.vhd
	ncvhdl -v93 ../rtl/dcache_stub.vhd
	ncvhdl -v93 ../rtl/gpr.vhd
	ncvhdl -v93 dummy_debugger.vhd
	ncvhdl -v93 ../rtl/ibuffer.vhd
	ncvhdl -v93 ../rtl/icache_stub.vhd
	ncvhdl -v93 ../rtl/moonshiner11.vhd
	ncvhdl -v93 ../rtl/resetgen.vhd
	ncvhdl -v93 textio.vhd
	ncvhdl -v93 pdp11_disasm.vhd
	ncvhdl -v93 bksim.vhd
	ncvhdl -v93 bus_monitor.vhd
	ncvhdl -v93 memory.vhd
	ncelab bksim
	ncsim bksim

.vhd.o:
	$(GHDL) -a $(GHDLFLAGS) $<

.SUFFIXES: .vhd

moonshiner11.o: icache_stub.o dcache_stub.o ibuffer.o gpr.o alu.o dummy_debugger.o
dcache_stub.o gpr.o dummy_debugger.o ibuffer.o icache_stub.o iface.o moonshiner11.o pdp11_disasm.o: def.o
alu.o dcache_stub.o gpr.o dummy_debugger.o ibuffer.o icache_stub.o moonshiner11.o bus_monitor.o bksim.o: iface.o
dcache_stub.o icache_stub.o moonshiner11.o: config.o
bus_monitor.o: textio.o pdp11_disasm.o
