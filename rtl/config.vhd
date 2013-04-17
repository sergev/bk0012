library IEEE;
use IEEE.STD_LOGIC_1164.all;

package config is
	constant DEVICE_FAMILY: string := "Cyclone III";
	constant DCACHE_DATA_WIDTH: natural:= 16;	
	constant DCACHE_ASSOCIATIVITY: natural:= 2;
	constant DCACHE_SIZE: natural := 8192;
	constant DCACHE_LINE_SIZE: natural := 8;
	constant ICACHE_DATA_WIDTH: natural:= 32;
	constant ICACHE_ASSOCIATIVITY: natural:= 2;
	constant ICACHE_SIZE: natural := 8192;
	constant ICACHE_LINE_SIZE: natural := 8;	
end package config;

