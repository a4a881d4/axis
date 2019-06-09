---------------------------------------------------------------------------------------------------
--
-- Title       : ethrx_zcpsm
-- Design      : eth_new
-- Author      : a4a881d4
-- Company     : a4a881d4
--
---------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity ethrx_zcpsm is
	port(
		reset				:	in	std_logic;
		clk					:	in	std_logic;
		
		port_id				:	out	std_logic_vector(7 downto 0);
		write_strobe		:	out	std_logic;
		out_port			:	out	std_logic_vector(7 downto 0);
		read_strobe			:	out	std_logic;
		in_port				:	in	std_logic_vector(7 downto 0)
		
		);
end ethrx_zcpsm;

--}} End of automatically maintained section

architecture behavior of ethrx_zcpsm is

	component zcpsm
		Port (     
			address 	: 	out std_logic_vector(11 downto 0);
			instruction :	in std_logic_vector(17 downto 0);
			port_id 	:	out std_logic_vector(7 downto 0);
			write_strobe :	out std_logic;
			out_port 	:	out std_logic_vector(7 downto 0);
			read_strobe :	out std_logic;
			in_port 	:	in std_logic_vector(7 downto 0);
			interrupt 	:	in std_logic;
			reset 		:	in std_logic;
			clk 		:	in std_logic);
	end component;

	component ethrxrom_romonly
	port(
		addrb : in std_logic_vector(11 downto 0);
		clkb : in std_logic;
		dob : out std_logic_vector(17 downto 0));
	end component;	

	signal address : std_logic_vector(11 downto 0);
	signal instruction : std_logic_vector(17 downto 0);	 

begin

	u_rx_zcpsm : zcpsm
	port map(
		address => address,
		instruction => instruction,
		port_id => port_id,
		write_strobe => write_strobe,
		out_port => out_port,
		read_strobe => read_strobe,
		in_port => in_port,
		interrupt => '0',
		reset => reset,
		clk => clk
		);

	u_rx_rom : ethrxrom_romonly
	port map(
		addrb => address,
		clkb => clk,
		dob => instruction
		); 		

end behavior;
