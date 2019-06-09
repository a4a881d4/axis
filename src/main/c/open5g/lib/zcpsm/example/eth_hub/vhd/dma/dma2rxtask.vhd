---------------------------------------------------------------------------------------------------
--
-- Title       : dma2rxtask
-- Design      : eth_new
-- Author      : lihf
-- Company     : wireless
--
---------------------------------------------------------------------------------------------------
--
-- File        : dma2rxtask.vhd
-- Generated   : Fri Sep  8 11:59:12 2006
-- From        : interface description file
-- By          : Itf2Vhdl ver. 1.20
--
---------------------------------------------------------------------------------------------------
--
-- Description : 
--
---------------------------------------------------------------------------------------------------

--{{ Section below this comment is automatically maintained
--   and may be overwritten
--{entity {dma2rxtask} architecture {arch_dma2rxtask}}

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity dma2rxtask is 
	port(			
		reset			:	in	std_logic;
		zcpsm_clk		:	in	std_logic;
		busy			:	in std_logic;
		lastframe		:	in	std_logic;
		rxtask_wr_block	:	out	std_logic
		
	);
end dma2rxtask;

--}} End of automatically maintained section

architecture arch_dma2rxtask of dma2rxtask is

	signal lastframe_flag	:	std_logic;
	signal task_wr_block_reg:	std_logic;
	signal busy_flag		:	std_logic;

begin	
	

	rxtask_wr_block <=  task_wr_block_reg;
	
	process(reset, zcpsm_clk)
	begin
		if reset = '1' then
			busy_flag <= '0';
		elsif rising_edge(zcpsm_clk) then
			if busy = '1' then
				busy_flag <= '1';
			else
				busy_flag <= '0';
			end if;
		end if;	
	end process;
	
	process(reset, zcpsm_clk)
	begin
		if reset = '1' then
			lastframe_flag <= '0';
		elsif rising_edge(zcpsm_clk) then
			if lastframe = '1' then
				lastframe_flag <= '1';
			elsif lastframe_flag = '1' and busy = '0' then
				lastframe_flag <= '0';
			end if;
		end if;	  
	end process;					
	
	process(reset, zcpsm_clk)
	begin
		if reset = '1' then
			task_wr_block_reg <= '0';
		elsif rising_edge(zcpsm_clk) then
			if lastframe_flag = '1' and busy = '0' then
--				if busy = '0' then
					task_wr_block_reg <= '1';
--				end if;	
			elsif  task_wr_block_reg = '1' then
				task_wr_block_reg <= '0';
			end if;
		end if;
	end process;
	
		
	 -- enter your statements here --

end arch_dma2rxtask;
