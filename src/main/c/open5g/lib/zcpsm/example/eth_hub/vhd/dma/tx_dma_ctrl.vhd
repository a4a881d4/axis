library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity dma_ctrl is
	generic(
		DWIDTH				:	natural;  			-- 8
		RD_CYCLE			:	natural;		    -- 1
		RD_DELAY			:	natural;			-- 1   
		RAM_AWIDTH          :   natural
		);
	port(
		clk					:	in	std_logic;
		reset				:	in	std_logic;
		
		ena					:	in	std_logic;
		start				:	in	std_logic;
		length				:	in	std_logic_vector(15 downto 0);
		start_waddr			:	in	std_logic_vector(RAM_AWIDTH - 1 downto 0);
--		start_raddr			:	in	std_logic_vector(23 downto 0);
		start_raddr			:	in	std_logic_vector(RAM_AWIDTH - 1 downto 0);

		wstep				:	in	std_logic_vector(7 downto 0);
		rstep				:	in	std_logic_vector(7 downto 0);
		busy				:	out	std_logic;
		
--		raddr				:	out	std_logic_vector(23 downto 0); 
		raddr				:	out	std_logic_vector(RAM_AWIDTH - 1 downto 0); 

		rdata				:	in	std_logic_vector(DWIDTH - 1 downto 0);
		wren				:	out	std_logic;
		waddr				:	out	std_logic_vector(RAM_AWIDTH - 1 downto 0);
		wdata				:	out	std_logic_vector(DWIDTH - 1 downto 0)
		);
end entity;

architecture behave of dma_ctrl is

	component shiftreg
	generic(
		width : INTEGER;
		depth : INTEGER);
	port(
		clk : in std_logic;
		ce : in std_logic;
		D : in std_logic_vector((width-1) downto 0);
		Q : out std_logic_vector((width-1) downto 0);
		S : out std_logic_vector((width-1) downto 0));
	end component;
	
	signal length_reg		:	std_logic_vector(15 downto 0);
	signal start_waddr_reg	:	std_logic_vector(RAM_AWIDTH - 1 downto 0);
--	signal start_raddr_reg	:	std_logic_vector(23 downto 0);	 
	signal start_raddr_reg	:	std_logic_vector(RAM_AWIDTH - 1 downto 0);

	signal wstep_reg		:	std_logic_vector(7 downto 0);
	signal rstep_reg		:	std_logic_vector(7 downto 0);
	
	signal busy_buf			:	std_logic;
	signal cycle_cnt		:	std_logic_vector(7 downto 0);
	signal cnt				:	std_logic_vector(15 downto 0);
	signal rden_buf			:	std_logic;
--	signal raddr_buf		:	std_logic_vector(23 downto 0);
	signal raddr_buf		:	std_logic_vector(RAM_AWIDTH - 1 downto 0);

	signal wren_buf			:	std_logic;
	signal waddr_buf		:	std_logic_vector(RAM_AWIDTH - 1 downto 0);
	signal rd_start			:	std_logic;
	signal wr_start			:	std_logic;	
	signal rd_ctrl			:	std_logic_vector(1 downto 0);
	signal wr_ctrl			:	std_logic_vector(1 downto 0);
	
begin
	
	--	DMA Status
	
	process(clk, reset)
	begin
		if reset = '1' then
			length_reg <= (others => '0');
			start_waddr_reg <= (others => '0');
			start_raddr_reg <= (others => '0');
			wstep_reg <= (others => '0');
			rstep_reg <= (others => '0');
		elsif rising_edge(clk) then
			if start = '1' then
				length_reg <= length;
				start_waddr_reg <= start_waddr;
				start_raddr_reg <= start_raddr;
				wstep_reg <= wstep;
				rstep_reg <= rstep;
			end if;
		end if;
	end process;
	
	process(clk, reset)
	begin
		if reset = '1' then
			busy_buf <= '0';
		elsif rising_edge(clk) then
			if start = '1' then
				busy_buf <= '1';
			elsif cnt = length_reg and cycle_cnt = RD_CYCLE - 1 then
				busy_buf <= '0';
			end if;
		end if;
	end process;
	
	busy <= busy_buf;
	
	process(clk, reset)
	begin
		if reset = '1' then
			cycle_cnt <= (others => '0');
		elsif rising_edge(clk) then
			if busy_buf = '1' and ena = '1' then
				if cycle_cnt = RD_CYCLE - 1 then
					cycle_cnt <= (others => '0');
				else
					cycle_cnt <= cycle_cnt + 1;
				end if;
			else
				cycle_cnt <= (others => '0');
			end if;
		end if;
	end process;
	
	process(clk, reset)
	begin
		if reset = '1' then
			cnt <= X"0000";
		elsif rising_edge(clk) then
			if start = '1' then
				cnt <= X"0001";
			elsif busy_buf = '1' and ena = '1' and cycle_cnt = RD_CYCLE - 1 then
				cnt <= cnt + 1;
			end if;
		end if;
	end process;
	
	-- Read Ctrl
	
	process(clk, reset)
	begin
		if reset = '1' then
			rden_buf <= '0';
		elsif rising_edge(clk) then
			if busy_buf = '1' and ena = '1' and cycle_cnt = RD_CYCLE - 1 and cnt < length_reg then
				rden_buf <= '1';
			else
				rden_buf <= '0';
			end if;
		end if;
	end process;
	
--	process(clk, reset)
--	begin
--		if reset = '1' then
--			raddr_buf <= (others => '0');
--		elsif rising_edge(clk) then
--			if rd_start = '1' then
--				raddr_buf <= start_raddr_reg;
--			elsif rden_buf = '1' then
--				raddr_buf <= raddr_buf + rstep_reg;
--			end if;
--		end if;
--	end process;

	process(clk, reset)
	begin
		if reset = '1' then
			raddr_buf <= (others => '0');
		elsif rising_edge(clk) then
			if rd_start = '1' then
				raddr_buf <= start_raddr_reg;
			elsif rden_buf = '1' then
				raddr_buf <= raddr_buf + rstep_reg;
			end if;
		end if;
	end process;	
	
	raddr <= raddr_buf;
	
	-- Write Ctrl
	
	u_wr_ctrl : shiftreg
	generic map(
		width => 2,
		depth => RD_DELAY + RD_CYCLE -- 100M RD_DELAY ??? a4a881d4
		)
	port map(
		clk => clk,
		ce => '1',
		D => rd_ctrl,
		Q => wr_ctrl,
		S => open
		);
		
	rd_ctrl(0) <= rden_buf;
	rd_ctrl(1) <= rd_start;
	
	wren_buf <= wr_ctrl(0);
	wr_start <= wr_ctrl(1);
	
	process(clk, reset)
	begin
		if reset = '1' then
			waddr_buf <= (others => '0');
		elsif rising_edge(clk) then
			if wr_start = '1' then
				waddr_buf <= start_waddr_reg;
			elsif wren_buf = '1' then
				waddr_buf <= waddr_buf + wstep_reg;
			end if;
		end if;
	end process;
	
	process(clk)
	begin
		if rising_edge(clk) then
			rd_start <= start;
			wren <= wr_start or wren_buf;
		end if;
	end process;
	
	waddr <= waddr_buf;	
	wdata <= rdata;
	
end behave;
