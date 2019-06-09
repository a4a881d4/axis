library ieee;
use ieee.std_logic_1164.all;

entity zcpsm2dma is
	generic	(
		RAM_AWIDTH			:	natural
	);
	port(
		clk					:	in	std_logic;
		reset				:	in	std_logic;
	
		zcpsm_clk			:	in	std_logic;
		zcpsm_ce			:	in	std_logic;
		zcpsm_port_id		:	in	std_logic_vector(3 downto 0);
		zcpsm_write_strobe	:	in	std_logic;
		zcpsm_out_port		:	in	std_logic_vector(7 downto 0);
		zcpsm_read_strobe	:	in	std_logic;
		zcpsm_in_port		:	out	std_logic_vector(7 downto 0); 
		
		lastframe_flag		:	out std_logic;
		
		start				:	out	std_logic;
		length				:	out	std_logic_vector(15 downto 0);
		start_waddr			:	out	std_logic_vector(RAM_AWIDTH - 1 downto 0);
		start_raddr			:	out	std_logic_vector(RAM_AWIDTH - 1 downto 0);
		wstep				:	out	std_logic_vector(7 downto 0);
		rstep				:	out	std_logic_vector(7 downto 0);
		busy				:	in	std_logic
		);
end entity;

architecture behave of zcpsm2dma is

	component asyncwrite
	port(
		reset : in std_logic;
		async_clk : in std_logic;
		sync_clk : in std_logic;
		async_wren : in std_logic;
		trigger : in std_logic;
		sync_wren : out std_logic;
		over : out std_logic;
		flag : out std_logic);
	end component;
	
	constant PORT_DMA_START		:	std_logic_vector(3 downto 0)	:=	X"0";
	constant PORT_DMA_LENGTH_0	:	std_logic_vector(3 downto 0)	:=	X"1";
	constant PORT_DMA_LENGTH_1	:	std_logic_vector(3 downto 0)	:=	X"2";
	constant PORT_DMA_RADDR_0	:	std_logic_vector(3 downto 0)	:=	X"3";
	constant PORT_DMA_RADDR_1	:	std_logic_vector(3 downto 0)	:=	X"4";
	constant PORT_DMA_RADDR_2	:	std_logic_vector(3 downto 0)	:=	X"5";
	constant PORT_DMA_RADDR_3	:	std_logic_vector(3 downto 0)	:=	X"6";	
	constant PORT_DMA_WADDR_0	:	std_logic_vector(3 downto 0)	:=	X"7";
	constant PORT_DMA_WADDR_1	:	std_logic_vector(3 downto 0)	:=	X"8";
	constant PORT_DMA_WADDR_2	:	std_logic_vector(3 downto 0)	:=	X"9"; 
	constant PORT_DMA_WADDR_3	:	std_logic_vector(3 downto 0)	:=	X"A"; 	
	constant PORT_DMA_RSTEP		:	std_logic_vector(3 downto 0)	:=	X"B";
	constant PORT_DMA_WSTEP		:	std_logic_vector(3 downto 0)	:=	X"C";
	constant PORT_DMA_BUSY		:	std_logic_vector(3 downto 0)	:=	X"D"; 
	
	constant PORT_RX_LAST_FRAME	:	std_logic_vector(3 downto 0)	:=	X"E";
	
	signal zcpsm_we		:	std_logic;
	signal zcpsm_re		:	std_logic;
	signal zcpsm_addr	:	std_logic_vector(3 downto 0);
	signal start_en		:	std_logic;
	
begin
	
	zcpsm_we <= zcpsm_ce and zcpsm_write_strobe;
	zcpsm_re <= zcpsm_ce and zcpsm_read_strobe;
	zcpsm_addr <= zcpsm_port_id(3 downto 0);

	zcpsm_in_port <= "0000000" & busy when zcpsm_ce = '1' and zcpsm_addr = PORT_DMA_BUSY else (others => 'Z');
	
	WriteIO : process(zcpsm_clk, reset)
	begin
		if reset = '1' then
			wstep <= X"01";
			rstep <= X"01";
			length <= (others => '0');
			start_raddr <= (others => '0');
			start_waddr <= (others => '0');
		elsif rising_edge(zcpsm_clk) then			
			if zcpsm_we = '1' then
				case zcpsm_addr is
					when PORT_DMA_LENGTH_0	=>	length(7 downto 0) <= zcpsm_out_port;
					when PORT_DMA_LENGTH_1	=>	length(15 downto 8) <= zcpsm_out_port;
					when PORT_DMA_RADDR_0	=>	start_raddr(7 downto 0) <= zcpsm_out_port;
					when PORT_DMA_RADDR_1	=>	start_raddr(15 downto 8) <= zcpsm_out_port;
					when PORT_DMA_RADDR_2	=>	start_raddr(23 downto 16) <= zcpsm_out_port;
					when PORT_DMA_RADDR_3	=>	start_raddr(31 downto 24) <= zcpsm_out_port;
					when PORT_DMA_WADDR_0	=>	start_waddr(7 downto 0) <= zcpsm_out_port;
					when PORT_DMA_WADDR_1	=>	start_waddr(15 downto 8) <= zcpsm_out_port;
					when PORT_DMA_WADDR_2	=>	start_waddr(23 downto 16) <= zcpsm_out_port;
					when PORT_DMA_WADDR_3	=>	start_waddr(31 downto 24) <= zcpsm_out_port;
					when PORT_DMA_RSTEP		=>	rstep <= zcpsm_out_port;
					when PORT_DMA_WSTEP		=>	wstep <= zcpsm_out_port;
					when others => null;
				end case;
			end if;
		end if;
	end process;
	
	u_start : asyncwrite
	port map(
		reset => reset,
		async_clk => zcpsm_clk,
		sync_clk => clk,
		async_wren => start_en,
		trigger => '1',
		sync_wren => start,
		over => open,
		flag => open
		);
	
	start_en <= '1' when zcpsm_we = '1' and zcpsm_addr = PORT_DMA_START else '0';
	lastframe_flag <= '1' when zcpsm_we = '1' and zcpsm_addr = PORT_RX_LAST_FRAME else '0';
		
end behave;
