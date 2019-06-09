library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;  

use work.Eth_TestSig_Cfg.all;

entity g_ethrx_input is
	generic(
		HEAD_AWIDTH		:	natural		:=	5;		-- 接收队列地址宽度 2^5 = 32 字节
		BUFF_AWIDTH		:	natural		:=	16 		-- BUFF16位地址线
		);
	port( 
--		test_crc		:	out std_logic_vector(3 downto 0);
	
		clk				:	in	std_logic;	    -- FPGA时钟
		reset			:	in	std_logic;		
		rxclk			:	in	std_logic; 	-- GMII输出时钟
		rxd				:	in	std_logic_vector(7 downto 0);	 -- 并口输入数据，8bit为单位（byte）
		rxdv			:	in	std_logic;	          -- 在RXDV='1'的情况下检测到"5..5D"，表示一个以太包的开始
		
		recvtime 		:	out std_logic_vector(31 downto 0);
		recvtime_valid	:	out	std_logic; 
		localtime_locked:	out std_logic;
		
		head_wren		:	out	std_logic;		
		head_waddr		:	out	std_logic_vector(HEAD_AWIDTH - 1 downto 0);	 --每一个包从0开始递增计数
		head_wdata		:	out	std_logic_vector(7 downto 0);
		head_wr_block	:	out	std_logic;			--  指示CRC结果，'1'表示正确，'0'表示不正确	  
		buff_wren		:	out	std_logic;
		buff_waddr		:	out	std_logic_vector(BUFF_AWIDTH - 1 downto 0);	  
		buff_wdata		:	out	std_logic_vector(31 downto 0)	 --  以太包数据按字节写出，写地址从上次写地址的末尾递增，包含以太包头 
		
		);
end g_ethrx_input;

architecture arch_ethrx_input of g_ethrx_input is
	
	component fifo_async_almost_full
	generic(
		DEPTH : NATURAL;
		AWIDTH : NATURAL;
		DWIDTH : NATURAL;
		RAM_TYPE : STRING);
	port(
		reset : in std_logic;
		clr : in std_logic;
		clka : in std_logic;
		wea : in std_logic;
		dia : in std_logic_vector((DWIDTH-1) downto 0);
		clkb : in std_logic;
		rdb : in std_logic;
		dob : out std_logic_vector((DWIDTH-1) downto 0);
		empty : out std_logic;	
		almost_full	: out std_logic;
		full : out std_logic;
		dn : out std_logic_vector((AWIDTH-1) downto 0));
	end component;
	for all: fifo_async_almost_full use entity WORK.fifo_async_almost_full(fast_read);

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
	
--	component crcrom
--	port(
--		addr : in std_logic_vector(3 downto 0);
--		dout : out std_logic_vector(31 downto 0));
--	end component;
	
	component crc8_blkrom
		port(	  
			clk	: in std_logic;
			addr : in std_logic_vector(7 downto 0);
			dout : out std_logic_vector(31 downto 0));
	end component;		
	
	component ASYNCWRITE
		port(
			reset: in std_logic;
			async_clk: in std_logic;
			sync_clk: in std_logic;
			async_wren: in std_logic;
			trigger: in std_logic;
			sync_wren: out std_logic;
			over: out std_logic;
			flag: out std_logic
			);
	end component;	
	
	constant INFO_LENGTH	:	natural		:=	4;	  		
	constant HEAD_LENGTH	:	natural		:=	2 ** HEAD_AWIDTH - INFO_LENGTH;
	
	signal rxdv_buf			:	std_logic;
	signal rxd_buf			:	std_logic_vector(7 downto 0);
--	signal d_ext			:	std_logic_vector(8 downto 0);
--	signal rxdv_int			:	std_logic;
--	signal rxd_int			:	std_logic_vector(7 downto 0);
--	signal d_int			:	std_logic_vector(8 downto 0);	
	signal ce				:	std_logic;
--	signal rd_ena			:	std_logic;
--	signal empty			:	std_logic;

	signal RxFIFO_clr		:	std_logic;
	signal RxFIFO_wea		:	std_logic;
	signal RxFIFO_dia		:	std_logic_vector(31 downto 0);
	signal RxFIFO_rdb		:	std_logic;
	signal RxFIFO_dob		:	std_logic_vector(31 downto 0);
	signal RxFIFO_empty		:	std_logic;
	signal RxFIFO_not_empty		:	std_logic;
	signal RxFIFO_clr_int	:	std_logic;

	signal rx_state			:	std_logic_vector(1 downto 0);
	signal rx_state_d1		:	std_logic_vector(1 downto 0);
	signal rx_state_int		:	std_logic_vector(1 downto 0);
	signal rx_state_int_d1	:	std_logic_vector(1 downto 0);
	signal byte_cnt			:	std_logic_vector(11 downto 0);	    
	signal byte_cnt_int		:	std_logic_vector(11 downto 0);
	signal byte_cnt_int_d1	:	std_logic_vector(11 downto 0);
	signal rx_end_ext		:	std_logic;
	signal rx_end_int		:	std_logic;
	
	signal rxd_buf_d1		:	std_logic_vector(7 downto 0);
	signal rxd_buf_d2		:	std_logic_vector(7 downto 0);
	signal rxd_buf_d3		:	std_logic_vector(7 downto 0);
	signal rxd_buf_d4		:	std_logic_vector(7 downto 0);
	signal dword_data_ext	:	std_logic_vector(31 downto 0);
	signal dword_data_int	:	std_logic_vector(31 downto 0);
	signal dword_data_buf	:	std_logic_vector(23 downto 0);
	signal byte_data_int	:	std_logic_vector(7 downto 0);
	
	signal buff_wren_buf	:	std_logic;
	signal buff_waddr_buf	:	std_logic_vector(BUFF_AWIDTH - 1 downto 0);
	
	signal crc_din			:	std_logic_vector(7 downto 0);
	signal crc_reg			:	std_logic_vector(31 downto 0);
	signal crc_reg_d1		:	std_logic_vector(31 downto 0);
	signal crcrom_addr		:	std_logic_vector(7 downto 0);
	signal crcrom_dout		:	std_logic_vector(31 downto 0);
	signal crc_flag			:	std_logic;
	
	signal info_cnt			:	integer range 0 to INFO_LENGTH;
	signal info_ena			:	std_logic;
	
	signal start_addr		:	std_logic_vector(15 downto 0);
	signal length			:	std_logic_vector(15 downto 0);
	
	signal head_wren_buf	:	std_logic;
	signal head_waddr_buf	:	std_logic_vector(HEAD_AWIDTH - 1 downto 0);
	signal head_wr_block_buf:	std_logic;
	
	signal rxclk_temp		:	std_logic;	
	signal localtime_locked_reg:	std_logic;
	
begin	  
	
 	
--	process(clk)
--	begin											 
--		if rising_edge(clk) then  
--			if info_ena = '1' then
--				test_crc(0) <=  crc_flag;    
--				test_crc(1) <=  crc_reg(2);  
--				test_crc(2) <=  crc_reg(4);    
--				test_crc(3) <=  crc_reg(8);      				
--			end if;
--		end if;
--	end process;
		
		
		
	p_mii_din : process(rxclk, reset)				   -- MII->ETH-RX 输入数据缓存
	begin																  
		if reset = '1' then
			rxdv_buf <= '0';
			rxd_buf <= (others=>'0');
		elsif rising_edge(rxclk) then
--		if falling_edge(rxclk) then
			rxdv_buf <= rxdv;
			rxd_buf <= rxd;
		end if;
	end process;
	
--	rxclk_temp	<= not rxclk;
	
--	u_din_sync : fifo_async
--	generic map(
--		DEPTH => 4,
--		AWIDTH => 2,
--		DWIDTH => 9,
--		RAM_TYPE => "DIS_RAM")
--	port map(
--		reset => reset,
--		clr => '0',
--		clka => rxclk, 
----		clka => rxclk_temp,
--		wea => '1',
--		dia => d_ext,
--		clkb => clk,
--		rdb => rd_ena,
--		dob => d_int,
--		empty => empty,
--		full => open,
--		dn => open
--		);
--			
--	d_ext <= rxdv_buf & rxd_buf;				
--	rxdv_int <= d_int(8);
--	rxd_int <= d_int(7 downto 0);
--	rd_ena <= not empty;
--	
--	p_ce : process(clk)
--	begin
--		if rising_edge(clk) then
--			ce <= rd_ena;
--		end if;
--	end process;
	
	------------------------------------------------------------------------------
	
--	p_state_machine : process(clk, reset)
--	begin
--		if reset = '1' then
--			rx_state <= (others => '0');   
--			rx_state_d1 <= (others => '0');
--		elsif rising_edge(clk) then
--			if ce = '1' then
--				case rx_state is
--					when "00" =>
--						if rxdv_int = '1' and rxd_int = "01010101" then
--							rx_state <= "01";
--						else
--							rx_state <= "00";
--						end if;
--					when "01" =>
--						if rxdv_int = '1' then	   			
--							if rxd_int = "11010101" then
--								rx_state <= "10";
--							elsif rxd_int = "01010101" then
--								rx_state <= "01";
--							else
--								rx_state <= "00";
--							end if;
--						else
--							rx_state <= "00";
--						end if;
--					when "10" =>
--						if rxdv_int = '1' then
--							rx_state <= "11";
--						end if;
--					when "11" =>
--						if rxdv_int = '0' then
--							rx_state <= "00";
--						end if;
--					when others	=>
--						NULL;
--				end case;
--			end if;	   
--			rx_state_d1 <= rx_state;
--		end if;
--	end process;  
	
	p_state_machine : process(rxclk, reset)
	begin
		if reset = '1' then
			rx_state <= (others => '0');   
			rx_state_d1 <= (others => '0');
		elsif rising_edge(rxclk) then
--			if ce = '1' then
				case rx_state is
					when "00" =>
						if rxdv_buf = '1' and rxd_buf = "01010101" then
							rx_state <= "01";
						else
							rx_state <= "00";
						end if;
					when "01" =>
						if rxdv_buf = '1' then	   			
							if rxd_buf = "11010101" then
								rx_state <= "10";
							elsif rxd_buf = "01010101" then
								rx_state <= "01";
							else
								rx_state <= "00";
							end if;
						else
							rx_state <= "00";
						end if;
					when "10" =>
						if rxdv_buf = '1' then
							rx_state <= "11";
						end if;
					when "11" =>
						if rxdv_buf = '0' then
							rx_state <= "00";
						end if;
					when others	=>
						NULL;
				end case;
--			end if;	   
			rx_state_d1 <= rx_state;
		end if;
	end process;
		
	p_byte_cnt : process(rxclk, reset)				-- byte count
	begin
		if reset = '1' then
			byte_cnt <= (others => '0');
		elsif rising_edge(rxclk) then
--			if ce = '1' then
				if rx_state = "00" then
					byte_cnt <= (others => '0');
				elsif rx_state = "11" then
					byte_cnt <= byte_cnt + 1;
				end if;
--			end if;
		end if;
	end process;
	
	p_length : process(rxclk, reset)
	begin
		if reset = '1' then
			length <= (others => '0');
		elsif rising_edge(rxclk) then
--			if ce = '1' then
				if rx_state = "11" and rxdv_buf = '0' then
					length <= "0000" & byte_cnt - 3;   -- 数据长度，除掉4Bytes CRC校验
				end if;
--			end if;
		end if;
	end process;
	
	------------------------------------------------------------------------------
	
--	u_crc_rom : CRCRom
--	port map(
--		addr => crcrom_addr,
--		dout => crcrom_dout
--		);
--	
--	crcrom_addr <= crc_reg(31 downto 28);
--	
--	p_calc_crc : process(clk, reset)
--	begin
--		if reset = '1' then
--			crc_din <= (others => '0');
--			crc_reg <= (others => '0');
--		elsif rising_edge(clk) then
--			if ce = '1' then
--				if nibble_cnt < 7 then
--					crc_din <= not (rxd_int(0) & rxd_int(1) & rxd_int(2) & rxd_int(3));
--				else
--					crc_din <= rxd_int(0) & rxd_int(1) & rxd_int(2) & rxd_int(3);
--				end if;
--				
--				if rx_state = "10" then
--					crc_reg <= (others => '0');
--				elsif rx_state = "11" then
--					crc_reg <= (crc_reg(27 downto 0) & crc_din) xor crcrom_dout;
--				end if;
--			end if;
--		end if;
--	end process;
	
	u_crc_rom : CRC8_BlkRom
	port map(  
		clk => rxclk,
		addr => crcrom_addr,
		dout => crcrom_dout
		);					
	
	crcrom_addr <= crc_reg(31 downto 24); 
	
	crc_reg <= (others=>'0') when rx_state_d1 = "10" else
		crc_reg_d1 xor crcrom_dout when rx_state_d1 = "11" else
		crc_reg_d1;
		
	p_calc_crc : process(rxclk, reset)
	begin
		if reset = '1' then
			crc_din <= (others => '0');
			crc_reg_d1 <= (others => '0');
		elsif rising_edge(rxclk) then
--			if ce = '1' then
				if byte_cnt < 3 then
					crc_din <= not (rxd_buf(0) & rxd_buf(1) & rxd_buf(2) & rxd_buf(3) & rxd_buf(4) & rxd_buf(5) & rxd_buf(6) & rxd_buf(7));
				else
					crc_din <= rxd_buf(0) & rxd_buf(1) & rxd_buf(2) & rxd_buf(3) & rxd_buf(4) & rxd_buf(5) & rxd_buf(6) & rxd_buf(7);
				end if;
				
				if rx_state = "10" then
					crc_reg_d1 <= (others => '0');
				elsif rx_state = "11" then
					crc_reg_d1 <= (crc_reg(23 downto 0) & crc_din);
				end if;
--			else
--				crc_reg_d1 <= crc_reg;
--			end if;
		end if;
	end process;
		
	p_crc_flag : process(rxclk, reset)
	begin
		if reset = '1' then
			crc_flag <= '0'; 
			g_Test_EthRec_CRCFlag	<= '0';
		elsif rising_edge(rxclk) then
--			if ce = '1' then
				if rx_state = "00" and crc_reg = X"FFFFFFFF" then
					crc_flag <= '1';
				elsif rx_state = "10" then
					crc_flag <= '0';
				end if;
--			end if;	 
			g_Test_EthRec_CRCFlag <= crc_flag;
		end if;
	end process;
--------------------------------------------------------------------------------
	p_s2p : process(rxclk, reset)
	begin
		if reset = '1' then
			rxd_buf_d1 <= (others => '0');
			rxd_buf_d2 <= (others => '0'); 
			rxd_buf_d3 <= (others => '0');
			rxd_buf_d4 <= (others => '0');
		elsif rising_edge(rxclk) then
--			if ce = '1' then
				rxd_buf_d1 <= rxd_buf;
				rxd_buf_d2 <= rxd_buf_d1;
				rxd_buf_d3 <= rxd_buf_d2;
				rxd_buf_d4 <= rxd_buf_d3;
--			end if;
		end if;
	end process;
	
--	dword_data_ext <= rxd_buf_d4 & rxd_buf_d3 & rxd_buf_d2 & rxd_buf_d1;
	dword_data_ext <= rxd_buf_d1 & rxd_buf_d2 & rxd_buf_d3 & rxd_buf_d4;
	
-----------------------------------------------------------------------------	

	RxFIFO_clr <= '1' when rx_state = "10" else '0';
	RxFIFO_wea <= '1' when rx_state = "11" and byte_cnt(1 downto 0) = "11" else '0';
	RxFIFO_dia <= dword_data_ext;

	u_din_sync : fifo_async_almost_full
	generic map(
		DEPTH => 2**(HEAD_AWIDTH-1),
		AWIDTH => HEAD_AWIDTH-1,
		DWIDTH => 32,
		RAM_TYPE => "DIS_RAM")
	port map(
		reset => reset,
		clr => RxFIFO_clr,
		clka => rxclk, 
--		clka => rxclk_temp,
		wea => RxFIFO_wea,
		dia => RxFIFO_dia,
		clkb => clk,
		rdb => RxFIFO_rdb,
		dob => RxFIFO_dob,
		empty => RxFIFO_empty,
		almost_full => open,
		full => open,
		dn => open
		);
	
	RxFIFO_not_empty <= not RxFIFO_empty;
	RxFIFO_rdb <= not RxFIFO_empty when (rx_state_int = "01" and byte_cnt_int(1 downto 0) = "00") or rx_state_int = "10" else '0';
	dword_data_int <= RxFIFO_dob;  
	
	p_ce : process(clk, reset)
	begin					 
		if reset = '1' then
			ce <= '0';
		elsif rising_edge(clk) then
			ce <= RxFIFO_rdb;
		end if;
	end process;  

	ASYNCWRITE_RxFIFO_clr_int	: ASYNCWRITE
		port map(
			reset		=> reset,
			async_clk	=> rxclk,
			sync_clk	=> clk,
			async_wren	=> RxFIFO_clr,
			trigger		=> RxFIFO_not_empty,
			sync_wren	=> RxFIFO_clr_int,
			over		=> open,
			flag		=> open
			); 
	
	p_rx_state_int : process( clk, reset )
	begin
		if reset = '1' then
			rx_state_int <= "00";
			rx_state_int_d1 <= "00";
		elsif rising_edge( clk ) then
			if RxFIFO_clr_int = '1' then 
				rx_state_int <= "01";
			else
				case rx_state_int is
					when "01" =>  -- head_ena
						if byte_cnt_int = HEAD_LENGTH-1 then
							rx_state_int <= "10";
						elsif rx_end_int = '1' then
							rx_state_int <= "11";
						else
							rx_state_int <= "01";
						end if;
					when "10" =>  -- data_ena
						if rx_end_int = '1' then
							rx_state_int <= "11";
						end if;
					when "11" =>  -- info_ena
						if info_cnt = INFO_LENGTH-1 then
							rx_state_int <= "00";
						end if;
					when others	=>
						NULL;
				end case;	
			end if;
			rx_state_int_d1 <= rx_state_int;
		end if;
	end process;
				
	rx_end_ext <= '1' when rx_state = "11" and rxdv_buf = '0' else '0';
		
	ASYNCWRITE_rx_end_int	: ASYNCWRITE
		port map(
			reset		=> reset,
			async_clk	=> rxclk,
			sync_clk	=> clk,
			async_wren	=> rx_end_ext,
			trigger		=> RxFIFO_empty,
			sync_wren	=> rx_end_int,
			over		=> open,
			flag		=> open
			); 
	
	p_dword_cnt : process(clk, reset, RxFIFO_clr)				-- byte count
	begin
		if reset = '1' then
			byte_cnt_int <= (others => '0');
			byte_cnt_int_d1 <= (others => '0');
		elsif rising_edge(clk) then 
			if rx_state_int = "00" then
				byte_cnt_int <= (others=>'0');
			elsif rx_state_int = "01" then
				byte_cnt_int <= byte_cnt_int + 1;
			elsif rx_state_int = "10" then
				byte_cnt_int <= byte_cnt_int + 4;
			end if;	  
			byte_cnt_int_d1 <= byte_cnt_int;
		end if;
	end process;
	
	------------------------------------------------------------------------------ 
	
--	p_recvtime	:	process(clk, reset)
--	begin
--		if reset = '1' then
--			recvtime(31 downto 0) <= (others => '0');
--			recvtime_valid <= '0';
--		elsif rising_edge(clk) then
--			if ce = '1' then
--				case dword_cnt is 
--					when X"00e" => recvtime(31 downto 24) <= rxd_int; 
--					when X"00f" => recvtime(23 downto 16) <= rxd_int;
--					when X"010" => recvtime(15 downto 8) <= rxd_int;
--					when X"011" => recvtime(7 downto 0) <= rxd_int;
--					when X"012" => recvtime_valid <= '1'; 
--					when others => 	recvtime_valid <= '0';
--				end case;
--			end if;
--		end if;			
--	end process; 

	p_p2s	: process( clk, reset )
	begin
		if reset = '1' then
			dword_data_buf <= (others=>'0');
		elsif rising_edge( clk ) then
			if byte_cnt_int_d1(1 downto 0) = "00" then
--				dword_data_buf <= dword_data_int(23 downto 0);
				dword_data_buf <= dword_data_int(31 downto 8);
			else
--				dword_data_buf <= dword_data_buf(15 downto 0) & x"00";
				dword_data_buf <= x"00" & dword_data_buf(23 downto 8);
			end if;
		end if;
	end process;
--	byte_data_int <= dword_data_int(31 downto 24) when byte_cnt_int_d1(1 downto 0) = "00" else
--		dword_data_buf(23 downto 16);
	byte_data_int <= dword_data_int(7 downto 0) when byte_cnt_int_d1(1 downto 0) = "00" else
		dword_data_buf(7 downto 0);
		
	p_recvtime	:	process(clk, reset)
	begin
		if reset = '1' then
			recvtime(31 downto 0) <= (others => '0');
			recvtime_valid <= '0';
		elsif rising_edge(clk) then
--			if ce = '1' then
				case byte_cnt_int_d1 is 
					when X"00e" => recvtime(31 downto 24) <= byte_data_int; 
					when X"00f" => recvtime(23 downto 16) <= byte_data_int; 
					when X"010" => recvtime(15 downto 8) <= byte_data_int; 
					when X"011" => recvtime(7 downto 0) <= byte_data_int; 
					when X"012" => recvtime_valid <= '1'; 
					when others => 	recvtime_valid <= '0';
				end case;
--			end if;
		end if;			
	end process; 	 
	
	p_localtime_locked	:	process(clk, reset)
	begin
		if reset = '1' then
			localtime_locked_reg <= '0';
		elsif rising_edge(clk) then
--			if ce = '1' and rx_state = "10" and localtime_locked_reg = '0' then	 
			if RxFIFO_clr_int = '1' and localtime_locked_reg = '0' then
				localtime_locked_reg <= '1';   
			elsif localtime_locked_reg = '1' then 
				localtime_locked_reg <= '0';
			end if;
		end if;			
	end process; 	
	
	localtime_locked <= localtime_locked_reg;
	------------------------------------------------------------------------------
	
	p_buff_wren : process(clk, reset)							
	begin
		if reset = '1' then
			buff_wren_buf <= '0';
		elsif rising_edge(clk) then
--			if ce = '1' then
--				if byte_cnt(1 downto 0) = "11" and rx_state = "11" then	 		-- 4byte写使能？
--					buff_wren_buf <= '1';
--				else
--					buff_wren_buf <= '0';
--				end if;
--			end if;		
--			if ce = '1' and (rx_state_int = "01" or rx_state_int = "10") then
--				buff_wren_buf <= '1';  
--			else
--				buff_wren_buf <= '0';
--			end if;
			buff_wren_buf <= ce;
		end if;			 
	end process;
	
	p_buff_waddr : process(clk, reset)
	begin
		if reset = '1' then
			buff_waddr_buf <= (others => '0');
		elsif rising_edge(clk) then
--			if ce = '1' then
				if buff_wren_buf = '1' then
					buff_waddr_buf <= buff_waddr_buf + 1;					-- buffer address ++
				end if;
--			end if;
		end if;
	end process;
	
	p_buff_wdata : process(clk, reset)
	begin
		if reset = '1' then
			buff_wdata <= (others => '0');
		elsif rising_edge(clk) then
			if ce = '1' then
				buff_wdata <= dword_data_int;
			end if;
		end if;
	end process;
	
	buff_wren <= buff_wren_buf;
	buff_waddr <= buff_waddr_buf;
	
	------------------------------------------------------------------------------
	
	p_start_addr : process(clk, reset)
	begin
		if reset = '1' then
			start_addr <= (others => '0');
		elsif rising_edge(clk) then
--			if ce = '1' then
--				if rx_state = "10" then
			if RxFIFO_clr_int = '1' then
					start_addr <= EXT(buff_waddr_buf, 16);	  --  包括CRC地址
--				end if;
			end if;
		end if;
	end process;
	
	------------------------------------------------------------------------------
	
	p_info_cnt : process(clk, reset)
	begin
		if reset = '1' then
--			info_ena <= '0';
			info_cnt <= 0;
		elsif rising_edge(clk) then
--			if ce = '1' then
--				if rx_state = "11" and rxdv_int = '0' then	   
--				if rx_end_int = '1' then
--					info_ena <= '1';
--				elsif info_cnt = INFO_LENGTH - 1 then
--					info_ena <=	'0';
--				end if;	
--				if info_ena = '0' then
--					info_cnt <= 0;
--				else
--					info_cnt <= info_cnt + 1;
--				end if;
--			end if;
			if rx_state_int = "11" then
				info_cnt <= info_cnt + 1;
			else
				info_cnt <= 0;
			end if;
		end if;
	end process;
	info_ena <= '1' when rx_state_int = "11" else '0';
	
	p_head_wren : process(clk, reset)
	begin
		if reset = '1' then
			head_wren_buf <= '0';
		elsif rising_edge(clk) then
--			if ce = '1' then
--				if (rx_state = "11" and rxdv_int = '1' and byte_cnt < HEAD_LENGTH) or info_ena = '1' then	  -- 前32个字节写时，以及最后写地址与长度4字节时为1
				if rx_state_int_d1 = "01" or rx_state_int = "11" then
					head_wren_buf <= '1';
				else
					head_wren_buf <= '0';
				end if;
--			end if;
		end if;
	end process;
	
	p_head_waddr : process(clk, reset)
	begin
		if reset = '1' then
			head_waddr_buf <= (others => '0');
		elsif rising_edge(clk) then
--			if ce = '1' then
				--if rx_state = "10" then
				if RxFIFO_clr_int = '1' then
					head_waddr_buf <= conv_std_logic_vector(INFO_LENGTH, HEAD_AWIDTH);
				--elsif rx_state = "11" and rxdv_int = '0' then
				elsif rx_end_int = '1' then
					head_waddr_buf <= conv_std_logic_vector(0, HEAD_AWIDTH);
				elsif head_wren_buf = '1' then
					head_waddr_buf <= head_waddr_buf + 1;
				end if;
--			end if;
		end if;
	end process;
	
	p_head_wdata : process(clk, reset)
	begin
		if reset = '1' then
			head_wdata <= (others => '0');
		elsif rising_edge(clk) then
--			if ce = '1' then
				if info_ena = '1' then
					case info_cnt is
						when 0 => head_wdata <= length(7 downto 0);
						when 1 => head_wdata <= length(15 downto 8);
						when 2 => head_wdata <= start_addr(5 downto 0)&"00";
						when 3 => head_wdata <= start_addr(13 downto 6);
						when others => null;
					end case;
				else
					head_wdata <= byte_data_int;
				end if;
--			end if;
		end if;
	end process;
	
	head_wren <= head_wren_buf;
	head_waddr <= head_waddr_buf;
	
	p_head_wr_block : process(clk, reset)
	begin
		if reset = '1' then
			head_wr_block_buf <= '0';
		elsif rising_edge(clk) then
--			if ce = '1' then
				if info_cnt = INFO_LENGTH and crc_flag = '1' then 		-- crc校验正确
					head_wr_block_buf <= '1';
				else
					head_wr_block_buf <= '0';
				end if;
--			end if;
		end if;
	end process;
	
	head_wr_block <= head_wr_block_buf;		   -- crc校验指针
	
end arch_ethrx_input;
