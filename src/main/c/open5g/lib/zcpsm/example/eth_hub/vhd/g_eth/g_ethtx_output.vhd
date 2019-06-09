library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity g_ethtx_output is
	generic(
		HEAD_AWIDTH			:	natural		:=	5;
		BUFF_AWIDTH			:	natural		:=	16;
		RAM_AWIDTH			:	natural		:= 32
		);
	port(
		clk					:	in	std_logic;
		reset				:	in	std_logic;
		txclk				:	in	std_logic;
		txd					:	out	std_logic_vector(7 downto 0);
		txen				:	out	std_logic;
		tx_queue_empty		:	in	std_logic;
		tx_head_raddr		:	out	std_logic_vector(HEAD_AWIDTH - 1 downto 0);
		tx_head_rdata		:	in	std_logic_vector(7 downto 0);
		tx_head_rd_block	:	out	std_logic;
		
		db_queue_empty		:	in	std_logic;
		db_head_raddr		:	out	std_logic_vector(HEAD_AWIDTH - 1 downto 0);
		db_head_rdata		:	in	std_logic_vector(7 downto 0);
		db_head_rd_block	:	out	std_logic;	
		
		buff_raddr			:	out	std_logic_vector(BUFF_AWIDTH - 1 downto 0);	
		buff_rdata			:	in	std_logic_vector(31 downto 0); 
		
		dma_start			:	out	std_logic; 
		
		dma_start_addr		:	out	std_logic_vector(RAM_AWIDTH - 1 downto 0);
		
		dma_length			:	out	std_logic_vector(15 downto 0);
		dma_step			:	out	std_logic_vector(7 downto 0);
		
		localtime			:   in 	std_logic_vector(31 downto 0)
		);
end g_ethtx_output;

architecture arch_ethtx_output of g_ethtx_output is
	
	component crc8_blkrom
		port(	  
			clk	: in std_logic;
			addr : in std_logic_vector(7 downto 0);
			dout : out std_logic_vector(31 downto 0));
	end component;
	
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
	for all: fifo_async_almost_full use entity WORK.fifo_async_almost_full(fast_write);

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
	
	constant INFO_LENGTH	:	natural		:=	8;
 
	signal ce				:	std_logic;
	signal ce_d1			:	std_logic;
	signal ce_ext			:	std_logic;
	signal ce_ext_d1		:	std_logic;
	signal ce_ext_d2		:	std_logic;
	signal txd_buf			:	std_logic_vector(7 downto 0);
	signal txen_buf			:	std_logic;
	signal txd_buf_d1		:	std_logic_vector(7 downto 0);
	signal txen_buf_d1		:	std_logic;

	signal TxFIFO_clr	:	std_logic;
	signal TxFIFO_wea	:	std_logic;
	signal TxFIFO_dia	:	std_logic_vector(31 downto 0);
	signal TxFIFO_rdb	:	std_logic;
	signal TxFIFO_rdb_d1	:	std_logic;
	signal TxFIFO_dob	:	std_logic_vector(31 downto 0);
	signal TxFIFO_almost_full	:	std_logic;
	signal TxFIFO_empty	:	std_logic;
	signal TxFIFO_DN	:	std_logic_vector(3 downto 0);

	signal busy				:	std_logic;
	signal byte_cnt		:	std_logic_vector(11 downto 0);	   
	signal byte_cnt_d1	:	std_logic_vector(11 downto 0);
	signal byte_cnt_ext		:	std_logic_vector(11 downto 0);	   
	signal byte_cnt_ext_d1	:	std_logic_vector(11 downto 0);
	signal byte_cnt_ext_d2	:	std_logic_vector(11 downto 0);
	signal head_length		:	std_logic_vector(7 downto 0);
	signal data_length		:	std_logic_vector(10 downto 0);
	
	signal source_select	:	std_logic;
	signal head_rd_block	:	std_logic;
	
	signal info_ena			:	std_logic;
	signal info_ena_d1		:	std_logic;
	signal info_cnt			:	integer range 0 to INFO_LENGTH;
	signal info_cnt_d1		:	integer range 0 to INFO_LENGTH;
	signal data_ena			:	std_logic;
	signal data_ena_d8		:	std_logic;	
	signal data_ena_ext			:	std_logic;
	signal data_ena_ext_d1		:	std_logic;
	signal data_ena_ext_d2		:	std_logic;
	signal data_ena_ext_d6		:	std_logic;
	signal data_ena_ext_d8		:	std_logic;	
	signal data_ena_ext_d12		:	std_logic;
	signal data_ena_ext_d13		:	std_logic;
	
	signal head_ena			:	std_logic; 
	signal head_ena_d1		:	std_logic;
	signal buff_ena			:	std_logic;
	signal buff_ena_d1		:	std_logic;
	
	signal info_start		:	std_logic;
	signal data_start		:	std_logic;
	signal data_start_ext	:	std_logic;
	signal data_start_ext_wren	:	std_logic;
	
	signal dword_data_int		:	std_logic_vector(31 downto 0);
	signal dword_data_ext		:	std_logic_vector(31 downto 0);
	signal byte_data		:	std_logic_vector(7 downto 0);
	signal byte_data_buf	:	std_logic_vector(31 downto 0);
	signal byte_data_dly	:	std_logic_vector(7 downto 0);
	
	signal head_rden		:	std_logic;
	signal head_rdata		:	std_logic_vector(7 downto 0);
	signal head_rdata_buf	:	std_logic_vector(23 downto 0);
	signal head_raddr_buf	:	std_logic_vector(HEAD_AWIDTH - 1 downto 0);
	signal buff_rden		:	std_logic;
	signal buff_rden_d1		:	std_logic;
	signal buff_raddr_buf	:	std_logic_vector(BUFF_AWIDTH - 1 downto 0);
	signal buff_rdata_buf	:	std_logic_vector(31 downto 0);
	
	signal crc_din			:	std_logic_vector(7 downto 0);
	signal crc_reg			:	std_logic_vector(31 downto 0);
	signal crc_reg_d1		:	std_logic_vector(31 downto 0);
	signal crcrom_addr		:	std_logic_vector(7 downto 0);
	signal crcrom_dout		:	std_logic_vector(31 downto 0);
	
	signal v0				:	std_logic_vector(0 downto 0);
	signal v1				:	std_logic_vector(0 downto 0);
	signal v2				:	std_logic_vector(0 downto 0);	
	signal v3				:	std_logic_vector(0 downto 0);	
	
	signal localtime_reg	:	std_logic_vector(31 downto 0);

	signal crc_reg_dly		:	std_logic_vector(7 downto 0);  
	signal IFG_cnt			:	std_logic_vector(4 downto 0);
	signal IFG_busy			:	std_logic;				
	
	signal m4_TxFIFO_DN		:	std_logic_vector( 3 downto 0 );
	signal s_N_Empty		:	std_logic;
	signal s_N_Empty_TxClk	:	std_logic;
	signal s_N_Empty_TxClk_D1	:	std_logic;
	
begin 
	
	p_info_start : process(clk, reset)
	begin
		if reset = '1' then
			info_start <= '0';
			source_select <= '0';
			ce_d1 <= '0';
		elsif rising_edge(clk) then
			if ce = '1' then
				if busy = '0' then
					if tx_queue_empty = '0' then
						info_start <= '1';
						source_select <= '0';
					elsif db_queue_empty = '0' then
						info_start <= '1';
						source_select <= '1';
					end if;
				else
					info_start <= '0';
				end if;
			end if;
			ce_d1 <= ce;
		end if;
	end process;
	
	busy <= info_start or info_ena or data_start or data_ena or data_ena_d8 or data_ena_ext or data_ena_ext_d13 or IFG_busy;

	p_info_cnt : process(clk, reset)
	begin
		if reset = '1' then
			info_ena <= '0';
			info_cnt <= 0;
			info_ena_d1 <= '0';
			info_cnt_d1 <= 0;
		elsif rising_edge(clk) then
			if ce = '1' then
				if info_start = '1' then
					info_ena <= '1';
				elsif info_cnt = INFO_LENGTH - 1 then
					info_ena <= '0';
				end if;
				
				if info_ena = '0' then
					info_cnt <= 0;
				else
					info_cnt <= info_cnt + 1;
				end if;
			end if;	   
			info_ena_d1 <= info_ena;
			info_cnt_d1 <= info_cnt;
		end if;
	end process;
	
	------------------------------------------------------------------------------
	
	data_start <= '1' when info_cnt = INFO_LENGTH else '0';
	

	p_byte_cnt : process(clk, reset)
	begin
		if reset = '1' then
			data_ena <= '0';
			byte_cnt <= (others => '0');  
			byte_cnt_d1 <= (others=>'0');  
			head_ena_d1 <= '0';
		elsif rising_edge(clk) then
			if ce = '1' then
				if data_start = '1' then
					data_ena <= '1';
				elsif buff_ena = '0' and byte_cnt >= data_length-1 then
					data_ena <= '0';
				elsif buff_ena = '1' and byte_cnt >= data_length - 4 then
					data_ena <= '0';
				end if;
				
				if data_start = '1' then
					byte_cnt <= (others => '0');
				elsif head_ena = '1' then
					byte_cnt <= byte_cnt + 1;
				elsif buff_ena = '1' then 
					byte_cnt <= byte_cnt + 4;
				end if;	
			end if;	 
			byte_cnt_d1 <= byte_cnt;
			head_ena_d1 <= head_ena; 
		end if;
	end process;
	
	head_ena <= '1' when data_ena = '1' and byte_cnt < head_length else '0';
	buff_ena <= '1' when data_ena = '1' and byte_cnt >= head_length else '0';
		
	------------------------------------------------------------------------------
	
	head_rden <= (info_ena or head_ena) and ce;
	
	p_head_raddr : process(clk, reset)
	begin
		if reset = '1' then
			head_raddr_buf <= (others => '0');
		elsif rising_edge(clk) then
			if ce = '1' then
				if info_start = '1' then
					head_raddr_buf <= (others => '0');
				elsif head_rden = '1' then
					head_raddr_buf <= head_raddr_buf + 1;
				end if;
			end if;
		end if;
	end process;
	
	tx_head_raddr <= head_raddr_buf;
	db_head_raddr <= head_raddr_buf;
		
	head_rdata <= tx_head_rdata when source_select = '0' else db_head_rdata;
	
	
		
	head_rd_block <= '1' when byte_cnt = head_length else '0';
	
	tx_head_rd_block <= head_rd_block and ce and (not source_select);
	db_head_rd_block <= head_rd_block and ce and source_select;
	
	p_get_info : process(clk, reset)
	begin
		if reset = '1' then
			head_length <= (others => '0');
			data_length <= (others => '0');
			dma_start_addr <= (others => '0');
			dma_step <= (others => '0');
		elsif rising_edge(clk) then
			if ce_d1 = '1' then
				if info_ena_d1 = '1' then
					case info_cnt_d1 is
						when 0 =>
						head_length(7 downto 0) <= head_rdata;
						when 1 =>
						data_length(7 downto 0) <= head_rdata;
						when 2 =>
						data_length(10 downto 8) <= head_rdata(2 downto 0);
						when 3 =>
						dma_start_addr(7 downto 0) <= head_rdata;
						when 4 =>
						dma_start_addr(15 downto 8) <= head_rdata;
						when 5 =>
						dma_start_addr(23 downto 16) <= head_rdata;	 
						when 6 =>
						dma_start_addr(31 downto 24) <= head_rdata;
						when 7 =>
						dma_step <= head_rdata;
						when others =>
						null;
					end case; 
				end if;
			end if;
		end if;
	end process;
	
	dma_start <= '1' when info_cnt_d1 = INFO_LENGTH and ce_d1 = '1' and data_length /= head_length else '0';
	dma_length <= EXT(data_length, 16) - EXT(head_length, 16);

	-----------------------------------------------------------------------------
	p_get_head : process(clk, reset)
	begin
		if reset = '1' then	 
			head_rdata_buf <= (others=>'0');
		elsif rising_edge(clk) then
			if ce_d1 = '1' then
				if head_ena_d1 = '1' then	
					if byte_cnt_d1(1 downto 0) = "00" then
						head_rdata_buf <= head_rdata & X"0000";
					else
						head_rdata_buf <= head_rdata & head_rdata_buf(23 downto 8);
					end if;
				end if;
			end if;
		end if;
	end process;

	------------------------------------------------------------------------------
	
	buff_rden <= buff_ena and ce;
	
	p_buff_raddr : process(clk, reset)
	begin
		if reset = '1' then
			buff_raddr_buf <= (others => '0');	  
			buff_rdata_buf <= (others=>'0');
			buff_ena_d1 <= '0';	  
			buff_rden_d1 <= '0';
		elsif rising_edge(clk) then
			if ce = '1' then
				if data_start = '1' then
					buff_raddr_buf <= (others => '0');
				elsif buff_rden = '1' then
					buff_raddr_buf <= buff_raddr_buf + 1;
				end if;	 
				buff_rdata_buf <= buff_rdata;  
			end if;
			buff_ena_d1 <= buff_ena; 
			buff_rden_d1 <= buff_rden;
		end if;
	end process;
	
	buff_raddr <= buff_raddr_buf;
	
	------------------------------------------------------------------------------

	dword_data_int <= buff_rdata_buf when buff_ena_d1 = '1' else
			head_rdata & head_rdata_buf when head_ena_d1 = '1' else
			(others=>'0');

	ce <= not TxFIFO_almost_full;

	TxFIFO_clr <= '1' when data_start = '1' else '0';
	TxFIFO_wea <= '1' when (head_ena_d1 = '1' and byte_cnt_d1(1 downto 0) = "11") or buff_rden_d1 = '1' else '0';
	TxFIFO_dia <= dword_data_int;

	u_txclk_sync : fifo_async_almost_full
	generic map(
		depth => 16,
		awidth => 4,
		dwidth => 32,
		ram_type => "DIS_RAM"
		)
	port map(
		reset => reset,
		clr => TxFIFO_clr,
		clka => clk,
		wea => TxFIFO_wea,
		dia => TxFIFO_dia,
		clkb => txclk,
		rdb => TxFIFO_rdb,
		dob => TxFIFO_dob,
		empty => TxFIFO_empty, 
		almost_full	=> TxFIFO_almost_full,
		full => open,
		dn => TxFIFO_DN
		);
   
	dword_data_ext <= TxFIFO_dob;
	TxFIFO_rdb <= data_ena_ext and ce_ext when byte_cnt_ext(1 downto 0) = "00" else '0'; 
		
-------------------------------------------------------------------------------------------------------

	data_start_ext_wren <= (not head_ena) and head_ena_d1;
	ASYNCWRITE_data_start_ext	: ASYNCWRITE
		port map(
			reset		=> reset,
			async_clk	=> clk,
			sync_clk	=> txclk,
			async_wren	=> data_start_ext_wren,
			trigger		=> '1',
			sync_wren	=> data_start_ext,
			over		=> open,
			flag		=> open
			); 
	ce_ext <= '1';
	
	p_byte_cnt_ext : process(txclk, reset)
	begin
		if reset = '1' then	 
			ce_ext_d1 <= '0';
			ce_ext_d2 <= '0';
			data_ena_ext <= '0';
			data_ena_ext_d1 <= '0';
			data_ena_ext_d2 <= '0';
			byte_cnt_ext <= (others => '0');  
			byte_cnt_ext_d1 <= (others=>'0');
			byte_cnt_ext_d2 <= (others=>'0');
			TxFIFO_rdb_d1 <= '0';
		elsif rising_edge(txclk) then
			if ce_ext = '1' then
				if data_start_ext = '1' then
					data_ena_ext <= '1';
				elsif byte_cnt_ext = data_length - 1 then
					data_ena_ext <= '0';
				end if;
				
				if data_start_ext = '1' then
					byte_cnt_ext <= (others => '0');
				else
					byte_cnt_ext <= byte_cnt_ext + 1;
				end if;	
			end if;	
			ce_ext_d1 <= ce_ext;
			ce_ext_d2 <= ce_ext_d1;
			data_ena_ext_d1 <= data_ena_ext;
			data_ena_ext_d2 <= data_ena_ext_d1;
			byte_cnt_ext_d1 <= byte_cnt_ext;
			byte_cnt_ext_d2 <= byte_cnt_ext_d1;
			TxFIFO_rdb_d1 <= TxFIFO_rdb;
		end if;	
	end process;
	
	
	p_byte_data_buf : process(txclk, reset)
	begin
		if reset = '1' then
			byte_data_buf <= (others => '0');
		elsif rising_edge(txclk) then
			if ce_ext_d1 = '1' then
				if TxFIFO_rdb_d1 = '1' then
					byte_data_buf <= TxFIFO_dob;
				else
					byte_data_buf <= X"00" & byte_data_buf(31 downto 8);
				end if;
			end if;
		end if;
	end process;

	p_localtime	: process(reset, txclk)
	begin
		if reset = '1' then
			localtime_reg <= (others => '0');
		elsif rising_edge(txclk) then
			if byte_cnt_ext = 7 then
				localtime_reg <= localtime;
			end if;
		end if;
	end process; 

	byte_data <= localtime_reg(31 downto 24) when byte_cnt_ext_d1 = 14 and source_select = '0' else
			       localtime_reg(23 downto 16) when byte_cnt_ext_d1 = 15 and source_select = '0' else
			       localtime_reg(15 downto 8) when byte_cnt_ext_d1 = 16 and source_select = '0' else
			       localtime_reg(7 downto 0)   when byte_cnt_ext_d1 = 17 and source_select = '0' else
				   TxFIFO_dob(7 downto 0) when byte_cnt_ext_d1(1 downto 0) = "00" else
				   byte_data_buf(15 downto 8);
					   
	u_crc_rom : CRC8_BlkRom
	port map(  
		clk => txclk,
		addr => crcrom_addr,
		dout => crcrom_dout
		);
	
	crcrom_addr <= crc_reg(31 downto 24); 

	crc_din <= (others => '0') when data_ena_ext_d1 = '0' else
			   not (byte_data(0) & byte_data(1) & byte_data(2) & byte_data(3) & byte_data(4) & byte_data(5) & byte_data(6) & byte_data(7)) when byte_cnt_ext_d1 < 4 else
			   byte_data(0) & byte_data(1) & byte_data(2) & byte_data(3) & byte_data(4) & byte_data(5) & byte_data(6) & byte_data(7);
	
	crc_reg <= (others=>'0') when data_start_ext = '1' and ce_ext = '1' else
			crc_reg_d1 xor crcrom_dout when (data_ena_ext_d2 = '1' or data_ena_ext_d6 = '1') and ce_ext_d2 = '1' else 
			crc_reg_d1;
		

	p_calc_crc : process(txclk, reset)
	begin
		if reset = '1' then
			crc_reg_d1 <= (others => '0');
		elsif rising_edge(txclk) then
			if ce_ext_d1 = '1' then
				if data_start_ext = '1' then
					crc_reg_d1 <= (others => '0');
				else
					crc_reg_d1 <= (crc_reg(23 downto 0) & crc_din);
				end if;	 
			else
				crc_reg_d1 <= crc_reg(23 downto 0) & crc_din;
			end if;
		end if;
	end process;
	------------------------------------------------------------------------------
	
	u_nibble_data_dly : ShiftReg
	generic map(
		WIDTH => 8,
		DEPTH => 7
		)
	port map(
		clk => txclk,
		ce => '1',
		D => byte_data,
		Q => byte_data_dly,
		S => open
		);

	u_crc_reg_dly : ShiftReg
	generic map(
		WIDTH => 8,
		DEPTH => 3
		)
	port map(
		clk => txclk,
		ce => '1',
		D => crc_reg(31 downto 24),
		Q => crc_reg_dly(7 downto 0),
		S => open
		);
		
	u_data_ena_d0 : ShiftReg
	generic map(
		WIDTH => 1,
		DEPTH => 8
		)
	port map(
		clk => clk,
		ce => '1',
		D(0) => data_ena,
		Q(0) => data_ena_d8,
		S => open
		);

	u_data_ena_d1 : ShiftReg
	generic map(
		WIDTH => 1,
		DEPTH => 4
		)
	port map(
		clk => txclk,
		ce => '1',
		D => v0,
		Q => v1,
		S => open
		);
	
	u_data_ena_d2 : ShiftReg
	generic map(
		WIDTH => 1,
		DEPTH => 2 
		)
	port map(
		clk => txclk,
		ce => '1',
		D => v1,
		Q => v2,
		S => open
		);

	u_data_ena_d3 : ShiftReg
	generic map(
		WIDTH => 1,
		DEPTH => 4 
		)
	port map(
		clk => txclk,
		ce => '1',
		D => v2,
		Q => v3,
		S => open
		);	  
		
	v0(0) <= data_ena_ext_d2;
	data_ena_ext_d6 <= v1(0);
	data_ena_ext_d8 <= v2(0);
	data_ena_ext_d12 <= v3(0); 
	
	txd_buf <= "01010101" when data_ena_ext = '1' and byte_cnt_ext < 7 else
	           "11010101" when data_ena_ext = '1' and byte_cnt_ext = 7 else
			   -- 	   
			   byte_data_dly when data_ena_ext_d8 = '1' else
			   not(crc_reg_dly(0) & crc_reg_dly(1) & crc_reg_dly(2) & crc_reg_dly(3) & crc_reg_dly(4) & crc_reg_dly(5) & crc_reg_dly(6) & crc_reg_dly(7)); 
	
	txen_buf <= data_ena_ext or data_ena_ext_d12;
	------------------------------------------------------------------------------
	
	p_mii_dout : process(reset, txclk)
	begin						
		if ( reset = '1' ) then	
			txen	<= '0';
			txd		<= ( others => '0' ); 
		elsif rising_edge(txclk) then
			txen <= txen_buf;
			txd <= txd_buf;
		end if;
	end process;   
	
	p_ifg_count : process(txclk, reset)
	begin				 
		if reset = '1' then
			IFG_cnt <= "00000";	
			data_ena_ext_d13 <= '0';
		elsif rising_edge(txclk) then
			data_ena_ext_d13 <= data_ena_ext_d12;	
			if IFG_busy = '1' then
				IFG_cnt <= IFG_cnt + '1'; 
			else
				IFG_cnt <= "00000";
			end if;
		end if;
	end process;  
	
	p_ifg_busy_flag : process(txclk, reset)
	begin				 
		if reset = '1' then
			IFG_busy <= '0';
		elsif rising_edge(txclk) then
			if data_ena_ext_d12 = '0' and data_ena_ext_d13 = '1' then
				IFG_busy <= '1'; 
			elsif IFG_cnt = "11111" then
				IFG_busy <= '0';
			end if;
		end if;
	end process;  	
	
	
end arch_ethtx_output;
