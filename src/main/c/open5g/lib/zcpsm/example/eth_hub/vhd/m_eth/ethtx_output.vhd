library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity ethtx_output is
	generic(
		HEAD_AWIDTH			:	natural		:=	5;
		BUFF_AWIDTH			:	natural		:=	16;
		RAM_AWIDTH			:	natural		:= 32
		);
	port(
		clk					:	in	std_logic;
		reset				:	in	std_logic;
		txclk				:	in	std_logic;
		txd					:	out	std_logic_vector(3 downto 0);
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
		buff_rdata			:	in	std_logic_vector(7 downto 0); 
		
		dma_start			:	out	std_logic; 
		
		dma_start_addr		:	out	std_logic_vector(RAM_AWIDTH - 1 downto 0);
--		 dma_start_addr		:	out	std_logic_vector(23 downto 0);
		
		dma_length			:	out	std_logic_vector(15 downto 0);
		dma_step			:	out	std_logic_vector(7 downto 0);
		
		localtime			:   in 	std_logic_vector(31 downto 0)
		);
end ethtx_output;

architecture arch_ethtx_output of ethtx_output is
	
	component crcrom
		port(
			addr : in std_logic_vector(3 downto 0);
			dout : out std_logic_vector(31 downto 0));
	end component;
	
	component fifo_async
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
		full : out std_logic;
		dn : out std_logic_vector((AWIDTH-1) downto 0));
	end component;
	for all: fifo_async use entity WORK.fifo_async(fast_write);

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
	
--	constant INFO_LENGTH	:	natural		:=	7;
	constant INFO_LENGTH	:	natural		:=	8;

	signal full				:	std_logic;	
	signal ce				:	std_logic;
	signal txd_int			:	std_logic_vector(3 downto 0);
	signal txen_int			:	std_logic;
	signal txd_buf			:	std_logic_vector(3 downto 0);
	signal txen_buf			:	std_logic;
	signal d_int			:	std_logic_vector(4 downto 0);
	signal d_ext			:	std_logic_vector(4 downto 0);
	
	signal busy				:	std_logic;
	signal nibble_cnt		:	std_logic_vector(11 downto 0);
	signal head_length		:	std_logic_vector(7 downto 0);
	signal data_length		:	std_logic_vector(10 downto 0);
	
	signal source_select	:	std_logic;
	signal head_rd_block	:	std_logic;
	
	signal info_ena			:	std_logic;
	signal info_cnt			:	integer range 0 to INFO_LENGTH;
	signal data_ena			:	std_logic;
	signal data_ena_d1		:	std_logic;
	signal data_ena_d2		:	std_logic;
	signal data_ena_d3		:	std_logic;	 
	signal data_ena_d4		:	std_logic;	
	signal data_ena_d5		:	std_logic;	
	
	signal head_ena			:	std_logic;
	signal buff_ena			:	std_logic;
	
	signal info_start		:	std_logic;
	signal data_start		:	std_logic;
	
	signal byte_data		:	std_logic_vector(7 downto 0);
	signal nibble_data		:	std_logic_vector(3 downto 0);
	signal nibble_data_buf	:	std_logic_vector(3 downto 0);
	signal nibble_data_dly	:	std_logic_vector(3 downto 0);
	
	signal head_rden		:	std_logic;
	signal head_rdata		:	std_logic_vector(7 downto 0);
	signal head_raddr_buf	:	std_logic_vector(HEAD_AWIDTH - 1 downto 0);
	signal buff_rden		:	std_logic;
	signal buff_raddr_buf	:	std_logic_vector(BUFF_AWIDTH - 1 downto 0);
	
	signal crc_din			:	std_logic_vector(3 downto 0);
	signal crc_reg			:	std_logic_vector(31 downto 0);
	signal crcrom_addr		:	std_logic_vector(3 downto 0);
	signal crcrom_dout		:	std_logic_vector(31 downto 0);
	
	signal v0				:	std_logic_vector(0 downto 0);
	signal v1				:	std_logic_vector(0 downto 0);
	signal v2				:	std_logic_vector(0 downto 0);	
	signal v3				:	std_logic_vector(0 downto 0);	
	
	signal localtime_reg	:	std_logic_vector(31 downto 0);

	signal crc_reg_dly		:	std_logic_vector(3 downto 0);  
	signal IFG_cnt			:	std_logic_vector(4 downto 0);
	signal IFG_busy			:	std_logic;				
	
	signal m4_TxFIFO_DN		:	std_logic_vector( 3 downto 0 );
	signal s_N_Empty		:	std_logic;
	signal s_N_Empty_TxClk	:	std_logic;
	signal s_N_Empty_TxClk_D1	:	std_logic;
	
begin 
	
--	p_IFG_count	: process(clk, reset)
	
	
	p_info_start : process(clk, reset)
	begin
		if reset = '1' then
			info_start <= '0';
			source_select <= '0';
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
		end if;
	end process;
	
--	busy <= info_start or info_ena or data_start or data_ena or data_ena_d3;
	busy <= info_start or info_ena or data_start or data_ena or data_ena_d4 or IFG_busy;

	p_info_cnt : process(clk, reset)
	begin
		if reset = '1' then
			info_ena <= '0';
			info_cnt <= 0;
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
		end if;
	end process;
	
	------------------------------------------------------------------------------
	
	data_start <= '1' when info_cnt = INFO_LENGTH else '0';
	
	p_nibble_cnt : process(clk, reset)
	begin
		if reset = '1' then
			data_ena <= '0';
			nibble_cnt <= (others => '0');
		elsif rising_edge(clk) then
			if ce = '1' then
				if data_start = '1' then
					data_ena <= '1';
				elsif nibble_cnt = (data_length & '0') - 1 then
					data_ena <= '0';
				end if;
				
				if data_start = '1' then
					nibble_cnt <= (others => '0');
				else
					nibble_cnt <= nibble_cnt + 1;
				end if;
			end if;
		end if;
	end process;
	
	head_ena <= '1' when data_ena = '1' and nibble_cnt < head_length & '0' else '0';
	buff_ena <= '1' when data_ena = '1' and nibble_cnt >= head_length & '0' else '0';
	
	------------------------------------------------------------------------------
	
	head_rden <= (info_ena or (head_ena and (not nibble_cnt(0)))) and ce;
	
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
	
	
		
	head_rd_block <= '1' when nibble_cnt = head_length & '0' else '0';
	
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
			if ce = '1' then
				if info_ena = '1' then
					case info_cnt is
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
						-- 4 bytes dma addr --  
						when 6 =>
						dma_start_addr(31 downto 24) <= head_rdata;
						when 7 =>
						dma_step <= head_rdata;
						--when 6 =>
						--dma_step <= head_rdata;
						when others =>
						null;
					end case;
				end if;
			end if;
		end if;
	end process;
	
	dma_start <= '1' when info_cnt = INFO_LENGTH and ce = '1' and data_length /= head_length else '0';
	dma_length <= SXT(data_length, 16) - SXT(head_length, 16);
	
	------------------------------------------------------------------------------
	
	buff_rden <= buff_ena and (not nibble_cnt(0)) and ce;
	
	p_buff_raddr : process(clk, reset)
	begin
		if reset = '1' then
			buff_raddr_buf <= (others => '0');
		elsif rising_edge(clk) then
			if ce = '1' then
				if data_start = '1' then
					buff_raddr_buf <= (others => '0');
				elsif buff_rden = '1' then
					buff_raddr_buf <= buff_raddr_buf + 1;
				end if;
			end if;
		end if;
	end process;
	
	buff_raddr <= buff_raddr_buf;
	
	------------------------------------------------------------------------------
	
	byte_data <= head_rdata when head_ena = '1' else 
				 buff_rdata when buff_ena = '1' else 
				 (others => '0');	
				   
	p_nibble_data_buf : process(clk, reset)
	begin
		if reset = '1' then
			nibble_data_buf <= (others => '0');
		elsif rising_edge(clk) then
			if ce = '1' then
				nibble_data_buf <= byte_data(7 downto 4);
			end if;
		end if;
	end process;

			   --local time --	
	p_localtime	: process(reset, clk)
	begin
		if reset = '1' then
			localtime_reg <= (others => '0');
		elsif rising_edge(clk) then
			if nibble_cnt = 15 then
				localtime_reg <= localtime;
			end if;
		end if;
	end process;
	
	nibble_data <= localtime_reg(31 downto 28) when nibble_cnt = 29 and source_select = '0' else
			       localtime_reg(27 downto 24) when nibble_cnt = 28 and source_select = '0' else
			       localtime_reg(23 downto 20) when nibble_cnt = 31 and source_select = '0' else
			       localtime_reg(19 downto 16) when nibble_cnt = 30 and source_select = '0' else
			       localtime_reg(15 downto 12) when nibble_cnt = 33 and source_select = '0' else
			       localtime_reg(11 downto 8)  when nibble_cnt = 32 and source_select = '0' else
			       localtime_reg(7 downto 4)   when nibble_cnt = 35 and source_select = '0' else
			       localtime_reg(3 downto 0)   when nibble_cnt = 34 and source_select = '0' else
				   byte_data(3 downto 0)   when nibble_cnt(0) = '0' else nibble_data_buf;
					   
--	nibble_data <= byte_data(3 downto 0)   when nibble_cnt(0) = '0' else nibble_data_buf;
	
	------------------------------------------------------------------------------
	
	u_crc_rom : CRCRom
	port map(
		addr => crcrom_addr,
		dout => crcrom_dout
		);
	
	crcrom_addr <= crc_reg(31 downto 28);
	
	crc_din <= (others => '0') when data_ena = '0' else
			   not (nibble_data(0) & nibble_data(1) & nibble_data(2) & nibble_data(3)) when nibble_cnt < 8 else
			   nibble_data(0) & nibble_data(1) & nibble_data(2) & nibble_data(3);
	
	p_calc_crc : process(clk, reset)
	begin
		if reset = '1' then
			crc_reg <= (others => '0');
		elsif rising_edge(clk) then
			if ce = '1' then
				if data_start = '1' then
					crc_reg <= (others => '0');
				elsif data_ena_d1 = '1' then
					crc_reg <= (crc_reg(27 downto 0) & crc_din) xor crcrom_dout;
				else
					crc_reg <= (crc_reg(27 downto 0) & crc_din);
				end if;
			end if;
		end if;
	end process;
	
	------------------------------------------------------------------------------
	
	u_nibble_data_dly : ShiftReg
	generic map(
		WIDTH => 4,
		DEPTH => 16 -- 8
		)
	port map(
		clk => clk,
		ce => ce,
		D => nibble_data,
		Q => nibble_data_dly,
		S => open
		);

	u_crc_reg_dly : ShiftReg
	generic map(
		WIDTH => 4,
		DEPTH => 8 -- 8
		)
	port map(
		clk => clk,
		ce => ce,
		D => crc_reg(31 downto 28),
		Q => crc_reg_dly(3 downto 0),
		S => open
		);
		
	u_data_ena_d1 : ShiftReg
	generic map(
		WIDTH => 1,
		DEPTH => 8 -- 8
		)
	port map(
		clk => clk,
		ce => ce,
		D => v0,
		Q => v1,
		S => open
		);
	
	u_data_ena_d2 : ShiftReg
	generic map(
		WIDTH => 1,
		DEPTH => 8 --8 
		)
	port map(
		clk => clk,
		ce => ce,
		D => v1,
		Q => v2,
		S => open
		);

	u_data_ena_d3 : ShiftReg
	generic map(
		WIDTH => 1,
		DEPTH => 8 --8 
		)
	port map(
		clk => clk,
		ce => ce,
		D => v2,
		Q => v3,
		S => open
		);
		
	v0(0) <= data_ena;
	data_ena_d1 <= v1(0);
	data_ena_d2 <= v2(0); 
	data_ena_d3 <= v3(0);	
	
	txd_int <= "0101" when data_ena = '1' and nibble_cnt < 15 else
	           "1101" when data_ena = '1' and nibble_cnt = 15 else
			   -- 	   
			   nibble_data_dly when data_ena_d2 = '1' else
			   not(crc_reg_dly(0) & crc_reg_dly(1) & crc_reg_dly(2) & crc_reg_dly(3)); 
	
	txen_int <= data_ena or data_ena_d3;
	
	------------------------------------------------------------------------------

--	u_dout_sync : fifo_async
--	generic map(
--		depth => 4,
--		awidth => 2,
--		dwidth => 5,
--		ram_type => "DIS_RAM"
--		)
--	port map(
--		reset => reset,
--		clr => '0',
--		clka => clk,
--		wea => ce,
--		dia => d_int,
--		clkb => txclk,
--		rdb => '1',
--		dob => d_ext,
--		empty => open,
--		full => full,
--		dn => open
--		);

	u_dout_sync : fifo_async
	generic map(
		depth => 16,
		awidth => 4,
		dwidth => 5,
		ram_type => "DIS_RAM"
		)
	port map(
		reset => reset,
		clr => '0',
		clka => clk,
		wea => ce,
		dia => d_int,
		clkb => txclk,
		rdb => s_N_Empty_TxClk,
		dob => d_ext,
		empty => open,
		full => full,
		dn => m4_TxFIFO_DN
		);
	
	NEmpty : process( reset, clk )
	begin
		if ( reset = '1' ) then	
			s_N_Empty	<= '0';
		elsif ( rising_edge( clk ) ) then
			if ( m4_TxFIFO_DN > "0111" ) then 
				s_N_Empty	<= '1';	
--			else
--				s_N_Empty	<= '0';
			end if;			
		end if;		
	end process;
	
	NEmpty_TxClk : process( reset, txclk )
	begin
		if ( reset = '1' ) then
			s_N_Empty_TxClk		<= '0';
			s_N_Empty_TxClk_D1	<= '0';
		elsif ( rising_edge( txclk ) ) then
			s_N_Empty_TxClk		<= s_N_Empty;
			s_N_Empty_TxClk_D1	<= s_N_Empty_TxClk;
		end if;		
	end process;
	
	d_int <= txen_int & txd_int;
	txen_buf <= d_ext(4) and s_N_Empty_TxClk_D1;
	txd_buf <= d_ext(3 downto 0) and ( s_N_Empty_TxClk_D1 & s_N_Empty_TxClk_D1 & s_N_Empty_TxClk_D1 & s_N_Empty_TxClk_D1 );	
	ce <= not full;
	
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
	
	--------------------------------------------------------------
	---        IFG - Inter Frame Gap generation
	----------------------------------------------------------------	
	
	p_ifg_count : process(clk, reset)
	begin				 
		if reset = '1' then
			IFG_cnt <= "00000";
		elsif rising_edge(clk) then
			data_ena_d4 <= data_ena_d3;	
			data_ena_d5 <= data_ena_d4;
			if IFG_busy = '1' then
				IFG_cnt <= IFG_cnt + '1'; 
			else
				IFG_cnt <= "00000";
			end if;
		end if;
	end process;  
	
	p_ifg_busy_flag : process(clk, reset)
	begin				 
		if reset = '1' then
			IFG_busy <= '0';
		elsif rising_edge(clk) then
			if data_ena_d3 = '0' and data_ena_d4 = '1' then
				IFG_busy <= '1'; 
			elsif IFG_cnt = "11111" then
				IFG_busy <= '0';
			end if;
		end if;
	end process;  	
	
	
end arch_ethtx_output;
