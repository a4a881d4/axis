--=============================================================================
-- Project: DFE_TDD   
-- Author: Zhao Yifei 
-- Data: April/09/2008   
-- Module Name: Eth_Tx_HighPriority    
-- Revision: 1.0 
-------------------------------------------------------------------------------
-- Description:
-- 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Revision History:
-- 
--=============================================================================	

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

use work.eth_config.all;

entity Eth_Tx_HighPriority is
	port(
	reset				: in std_logic;
	clk					: in std_logic;
	clk_zcpsm			: in std_logic;
	
	s_Tx_Req			: in std_logic;
	m48_Tx_Req_DesMac	: in std_logic_vector( 47 downto 0 );
	m16_Tx_Req_Addr		: in std_logic_vector( 15 downto 0 );
	m16_Tx_Req_Data		: in std_logic_vector( 15 downto 0 );
	
	port_id 			: in std_logic_vector(7 downto 0);
	write_strobe 		: in std_logic;
	out_port 			: in std_logic_vector(7 downto 0);
	read_strobe 		: in std_logic;
	in_port 			: out std_logic_vector(7 downto 0)
	);
end Eth_Tx_HighPriority;

architecture arch_Eth_Tx_HighPriority of Eth_Tx_HighPriority is

component zcpsm2DSP
	port(
	clk				: in std_logic;
	port_id			: in std_logic_vector(7 downto 0); 
	write_strobe	: in std_logic;
	out_port		: in std_logic_vector(7 downto 0);
	read_strobe		: in std_logic;
	in_port			: out std_logic_vector(7 downto 0);
	interrupt		: out std_logic;
	
	dsp_io			: out std_logic;
	dsp_rw			: out std_logic;
	dsp_a			: out std_logic_vector(7 downto 0);
	dsp_din			: out std_logic_vector(7 downto 0);
	dsp_dout		: in std_logic_vector(7 downto 0);
	dsp_int			: in std_logic
	);
end component;

component ASYNCWRITE
	port(
	reset		: in std_logic;
	async_clk	: in std_logic;
	sync_clk	: in std_logic;
	async_wren	: in std_logic;
	trigger		: in std_logic;
	sync_wren	: out std_logic;
	over		: out std_logic;
	flag		: out std_logic
	);
end component;

signal s_DSP_IO					: std_logic;
signal s_DSP_RW					: std_logic;
signal m8_DSP_A					: std_logic_vector( 7 downto 0 );
signal m8_DSP_DIn				: std_logic_vector( 7 downto 0 );	
signal m8_DSP_DOut				: std_logic_vector( 7 downto 0 );

signal s_DSP_CE					: std_logic := '0';
signal s_SyncWE					: std_logic := '0';
signal s_AsyncWE				: std_logic := '0';
signal m4_AsyncAddrReg			: std_logic_vector( 3 downto 0 );
signal m4_SyncAddrReg			: std_logic_vector( 3 downto 0 );

signal s_Req					: std_logic;
signal m48_Tx_Req_DesMac_Reg	: std_logic_vector( 47 downto 0 );
signal m16_Tx_Req_Addr_Reg		: std_logic_vector( 15 downto 0 );
signal m16_Tx_Req_Data_Reg		: std_logic_vector( 15 downto 0 );

begin

	mo_DspInterface : zcpsm2DSP
		port map(
		clk				=> clk_zcpsm,
		port_id			=> port_id,
		write_strobe	=> write_strobe,
		out_port		=> out_port,
		read_strobe		=> read_strobe,
		in_port			=> open, --in_port,
		interrupt		=> open,
		
		dsp_io			=> s_DSP_IO,
		dsp_rw			=> s_DSP_RW,
		dsp_a			=> m8_DSP_A,
		dsp_din			=> m8_DSP_DIn,
		dsp_dout		=> m8_DSP_DOut,
		dsp_int			=> '0'
		);

	s_DSP_CE	<= '1' when ( m8_DSP_A( 7 downto 4 ) = PORT_ETH_TX_HIGHPRI_CE ) else
				   '0';

	mo_AsyncWrite_DSPInterface : ASYNCWRITE
		port map(
		reset		=> reset,
		async_clk	=> s_DSP_IO,
		sync_clk	=> clk,
		async_wren	=> s_AsyncWE,
		trigger		=> '1',
		sync_wren	=> s_SyncWE,
		over		=> open,
		flag		=> open
		);

	s_AsyncWE	<= ( not s_DSP_RW ) and s_DSP_CE;
	
	AsyncReg : process( reset, s_DSP_IO )
	begin
		if ( reset = '1' ) then
			m4_AsyncAddrReg		<= ( others => '0' );
		elsif ( rising_edge( s_DSP_IO ) ) then
			if ( ( s_DSP_CE = '1' ) and ( s_AsyncWE = '1' ) ) then 
				m4_AsyncAddrReg		<= m8_DSP_A( 3 downto 0 );
			end if;			
		end if;		
	end process;

	SyncReg : process( reset, clk )
	begin
		if ( reset = '1' ) then
			m4_SyncAddrReg		<= ( others => '0' );
		elsif ( rising_edge( clk ) ) then
			m4_SyncAddrReg		<= m4_AsyncAddrReg;
		end if;		
	end process;

	Req : process( reset, clk )
	begin
		if ( reset = '1' ) then
			s_Req					<= '0';
			m48_Tx_Req_DesMac_Reg	<= ( others => '0' );
			m16_Tx_Req_Addr_Reg		<= ( others => '0' );
			m16_Tx_Req_Data_Reg		<= ( others => '0' );
		elsif ( rising_edge( clk ) ) then
			if ( ( s_SyncWE = '1' ) and ( m4_SyncAddrReg = PORT_ETH_TX_HIGHPRI_REQ ) ) then
				s_Req	<= '0';
			elsif ( s_Tx_Req = '1' ) then						   
				s_Req	<= '1';
			end if;	
			if ( s_Tx_Req = '1' ) then						   
				m48_Tx_Req_DesMac_Reg	<= m48_Tx_Req_DesMac;
				m16_Tx_Req_Addr_Reg		<= m16_Tx_Req_Addr;
				m16_Tx_Req_Data_Reg		<= m16_Tx_Req_Data;
			end if;	
		end if;		
	end process;

	in_port	<= 	( "0000000" & s_Req ) when ( ( s_DSP_CE = '1' ) and ( s_DSP_RW = '1' ) and ( m8_DSP_A( 3 downto 0 ) = PORT_ETH_TX_HIGHPRI_REQ ) ) else
					m16_Tx_Req_Addr_Reg( 7 downto 0 ) when ( ( s_DSP_CE = '1' ) and ( s_DSP_RW = '1' ) and ( m8_DSP_A( 3 downto 0 ) = PORT_ETH_TX_HIGHPRI_ADDR_L ) ) else
					m16_Tx_Req_Addr_Reg( 15 downto 8 ) when ( ( s_DSP_CE = '1' ) and ( s_DSP_RW = '1' ) and ( m8_DSP_A( 3 downto 0 ) = PORT_ETH_TX_HIGHPRI_ADDR_H ) ) else
					m16_Tx_Req_Data_Reg( 7 downto 0 ) when ( ( s_DSP_CE = '1' ) and ( s_DSP_RW = '1' ) and ( m8_DSP_A( 3 downto 0 ) = PORT_ETH_TX_HIGHPRI_DATA_L ) ) else
					m16_Tx_Req_Data_Reg( 15 downto 8 ) when ( ( s_DSP_CE = '1' ) and ( s_DSP_RW = '1' ) and ( m8_DSP_A( 3 downto 0 ) = PORT_ETH_TX_HIGHPRI_DATA_H ) ) else
					m48_Tx_Req_DesMac_Reg( 47 downto 40 ) when ( ( s_DSP_CE = '1' ) and ( s_DSP_RW = '1' ) and ( m8_DSP_A( 3 downto 0 ) = PORT_ETH_TX_HIGHPRI_DESMAC_0 ) ) else
					m48_Tx_Req_DesMac_Reg( 39 downto 32 ) when ( ( s_DSP_CE = '1' ) and ( s_DSP_RW = '1' ) and ( m8_DSP_A( 3 downto 0 ) = PORT_ETH_TX_HIGHPRI_DESMAC_1 ) ) else
					m48_Tx_Req_DesMac_Reg( 31 downto 24 ) when ( ( s_DSP_CE = '1' ) and ( s_DSP_RW = '1' ) and ( m8_DSP_A( 3 downto 0 ) = PORT_ETH_TX_HIGHPRI_DESMAC_2 ) ) else
					m48_Tx_Req_DesMac_Reg( 23 downto 16 ) when ( ( s_DSP_CE = '1' ) and ( s_DSP_RW = '1' ) and ( m8_DSP_A( 3 downto 0 ) = PORT_ETH_TX_HIGHPRI_DESMAC_3 ) ) else
					m48_Tx_Req_DesMac_Reg( 15 downto 8 ) when ( ( s_DSP_CE = '1' ) and ( s_DSP_RW = '1' ) and ( m8_DSP_A( 3 downto 0 ) = PORT_ETH_TX_HIGHPRI_DESMAC_4 ) ) else
					m48_Tx_Req_DesMac_Reg( 7 downto 0 ) when ( ( s_DSP_CE = '1' ) and ( s_DSP_RW = '1' ) and ( m8_DSP_A( 3 downto 0 ) = PORT_ETH_TX_HIGHPRI_DESMAC_5 ) ) else
					( others => 'Z' );	

					
end arch_Eth_Tx_HighPriority;
