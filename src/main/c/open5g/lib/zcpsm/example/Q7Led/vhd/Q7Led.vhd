--Copyright 1986-2014 Xilinx, Inc. All Rights Reserved.
----------------------------------------------------------------------------------
--Tool Version: Vivado v.2014.4 (win64) Build 1071353 Tue Nov 18 18:24:04 MST 2014
--Date        : Sun Mar 01 22:52:38 2015
--Host        : dodo-PC running 64-bit Service Pack 1  (build 7601)
--Command     : generate_target Q7Led.bd
--Design      : Q7Led
--Purpose     : IP block netlist
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
entity Q7Led is
  port (
    DDR_addr : inout STD_LOGIC_VECTOR ( 14 downto 0 );
    DDR_ba : inout STD_LOGIC_VECTOR ( 2 downto 0 );
    DDR_cas_n : inout STD_LOGIC;
    DDR_ck_n : inout STD_LOGIC;
    DDR_ck_p : inout STD_LOGIC;
    DDR_cke : inout STD_LOGIC;
    DDR_cs_n : inout STD_LOGIC;
    DDR_dm : inout STD_LOGIC_VECTOR ( 3 downto 0 );
    DDR_dq : inout STD_LOGIC_VECTOR ( 31 downto 0 );
    DDR_dqs_n : inout STD_LOGIC_VECTOR ( 3 downto 0 );
    DDR_dqs_p : inout STD_LOGIC_VECTOR ( 3 downto 0 );
    DDR_odt : inout STD_LOGIC;
    DDR_ras_n : inout STD_LOGIC;
    DDR_reset_n : inout STD_LOGIC;
    DDR_we_n : inout STD_LOGIC;
    FIXED_IO_ddr_vrn : inout STD_LOGIC;
    FIXED_IO_ddr_vrp : inout STD_LOGIC;
    FIXED_IO_mio : inout STD_LOGIC_VECTOR ( 53 downto 0 );
    FIXED_IO_ps_clk : inout STD_LOGIC;
    FIXED_IO_ps_porb : inout STD_LOGIC;
    FIXED_IO_ps_srstb : inout STD_LOGIC;
    aBus_in_port : in STD_LOGIC_VECTOR ( 7 downto 0 );
    aBus_out_port : out STD_LOGIC_VECTOR ( 7 downto 0 );
    aBus_port_ce : out STD_LOGIC_VECTOR ( 15 downto 0 );
    aBus_port_id : out STD_LOGIC_VECTOR ( 3 downto 0 );
    aBus_read_strobe : out STD_LOGIC;
    aBus_write_strobe : out STD_LOGIC;
    clk : in STD_LOGIC
  );
end Q7Led;

architecture STRUCTURE of Q7Led is
  component ARM_wrapper is
  port (
    BRAM_PORTA_addr : out STD_LOGIC_VECTOR ( 12 downto 0 );
    BRAM_PORTA_clk : out STD_LOGIC;
    BRAM_PORTA_din : out STD_LOGIC_VECTOR ( 31 downto 0 );
    BRAM_PORTA_dout : in STD_LOGIC_VECTOR ( 31 downto 0 );
    BRAM_PORTA_en : out STD_LOGIC;
    BRAM_PORTA_rst : out STD_LOGIC;
    BRAM_PORTA_we : out STD_LOGIC_VECTOR ( 3 downto 0 );
    DDR_addr : inout STD_LOGIC_VECTOR ( 14 downto 0 );
    DDR_ba : inout STD_LOGIC_VECTOR ( 2 downto 0 );
    DDR_cas_n : inout STD_LOGIC;
    DDR_ck_n : inout STD_LOGIC;
    DDR_ck_p : inout STD_LOGIC;
    DDR_cke : inout STD_LOGIC;
    DDR_cs_n : inout STD_LOGIC;
    DDR_dm : inout STD_LOGIC_VECTOR ( 3 downto 0 );
    DDR_dq : inout STD_LOGIC_VECTOR ( 31 downto 0 );
    DDR_dqs_n : inout STD_LOGIC_VECTOR ( 3 downto 0 );
    DDR_dqs_p : inout STD_LOGIC_VECTOR ( 3 downto 0 );
    DDR_odt : inout STD_LOGIC;
    DDR_ras_n : inout STD_LOGIC;
    DDR_reset_n : inout STD_LOGIC;
    DDR_we_n : inout STD_LOGIC;
    FIXED_IO_ddr_vrn : inout STD_LOGIC;
    FIXED_IO_ddr_vrp : inout STD_LOGIC;
    FIXED_IO_mio : inout STD_LOGIC_VECTOR ( 53 downto 0 );
    FIXED_IO_ps_clk : inout STD_LOGIC;
    FIXED_IO_ps_porb : inout STD_LOGIC;
    FIXED_IO_ps_srstb : inout STD_LOGIC
  );
  end component ARM_wrapper;
  component zcpsmISP is
  port (
    reset : in STD_LOGIC;
    clk : in STD_LOGIC;
    port_ce : out STD_LOGIC_VECTOR ( 15 downto 0 );
    port_id : out STD_LOGIC_VECTOR ( 3 downto 0 );
    write_strobe : out STD_LOGIC;
    out_port : out STD_LOGIC_VECTOR ( 7 downto 0 );
    read_strobe : out STD_LOGIC;
    in_port : in STD_LOGIC_VECTOR ( 7 downto 0 );
    prog_we : in STD_LOGIC;
    prog_clk : in STD_LOGIC;
    prog_addr : in STD_LOGIC_VECTOR ( 9 downto 0 );
    prog_din : in STD_LOGIC_VECTOR ( 17 downto 0 )
  );
  end component zcpsmISP;
  component zProg2Bram is
  port (
    BRAM_PORTA_addr : in STD_LOGIC_VECTOR ( 12 downto 0 );
    BRAM_PORTA_clk : in STD_LOGIC;
    BRAM_PORTA_din : in STD_LOGIC_VECTOR ( 31 downto 0 );
    BRAM_PORTA_dout : out STD_LOGIC_VECTOR ( 31 downto 0 );
    BRAM_PORTA_en : in STD_LOGIC;
    BRAM_PORTA_rst : in STD_LOGIC;
    BRAM_PORTA_we : in STD_LOGIC_VECTOR ( 3 downto 0 );
    prog_we : out STD_LOGIC;
    prog_clk : out STD_LOGIC;
    prog_addr : out STD_LOGIC_VECTOR ( 9 downto 0 );
    prog_din : out STD_LOGIC_VECTOR ( 17 downto 0 )
  );
  end component zProg2Bram;
  signal ARM_wrapper_0_BRAM_PORTA_addr : STD_LOGIC_VECTOR ( 12 downto 0 );
  signal ARM_wrapper_0_BRAM_PORTA_clk : STD_LOGIC;
  signal ARM_wrapper_0_BRAM_PORTA_din : STD_LOGIC_VECTOR ( 31 downto 0 );
  signal ARM_wrapper_0_BRAM_PORTA_en : STD_LOGIC;
  signal ARM_wrapper_0_BRAM_PORTA_rst : STD_LOGIC;
  signal ARM_wrapper_0_BRAM_PORTA_we : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal ARM_wrapper_0_DDR_ADDR : STD_LOGIC_VECTOR ( 14 downto 0 );
  signal ARM_wrapper_0_DDR_BA : STD_LOGIC_VECTOR ( 2 downto 0 );
  signal ARM_wrapper_0_DDR_CAS_N : STD_LOGIC;
  signal ARM_wrapper_0_DDR_CKE : STD_LOGIC;
  signal ARM_wrapper_0_DDR_CK_N : STD_LOGIC;
  signal ARM_wrapper_0_DDR_CK_P : STD_LOGIC;
  signal ARM_wrapper_0_DDR_CS_N : STD_LOGIC;
  signal ARM_wrapper_0_DDR_DM : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal ARM_wrapper_0_DDR_DQ : STD_LOGIC_VECTOR ( 31 downto 0 );
  signal ARM_wrapper_0_DDR_DQS_N : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal ARM_wrapper_0_DDR_DQS_P : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal ARM_wrapper_0_DDR_ODT : STD_LOGIC;
  signal ARM_wrapper_0_DDR_RAS_N : STD_LOGIC;
  signal ARM_wrapper_0_DDR_RESET_N : STD_LOGIC;
  signal ARM_wrapper_0_DDR_WE_N : STD_LOGIC;
  signal FIXED_IO_1_DDR_VRN : STD_LOGIC;
  signal FIXED_IO_1_DDR_VRP : STD_LOGIC;
  signal FIXED_IO_1_MIO : STD_LOGIC_VECTOR ( 53 downto 0 );
  signal FIXED_IO_1_PS_CLK : STD_LOGIC;
  signal FIXED_IO_1_PS_PORB : STD_LOGIC;
  signal FIXED_IO_1_PS_SRSTB : STD_LOGIC;
  signal clk_1 : STD_LOGIC;
  signal zProg2Bram_0_BRAM_PORTA_dout : STD_LOGIC_VECTOR ( 31 downto 0 );
  signal zProg2Bram_0_prog_addr : STD_LOGIC_VECTOR ( 9 downto 0 );
  signal zProg2Bram_0_prog_clk : STD_LOGIC;
  signal zProg2Bram_0_prog_din : STD_LOGIC_VECTOR ( 17 downto 0 );
  signal zProg2Bram_0_prog_we : STD_LOGIC;
  signal zcpsmISP_0_aBus_in_port : STD_LOGIC_VECTOR ( 7 downto 0 );
  signal zcpsmISP_0_aBus_out_port : STD_LOGIC_VECTOR ( 7 downto 0 );
  signal zcpsmISP_0_aBus_port_ce : STD_LOGIC_VECTOR ( 15 downto 0 );
  signal zcpsmISP_0_aBus_port_id : STD_LOGIC_VECTOR ( 3 downto 0 );
  signal zcpsmISP_0_aBus_read_strobe : STD_LOGIC;
  signal zcpsmISP_0_aBus_write_strobe : STD_LOGIC;
begin
  aBus_out_port(7 downto 0) <= zcpsmISP_0_aBus_out_port(7 downto 0);
  aBus_port_ce(15 downto 0) <= zcpsmISP_0_aBus_port_ce(15 downto 0);
  aBus_port_id(3 downto 0) <= zcpsmISP_0_aBus_port_id(3 downto 0);
  aBus_read_strobe <= zcpsmISP_0_aBus_read_strobe;
  aBus_write_strobe <= zcpsmISP_0_aBus_write_strobe;
  clk_1 <= clk;
  zcpsmISP_0_aBus_in_port(7 downto 0) <= aBus_in_port(7 downto 0);
ARM_wrapper_0: component ARM_wrapper
    port map (
      BRAM_PORTA_addr(12 downto 0) => ARM_wrapper_0_BRAM_PORTA_addr(12 downto 0),
      BRAM_PORTA_clk => ARM_wrapper_0_BRAM_PORTA_clk,
      BRAM_PORTA_din(31 downto 0) => ARM_wrapper_0_BRAM_PORTA_din(31 downto 0),
      BRAM_PORTA_dout(31 downto 0) => zProg2Bram_0_BRAM_PORTA_dout(31 downto 0),
      BRAM_PORTA_en => ARM_wrapper_0_BRAM_PORTA_en,
      BRAM_PORTA_rst => ARM_wrapper_0_BRAM_PORTA_rst,
      BRAM_PORTA_we(3 downto 0) => ARM_wrapper_0_BRAM_PORTA_we(3 downto 0),
      DDR_addr(14 downto 0) => DDR_addr(14 downto 0),
      DDR_ba(2 downto 0) => DDR_ba(2 downto 0),
      DDR_cas_n => DDR_cas_n,
      DDR_ck_n => DDR_ck_n,
      DDR_ck_p => DDR_ck_p,
      DDR_cke => DDR_cke,
      DDR_cs_n => DDR_cs_n,
      DDR_dm(3 downto 0) => DDR_dm(3 downto 0),
      DDR_dq(31 downto 0) => DDR_dq(31 downto 0),
      DDR_dqs_n(3 downto 0) => DDR_dqs_n(3 downto 0),
      DDR_dqs_p(3 downto 0) => DDR_dqs_p(3 downto 0),
      DDR_odt => DDR_odt,
      DDR_ras_n => DDR_ras_n,
      DDR_reset_n => DDR_reset_n,
      DDR_we_n => DDR_we_n,
      FIXED_IO_ddr_vrn => FIXED_IO_ddr_vrn,
      FIXED_IO_ddr_vrp => FIXED_IO_ddr_vrp,
      FIXED_IO_mio(53 downto 0) => FIXED_IO_mio(53 downto 0),
      FIXED_IO_ps_clk => FIXED_IO_ps_clk,
      FIXED_IO_ps_porb => FIXED_IO_ps_porb,
      FIXED_IO_ps_srstb => FIXED_IO_ps_srstb
    );
zProg2Bram_0: component zProg2Bram
    port map (
      BRAM_PORTA_addr(12 downto 0) => ARM_wrapper_0_BRAM_PORTA_addr(12 downto 0),
      BRAM_PORTA_clk => ARM_wrapper_0_BRAM_PORTA_clk,
      BRAM_PORTA_din(31 downto 0) => ARM_wrapper_0_BRAM_PORTA_din(31 downto 0),
      BRAM_PORTA_dout(31 downto 0) => zProg2Bram_0_BRAM_PORTA_dout(31 downto 0),
      BRAM_PORTA_en => ARM_wrapper_0_BRAM_PORTA_en,
      BRAM_PORTA_rst => ARM_wrapper_0_BRAM_PORTA_rst,
      BRAM_PORTA_we(3 downto 0) => ARM_wrapper_0_BRAM_PORTA_we(3 downto 0),
      prog_addr(9 downto 0) => zProg2Bram_0_prog_addr(9 downto 0),
      prog_clk => zProg2Bram_0_prog_clk,
      prog_din(17 downto 0) => zProg2Bram_0_prog_din(17 downto 0),
      prog_we => zProg2Bram_0_prog_we
    );
zcpsmISP_0: component zcpsmISP
    port map (
      clk => clk_1,
      in_port(7 downto 0) => zcpsmISP_0_aBus_in_port(7 downto 0),
      out_port(7 downto 0) => zcpsmISP_0_aBus_out_port(7 downto 0),
      port_ce(15 downto 0) => zcpsmISP_0_aBus_port_ce(15 downto 0),
      port_id(3 downto 0) => zcpsmISP_0_aBus_port_id(3 downto 0),
      prog_addr(9 downto 0) => zProg2Bram_0_prog_addr(9 downto 0),
      prog_clk => zProg2Bram_0_prog_clk,
      prog_din(17 downto 0) => zProg2Bram_0_prog_din(17 downto 0),
      prog_we => zProg2Bram_0_prog_we,
      read_strobe => zcpsmISP_0_aBus_read_strobe,
      reset => ARM_wrapper_0_BRAM_PORTA_rst,
      write_strobe => zcpsmISP_0_aBus_write_strobe
    );
end STRUCTURE;
