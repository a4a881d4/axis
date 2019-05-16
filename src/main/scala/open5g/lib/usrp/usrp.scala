package open5g.lib.usrp

import spinal.core._
import spinal.lib._

case class axis(dt:Int,ut:Int) extends Bundle {
	val user = Bits(ut bits)
	val data = Bits(dt bits)
	val last = Bool
}

case class n3xx_chdr_eth_framer(	BASE : Int = 0,
	AWIDTH : Int = 8) extends BlackBox {
	val io = new Bundle {
		val mac_src = in Bits(48 bits)
		val ip_src = in Bits(32 bits)
		val IN = slave Stream(axis(64,4))
		val clk = in Bool
		val clear = in Bool
		val set_stb = in Bool
		val set_addr = in Bits(AWIDTH bits)
		val set_data = in Bits(32 bits)
		val OUT = master Stream(axis(64,4))
		val debug = out Bits(32 bits)
		val udp_src = in Bits(16 bits)
		val reset = in Bool
	}
	noIoPrefix()
}
/*
case class n3xx_core(	REG_DWIDTH : Int = 32,
	REG_AWIDTH : Int = 32,
	BUS_CLK_RATE : Int = 200000000,
	CHANNEL_WIDTH : Int = 32,
	NUM_RADIO_CORES : Int = 4,
	NUM_CHANNELS_PER_RADIO : Int = 1,
	NUM_CHANNELS : Int = 4,
	NUM_DBOARDS : Int = 2,
	NUM_SPI_PER_DBOARD : Int = 8,
	RADIO_NOC_ID : Int = 0,
	USE_CORRECTION : Int = 0,
	USE_REPLAY : Int = 0,
	FP_GPIO_WIDTH : Int = 12) extends BlackBox {
	val io = new Bundle {
		val miso_flat = in Bits(NUM_DBOARDS bits)
		val bus_clk = in Bool
		val bus_rst = in Bool
		val db_gpio_ddr_flat = out Bits(16*NUM_CHANNELS bits)
		val meas_clk_reset = out Reg(Bool)
		val meas_clk_locked = in Bool
		val fp_gpio_inout = in Bits(FP_GPIO_WIDTH bits)
		val radio_clk = in Bool
		val radio_rst = in Bool
		val e2v1 = slave Stream(Bundle{val data = Bits(64 bits);val last = Bool})
		val dmao = master Stream(Bundle{val data = Bits(64 bits);val last = Bool})
		val mosi_flat = out Bits(NUM_DBOARDS bits)
		val db_gpio_fab_flat = in Bits(16*NUM_CHANNELS bits)
		val db_gpio_in_flat = in Bits(16*NUM_CHANNELS bits)
		val v2e0 = master Stream(Bundle{val data = Bits(64 bits);val last = Bool})
		val ddr3_running = in Bool
		val reg_wr_data_npio = out Bits(REG_DWIDTH bits)
		val s_axi_aclk = in Bool
		val s_axi_aresetn = in Bool
		val s_axi_awaddr = in Bits(REG_AWIDTH bits)
		val s_axi_awvalid = in Bool
		val s_axi_awready = out Bool
		val s_axi_wdata = in Bits(REG_DWIDTH bits)
		val s_axi_wstrb = in Bits(REG_DWIDTH/8 bits)
		val s_axi_wvalid = in Bool
		val s_axi_wready = out Bool
		val s_axi_bresp = out Bits(2 bits)
		val s_axi_bvalid = out Bool
		val s_axi_bready = in Bool
		val s_axi_araddr = in Bits(REG_AWIDTH bits)
		val s_axi_arvalid = in Bool
		val s_axi_arready = out Bool
		val s_axi_rdata = out Bits(REG_DWIDTH bits)
		val s_axi_rresp = out Bits(2 bits)
		val s_axi_rvalid = out Bool
		val s_axi_rready = in Bool
		val build_datestamp = in Bits(32 bits)
		val ref_clk_reset = out Reg(Bool)
		val ref_clk_locked = in Bool
		val rx_atr = out Bits(NUM_CHANNELS bits)
		val rx_stb = in Bits(NUM_CHANNELS bits)
		val rx = in Bits(CHANNEL_WIDTH*NUM_CHANNELS bits)
		val reg_wr_addr_npio = out Bits(REG_AWIDTH bits)
		val reg_rd_data_npio = in Bits(REG_DWIDTH bits)
		val reg_rd_resp_npio = in Bool
		val reg_rd_req_npio = out Bool
		val db_gpio_out_flat = out Bits(16*NUM_CHANNELS bits)
		val ps_gpio_out = in Bits(FP_GPIO_WIDTH bits)
		val ps_gpio_tri = in Bits(FP_GPIO_WIDTH bits)
		val ps_gpio_in = out Bits(FP_GPIO_WIDTH bits)
		val xadc_readback = in Bits(32 bits)
		val ddr3_axi_clk = in Bool
		val ddr3_axi_rst = in Bool
		val ddr3_axi_awid = out Bits(4 bits)
		val ddr3_axi_awaddr = out Bits(32 bits)
		val ddr3_axi_awlen = out Bits(8 bits)
		val ddr3_axi_awsize = out Bits(3 bits)
		val ddr3_axi_awburst = out Bits(2 bits)
		val ddr3_axi_awlock = out Bits(1 bits)
		val ddr3_axi_awcache = out Bits(4 bits)
		val ddr3_axi_awprot = out Bits(3 bits)
		val ddr3_axi_awqos = out Bits(4 bits)
		val ddr3_axi_awvalid = out Bool
		val ddr3_axi_awready = in Bool
		val ddr3_axi_wdata = out Bits(256 bits)
		val ddr3_axi_wstrb = out Bits(32 bits)
		val ddr3_axi_wlast = out Bool
		val ddr3_axi_wvalid = out Bool
		val ddr3_axi_wready = in Bool
		val ddr3_axi_bready = out Bool
		val ddr3_axi_bid = in Bits(4 bits)
		val ddr3_axi_bresp = in Bits(2 bits)
		val ddr3_axi_bvalid = in Bool
		val ddr3_axi_arid = out Bits(4 bits)
		val ddr3_axi_araddr = out Bits(32 bits)
		val ddr3_axi_arlen = out Bits(8 bits)
		val ddr3_axi_arsize = out Bits(3 bits)
		val ddr3_axi_arburst = out Bits(2 bits)
		val ddr3_axi_arlock = out Bits(1 bits)
		val ddr3_axi_arcache = out Bits(4 bits)
		val ddr3_axi_arprot = out Bits(3 bits)
		val ddr3_axi_arqos = out Bits(4 bits)
		val ddr3_axi_arvalid = out Bool
		val ddr3_axi_arready = in Bool
		val ddr3_axi_rready = out Bool
		val ddr3_axi_rid = in Bits(4 bits)
		val ddr3_axi_rdata = in Bits(256 bits)
		val ddr3_axi_rresp = in Bits(2 bits)
		val ddr3_axi_rlast = in Bool
		val ddr3_axi_rvalid = in Bool
		val reg_wr_req_npio = out Bool
		val dmai = slave Stream(Bundle{val data = Bits(64 bits);val last = Bool})
		val pps = in Bool
		val pps_select = out Reg(Bits(4 bits))
		val sen_flat = out Bits(NUM_SPI_PER_DBOARD*NUM_DBOARDS bits)
		val pps_select_sfp = out Reg(Bits(2 bits))
		val ddr3_dma_clk = in Bool
		val sfp_ports_info = in Bits(64 bits)
		val sclk_flat = out Bits(NUM_DBOARDS bits)
		val reg_rd_addr_npio = out Bits(REG_AWIDTH bits)
		val pps_out_enb = out Reg(Bool)
		val tx_atr = out Bits(NUM_CHANNELS bits)
		val tx_stb = in Bits(NUM_CHANNELS bits)
		val tx = out Bits(CHANNEL_WIDTH*NUM_CHANNELS bits)
		val v2e1 = master Stream(Bundle{val data = Bits(64 bits);val last = Bool})
		val e2v0 = slave Stream(Bundle{val data = Bits(64 bits);val last = Bool})
		val enable_ref_clk_async = out Reg(Bool)
	}
	noIoPrefix()
}

case class n3xx_db_fe_core(	USE_SPI_CLK : Int = 0,
	SR_DB_FE_BASE : Int = 160,
	RB_DB_FE_BASE : Int = 16,
	WIDTH : Int = 32,
	NUM_SPI_SEN : Int = 8,
	USE_CORRECTION : Int = 0) extends BlackBox {
	val io = new Bundle {
		val miso = in Bool
		val clk = in Bool
		val fp_gpio_in = in Bits(32 bits)
		val fp_gpio_out = out Bits(32 bits)
		val fp_gpio_ddr = out Bits(32 bits)
		val fp_gpio_fab = in Bits(32 bits)
		val db_gpio_in = in Bits(32 bits)
		val db_gpio_out = out Bits(32 bits)
		val db_gpio_ddr = out Bits(32 bits)
		val db_gpio_fab = in Bits(32 bits)
		val mosi = out Bool
		val set_stb = in Bool
		val set_addr = in Bits(8 bits)
		val set_data = in Bits(32 bits)
		val rb_stb = out Bool
		val rb_addr = in Bits(8 bits)
		val rb_data = out Bits(64 bits)
		val rx_stb = in Bool
		val rx_running = in Bool
		val rx_data_in = in Bits(WIDTH bits)
		val rx_data_out = out Bits(WIDTH bits)
		val leds = out Bits(32 bits)
		val tx_data_in = in Bits(WIDTH bits)
		val tx_data_out = out Bits(WIDTH bits)
		val spi_clk = in Bool
		val spi_rst = in Bool
		val misc_ins = in Bits(32 bits)
		val misc_outs = out Bits(32 bits)
		val reset = in Bool
		val sen = out Bits(NUM_SPI_SEN bits)
		val time_sync = in Bool
		val sclk = out Bool
		val tx_stb = in Bool
		val tx_running = in Bool
	}
	noIoPrefix()
}

case class n3xx_eth_dispatch(	BASE : Int = 0,
	DROP_UNKNOWN_MAC : Int = 1,
	AWIDTH : Int = 8) extends BlackBox {
	val io = new Bundle {
		val in = slave Stream(Bundle{val data = Bits(64 bits);val last = Bool; val user = Bits(4 bits)})
		val clk = in Bool
		val clear = in Bool
		val eth_mac = in Bits(48 bits)
		val set_stb = in Bool
		val set_addr = in Bits(AWIDTH bits)
		val set_data = in Bits(32 bits)
		val bridge_mac = in Bits(48 bits)
		val xo = master Stream(Bundle{val data = Bits(64 bits);val last = Bool; val user = Bits(4 bits)})
		val debug_flags = out Bits(3 bits)
		val debug = out Bits(32 bits)
		val my_ip = in Bits(32 bits)
		val my_port0 = in Bits(16 bits)
		val my_port1 = in Bits(16 bits)
		val cpu = master Stream(Bundle{val data = Bits(64 bits);val last = Bool; val user = Bits(4 bits)})
		val reset = in Bool
		val vita = master Stream(Bundle{val data = Bits(64 bits);val last = Bool})
	}
	noIoPrefix()
}

case class n3xx_eth_switch(	BASE : Int = 0,
	XO_FIFOSIZE : Int = 1,
	CPU_FIFOSIZE : Int = 10,
	VITA_FIFOSIZE : Int = 11,
	ETHOUT_FIFOSIZE : Int = 1,
	REG_DWIDTH : Int = 32,
	REG_AWIDTH : Int = 14,
	DEFAULT_MAC_ADDR : Int = 0,
	DEFAULT_IP_ADDR : Int = 0,
	DEFAULT_UDP_PORTS : Int = 0) extends BlackBox {
	val io = new Bundle {
		val v2e = slave Stream(Bundle{val data = Bits(64 bits);val last = Bool})
		val clk = in Bool
		val clear = in Bool
		val reg_wr_req = in Bool
		val reg_wr_addr = in Bits(REG_AWIDTH bits)
		val reg_wr_data = in Bits(REG_DWIDTH bits)
		val reg_wr_keep = in Bits(REG_DWIDTH/8 bits)
		val xo = master Stream(Bundle{val data = Bits(64 bits);val last = Bool; val user = Bits(4 bits)})
		val debug = out Bits(32 bits)
		val e2c = master Stream(Bundle{val data = Bits(64 bits);val last = Bool; val user = Bits(4 bits)})
		val eth_tx = master Stream(Bundle{val data = Bits(64 bits);val last = Bool; val user = Bits(4 bits)})
		val c2e = slave Stream(Bundle{val data = Bits(64 bits);val last = Bool; val user = Bits(4 bits)})
		val reset = in Bool
		val e2v = master Stream(Bundle{val data = Bits(64 bits);val last = Bool})
		val eth_rx = slave Stream(Bundle{val data = Bits(64 bits);val last = Bool; val user = Bits(4 bits)})
		val xi = slave Stream(Bundle{val data = Bits(64 bits);val last = Bool; val user = Bits(4 bits)})
		val reg_rd_req = in Bool
		val reg_rd_addr = in Bits(REG_AWIDTH bits)
		val reg_rd_resp = out Reg(Bool)
		val reg_rd_data = out Reg(Bits(REG_DWIDTH bits))
	}
	noIoPrefix()
}

case class n3xx_mgt_channel_wrapper(	PROTOCOL : Int = 10,
	LANES : Int = 2,
	REG_BASE : Int = 0,
	PORTNUM_BASE : Int = 4,
	MDIO_EN : Int = 1,
	MDIO_PHYADDR : Int = 0,
	REG_DWIDTH : Int = 32,
	REG_AWIDTH : Int = 14,
	GT_COMMON : Int = 1) extends BlackBox {
	val io = new Bundle {
		val rxn = in Bits(LANES bits)
		val v2e = slave Stream(Bundle{val data = Bits((((~LANES*64)~)) bits);val last = Bool})
		val gt_tx_out_clk_unbuf = out Bool
		val bus_rst = in Bool
		val bus_clk = in Bool
		val wr_reset_n = in Bool
		val wr_refclk = in Bool
		val mod_rxlos = in Bool
		val mod_lpmode = out Bool
		val mod_pps = out Bool
		val mod_refclk = out Bool
		val mmcm_locked = in Bool
		val wr_dac_clr_n = out Bool
		val s_axi_aclk = in Bool
		val s_axi_aresetn = in Bool
		val s_axi_awaddr = in Bits(REG_AWIDTH bits)
		val s_axi_awvalid = in Bool
		val s_axi_awready = out Bool
		val s_axi_wdata = in Bits(REG_DWIDTH bits)
		val s_axi_wstrb = in Bits(REG_DWIDTH/8 bits)
		val s_axi_wvalid = in Bool
		val s_axi_wready = out Bool
		val s_axi_bresp = out Bits(2 bits)
		val s_axi_bvalid = out Bool
		val s_axi_bready = in Bool
		val s_axi_araddr = in Bits(REG_AWIDTH bits)
		val s_axi_arvalid = in Bool
		val s_axi_arready = out Bool
		val s_axi_rdata = out Bits(REG_DWIDTH bits)
		val s_axi_rresp = out Bits(2 bits)
		val s_axi_rvalid = out Bool
		val s_axi_rready = in Bool
		val xo = master Stream(Bundle{val data = Bits(LANES*64 bits);val last = Bool; val user = Bits(LANES*4 bits)})
		val wr_axi_aclk = out Bool
		val wr_axi_aresetn = in Bool
		val wr_axi_awaddr = in Bits(32 bits)
		val wr_axi_awvalid = in Bool
		val wr_axi_awready = out Bool
		val wr_axi_wdata = in Bits(REG_DWIDTH bits)
		val wr_axi_wstrb = in Bits(REG_DWIDTH/8 bits)
		val wr_axi_wvalid = in Bool
		val wr_axi_wready = out Bool
		val wr_axi_bresp = out Bits(2 bits)
		val wr_axi_bvalid = out Bool
		val wr_axi_bready = in Bool
		val wr_axi_araddr = in Bits(32 bits)
		val wr_axi_arvalid = in Bool
		val wr_axi_arready = out Bool
		val wr_axi_rdata = out Bits(REG_DWIDTH bits)
		val wr_axi_rresp = out Bits(2 bits)
		val wr_axi_rvalid = out Bool
		val wr_axi_rready = in Bool
		val wr_axi_rlast = out Bool
		val txp = out Bits(LANES bits)
		val mod_reset_n = in Bool
		val qpllrefclklost = in Bool
		val e2c = master Stream(Bundle{val data = Bits(LANES*64 bits);val last = Bool})
		val mod_sel_n = out Bool
		val wr_eeprom_scl_o = out Bool
		val wr_eeprom_scl_i = in Bool
		val port_info = out Bits(LANES*32 bits)
		val qplloutclk = in Bool
		val wr_dac_sclk = out Bool
		val wr_dac_din = out Bool
		val link_up = out Bits(LANES bits)
		val sync_clk = in Bool
		val misc_clk = in Bool
		val c2e = slave Stream(Bundle{val data = Bits(LANES*64 bits);val last = Bool})
		val wr_uart_rx = in Bool
		val wr_uart_tx = out Bool
		val qplloutrefclk = in Bool
		val e2v = master Stream(Bundle{val data = Bits((((~LANES*64)~)) bits);val last = Bool})
		val mod_tx_fault = in Bool
		val mod_tx_disable = out Bool
		val wr_eeprom_sda_o = out Bool
		val wr_eeprom_sda_i = in Bool
		val xi = slave Stream(Bundle{val data = Bits(LANES*64 bits);val last = Bool; val user = Bits(LANES*4 bits)})
		val mod_present_n = in Bool
		val activity = out Bits(LANES bits)
		val qpllreset = out Bool
		val txn = out Bits(LANES bits)
		val gb_refclk = in Bool
		val mod_int_n = in Bool
		val gt_refclk = in Bool
		val gt_pll_lock = out Bool
		val qplllock = in Bool
		val user_clk = in Bool
		val wr_dac_cs_n = out Bool
		val rxp = in Bits(LANES bits)
		val areset = in Bool
		val wr_dac_ldac_n = out Bool
	}
	noIoPrefix()
}

case class n3xx_mgt_io_core(	PROTOCOL : Int = 10,
	REG_BASE : Int = 0,
	REG_DWIDTH : Int = 32,
	REG_AWIDTH : Int = 14,
	GT_COMMON : Int = 1,
	PORTNUM : Int = 0,
	MDIO_EN : Int = 0,
	MDIO_PHYADDR : Int = 0) extends BlackBox {
	val io = new Bundle {
		val rxn = in Bool
		val gt_tx_out_clk_unbuf = out Bool
		val bus_rst = in Bool
		val bus_clk = in Bool
		val sfpp_tx_fault = in Bool
		val sfpp_tx_disable = out Bool
		val mmcm_locked = in Bool
		val reg_wr_req = in Bool
		val reg_wr_addr = in Bits(REG_AWIDTH bits)
		val reg_wr_data = in Bits(REG_DWIDTH bits)
		val txp = out Bool
		val qpllrefclklost = in Bool
		val s_axis = slave Stream(Bundle{val data = Bits(64 bits);val last = Bool; val user = Bits(4 bits)})
		val port_info = out Bits(32 bits)
		val qplloutclk = in Bool
		val link_up = out Bool
		val sfpp_rxlos = in Bool
		val sync_clk = in Bool
		val misc_clk = in Bool
		val m_axis = master Stream(Bundle{val data = Bits(64 bits);val last = Bool; val user = Bits(4 bits)})
		val qplloutrefclk = in Bool
		val reg_rd_req = in Bool
		val reg_rd_addr = in Bits(REG_AWIDTH bits)
		val reg_rd_resp = out Bool
		val reg_rd_data = out Bits(REG_DWIDTH bits)
		val activity = out Reg(Bool)
		val qpllreset = out Bool
		val txn = out Bool
		val gb_refclk = in Bool
		val gt_refclk = in Bool
		val gt_pll_lock = out Bool
		val qplllock = in Bool
		val user_clk = in Bool
		val rxp = in Bool
		val areset = in Bool
	}
	noIoPrefix()
}

case class n3xx_mgt_wrapper(	PROTOCOL : Int = 10,
	REG_DWIDTH : Int = 32,
	REG_AWIDTH : Int = 14,
	GT_COMMON : Int = 1,
	PORTNUM : Int = 0,
	MDIO_EN : Int = 0,
	MDIO_PHYADDR : Int = 0,
	REG_BASE : Int = 0) extends BlackBox {
	val io = new Bundle {
		val rxn = in Bool
		val v2e = slave Stream(Bundle{val data = Bits(64 bits);val last = Bool})
		val gt_tx_out_clk_unbuf = out Bool
		val bus_rst = in Bool
		val bus_clk = in Bool
		val wr_reset_n = in Bool
		val wr_refclk = in Bool
		val mod_rxlos = in Bool
		val mod_pps = out Bool
		val mod_refclk = out Bool
		val mmcm_locked = in Bool
		val wr_dac_clr_n = out Bool
		val reg_wr_req = in Bool
		val reg_wr_addr = in Bits(REG_AWIDTH bits)
		val reg_wr_data = in Bits(REG_DWIDTH bits)
		val xo = master Stream(Bundle{val data = Bits(64 bits);val last = Bool; val user = Bits(4 bits)})
		val wr_axi_aclk = out Bool
		val wr_axi_aresetn = in Bool
		val wr_axi_awaddr = in Bits(32 bits)
		val wr_axi_awvalid = in Bool
		val wr_axi_awready = out Bool
		val wr_axi_wdata = in Bits(REG_DWIDTH bits)
		val wr_axi_wstrb = in Bits(REG_DWIDTH/8 bits)
		val wr_axi_wvalid = in Bool
		val wr_axi_wready = out Bool
		val wr_axi_bresp = out Bits(2 bits)
		val wr_axi_bvalid = out Bool
		val wr_axi_bready = in Bool
		val wr_axi_araddr = in Bits(32 bits)
		val wr_axi_arvalid = in Bool
		val wr_axi_arready = out Bool
		val wr_axi_rdata = out Bits(REG_DWIDTH bits)
		val wr_axi_rresp = out Bits(2 bits)
		val wr_axi_rvalid = out Bool
		val wr_axi_rready = in Bool
		val wr_axi_rlast = out Bool
		val txp = out Bool
		val qpllrefclklost = in Bool
		val e2c = master Stream(Bundle{val data = Bits(64 bits);val last = Bool})
		val wr_eeprom_scl_o = out Bool
		val wr_eeprom_scl_i = in Bool
		val port_info = out Bits(32 bits)
		val qplloutclk = in Bool
		val wr_dac_sclk = out Bool
		val wr_dac_din = out Bool
		val link_up = out Bool
		val sync_clk = in Bool
		val misc_clk = in Bool
		val c2e = slave Stream(Bundle{val data = Bits(64 bits);val last = Bool})
		val wr_uart_rx = in Bool
		val wr_uart_tx = out Bool
		val qplloutrefclk = in Bool
		val e2v = master Stream(Bundle{val data = Bits(64 bits);val last = Bool})
		val mod_tx_fault = in Bool
		val mod_tx_disable = out Bool
		val wr_eeprom_sda_o = out Bool
		val wr_eeprom_sda_i = in Bool
		val xi = slave Stream(Bundle{val data = Bits(64 bits);val last = Bool; val user = Bits(4 bits)})
		val reg_rd_req = in Bool
		val reg_rd_addr = in Bits(REG_AWIDTH bits)
		val reg_rd_resp = out Bool
		val reg_rd_data = out Bits(REG_DWIDTH bits)
		val mod_present_n = in Bool
		val activity = out Bool
		val qpllreset = out Bool
		val txn = out Bool
		val gb_refclk = in Bool
		val gt_refclk = in Bool
		val gt_pll_lock = out Bool
		val qplllock = in Bool
		val user_clk = in Bool
		val wr_dac_cs_n = out Bool
		val rxp = in Bool
		val areset = in Bool
		val wr_dac_ldac_n = out Bool
	}
	noIoPrefix()
}

*/