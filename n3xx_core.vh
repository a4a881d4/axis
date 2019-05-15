module n3xx_core #(
  parameter REG_DWIDTH  = 32, // Width of the AXI4-Lite data bus (must be 32 or 64)
  parameter REG_AWIDTH  = 32,  // Width of the address bus
  parameter BUS_CLK_RATE = 200000000, // BUS_CLK rate
  parameter CHANNEL_WIDTH = 32,
  parameter NUM_RADIO_CORES = 4,
  parameter NUM_CHANNELS_PER_RADIO = 1,
  parameter NUM_CHANNELS = 4,
  parameter NUM_DBOARDS = 2,
  parameter NUM_SPI_PER_DBOARD = 8,
  parameter RADIO_NOC_ID = 0, //64'h12AD_1000_0001_1310 + NUM_CHANNELS_PER_RADIO,
  parameter USE_CORRECTION = 0,
  parameter USE_REPLAY = 0,    // 1 for Replay block instead of DMA FIFO
  parameter FP_GPIO_WIDTH = 12 // Front panel GPIO width
)(
  // Clocks and resets
  input         radio_clk,
  input         radio_rst,
  input         bus_clk,
  input         bus_rst,
  input         ddr3_dma_clk,

  // Clocking and PPS Controls/Indicators
  input            pps,
  output reg[3:0]  pps_select,
  output reg       pps_out_enb,
  output reg[1:0]  pps_select_sfp,
  output reg       ref_clk_reset,
  output reg       meas_clk_reset,
  input            ref_clk_locked,
  input            meas_clk_locked,
  output reg       enable_ref_clk_async,

  // AXI lite interface
  input                    s_axi_aclk,
  input                    s_axi_aresetn,
  input [REG_AWIDTH-1:0]   s_axi_awaddr,
  input                    s_axi_awvalid,
  output                   s_axi_awready,

  input [REG_DWIDTH-1:0]   s_axi_wdata,
  input [REG_DWIDTH/8-1:0] s_axi_wstrb,
  input                    s_axi_wvalid,
  output                   s_axi_wready,

  output [1:0]             s_axi_bresp,
  output                   s_axi_bvalid,
  input                    s_axi_bready,

  input [REG_AWIDTH-1:0]   s_axi_araddr,
  input                    s_axi_arvalid,
  output                   s_axi_arready,

  output [REG_DWIDTH-1:0]  s_axi_rdata,
  output [1:0]             s_axi_rresp,
  output                   s_axi_rvalid,
  input                    s_axi_rready,

  // PS GPIO source
  input  [FP_GPIO_WIDTH-1:0]  ps_gpio_out,
  input  [FP_GPIO_WIDTH-1:0]  ps_gpio_tri,
  output [FP_GPIO_WIDTH-1:0]  ps_gpio_in,

  // Front Panel GPIO
  inout  [FP_GPIO_WIDTH-1:0] fp_gpio_inout,

  // Radio GPIO control for DSA
  output [16*NUM_CHANNELS-1:0] db_gpio_out_flat,
  output [16*NUM_CHANNELS-1:0] db_gpio_ddr_flat,
  input  [16*NUM_CHANNELS-1:0] db_gpio_in_flat,
  input  [16*NUM_CHANNELS-1:0] db_gpio_fab_flat,

  // Radio ATR
  output [NUM_CHANNELS-1:0] rx_atr,
  output [NUM_CHANNELS-1:0] tx_atr,

  // Radio Data
  input  [NUM_CHANNELS-1:0]    rx_stb,
  input  [NUM_CHANNELS-1:0]    tx_stb,
  input  [CHANNEL_WIDTH*NUM_CHANNELS-1:0] rx,
  output [CHANNEL_WIDTH*NUM_CHANNELS-1:0] tx,

  // CPLD
  output [NUM_SPI_PER_DBOARD*NUM_DBOARDS-1:0] sen_flat,
  output [NUM_DBOARDS-1:0]   sclk_flat,
  output [NUM_DBOARDS-1:0]   mosi_flat,
  input  [NUM_DBOARDS-1:0]   miso_flat,

  // DMA
  output [63:0] dmao_tdata,
  output        dmao_tlast,
  output        dmao_tvalid,
  input         dmao_tready,

  input [63:0]  dmai_tdata,
  input         dmai_tlast,
  input         dmai_tvalid,
  output        dmai_tready,

  // AXI4 (256b@200MHz) interface to DDR3 controller
  input          ddr3_axi_clk,
  input          ddr3_axi_rst,
  input          ddr3_running,
  // Write Address Ports
  output [3:0]   ddr3_axi_awid,
  output [31:0]  ddr3_axi_awaddr,
  output [7:0]   ddr3_axi_awlen,
  output [2:0]   ddr3_axi_awsize,
  output [1:0]   ddr3_axi_awburst,
  output [0:0]   ddr3_axi_awlock,
  output [3:0]   ddr3_axi_awcache,
  output [2:0]   ddr3_axi_awprot,
  output [3:0]   ddr3_axi_awqos,
  output         ddr3_axi_awvalid,
  input          ddr3_axi_awready,
  // Write Data Ports
  output [255:0] ddr3_axi_wdata,
  output [31:0]  ddr3_axi_wstrb,
  output         ddr3_axi_wlast,
  output         ddr3_axi_wvalid,
  input          ddr3_axi_wready,
  // Write Response Ports
  output         ddr3_axi_bready,
  input [3:0]    ddr3_axi_bid,
  input [1:0]    ddr3_axi_bresp,
  input          ddr3_axi_bvalid,
  // Read Address Ports
  output [3:0]   ddr3_axi_arid,
  output [31:0]  ddr3_axi_araddr,
  output [7:0]   ddr3_axi_arlen,
  output [2:0]   ddr3_axi_arsize,
  output [1:0]   ddr3_axi_arburst,
  output [0:0]   ddr3_axi_arlock,
  output [3:0]   ddr3_axi_arcache,
  output [2:0]   ddr3_axi_arprot,
  output [3:0]   ddr3_axi_arqos,
  output         ddr3_axi_arvalid,
  input          ddr3_axi_arready,
  // Read Data Ports
  output         ddr3_axi_rready,
  input [3:0]    ddr3_axi_rid,
  input [255:0]  ddr3_axi_rdata,
  input [1:0]    ddr3_axi_rresp,
  input          ddr3_axi_rlast,
  input          ddr3_axi_rvalid,

  // v2e (vita to ethernet) and e2v (eth to vita)
  output [63:0] v2e0_tdata,
  output        v2e0_tvalid,
  output        v2e0_tlast,
  input         v2e0_tready,

  output [63:0] v2e1_tdata,
  output        v2e1_tlast,
  output        v2e1_tvalid,
  input         v2e1_tready,

  input  [63:0] e2v0_tdata,
  input         e2v0_tlast,
  input         e2v0_tvalid,
  output        e2v0_tready,

  input  [63:0] e2v1_tdata,
  input         e2v1_tlast,
  input         e2v1_tvalid,
  output        e2v1_tready,

  // RegPort interface to NPIO
  output                  reg_wr_req_npio,
  output [REG_AWIDTH-1:0] reg_wr_addr_npio,
  output [REG_DWIDTH-1:0] reg_wr_data_npio,
  output                  reg_rd_req_npio,
  output [REG_AWIDTH-1:0] reg_rd_addr_npio,
  input                   reg_rd_resp_npio,
  input  [REG_DWIDTH-1:0] reg_rd_data_npio,

  // Misc
  input  [31:0]   build_datestamp,
  input  [31:0]   xadc_readback,
  input  [63:0]   sfp_ports_info
);