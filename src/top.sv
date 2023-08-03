`include "CPU.sv"
`include "define.svh"
`include "../SRAM/SRAM_wrapper.sv"
`include "../SRAM/SRAM_rtl.sv"

module top(
  clk,
  rst
);
//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input logic clk;
input logic rst;
//---------------------------------------------------------------------
//        LOGIC DECLARATION                             
//---------------------------------------------------------------------
// << IM Ports >> 
logic [$clog2(`IM_DEPTH)-1:0]  IM_A;
logic [`DATA_WIDTH-1:0]       IM_DO;
// << DM Ports >> 
logic                         DM_OE;
logic [$clog2(`DM_DEPTH)-1:0]  DM_A;
logic [`DATA_WIDTH-1:0]       DM_DO;
logic [3:0]                  DM_WEB;
logic [`DATA_WIDTH-1:0]       DM_DI;
//---------------------------------------------------------------------
//        MODULE INSTANTIATION                             
//---------------------------------------------------------------------
// << Single Cycle CPU >>
CPU CPU(
  .clk   (clk   ),// clock signal
  .rst   (rst   ),// activate HIGH reset signal
  .IM_A  (IM_A  ),// Instruction Memory Read Address
  .IM_DO (IM_DO ),// Instruction Memory Read Data
  .DM_OE (DM_OE ),// Data Memory Read Enable
  .DM_A  (DM_A  ),// Data Memory Read/Write Address 
  .DM_DO (DM_DO ),// Data Memory Read Data
  .DM_WEB(DM_WEB),// Data Memory Write Enable (activate LOW)
  .DM_DI (DM_DI ) // Data Memory Write Data
);
// << Instruction Memory - 64KB >>
SRAM_wrapper IM1(
  .CK (clk   ),// SRAM clock
  .CS (1'b1  ),// SRAM chip Select   (activate HIGH)
  .OE (1'b1  ),// SRAM output enable (activate HIGH)
  .WEB(4'hf  ),// SRAM write enable  (activate LOW)
  .A  (IM_A  ),// SRAM address
  .DI (32'd0 ),// SRAM write data
  .DO (IM_DO ) // SRAM read data
);
// << Data Memory - 64KB >>
SRAM_wrapper DM1(
  .CK (clk   ),// SRAM clock
  .CS (1'b1  ),// SRAM chip Select   (activate HIGH)
  .OE (DM_OE ),// SRAM output enable (activate HIGH)
  .WEB(DM_WEB),// SRAM write enable  (activate LOW)
  .A  (DM_A  ),// SRAM address
  .DI (DM_DI ),// SRAM write data
  .DO (DM_DO ) // SRAM read data
);

endmodule
