module Forwarding_Unit(
  rs1_addr_ID_i,
  rs2_addr_ID_i,
  rs1_addr_EX_i,
  rs2_addr_EX_i,
  RegWrite_ME_i,
  rd_addr_ME_i,
  RegWrite_WB_i,
  rd_addr_WB_i,
  FWsrcAsel_o,
  FWsrcBsel_o,
  FWCompsrcAsel_o,
  FWCompsrcBsel_o,
  FWJalrSel_o
);

//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic  [4:0]   rs1_addr_ID_i;
input  logic  [4:0]   rs2_addr_ID_i;
input  logic  [4:0]   rs1_addr_EX_i;
input  logic  [4:0]   rs2_addr_EX_i;
input  logic          RegWrite_ME_i;
input  logic  [4:0]    rd_addr_ME_i;
input  logic          RegWrite_WB_i;
input  logic  [4:0]    rd_addr_WB_i;
output logic  [1:0]     FWsrcAsel_o;
output logic  [1:0]     FWsrcBsel_o;
output logic        FWCompsrcAsel_o;
output logic        FWCompsrcBsel_o;
output logic            FWJalrSel_o;
//---------------------------------------------------------------------
//        ALWAYS BLOCK                             
//---------------------------------------------------------------------

// << FWsrcAsel >>
always_comb begin
  if(RegWrite_ME_i && rd_addr_ME_i != 5'd0 && rd_addr_ME_i == rs1_addr_EX_i)FWsrcAsel_o = 2'd1;
  else if(RegWrite_WB_i && rd_addr_WB_i != 5'd0 && rd_addr_WB_i == rs1_addr_EX_i)FWsrcAsel_o = 2'd2;
  else FWsrcAsel_o = 2'd0;
end

// << FWsrcBsel >>
always_comb begin
  if(RegWrite_ME_i && rd_addr_ME_i != 5'd0 && rd_addr_ME_i == rs2_addr_EX_i)FWsrcBsel_o = 2'd1;
  else if(RegWrite_WB_i && rd_addr_WB_i != 5'd0 && rd_addr_WB_i == rs2_addr_EX_i)FWsrcBsel_o = 2'd2;
  else FWsrcBsel_o = 2'd0;
end

// << FWCompsrcAsel >>
always_comb begin
  if(RegWrite_ME_i && rd_addr_ME_i != 5'd0 && rd_addr_ME_i == rs1_addr_ID_i)FWCompsrcAsel_o = 1'b1;
  else FWCompsrcAsel_o = 1'b0;
end

// << FWCompsrcBsel >>
always_comb begin
  if(RegWrite_ME_i && rd_addr_ME_i != 5'd0 && rd_addr_ME_i == rs2_addr_ID_i)FWCompsrcBsel_o = 1'b1;
  else FWCompsrcBsel_o = 1'b0;
end

// << FWJalrSel >>
always_comb begin
  if(RegWrite_ME_i && rd_addr_ME_i != 5'd0 && rd_addr_ME_i == rs1_addr_ID_i)FWJalrSel_o = 1'b1;
  else FWJalrSel_o = 1'b0;
end

endmodule

