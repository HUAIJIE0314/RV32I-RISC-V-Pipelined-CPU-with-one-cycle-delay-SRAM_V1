module Stall_Detection(
  rs1_addr_ID_i,
  rs2_addr_ID_i,
  Branch_ID_i,
  Jalr_ID_i,
  rd_addr_EX_i,
  RegWrite_EX_i,
  MemRead_EX_i,
  rd_addr_ME_i,
  MemRead_ME_i,
  stall_o
);
//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic [4:0] rs1_addr_ID_i;
input  logic [4:0] rs2_addr_ID_i;
input  logic         Branch_ID_i;
input  logic           Jalr_ID_i;
input  logic [4:0]  rd_addr_EX_i;
input  logic       RegWrite_EX_i;
input  logic        MemRead_EX_i;
input  logic [4:0]  rd_addr_ME_i;
input  logic        MemRead_ME_i;
output logic             stall_o;

//---------------------------------------------------------------------
//        ALWAYS BLOCK                             
//---------------------------------------------------------------------

always_comb begin
  if( RegWrite_EX_i && (Branch_ID_i|Jalr_ID_i) && (rd_addr_EX_i == rs1_addr_ID_i || rd_addr_EX_i == rs2_addr_ID_i) )begin//Rtype-Branch
    stall_o = 1'b1;
  end
  else if( MemRead_EX_i && RegWrite_EX_i && (rd_addr_EX_i == rs1_addr_ID_i || rd_addr_EX_i == rs2_addr_ID_i) )begin//Load-(Rtype or Branch)
    stall_o = 1'b1;
  end
  else if( MemRead_ME_i && (Branch_ID_i|Jalr_ID_i) && (rd_addr_ME_i == rs1_addr_ID_i || rd_addr_ME_i == rs2_addr_ID_i) )begin//Load-Branch
    stall_o = 1'b1;
  end
  else begin
    stall_o = 1'b0;
  end
end


endmodule