module Register_File(
  clk_i,
  rst_i,
  rs1_addr_i,
  rs2_addr_i,
  rd_addr_i,
  rd_data_i,
  RegWrite_i,
  rs1_data_o,
  rs2_data_o
);
//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic                               clk_i;
input  logic                               rst_i;
input  logic [$clog2(`REG_DEPTH)-1:0] rs1_addr_i;
input  logic [$clog2(`REG_DEPTH)-1:0] rs2_addr_i;
input  logic [$clog2(`REG_DEPTH)-1:0]  rd_addr_i;
input  logic [`DATA_WIDTH-1:0]         rd_data_i;
input  logic                          RegWrite_i;
output logic [`DATA_WIDTH-1:0]        rs1_data_o;
output logic [`DATA_WIDTH-1:0]        rs2_data_o;
//---------------------------------------------------------------------
//        LOGIC & VARIABLES DECLARATION                             
//---------------------------------------------------------------------
logic [`DATA_WIDTH-1:0] Register [0:`REG_DEPTH-1];
integer i;
//---------------------------------------------------------------------
//        WIRE CONNECTION                             
//---------------------------------------------------------------------
assign rs1_data_o = (RegWrite_i && rd_addr_i==rs1_addr_i)?(rd_data_i):(Register[rs1_addr_i]);
assign rs2_data_o = (RegWrite_i && rd_addr_i==rs2_addr_i)?(rd_data_i):(Register[rs2_addr_i]);
//---------------------------------------------------------------------
//        ALWAYS BLOCK                             
//---------------------------------------------------------------------
always_ff@(posedge clk_i or posedge rst_i)begin
  if(rst_i)begin
    for(i=0;i<`REG_DEPTH;i=i+1)Register[i] <= {`DATA_WIDTH{1'b0}};
  end
  else begin
    unique if(RegWrite_i && rd_addr_i != {($clog2(`REG_DEPTH)){1'b0}} )begin
      Register[rd_addr_i] <= rd_data_i;
    end
    else begin
      Register[rd_addr_i] <= Register[rd_addr_i];
    end
  end
end


endmodule
