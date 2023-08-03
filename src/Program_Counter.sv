module Program_Counter(
  clk_i,
  rst_i,
  stall_i,
  PC_Present_i,
  PC_next_o
);
//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic                        clk_i;
input  logic                        rst_i;
input  logic                      stall_i;
input  logic [`PC_WIDTH-1:0] PC_Present_i;
output logic [`PC_WIDTH-1:0]    PC_next_o;
//---------------------------------------------------------------------
//        ALWAYS BLOCK                             
//---------------------------------------------------------------------
always_ff@(posedge clk_i or posedge rst_i)begin
  if(rst_i)PC_next_o <= {`PC_WIDTH{1'b0}};
  else if(stall_i)PC_next_o <= PC_next_o;
  else            PC_next_o <= PC_Present_i;
end

endmodule