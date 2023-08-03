module Pipe_Reg_SF #(parameter WIDTH=32)(
  clk_i,
  rst_i,
  stall_i,
  flush_i,
  data_i,
  data_o
);
//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic              clk_i;
input  logic              rst_i;
input  logic            stall_i;
input  logic            flush_i;
input  logic [WIDTH-1:0] data_i;
output logic [WIDTH-1:0] data_o;

//---------------------------------------------------------------------
//        ALWAYS BLOCK                             
//---------------------------------------------------------------------
always_ff@(posedge clk_i or posedge rst_i)begin
  if(rst_i)       data_o <= {WIDTH{1'b0}};
  else if(stall_i)data_o <= data_o;
  else if(flush_i)data_o <= {WIDTH{1'b0}};
  else            data_o <= data_i;
end

endmodule