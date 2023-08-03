module MUX_4_1 #(parameter WIDTH = 32)(
  in0_i,
  in1_i,
  in2_i,
  in3_i,
  sel_i,
  out_o
);
//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic [WIDTH-1:0] in0_i;
input  logic [WIDTH-1:0] in1_i;
input  logic [WIDTH-1:0] in2_i;
input  logic [WIDTH-1:0] in3_i;
input  logic [1:0]       sel_i;
output logic [WIDTH-1:0] out_o;
//---------------------------------------------------------------------
//        ALWAYS BLOCK                          
//---------------------------------------------------------------------
always_comb begin
  unique case(sel_i)
    2'd0:out_o = in0_i;
    2'd1:out_o = in1_i;
    2'd2:out_o = in2_i;
    2'd3:out_o = in3_i;
    default:out_o = in0_i;
  endcase
end

endmodule