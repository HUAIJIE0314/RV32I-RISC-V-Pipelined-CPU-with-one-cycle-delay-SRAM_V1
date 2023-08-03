module MUX_2_1 #(parameter WIDTH = 32)(
  in0_i,
  in1_i,
  sel_i,
  out_o
);
//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic [WIDTH-1:0] in0_i;
input  logic [WIDTH-1:0] in1_i;
input  logic             sel_i;
output logic [WIDTH-1:0] out_o;
//---------------------------------------------------------------------
//        WIRE CONNECTION                             
//---------------------------------------------------------------------
assign out_o = ( sel_i )?( in1_i ):( in0_i );

endmodule