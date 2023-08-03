module alignment_Correction(
  DM_A_2b_i,
  DM_WEB_i,
  DM_DI_i,
  DM_WEB_o,
  DM_DI_o
);

//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic [1:0]             DM_A_2b_i;
input  logic [3:0]              DM_WEB_i;
input  logic [`DATA_WIDTH-1:0]   DM_DI_i;
output logic [3:0]              DM_WEB_o;
output logic [`DATA_WIDTH-1:0]   DM_DI_o;

//---------------------------------------------------------------------
//        ALWAYS BLOCK                             
//---------------------------------------------------------------------

// << DM_WEB >>
always_comb begin
  unique case(DM_A_2b_i)
    2'd0:DM_WEB_o = DM_WEB_i;
    2'd1:DM_WEB_o = {DM_WEB_i[2:0], 1'd0};//left shift 1
    2'd2:DM_WEB_o = {DM_WEB_i[1:0], 2'd0};//left shift 2
    2'd3:DM_WEB_o = {DM_WEB_i[0],   3'd0};//left shift 3
    default:DM_WEB_o = DM_WEB_i;
  endcase
end

// << DM_DI >>
always_comb begin
  unique case(DM_A_2b_i)
    2'd0:DM_DI_o = DM_DI_i;
    2'd1:DM_DI_o = {DM_DI_i[23:0],  8'd0};//left shift 1*8
    2'd2:DM_DI_o = {DM_DI_i[15:0], 16'd0};//left shift 2*8
    2'd3:DM_DI_o = {DM_DI_i[7:0 ], 24'd0};//left shift 3*8
    default:DM_DI_o = DM_DI_i;
  endcase
end


endmodule
