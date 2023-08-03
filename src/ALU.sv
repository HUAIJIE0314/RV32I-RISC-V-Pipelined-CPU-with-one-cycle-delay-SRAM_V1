module ALU(
  srcA_i,
  srcB_i,
  ALUctrl_i,
  sign_i,
  ALUresult_o
  //zero_o,
  //carry_o
);
//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic signed [`DATA_WIDTH-1:0]      srcA_i;
input  logic signed [`DATA_WIDTH-1:0]      srcB_i;
input  logic signed [2:0]               ALUctrl_i;
input  logic                               sign_i;
output logic signed [`DATA_WIDTH-1:0] ALUresult_o;
//output logic                        zero_o;
//output logic                       carry_o;
//---------------------------------------------------------------------
//        LOGIC & VARIABLES DECLARATION                            
//---------------------------------------------------------------------
logic [`DATA_WIDTH-1:0] srcA_unsigned;
logic [`DATA_WIDTH-1:0] srcB_unsigned;
//---------------------------------------------------------------------
//        WIRE CONNECTION                             
//---------------------------------------------------------------------
assign srcA_unsigned = srcA_i;
assign srcB_unsigned = srcB_i;
//---------------------------------------------------------------------
//        ALWAYS BLOCK                             
//---------------------------------------------------------------------
always_comb begin
  unique case(ALUctrl_i)
    `ADD:              ALUresult_o = srcA_i + srcB_i;
    `SUB:              ALUresult_o = srcA_i - srcB_i;
    `SLL:              ALUresult_o = srcA_i << srcB_i[4:0];
    `SLT_SLTU:begin
      unique if(sign_i)ALUresult_o = ( srcA_i < srcB_i )?(32'd1):(32'd0);
      else             ALUresult_o = (srcA_unsigned < srcB_unsigned)?(32'd1):(32'd0);
    end
    `XOR:              ALUresult_o = srcA_i ^ srcB_i;
    `SRL_SRA:begin
      unique if(sign_i)ALUresult_o = srcA_i >>> srcB_i[4:0];
      else             ALUresult_o = srcA_unsigned >> srcB_unsigned[4:0];
    end
    `OR:               ALUresult_o = srcA_i | srcB_i;
    `AND:              ALUresult_o = srcA_i & srcB_i;
    default:           ALUresult_o = srcA_i + srcB_i;
  endcase
end


endmodule
