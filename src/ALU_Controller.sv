module ALU_Controller(
  funct7_6_i,
  funct3_i,
  ALUop_i,
  ALUctrl_o,
  sign_o
);
//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic        funct7_6_i;
input  logic [2:0]    funct3_i;
input  logic [1:0]     ALUop_i;
output logic [2:0]   ALUctrl_o;
output logic            sign_o;
//---------------------------------------------------------------------
//        ALWAYS BLOCK                             
//---------------------------------------------------------------------
// << ALUctrl_o >>
always_comb begin
  if(ALUop_i == 2'd0)ALUctrl_o = `ADD;
  else begin
    unique case(funct3_i)
      3'b000:begin
        if(ALUop_i == 2'd2)ALUctrl_o = `ADD;
        else begin
          unique if(funct7_6_i)ALUctrl_o = `SUB;
          else                 ALUctrl_o = `ADD;
        end
      end
      3'b001:begin
        ALUctrl_o = `SLL;
      end
      3'b010, 3'b011:begin
        ALUctrl_o = `SLT_SLTU;
      end
      3'b100:begin
        ALUctrl_o = `XOR;
      end
      3'b101:begin
        ALUctrl_o = `SRL_SRA;
      end
      3'b110:begin
        ALUctrl_o = `OR;
      end
      3'b111:begin
        ALUctrl_o = `AND;
      end
      default:ALUctrl_o = `ADD;
    endcase
  end
end

// << sign_o >>
always_comb begin
  unique case(funct3_i)
    3'b000:sign_o = 1'b0;
    3'b001:sign_o = 1'b0;
    3'b010:sign_o = 1'b1;
    3'b011:sign_o = 1'b0;
    3'b100:sign_o = 1'b0;
    3'b101:sign_o = funct7_6_i;
    3'b110:sign_o = 1'b0;
    3'b111:sign_o = 1'b0;
    default:sign_o = 1'b0;
  endcase
end

endmodule

