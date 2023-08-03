module Imm_Gen(
  instr_i,
  imm_out_o
);
//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic [`DATA_WIDTH-1:0]   instr_i;
output logic [`DATA_WIDTH-1:0] imm_out_o;

//---------------------------------------------------------------------
//        ALWAYS BLOCK                             
//---------------------------------------------------------------------
always_comb begin
  unique case(instr_i[`OP_RANGE])
    `RTYPE               :imm_out_o = 32'd0;
    `ITYPE, `LOAD, `JALR :imm_out_o = {{20{instr_i[31]}}, instr_i[31:20]};
    `STYPE               :imm_out_o = {{20{instr_i[31]}}, instr_i[31:25], instr_i[11:7]};
    `BTYPE               :imm_out_o = {{20{instr_i[31]}}, instr_i[7], instr_i[30:25] , instr_i[11:8], 1'b0};
    `AUIPC, `LUI         :imm_out_o = {instr_i[31:12], 12'd0};
    `JTYPE               :imm_out_o = {{12{instr_i[31]}}, instr_i[19:12], instr_i[20], instr_i[30:21], 1'b0};
    default:imm_out_o = 32'd0;
  endcase
end

endmodule
