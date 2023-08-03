module Controller(
  opcode_i,
  funct3_i,
  RegWrite_o,
  rs2Sel_o,
  rdPCSel_o,
  MemWrite_o,
  MemtoReg_o,
  MemRead_o,
  Branch_o,
  Jal_o,
  Jalr_o,
  ALUop_o
);
//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic [`OP_WIDTH-1:0]     opcode_i;
input  logic [2:0]               funct3_i;
output logic                   RegWrite_o;
output logic                     rs2Sel_o;
output logic [1:0]              rdPCSel_o;
output logic [3:0]             MemWrite_o;
output logic [1:0]             MemtoReg_o;
output logic                    MemRead_o;
output logic                     Branch_o;
output logic                        Jal_o;
output logic                       Jalr_o;
output logic [1:0]                ALUop_o;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                               Control Signal Table                                                                               //
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// |   opcode_i   | RegWrite_o | rs2Sel_o | rdPCSel_o | MemWrite_o | MemtoReg_o  | MemRead_o | Branch_o | Jal_o | Jalr_o | ALUop_o  |                 Description                  |//
// +--------------+------------+----------+-----------+------------+-------------+-----------+----------+-------+--------+----------+----------------------------------------------|//
// |  7'b0110011  |     1      |    0     |  0 (00)   |     0      |    0 (00)   |     0     |     0    |   0   |    0   |  1 (01)  | RTYPE                                        |//
// |  7'b0010011  |     1      |    1     |  0 (00)   |     0      |    0 (00)   |     0     |     0    |   0   |    0   |  2 (10)  | ITYPE (RTYPE imm)                            |//
// |  7'b1100111  |     1      |    0     |  0 (00)   |     0      |    1 (01)   |     0     |     0    |   0   |    1   |  0 (00)  | JALR  (rd = PC + 4 ; PC = imm + rs1)         |//
// |  7'b0000011  |     1      |    1     |  0 (00)   |     0      |    2 (10)   |     1     |     0    |   0   |    0   |  0 (00)  | LOAD  (rd = Mem[rs1+imm])                    |//
// |  7'b0100011  |     0      |    1     |  0 (00)   |     1      |    0 (00)   |     0     |     0    |   0   |    0   |  0 (00)  | STYPE (Mem[rs1+imm] = rs2)                   |//
// |  7'b1100011  |     0      |    0     |  0 (00)   |     0      |    0 (00)   |     0     |     1    |   0   |    0   |  0 (00)  | BTYPE (need PC+imm & PC+4 & Compare rs1, rs2)|//
// |  7'b0010111  |     1      |    0     |  1 (01)   |     0      |    1 (01)   |     0     |     0    |   0   |    0   |  0 (00)  | AUIPC (rd = PC + imm)                        |//
// |  7'b0110111  |     1      |    0     |  2 (10)   |     0      |    1 (01)   |     0     |     0    |   0   |    0   |  0 (00)  | LUI   (rd = imm)                             |//
// |  7'b1101111  |     1      |    0     |  0 (00)   |     0      |    1 (01)   |     0     |     0    |   1   |    0   |  0 (00)  | JTYPE (rd = PC + 4 ; PC = PC + imm)          |//
// |  7'b1110011  |     1      |    0     |  0 (00)   |     0      |    3 (11)   |     0     |     0    |   0   |    0   |  0 (00)  | CSR   (rd = cycle or instret)                |//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// ========================= Note =========================

//  << rdPCSel_o >> select different PC branch to rd_data
// 0 : from PC + 4
// 1 : from PC + imm
// 2 : from imm

// << MemtoReg_o >>
// 0 : from ALU result
// 1 : from rdPC
// 2 : from Data Memory output
// 3 : from CSR

// << rs2Sel_o >>
// 0 : rs2_data
// 1 : immediate

// ** also need CSRSel & StoreSel & LoadSel_o **

//---------------------------------------------------------------------
//        ALWAYS BLOCK                             
//---------------------------------------------------------------------

// << RegWrite_o >> `LOAD
always_comb begin
  unique case(opcode_i)
    `RTYPE, `ITYPE, `JALR, `LOAD, `AUIPC, `LUI, `JTYPE, `CSR:RegWrite_o = 1'b1;
    default:RegWrite_o = 1'b0;
  endcase
end

// << rs2Sel_o >>
always_comb begin
  unique case(opcode_i)
    `ITYPE, `LOAD, `STYPE:rs2Sel_o = 1'b1;
    default:rs2Sel_o = 1'b0;
  endcase
end

// << rdPCSel_o >>
always_comb begin
  unique case(opcode_i)
    `AUIPC :rdPCSel_o = 2'd1;
    `LUI   :rdPCSel_o = 2'd2;
    default:rdPCSel_o = 2'd0;
  endcase
end

// << MemWrite_o >>
always_comb begin
  unique if(opcode_i == `STYPE)begin
    unique case(funct3_i[1:0])
      2'd0:MemWrite_o = 4'b1110; // Store Byte
      2'd1:MemWrite_o = 4'b1100; // Store Half
      2'd2:MemWrite_o = 4'b0000;
      default:MemWrite_o = 4'b1111;
    endcase
  end
  else begin
    MemWrite_o = 4'b1111;
  end 
end

// << MemtoReg_o >>
always_comb begin
  unique case(opcode_i)
    `JALR, `AUIPC, `LUI, `JTYPE:MemtoReg_o = 2'd1;
    `LOAD:MemtoReg_o = 2'd2;
    `CSR:MemtoReg_o = 2'd3;
    default:MemtoReg_o = 2'd0;
  endcase
end

// << MemRead_o >>
always_comb begin
  unique if(opcode_i == `LOAD)MemRead_o = 1'b1;
  else                        MemRead_o = 1'b0;
end

// << Branch_o >>
always_comb begin
  unique if(opcode_i == `BTYPE)Branch_o = 1'b1;
  else                         Branch_o = 1'b0;
end

// << Jal_o >>
always_comb begin
  unique if(opcode_i == `JTYPE)Jal_o = 1'b1;
  else                         Jal_o = 1'b0;
end

// << Jalr_o >>
always_comb begin
  unique if(opcode_i == `JALR)Jalr_o = 1'b1;
  else                        Jalr_o = 1'b0;
end

// << ALUop_o >>
always_comb begin
  unique case(opcode_i)
    `RTYPE: ALUop_o = 2'd1;
    `ITYPE: ALUop_o = 2'd2;
    default:ALUop_o = 2'd0;
  endcase
end

endmodule
