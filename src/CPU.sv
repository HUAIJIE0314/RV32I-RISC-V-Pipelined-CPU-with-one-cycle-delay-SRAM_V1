`include "define.svh"
`include "Program_Counter.sv"
`include "Register_File.sv"
`include "MUX_2_1.sv"
`include "MUX_4_1.sv"
`include "Controller.sv"
`include "ALU_Controller.sv"
`include "ALU.sv"
`include "Imm_Gen.sv"
`include "CSR_Unit.sv"
`include "Comparator.sv"
`include "alignment_Correction.sv"
`include "LoadData_Mask.sv"
`include "Stall_Detection.sv"
`include "Forwarding_Unit.sv"
`include "Pipe_Reg_SF.sv"
`include "Pipe_Reg_S.sv"
`include "Pipe_Reg_F.sv"
`include "Pipe_Reg.sv"

module CPU(
  clk,     
  rst,   
  IM_A,  
  IM_DO, 
  DM_OE, 
  DM_A,  
  DM_DO, 
  DM_WEB,
  DM_DI 
);
//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic                                clk;  
input  logic                                rst;
output logic [$clog2(`IM_DEPTH)-1:0]       IM_A;
input  logic [`DATA_WIDTH-1:0]            IM_DO;
output logic                              DM_OE;
output logic [$clog2(`DM_DEPTH)-1:0]       DM_A;
input  logic [`DATA_WIDTH-1:0]            DM_DO;
output logic [3:0]                       DM_WEB;
output logic [`DATA_WIDTH-1:0]            DM_DI;

//---------------------------------------------------------------------
//        LOGIC & VARIABLES DECLARATION                            
//---------------------------------------------------------------------
// << Program_Counter >>
logic        [`PC_WIDTH-1:0]         PC_pres_IF;
logic        [`PC_WIDTH-1:0]         PC_pres_ID;
logic        [`PC_WIDTH-1:0]         PC_pres_EX;
logic        [`PC_WIDTH-1:0]     PC_pres_IF_dly;
logic        [`PC_WIDTH-1:0]         PC_next_IF;
logic        [`DATA_WIDTH-1:0]   PC_pres_IF_tmp;
logic        [`DATA_WIDTH-1:0]   PC_next_IF_tmp;
logic                                  stall_IF;
// << about instruction >>
logic        [`DATA_WIDTH-1:0]          Inst_IF;
logic        [`DATA_WIDTH-1:0]          Inst_ID;
logic        [6:0]                    funct7_ID;
logic        [6:0]                    funct7_EX;
logic        [2:0]                    funct3_ID;
logic        [2:0]                    funct3_EX;
logic        [2:0]                    funct3_ME;
logic        [4:0]                  rs1_addr_ID;
logic        [4:0]                  rs1_addr_EX;
logic        [4:0]                  rs2_addr_ID;
logic        [4:0]                  rs2_addr_EX;
logic        [4:0]                   rd_addr_ID;
logic        [4:0]                   rd_addr_EX;
logic        [4:0]                   rd_addr_ME;
logic        [4:0]                   rd_addr_WB;
logic        [`OP_WIDTH-1:0]          opcode_ID;
// << Controller >>               
logic                               RegWrite_ID;
logic                               RegWrite_EX;
logic                               RegWrite_ME;
logic                               RegWrite_WB;
logic        [1:0]                ALUsrcAsel_ID;
logic        [1:0]                ALUsrcAsel_EX;
logic        [1:0]                ALUsrcBsel_ID;
logic        [1:0]                ALUsrcBsel_EX;
logic        [3:0]                  MemWrite_ID;
logic        [3:0]                  MemWrite_EX;
logic                                MemRead_ID;
logic                                MemRead_EX;
logic                                MemRead_ME;
logic                                 Branch_ID;
logic                                    Jal_ID;
logic                                   Jalr_ID;
logic        [1:0]                     ALUop_ID;
logic        [1:0]                     ALUop_EX;
// << ALU & ALU Controller >>  
logic        [`DATA_WIDTH-1:0]       ALUsrcA_EX;
logic        [`DATA_WIDTH-1:0]       ALUsrcB_EX;
logic        [`DATA_WIDTH-1:0]    srcAorForward;
logic        [`DATA_WIDTH-1:0]    srcBorForward;
logic        [`DATA_WIDTH-1:0]     ALUresult_EX;
logic        [`DATA_WIDTH-1:0]     ALUresult_ME;
logic        [2:0]                   ALUctrl_EX;
logic                                ALUSign_EX;
// << CSR >>
logic        [`DATA_WIDTH-1:0]       CSR_OUT_ID;    
logic        [`DATA_WIDTH-1:0]       CSR_OUT_EX;
logic        [1:0]                    CSRSel_ID;
// << Register_File >>         
logic        [`DATA_WIDTH-1:0]       rd_data_WB;
logic        [`DATA_WIDTH-1:0]      rs1_data_ID;
logic        [`DATA_WIDTH-1:0]      rs1_data_EX;
logic        [`DATA_WIDTH-1:0]      rs2_data_ID;
logic        [`DATA_WIDTH-1:0]      rs2_data_EX;
// << Imm_Gen >>               
logic        [`DATA_WIDTH-1:0]       imm_out_ID;
logic        [`DATA_WIDTH-1:0]       imm_out_EX;
// << for branch >>
logic        [`DATA_WIDTH-1:0]      PC_plus4_IF;
logic        [`DATA_WIDTH-1:0]      rs1orFWorPC;
logic        [`DATA_WIDTH-1:0]     branchTarget;
// << Comparator >>            
logic                            branch_flag_ID;
// << LoadData_Mask >>         
logic        [`DATA_WIDTH-1:0]      LoadData_ME;
// << Forwarding_Unit >>
logic        [1:0]                    FWsrcAsel;//Forwarding for ALU srcA
logic        [1:0]                    FWsrcBsel;//Forwarding for ALU srcB
logic                             FWCompsrcAsel;//Forwarding for Comparator srcA
logic                             FWCompsrcBsel;//Forwarding for Comparator srcB
logic                                 FWJalrSel;//Forwarding for "rs1"+imm(=PC)
// << Stall_Detection >>
logic                                     stall;
// << Forwarding MUX output data >>
logic        [`DATA_WIDTH-1:0]  rs1orForward_ID;
logic        [`DATA_WIDTH-1:0]         compSrcA;
logic        [`DATA_WIDTH-1:0]         compSrcB;
// << rd data temp >>
logic        [`DATA_WIDTH-1:0]       rd_data_ME;
// << branch_taken >>
logic                              branch_taken;
// << flush_signal >>
logic                               flush_IF_ID;
logic                           flush_IF_ID_dly;
logic                               flush_ID_EX;

//---------------------------------------------------------------------
//        WIRE CONNECTION                             
//---------------------------------------------------------------------
// << SRAM ports >>
assign IM_A  = PC_pres_IF[15:2];

// << instruction >>
assign Inst_IF     = IM_DO;//IM data out
assign funct7_ID   = Inst_ID[`funct7_RANGE];
assign funct3_ID   = Inst_ID[`funct3_RANGE];
assign rs2_addr_ID = Inst_ID[`rs2_RANGE   ];
assign rs1_addr_ID = Inst_ID[`rs1_RANGE   ];
assign rd_addr_ID  = Inst_ID[`rd_RANGE    ];
assign opcode_ID   = Inst_ID[`OP_RANGE    ];

// << for MUX selection >>
assign CSRSel_ID   = {Inst_ID[21], Inst_ID[27]};

// << branch target >>
assign PC_plus4_IF = PC_pres_IF_tmp + 32'd4;
assign branchTarget = rs1orFWorPC + imm_out_ID;

// << branch_taken >>
assign branch_taken = (Branch_ID&branch_flag_ID)|Jal_ID|Jalr_ID;

// << flush_signal >>
assign flush_IF_ID = branch_taken;
assign flush_ID_EX = stall;

//---------------------------------------------------------------------
//        MODULE INSTANTIATION                             
//---------------------------------------------------------------------

/////////////////////// IF_stage ////////////////////////////

// << Program_Counter >>
assign PC_next_IF = (branch_taken)?(PC_next_IF_tmp + 32'd4):(PC_next_IF_tmp);
assign stall_IF = stall;

Program_Counter PC(
  .clk_i       (clk           ),
  .rst_i       (rst           ),
  .stall_i     (stall_IF      ),
  .PC_Present_i(PC_next_IF    ),
  .PC_next_o   (PC_pres_IF_tmp) 
);

MUX_4_1 #(.WIDTH(32)) MUX_4_1_PC_pres_IF (
  .in0_i(PC_pres_IF_tmp),
  .in1_i(PC_pres_IF_dly),
  .in2_i(PC_next_IF_tmp),
  .in3_i(PC_pres_IF_dly),
  .sel_i({branch_taken, stall}),
  .out_o(PC_pres_IF    )
);

/////////////////////// ID_stage ////////////////////////////

// << About PC Branch >>
// rs1orFroward
MUX_2_1 #(.WIDTH(32)) MUX_2_1_rs1orFroward (
  .in0_i(rs1_data_ID    ),
  .in1_i(rd_data_ME     ),
  .sel_i(FWJalrSel      ),
  .out_o(rs1orForward_ID)
);
// rs1orFWorPC
MUX_2_1 #(.WIDTH(32)) MUX_2_1_rs1orFWorPC (
  .in0_i(PC_pres_ID     ),
  .in1_i(rs1orForward_ID),
  .sel_i(Jalr_ID        ),
  .out_o(rs1orFWorPC    )
);
// PC_next
MUX_2_1 #(.WIDTH(32)) MUX_2_1_PC_next (
  .in0_i(PC_plus4_IF   ),// PC + 4
  .in1_i(branchTarget  ),// (rs1 or rs1_FW or PC) + imm
  .sel_i(branch_taken  ),
  .out_o(PC_next_IF_tmp)
);

// << Register_File >>
Register_File Register_File(
  .clk_i     (clk        ),
  .rst_i     (rst        ),
  .rs1_addr_i(rs1_addr_ID),
  .rs2_addr_i(rs2_addr_ID),
  .rd_addr_i (rd_addr_WB ),
  .rd_data_i (rd_data_WB ),
  .RegWrite_i(RegWrite_WB),
  .rs1_data_o(rs1_data_ID),
  .rs2_data_o(rs2_data_ID)
);

// << Imm_Gen >>
Imm_Gen Imm_Gen(
  .instr_i  (Inst_ID   ),
  .imm_out_o(imm_out_ID)
);

// << Controller >>
Controller controller(
  .opcode_i    (opcode_ID      ),
  .funct3_i    (funct3_ID      ),
  .flush_i     (flush_IF_ID_dly),
  .RegWrite_o  (RegWrite_ID    ),
  .ALUsrcAsel_o(ALUsrcAsel_ID  ),
  .ALUsrcBsel_o(ALUsrcBsel_ID  ),
  .MemWrite_o  (MemWrite_ID    ),
  .MemRead_o   (MemRead_ID     ),
  .Branch_o    (Branch_ID      ),
  .Jal_o       (Jal_ID         ),
  .Jalr_o      (Jalr_ID        ),
  .ALUop_o     (ALUop_ID       )
);

// << About Comparator >>
// compSrcA
MUX_2_1 #(.WIDTH(32)) MUX_2_1_compSrcA (
  .in0_i(rs1_data_ID  ),
  .in1_i(rd_data_ME   ),
  .sel_i(FWCompsrcAsel),
  .out_o(compSrcA     )
);
// compSrcB
MUX_2_1 #(.WIDTH(32)) MUX_2_1_compSrcB (
  .in0_i(rs2_data_ID  ),
  .in1_i(rd_data_ME   ),
  .sel_i(FWCompsrcBsel),
  .out_o(compSrcB     )
);
// branch_flag_ID
Comparator Comparator(
  .rs1_data_i   (compSrcA      ),
  .rs2_data_i   (compSrcB      ),
  .funct3_i     (funct3_ID     ),
  .branch_flag_o(branch_flag_ID)
);


/////////////////////// EXE_stage ////////////////////////////

// << ALU_Controller >>
ALU_Controller ALU_Controller(
  .funct7_6_i(funct7_EX[5]),
  .funct3_i  (funct3_EX   ),
  .ALUop_i   (ALUop_EX    ),
  .ALUctrl_o (ALUctrl_EX  ),
  .sign_o    (ALUSign_EX  )
);

// << About ALU >>

// srcAorForward
MUX_4_1 #(.WIDTH(32)) MUX_4_1_srcAorForward (
  .in0_i(rs1_data_EX  ),
  .in1_i(rd_data_ME   ),
  .in2_i(rd_data_WB   ),
  .in3_i(32'd0        ),
  .sel_i(FWsrcAsel    ),
  .out_o(srcAorForward)
);

// ALUsrcA_EX
MUX_4_1 #(.WIDTH(32)) MUX_4_1_ALUsrcA_EX (
  .in0_i(srcAorForward),
  .in1_i(PC_pres_EX   ),
  .in2_i(32'd0        ),
  .in3_i(32'd0        ),
  .sel_i(ALUsrcAsel_EX),
  .out_o(ALUsrcA_EX   )
);

// srcBorForward
MUX_4_1 #(.WIDTH(32)) MUX_4_1_srcBorForward (
  .in0_i(rs2_data_EX  ),
  .in1_i(rd_data_ME   ),
  .in2_i(rd_data_WB   ),
  .in3_i(32'd0        ),
  .sel_i(FWsrcBsel    ),
  .out_o(srcBorForward)
);

// ALUsrcB_EX
MUX_4_1 #(.WIDTH(32)) MUX_4_1_ALUsrcB_EX (
  .in0_i(srcBorForward),
  .in1_i(imm_out_EX   ),
  .in2_i(32'd4        ),
  .in3_i(CSR_OUT_EX   ),
  .sel_i(ALUsrcBsel_EX),
  .out_o(ALUsrcB_EX   )
);

// ALU
ALU ALU(
  .srcA_i     (ALUsrcA_EX  ),
  .srcB_i     (ALUsrcB_EX  ),
  .ALUctrl_i  (ALUctrl_EX  ),
  .sign_i     (ALUSign_EX  ),
  .ALUresult_o(ALUresult_EX)
);

// << CRS_Unit >>
CRS_Unit CRS_Unit(
  .clk_i        (clk        ),
  .rst_i        (rst        ),
  .flush_IF_ID_i(flush_IF_ID),
  .flush_ID_EX_i(flush_ID_EX),
  .CSRSel_i     (CSRSel_ID  ),
  .CSR_OUT_o    (CSR_OUT_ID )
);

// << Forwarding_Unit >>
Forwarding_Unit Forwarding_Unit(
  .rs1_addr_ID_i  (rs1_addr_ID  ),
  .rs2_addr_ID_i  (rs2_addr_ID  ),
  .rs1_addr_EX_i  (rs1_addr_EX  ),
  .rs2_addr_EX_i  (rs2_addr_EX  ),
  .RegWrite_ME_i  (RegWrite_ME  ),
  .rd_addr_ME_i   (rd_addr_ME   ),
  .RegWrite_WB_i  (RegWrite_WB  ),
  .rd_addr_WB_i   (rd_addr_WB   ),
  .FWsrcAsel_o    (FWsrcAsel    ),
  .FWsrcBsel_o    (FWsrcBsel    ),
  .FWCompsrcAsel_o(FWCompsrcAsel),
  .FWCompsrcBsel_o(FWCompsrcBsel),
  .FWJalrSel_o    (FWJalrSel    )
);

// << Stall_Detection >>
Stall_Detection Stall_Detection(
  .rs1_addr_ID_i(rs1_addr_ID),
  .rs2_addr_ID_i(rs2_addr_ID),
  .Branch_ID_i  (Branch_ID  ),
  .Jalr_ID_i    (Jalr_ID    ),
  .rd_addr_EX_i (rd_addr_EX ),
  .RegWrite_EX_i(RegWrite_EX),
  .MemRead_EX_i (MemRead_EX ),
  .rd_addr_ME_i (rd_addr_ME ),
  .MemRead_ME_i (MemRead_ME ),
  .stall_o      (stall      )
);

/////////////////////// MEM_stage ////////////////////////////


assign DM_A  = ALUresult_EX[15:2];
assign DM_OE = MemRead_ME;

// << alignment_Correction >>
alignment_Correction alignment_Correction(
  .DM_A_2b_i(ALUresult_EX[1:0]),
  .DM_WEB_i (MemWrite_EX      ),
  .DM_DI_i  (srcBorForward    ),     
  .DM_WEB_o (DM_WEB           ),           
  .DM_DI_o  (DM_DI            )            
);

// << LoadData_Mask >>
LoadData_Mask LoadData_Mask(
  .LoadData_i(DM_DO      ),
  .funct3_i  (funct3_ME  ),
  .LoadData_o(LoadData_ME)
);

// << rd_data_ME >>
MUX_2_1 #(.WIDTH(32)) MUX_4_1_MemtoReg (
  .in0_i(ALUresult_ME),
  .in1_i(LoadData_ME ),
  .sel_i(MemRead_ME  ),
  .out_o(rd_data_ME  )
);

/////////////////////// WB_stage ////////////////////////////

// none



//**************************** Pipe_Reg *********************************

// Pipe_Reg_SF : pipe register with Stall & Flush
// Pipe_Reg_S  : pipe register only with Stall
// Pipe_Reg_F  : pipe register only with Flush
// Pipe_Reg    : pipe register without Stall & Flush

// IF-ID pipelined register
Pipe_Reg_SF #(.WIDTH(32)) IF_ID_Inst        (.clk_i(clk), .rst_i(rst), .stall_i(stall), .flush_i(flush_IF_ID), .data_i(Inst_IF       ), .data_o(Inst_ID        ));
Pipe_Reg_S  #(.WIDTH(32)) IF_ID_PC_pres_dly (.clk_i(clk), .rst_i(rst), .stall_i(stall),                        .data_i(PC_pres_IF    ), .data_o(PC_pres_IF_dly ));
Pipe_Reg_S  #(.WIDTH(32)) IF_ID_PC_pres     (.clk_i(clk), .rst_i(rst), .stall_i(stall),                        .data_i(PC_pres_IF_dly), .data_o(PC_pres_ID     ));

// ID-EXE pipelined register                                                                                                                                   
Pipe_Reg_F  #(.WIDTH(32)) ID_EX_PC_pres     (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(PC_pres_ID    ), .data_o(PC_pres_EX     ));                                                                                                                        
Pipe_Reg_F  #(.WIDTH( 7)) ID_EX_funct7      (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(funct7_ID     ), .data_o(funct7_EX      ));
Pipe_Reg_F  #(.WIDTH( 3)) ID_EX_funct3      (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(funct3_ID     ), .data_o(funct3_EX      ));
Pipe_Reg_F  #(.WIDTH( 5)) ID_EX_rs1_addr    (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(rs1_addr_ID   ), .data_o(rs1_addr_EX    ));
Pipe_Reg_F  #(.WIDTH( 5)) ID_EX_rs2_addr    (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(rs2_addr_ID   ), .data_o(rs2_addr_EX    ));
Pipe_Reg_F  #(.WIDTH( 5)) ID_EX_rd_addr     (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(rd_addr_ID    ), .data_o(rd_addr_EX     ));
Pipe_Reg_F  #(.WIDTH( 1)) ID_EX_RegWrite    (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(RegWrite_ID   ), .data_o(RegWrite_EX    ));
Pipe_Reg_F  #(.WIDTH( 2)) ID_EX_ALUsrcAsel  (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(ALUsrcAsel_ID ), .data_o(ALUsrcAsel_EX  ));
Pipe_Reg_F  #(.WIDTH( 2)) ID_EX_ALUsrcBsel  (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(ALUsrcBsel_ID ), .data_o(ALUsrcBsel_EX  ));
Pipe_Reg_F  #(.WIDTH( 4)) ID_EX_MemWrite    (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(MemWrite_ID   ), .data_o(MemWrite_EX    ));
Pipe_Reg_F  #(.WIDTH( 1)) ID_EX_MemRead     (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(MemRead_ID    ), .data_o(MemRead_EX     ));
Pipe_Reg_F  #(.WIDTH( 2)) ID_EX_ALUop       (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(ALUop_ID      ), .data_o(ALUop_EX       ));
Pipe_Reg_F  #(.WIDTH(32)) ID_EX_CSR_OUT     (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(CSR_OUT_ID    ), .data_o(CSR_OUT_EX     ));
Pipe_Reg_F  #(.WIDTH(32)) ID_EX_rs1_data    (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(rs1_data_ID   ), .data_o(rs1_data_EX    ));
Pipe_Reg_F  #(.WIDTH(32)) ID_EX_rs2_data    (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(rs2_data_ID   ), .data_o(rs2_data_EX    ));
Pipe_Reg_F  #(.WIDTH(32)) ID_EX_imm_out     (.clk_i(clk), .rst_i(rst),                  .flush_i(flush_ID_EX), .data_i(imm_out_ID    ), .data_o(imm_out_EX     ));

// EXE-MEM pipelined register                                                                                                                                  
Pipe_Reg    #(.WIDTH( 3)) EX_ME_funct3      (.clk_i(clk), .rst_i(rst),                                         .data_i(funct3_EX     ), .data_o(funct3_ME      ));
Pipe_Reg    #(.WIDTH( 5)) EX_ME_rd_addr     (.clk_i(clk), .rst_i(rst),                                         .data_i(rd_addr_EX    ), .data_o(rd_addr_ME     ));
Pipe_Reg    #(.WIDTH( 1)) EX_ME_RegWrite    (.clk_i(clk), .rst_i(rst),                                         .data_i(RegWrite_EX   ), .data_o(RegWrite_ME    ));
Pipe_Reg    #(.WIDTH( 1)) EX_ME_MemRead     (.clk_i(clk), .rst_i(rst),                                         .data_i(MemRead_EX    ), .data_o(MemRead_ME     ));
Pipe_Reg    #(.WIDTH(32)) EX_ME_ALUresult   (.clk_i(clk), .rst_i(rst),                                         .data_i(ALUresult_EX  ), .data_o(ALUresult_ME   ));

// MEM-WB pipelined register                                                                                                                                   
Pipe_Reg    #(.WIDTH( 5)) ME_WB_rd_addr     (.clk_i(clk), .rst_i(rst),                                         .data_i(rd_addr_ME    ), .data_o(rd_addr_WB     ));
Pipe_Reg    #(.WIDTH( 1)) ME_WB_RegWrite    (.clk_i(clk), .rst_i(rst),                                         .data_i(RegWrite_ME   ), .data_o(RegWrite_WB    ));
Pipe_Reg    #(.WIDTH(32)) ME_WB_rd_data     (.clk_i(clk), .rst_i(rst),                                         .data_i(rd_data_ME    ), .data_o(rd_data_WB     ));

// for delay one cycle
Pipe_Reg    #(.WIDTH( 1)) reg_flush_IF_ID   (.clk_i(clk), .rst_i(rst),                                         .data_i(flush_IF_ID   ), .data_o(flush_IF_ID_dly));

endmodule

