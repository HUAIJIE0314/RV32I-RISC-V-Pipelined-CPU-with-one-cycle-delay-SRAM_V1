module LoadData_Mask(
  LoadData_i,
  funct3_i,
  LoadData_o
);
//---------------------------------------------------------------------
//        PORTS DECLARATION                             
//---------------------------------------------------------------------
input  logic  [`DATA_WIDTH-1:0] LoadData_i;
input  logic  [2:0]               funct3_i;
output logic  [`DATA_WIDTH-1:0] LoadData_o;

//---------------------------------------------------------------------
//        ALWAYS BLOCK                             
//---------------------------------------------------------------------

always_comb begin
  unique case(funct3_i)
    3'd0:LoadData_o = {{24{LoadData_i[31]}}, LoadData_i[7:0] };//LB sign
    3'd1:LoadData_o = {{16{LoadData_i[31]}}, LoadData_i[15:0]};//LH sign
    3'd2:LoadData_o = LoadData_i;//LW sign
    3'd4:LoadData_o = {24'd0               , LoadData_i[7:0] };//LB unsign
    3'd5:LoadData_o = {16'd0               , LoadData_i[15:0]};//LH unsign
    default:LoadData_o = LoadData_i;
  endcase
end

endmodule