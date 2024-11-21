package lntest.top

import chisel3._
import chisel3.util._

class LuaScoreboard extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val sim_final = Input(Bool())
  })
  setInline(s"LuaScoreboard.sv",
    s"""
       |module LuaScoreboard (
       |  input  wire clock,
       |  input  wire reset,
       |  input  wire sim_final
       |);
       |`ifndef SYNTHESIS
       |  import "DPI-C" function void verilua_init();
       |  import "DPI-C" function void verilua_final();
       |  import "DPI-C" function void verilua_main_step();
       |`ifdef VERILATOR
       |  initial verilua_init();
       |`else
       |  initial #1 verilua_init();
       |`endif
       |
       |always @ (posedge clock) begin
       |  if(~reset) verilua_main_step();
       |  if(sim_final) verilua_final();
       |end
       |
       |`endif
       |endmodule
       |
       |""".stripMargin)
}
