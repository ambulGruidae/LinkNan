package linknan.cluster.power

import chisel3._
import chisel3.util._

package object pchannel {
  object PchannelState {
    val stateBits = log2Ceil(7)

    val sReset:UInt = 0.U(stateBits.W)
    val sStable0:UInt = 1.U(stateBits.W)
    val sRequest:UInt = 2.U(stateBits.W)
    val sAccept:UInt = 3.U(stateBits.W)
    val sDenied:UInt = 4.U(stateBits.W)
    val sComplete:UInt = 5.U(stateBits.W)
    val sContinue:UInt = 6.U(stateBits.W)
    val sStable1:UInt = 7.U(stateBits.W)
  }

  class PChannel(N:Int, M:Int) extends Bundle {
    val active = Input(UInt(N.W))
    val state = Output(UInt(M.W))
    val req = Output(Bool())
    val accept = Input(Bool())
    val deny = Input(Bool())
  }
}
