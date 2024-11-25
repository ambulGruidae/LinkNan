package linknan.cluster.hub.peripheral

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import zhujiang.tilelink.{BaseTLULPeripheral, TilelinkParams}

class CpuBootCtrl(tlParams: TilelinkParams)(implicit p: Parameters) extends BaseTLULPeripheral(tlParams) {
  val io = IO(new Bundle {
    val defaultBootAddr = Input(UInt(64.W))
    val cpuBootAddr = Output(UInt(64.W))
  })
  private val addrReg = Reg(UInt(64.W))
  private val addrWire = WireInit(addrReg)

  val regSeq = Seq(
    ("addr", addrReg, addrWire, 0x0, None, None),
  )
  private val writeMap = genWriteMap()
  when(reset.asBool) {
    addrReg := io.defaultBootAddr
  }.elsewhen(writeMap("addr")) {
    addrReg := addrWire
  }
  io.cpuBootAddr := addrReg
}
