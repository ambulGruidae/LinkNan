package linknan.cluster

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import xijiang.router.base.IcnBundle
import zhujiang.ZJModule
import zhujiang.device.bridge.tlul.TLULBridge
import linknan.cluster.interconnect._
import linknan.cluster.peripheral.{ClusterPLL, CpuCtrl}
import linknan.soc.PeripheralRemapper
import zhujiang.device.tlu2chi.TLUL2ChiBridge
import zhujiang.tilelink.{TLUBuffer, TLULBundle, TilelinkParams}
import zhujiang.DftWires

class ClusterInterconnectComplex(node: Node, cioParams: TilelinkParams)(implicit p: Parameters) extends ZJModule {
  require(node.nodeType == NodeType.CC)

  private val clusterHub = Module(new ClusterHub(node))
  private val chi2tl = Module(new TLULBridge(clusterHub.io.peripheral.node, 64, 3))
  private val cioXbar = Module(new CioXBar(Seq.fill(node.cpuNum)(cioParams)))
  private val tl2chi = Module(new TLUL2ChiBridge(clusterHub.io.cio.node, cioXbar.io.downstream.last.params))
  private val clusterPeriCx = Module(new ClusterPeripheralComplex(Seq(chi2tl.tl.params, cioXbar.io.downstream.head.params), node.cpuNum))

  chi2tl.icn <> clusterHub.io.peripheral
  clusterPeriCx.io.tls.head <> chi2tl.tl
  clusterPeriCx.io.tls.last <> cioXbar.io.downstream.head
  tl2chi.tlm <> cioXbar.io.downstream.last
  clusterHub.io.cio <> tl2chi.icn

  val io = IO(new Bundle {
    val icn = new ClusterDeviceBundle(node)
    val l2cache = new IcnBundle(clusterHub.io.l2cache.node)
    val cio = Vec(node.cpuNum, Flipped(new TLULBundle(cioParams)))
    val cpu = Flipped(new ClusterMiscWires(node))
    val imisc = Vec(node.cpuNum, new ImsicBundle)
    val dft = Output(new DftWires)
    val pllCfg = Output(Vec(8, UInt(32.W)))
    val pllLock = Input(Bool())
  })

  io.icn <> clusterHub.io.icn
  clusterHub.io.l2cache <> io.l2cache
  io.cpu <> clusterHub.io.cpu
  io.dft := clusterHub.io.dft
  cioXbar.misc.chip := clusterHub.io.cpu.mhartid(0)(clusterIdBits - 1, nodeAidBits)
  io.pllCfg := clusterPeriCx.io.pllCfg
  clusterPeriCx.io.pllLock := io.pllLock

  for(i <- 0 until node.cpuNum) {
    val cio = TLUBuffer(io.cio(i), name = Some(s"cio_buf_$i"))
    val rmp = PeripheralRemapper(cio.a.bits.address, p)
    cioXbar.io.upstream(i) <> cio
    cioXbar.io.upstream(i).a.bits.address := rmp
    clusterPeriCx.io.cpu(i).defaultBootAddr := clusterHub.io.cpu.resetVector(i)
    clusterPeriCx.io.cpu(i).defaultEnable := clusterHub.io.cpu.resetEnable(i)
    io.cpu.resetVector(i) := clusterPeriCx.io.cpu(i).bootAddr
    io.cpu.resetEnable(i) := clusterPeriCx.io.cpu(i).stop
    cioXbar.misc.core(i) := clusterHub.io.cpu.mhartid(i)
    clusterPeriCx.io.cpu(i).coreId := clusterHub.io.cpu.mhartid(i)(clusterIdBits - nodeAidBits - 1, 0)
    io.imisc(i) <> clusterPeriCx.io.cpu(i).imsic
  }
}
