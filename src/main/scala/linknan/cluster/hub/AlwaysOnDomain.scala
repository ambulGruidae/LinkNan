package linknan.cluster.hub

import chisel3._
import freechips.rocketchip.tilelink.TLBundleParameters
import linknan.cluster.hub.interconnect.{CioXBar, ClusterDeviceBundle, ClusterHub, ClusterMiscWires}
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import zhujiang.{DftWires, ZJBundle, ZJRawModule}
import zhujiang.device.bridge.tlul.TLULBridge
import linknan.soc.PeripheralRemapper
import linknan.utils.{IcnRationalIO, IcnSideRationalCrossing, TileLinkRationalIO, TileLinkRationalSlv, connectByName}
import xs.utils.{ClockManagerWrapper, ResetGen}
import zhujiang.device.tlu2chi.TLUL2ChiBridge
import zhujiang.tilelink.{TLUBuffer, TLULBundle, TilelinkParams}

class AlwaysOnDomainBundle(val node: Node, ioParams:TLBundleParameters)(implicit p: Parameters) extends ZJBundle {
  val l2cache = new IcnRationalIO(node)
  val cio = Flipped(new TileLinkRationalIO(ioParams))
  val cpu = Flipped(new ClusterMiscWires(node))
  val imsic = Vec(node.cpuNum, new ImsicBundle)
  val dft = Output(new DftWires)
  val clock = Output(Clock())
  val reset = Output(AsyncReset())
}

class AlwaysOnDomain(node: Node, ioParams:TLBundleParameters)(implicit p: Parameters) extends ZJRawModule
 with ImplicitReset with ImplicitClock {
  val implicitReset = Wire(AsyncReset())
  val implicitClock = Wire(Clock())
  require(node.nodeType == NodeType.CC)
  private val cioParams = TilelinkParams(
    addrBits = ioParams.addressBits,
    sourceBits = ioParams.sourceBits,
    sinkBits = ioParams.sinkBits,
    dataBits = ioParams.dataBits
  )
  private val clusterHub = Module(new ClusterHub(node))
  private val chi2tl = Module(new TLULBridge(clusterHub.io.peripheral.node, 64, 3))
  private val cioXbar = Module(new CioXBar(Seq(cioParams), node.cpuNum))
  private val tl2chi = Module(new TLUL2ChiBridge(clusterHub.io.cio.node, cioXbar.io.downstream.last.params))
  private val clusterPeriCx = Module(new ClusterPeriBlock(Seq(chi2tl.tl.params, cioXbar.io.downstream.head.params), node.cpuNum))

  chi2tl.icn <> clusterHub.io.peripheral
  clusterPeriCx.io.tls.head <> chi2tl.tl
  clusterPeriCx.io.tls.last <> cioXbar.io.downstream.head
  tl2chi.tlm <> cioXbar.io.downstream.last
  clusterHub.io.cio <> tl2chi.icn

  val io = IO(new Bundle {
    val icn = new ClusterDeviceBundle(node)
    val csu = new AlwaysOnDomainBundle(node.copy(nodeType = NodeType.RF), ioParams)
  })
  private val resetSync = withClockAndReset(implicitClock, io.icn.ccn.reset) { ResetGen(dft = Some(io.icn.dft.reset)) }
  private val pll = Module(new ClockManagerWrapper)
  private val l2rc = Module(new IcnSideRationalCrossing(node.copy(nodeType = NodeType.RF)))
  private val iorc = Module(new TileLinkRationalSlv(ioParams))
  private val rmp = PeripheralRemapper(iorc.io.tlm.a.bits.address, p)
  iorc.io.rc <> io.csu.cio
  connectByName(cioXbar.io.upstream.head.a, iorc.io.tlm.a)
  connectByName(iorc.io.tlm.d, cioXbar.io.upstream.head.d)
  cioXbar.io.upstream.head.a.bits.address := rmp
  pll.io.in_clock := io.icn.osc_clock
  implicitClock := pll.io.cpu_clock
  implicitReset := resetSync
  io.csu.clock := pll.io.cpu_clock
  io.csu.reset := resetSync

  io.icn <> clusterHub.io.icn
  l2rc.io.chi <> clusterHub.io.l2cache
  io.csu.l2cache <> l2rc.io.rc
  io.csu.cpu <> clusterHub.io.cpu
  io.csu.dft := clusterHub.io.dft
  cioXbar.misc.chip := clusterHub.io.cpu.mhartid(0)(clusterIdBits - 1, nodeAidBits)
  pll.io.cfg := clusterPeriCx.io.pllCfg
  clusterPeriCx.io.pllLock := pll.io.lock

  for(i <- 0 until node.cpuNum) {
    clusterPeriCx.io.cpu(i).defaultBootAddr := clusterHub.io.cpu.defaultBootAddr(i)
    clusterPeriCx.io.cpu(i).defaultEnable := clusterHub.io.cpu.defaultCpuEnable(i)
    io.csu.cpu.defaultBootAddr(i) := clusterPeriCx.io.cpu(i).bootAddr
    io.csu.cpu.defaultCpuEnable(i) := clusterPeriCx.io.cpu(i).stop
    cioXbar.misc.core(i) := clusterHub.io.cpu.mhartid(i)
    clusterPeriCx.io.cpu(i).coreId := clusterHub.io.cpu.mhartid(i)(clusterIdBits - nodeAidBits - 1, 0)
    io.csu.imsic(i) <> clusterPeriCx.io.cpu(i).imsic
  }
}
