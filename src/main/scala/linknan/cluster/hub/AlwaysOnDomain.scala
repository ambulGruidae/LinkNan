package linknan.cluster.hub

import chisel3._
import freechips.rocketchip.tilelink.TLBundleParameters
import linknan.cluster.hub.interconnect.{CioXBar, ClusterDeviceBundle, ClusterHub, ClusterMiscWires}
import linknan.cluster.hub.peripheral.{ClusterPLL, CpuCtrl}
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import xijiang.router.base.IcnBundle
import zhujiang.{DftWires, ZJBundle, ZJModule, ZJParametersKey, ZJRawModule}
import zhujiang.device.bridge.tlul.TLULBridge
import linknan.soc.PeripheralRemapper
import xs.utils.{ClockManagerWrapper, ResetGen}
import zhujiang.device.tlu2chi.TLUL2ChiBridge
import zhujiang.tilelink.{TLUBuffer, TLULBundle, TilelinkParams}

class AlwaysOnDomainBundle(val node: Node, ioParams:TLBundleParameters)(implicit p: Parameters) extends ZJBundle {
  private val cioParams = TilelinkParams(
    addrBits = ioParams.addressBits,
    sourceBits = ioParams.sourceBits,
    sinkBits = ioParams.sinkBits,
    dataBits = ioParams.dataBits
  )
  val l2cache = new IcnBundle(node)
  val cio = Vec(node.cpuNum, Flipped(new TLULBundle(cioParams)))
  val cpu = Flipped(new ClusterMiscWires(node))
  val imisc = Vec(node.cpuNum, new ImsicBundle)
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
  private val cioXbar = Module(new CioXBar(Seq.fill(node.cpuNum)(cioParams)))
  private val tl2chi = Module(new TLUL2ChiBridge(clusterHub.io.cio.node, cioXbar.io.downstream.last.params))
  private val clusterPeriCx = Module(new ClusterPeriBlock(Seq(chi2tl.tl.params, cioXbar.io.downstream.head.params), node.cpuNum))

  chi2tl.icn <> clusterHub.io.peripheral
  clusterPeriCx.io.tls.head <> chi2tl.tl
  clusterPeriCx.io.tls.last <> cioXbar.io.downstream.head
  tl2chi.tlm <> cioXbar.io.downstream.last
  clusterHub.io.cio <> tl2chi.icn

  val io = IO(new Bundle {
    val icn = new ClusterDeviceBundle(node)
    val cluster = new AlwaysOnDomainBundle(node.copy(nodeType = NodeType.RF), ioParams)
  })
  private val resetSync = withClockAndReset(implicitClock, io.icn.ccn.reset) { ResetGen(dft = Some(io.icn.dft.reset)) }
  private val pll = Module(new ClockManagerWrapper)
  pll.io.in_clock := io.icn.osc_clock
  implicitClock := pll.io.cpu_clock
  implicitReset := resetSync
  io.cluster.clock := pll.io.cpu_clock
  io.cluster.reset := resetSync

  io.icn <> clusterHub.io.icn
  clusterHub.io.l2cache <> io.cluster.l2cache
  io.cluster.cpu <> clusterHub.io.cpu
  io.cluster.dft := clusterHub.io.dft
  cioXbar.misc.chip := clusterHub.io.cpu.mhartid(0)(clusterIdBits - 1, nodeAidBits)
  pll.io.cfg := clusterPeriCx.io.pllCfg
  clusterPeriCx.io.pllLock := pll.io.lock

  for(i <- 0 until node.cpuNum) {
    val cio = TLUBuffer(io.cluster.cio(i), name = Some(s"cio_buf_$i"))
    val rmp = PeripheralRemapper(cio.a.bits.address, p)
    cioXbar.io.upstream(i) <> cio
    cioXbar.io.upstream(i).a.bits.address := rmp
    clusterPeriCx.io.cpu(i).defaultBootAddr := clusterHub.io.cpu.defaultBootAddr(i)
    clusterPeriCx.io.cpu(i).defaultEnable := clusterHub.io.cpu.defaultCpuEnable(i)
    io.cluster.cpu.defaultBootAddr(i) := clusterPeriCx.io.cpu(i).bootAddr
    io.cluster.cpu.defaultCpuEnable(i) := clusterPeriCx.io.cpu(i).stop
    cioXbar.misc.core(i) := clusterHub.io.cpu.mhartid(i)
    clusterPeriCx.io.cpu(i).coreId := clusterHub.io.cpu.mhartid(i)(clusterIdBits - nodeAidBits - 1, 0)
    io.cluster.imisc(i) <> clusterPeriCx.io.cpu(i).imsic
  }
}
