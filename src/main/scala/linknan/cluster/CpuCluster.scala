package linknan.cluster

import chisel3._
import chisel3.experimental.hierarchy.core.IsLookupable
import chisel3.experimental.hierarchy.{Definition, Instance, instantiable, public}
import chisel3.util.ReadyValidIO
import darecreek.exu.vfu.{VFuParameters, VFuParamsKey}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBundle, TLBundleParameters, TLClientNode, TLEdgeIn, TLManagerNode, TLXbar}
import linknan.cluster.hub.{AlwaysOnDomain, ImsicBundle}
import linknan.cluster.hub.interconnect.ClusterDeviceBundle
import linknan.generator.TestIoOptionsKey
import linknan.utils.{DevSideRationalCrossing, TileLinkRationalMst, connectByName}
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey
import xijiang.{Node, NodeType}
import xijiang.router.base.DeviceIcnBundle
import xs.utils.tl.{TLUserKey, TLUserParams}
import zhujiang.chi.Flit
import zhujiang.{ZJParametersKey, ZJRawModule}

class BlockTestIO(val params:BlockTestIOParams)(implicit p:Parameters) extends Bundle {
  val clock = Output(Clock())
  val reset = Output(AsyncReset())
  val cio = Flipped(new TLBundle(params.ioParams))
  val mhartid = Output(UInt(p(ZJParametersKey).clusterIdBits.W))
  val imsic = if(p(TestIoOptionsKey).keepImsic) Some(new ImsicBundle) else None
  val l2 = if(p(TestIoOptionsKey).removeCsu) None else Some(Flipped(new TLBundle(params.l2Params)))
  val icn = if(p(TestIoOptionsKey).removeCsu) Some(Flipped(new DeviceIcnBundle(params.node))) else None
}

class TestCioXBar(cioEdge: TLEdgeIn, cpuNum:Int)(implicit p:Parameters) extends LazyModule {
  private val cioInNodes = Seq.fill(cpuNum)(TLClientNode(Seq(cioEdge.master)))
  private val cioXbar = LazyModule(new TLXbar)
  val cioOutNode = TLManagerNode(Seq(cioEdge.slave))
  cioInNodes.foreach(n => cioXbar.node :*= n)
  cioOutNode :*= cioXbar.node
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle{
      val cio = Vec(cpuNum, Flipped(new TLBundle(cioEdge.bundle)))
      val hub = new TLBundle(cioOutNode.edges.in.head.bundle)
    })
    cioInNodes.zip(io.cio).foreach({case(a, b) => b <> a.out.head._1})
    io.hub <> cioOutNode.in.head._1
  }
}

case class BlockTestIOParams(ioParams:TLBundleParameters, l2Params: TLBundleParameters, node:Node) extends IsLookupable

@instantiable
class CpuCluster(node:Node)(implicit p:Parameters) extends ZJRawModule {
  private val removeCsu = p(TestIoOptionsKey).removeCsu
  private val removeCore = p(TestIoOptionsKey).removeCore || p(TestIoOptionsKey).removeCsu
  private val dcacheParams = p(XSCoreParamsKey).dcacheParametersOpt.get

  private val coreGen = LazyModule(new CoreWrapper()(p.alterPartial({
    case TLUserKey => TLUserParams(aliasBits = dcacheParams.aliasBitsOpt.getOrElse(0))
    case VFuParamsKey => VFuParameters()
  })))
  private val coreDef = if(!removeCore) Some(Definition(coreGen.module)) else None
  private val coreSeq = if(!removeCore) Some(Seq.fill(node.cpuNum)(Instance(coreDef.get))) else None
  coreSeq.foreach(_.zipWithIndex.foreach({case(c, i) => c.suggestName(s"core_$i")}))
  private val cioEdge = coreGen.cioNode.edges.in.head
  private val cl2Edge = coreGen.l2Node.edges.in.head

  private val csu = if(removeCsu) None else Some(LazyModule(new ClusterSharedUnit(cioEdge, cl2Edge, node.copy(nodeType = NodeType.RF))))
  private val cioXbar = if(removeCsu) Some(LazyModule(new TestCioXBar(cioEdge, node.cpuNum))) else None
  private val finalCioEdge = if(csu.isDefined) csu.get.cioOutNode.edges.in.head else cioXbar.get.cioOutNode.edges.in.head
  private val hub = Module(new AlwaysOnDomain(node, finalCioEdge.bundle))
  private val _cioXbar = if(removeCsu) Some(withClockAndReset(hub.io.csu.clock, hub.io.csu.reset){ Module(cioXbar.get.module) }) else None
  private val _csu = csu.map(c => Module(c.module))

  private val btNode = if(p(TestIoOptionsKey).keepImsic) hub.io.csu.l2cache.node.copy(splitFlit = false) else hub.io.csu.l2cache.node
  @public val coreIoParams = BlockTestIOParams(cioEdge.bundle, cl2Edge.bundle, btNode)
  @public val icn = IO(new ClusterDeviceBundle(node))
  @public val core = if(removeCore) Some(IO(Vec(node.cpuNum, new BlockTestIO(coreIoParams)))) else None

  icn <> hub.io.icn
  if(removeCsu) {
    hub.io.csu := DontCare
    val hubIoRc = withClockAndReset(hub.io.csu.clock, hub.io.csu.reset){ Module(new TileLinkRationalMst(cioEdge.bundle)) }
    hubIoRc.io.tls <> _cioXbar.get.io.hub
    hub.io.csu.cio <> hubIoRc.io.rc
  } else {
    hub.io.csu <> _csu.get.io.hub
  }

  private def connChn[T <: Data](sink:Option[ReadyValidIO[T]], src:Option[ReadyValidIO[T]]):Unit = {
    if(sink.isDefined) {
      sink.get.valid := src.get.valid
      src.get.ready := sink.get.ready
      sink.get.bits := src.get.bits.asTypeOf(sink.get.bits)
    }
  }

  for(i <- 0 until node.cpuNum) {
    if(removeCsu) {
      core.get(i).clock := hub.io.csu.clock
      core.get(i).reset := hub.io.csu.reset
      val l2rc = withClockAndReset(hub.io.csu.clock, hub.io.csu.reset){ Module(new DevSideRationalCrossing(hub.io.csu.l2cache.node)) }
      l2rc.suggestName(s"l2rc_$i")
      _cioXbar.get.io.cio(i) <> core.get(i).cio
      l2rc.io.rc <> hub.io.csu.l2cache
      core.get(i).mhartid := hub.io.csu.cpu.mhartid(i)
      val topIcn = core.get(i).icn.get
      val hubIcn = l2rc.io.chi
      connChn(topIcn.rx.req, hubIcn.tx.req)
      connChn(topIcn.rx.resp, hubIcn.tx.resp)
      connChn(topIcn.rx.data, hubIcn.tx.data)
      connChn(topIcn.rx.snoop, hubIcn.tx.snoop)
      connChn(hubIcn.rx.req, topIcn.tx.req)
      connChn(hubIcn.rx.resp, topIcn.tx.resp)
      connChn(hubIcn.rx.data, topIcn.tx.data)
      connChn(hubIcn.rx.snoop, topIcn.tx.snoop)
      if(core.get(i).imsic.isDefined) {
        core.get(i).imsic.get <> hub.io.csu.imsic(i)
      } else {
        hub.io.csu.imsic(i).fromCpu := DontCare
      }
    } else if(removeCore) {
      val iorc = withClockAndReset(hub.io.csu.clock, hub.io.csu.reset){ Module(new TileLinkRationalMst(cioEdge.bundle)) }
      iorc.suggestName(s"iorc_$i")
      val l2rc = withClockAndReset(hub.io.csu.clock, hub.io.csu.reset){ Module(new TileLinkRationalMst(cl2Edge.bundle)) }
      l2rc.suggestName(s"l2rc_$i")
      iorc.io.tls <> core.get(i).cio
      l2rc.io.tls <> core.get(i).l2.get
      _csu.get.io.core(i) := DontCare
      core.get(i).reset := _csu.get.io.core(i).reset
      core.get(i).clock := _csu.get.io.core(i).clock
      _csu.get.io.core(i).cio <> iorc.io.rc
      _csu.get.io.core(i).l2 <> l2rc.io.rc
      core.get(i).mhartid := _csu.get.io.core(i).mhartid
      if(core.get(i).imsic.isDefined) {
        core.get(i).imsic.get <> _csu.get.io.core(i).imsic
      } else {
        _csu.get.io.core(i).imsic.fromCpu := DontCare
      }
    } else {
      _csu.get.io.core(i) <> coreSeq.get(i).io
    }
  }
}