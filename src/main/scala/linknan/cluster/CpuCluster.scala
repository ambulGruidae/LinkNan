package linknan.cluster

import chisel3._
import chisel3.experimental.hierarchy.core.IsLookupable
import chisel3.experimental.hierarchy.{Definition, Instance, instantiable, public}
import chisel3.util.ReadyValidIO
import darecreek.exu.vfu.{VFuParameters, VFuParamsKey}
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.tilelink.{TLBundle, TLBundleParameters}
import linknan.cluster.hub.{AlwaysOnDomain, ImsicBundle}
import linknan.cluster.hub.interconnect.ClusterDeviceBundle
import linknan.generator.TestIoOptionsKey
import linknan.utils.connectByName
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey
import xijiang.Node
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

  private val hubNode = if(p(TestIoOptionsKey).keepImsic) node.copy(splitFlit = false) else node
  private val hub = Module(new AlwaysOnDomain(hubNode, cioEdge.bundle))
  private val csu = if(p(TestIoOptionsKey).removeCsu) None else Some(Module(new ClusterSharedUnit(cioEdge, cl2Edge, hub.io.cluster.node)))

  @public val coreIoParams = BlockTestIOParams(cioEdge.bundle, cl2Edge.bundle, hub.io.cluster.node)
  @public val icn = IO(new ClusterDeviceBundle(node))
  @public val core = if(removeCore) Some(IO(Vec(node.cpuNum, new BlockTestIO(coreIoParams)))) else None

  hub.io.icn.misc <> icn.misc
  hub.io.icn.osc_clock := icn.osc_clock
  hub.io.icn.dft := icn.dft
  hub.io.icn.ccn.reset := icn.ccn.reset
  hub.io.icn.ccn.async.foreach(_ <> icn.ccn.async.get)
  if(hub.io.icn.ccn.sync.isDefined) {
    val hubIcn = hub.io.icn.ccn.sync.get
    val nocIcn = icn.ccn.sync.get
    def connChn[T <: Data](sink:Option[ReadyValidIO[T]], src:Option[ReadyValidIO[T]]):Unit = {
      if(sink.isDefined) {
        sink.get.valid := src.get.valid
        src.get.ready := sink.get.ready
        sink.get.bits := src.get.bits.asTypeOf(sink.get.bits)
      }
    }
    connChn(hubIcn.rx.req, nocIcn.rx.req)
    connChn(hubIcn.rx.resp, nocIcn.rx.resp)
    connChn(hubIcn.rx.data, nocIcn.rx.data)
    connChn(hubIcn.rx.snoop, nocIcn.rx.snoop)
    connChn(nocIcn.tx.req, hubIcn.tx.req)
    connChn(nocIcn.tx.resp, hubIcn.tx.resp)
    connChn(nocIcn.tx.data, hubIcn.tx.data)
    connChn(nocIcn.tx.snoop, hubIcn.tx.snoop)
  }

  if(removeCsu) {
    hub.io.cluster := DontCare
  } else {
    hub.io.cluster <> csu.get.io.hub
  }

  for(i <- 0 until node.cpuNum) {
    if(removeCsu) {
      core.get(i).clock := hub.io.cluster.clock
      core.get(i).reset := hub.io.cluster.reset
      connectByName(hub.io.cluster.cio(i).a, core.get(i).cio.a)
      connectByName(core.get(i).cio.d, hub.io.cluster.cio(i).d)
      core.get(i).mhartid := hub.io.cluster.cpu.mhartid(i)
      hub.io.cluster.l2cache <> core.get(i).icn.get
      if(core.get(i).imsic.isDefined) {
        core.get(i).imsic.get <> hub.io.cluster.imsic(i)
      } else {
        hub.io.cluster.imsic(i).fromCpu := DontCare
      }
    } else if(removeCore) {
      csu.get.io.core(i) := DontCare
      core.get(i).reset := csu.get.io.core(i).reset
      core.get(i).clock := csu.get.io.core(i).clock
      csu.get.io.core(i).cio <> core.get(i).cio
      csu.get.io.core(i).l2 <> core.get(i).l2.get
      core.get(i).mhartid := csu.get.io.core(i).mhartid
      if(core.get(i).imsic.isDefined) {
        core.get(i).imsic.get <> csu.get.io.core(i).imsic
      } else {
        csu.get.io.core(i).imsic.fromCpu := DontCare
      }
    } else {
      csu.get.io.core(i) <> coreSeq.get(i).io
    }
  }
}