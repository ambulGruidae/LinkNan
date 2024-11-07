package linknan.cluster

import chisel3._
import chisel3.experimental.hierarchy.core.IsLookupable
import chisel3.experimental.hierarchy.{Definition, Instance, instantiable, public}
import darecreek.exu.vfu.{VFuParameters, VFuParamsKey}
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.tilelink.{TLBundle, TLBundleParameters}
import linknan.cluster.hub.{AlwaysOnDomain, ImsicBundle}
import linknan.cluster.hub.interconnect.ClusterDeviceBundle
import linknan.generator.TestIoOptionsKey
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey
import xijiang.Node
import xs.utils.tl.{TLUserKey, TLUserParams}
import zhujiang.{ZJParametersKey, ZJRawModule}

class CoreBlockTestIO(params:CoreBlockTestIOParams)(implicit p:Parameters) extends Bundle {
  val clock = Output(Clock())
  val reset = Output(AsyncReset())
  val cio = Flipped(new TLBundle(params.ioParams))
  val l2 = Flipped(new TLBundle(params.l2Params))
  val imsic = if(p(TestIoOptionsKey).keepImsic) Some(new ImsicBundle) else None
  val mhartid = Output(UInt(p(ZJParametersKey).clusterIdBits.W))
}

case class CoreBlockTestIOParams(ioParams:TLBundleParameters, l2Params: TLBundleParameters) extends IsLookupable

@instantiable
class CpuCluster(node:Node)(implicit p:Parameters) extends ZJRawModule {
  private val removeCore = p(TestIoOptionsKey).removeCore
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

  private val hub = Module(new AlwaysOnDomain(node, cioEdge.bundle))
  private val csu = Module(new ClusterSharedUnit(cioEdge, cl2Edge, hub.io.cluster.node))

  @public val coreIoParams = CoreBlockTestIOParams(cioEdge.bundle, cl2Edge.bundle)
  @public val icn = IO(new ClusterDeviceBundle(node))
  @public val core = if(removeCore) Some(IO(Vec(node.cpuNum, new CoreBlockTestIO(coreIoParams)))) else None

  icn <> hub.io.icn
  hub.io.cluster <> csu.io.hub

  if(removeCore) {
    for(i <- 0 until node.cpuNum) {
      core.get(i).l2 <> csu.io.core(i).l2
      core.get(i).cio <> csu.io.core(i).cio
      core.get(i).reset := csu.io.core(i).reset
      core.get(i).clock := csu.io.core(i).clock
      core.get(i).mhartid <> csu.io.core(i).mhartid
      if(core.get(i).imsic.isDefined) {
        core.get(i).imsic.get <> csu.io.core(i).imsic
      } else {
        csu.io.core(i).imsic.fromCpu := DontCare
      }
      csu.io.core(i).halt := false.B
      csu.io.core(i).icacheErr := DontCare
      csu.io.core(i).dcacheErr := DontCare
      csu.io.core(i).reset_state := false.B
    }
  } else {
    for(i <- 0 until node.cpuNum) csu.io.core(i) <> coreSeq.get(i).io
  }
}