package linknan.cluster.hub

import aia.{CSRToIMSICBundle, IMSICParams, IMSICToCSRBundle, TLIMSIC}
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLClientNode, TLMasterParameters, TLMasterPortParameters}
import linknan.cluster.hub.interconnect.{ClusterPeriParams, PeriXBar}
import linknan.cluster.hub.peripheral.{ClusterPLL, CpuCtrl}
import linknan.soc.LinkNanParamsKey
import org.chipsalliance.cde.config.Parameters
import zhujiang.ZJBundle
import zhujiang.tilelink.{TLULBundle, TilelinkParams}

class ImsicBundle(implicit p:Parameters) extends Bundle {
  val fromCpu = Flipped(new CSRToIMSICBundle(p(LinkNanParamsKey).imiscParams))
  val toCpu = new IMSICToCSRBundle(p(LinkNanParamsKey).imiscParams)
}

class ImsicWrapper(mst:TilelinkParams)(implicit p:Parameters) extends LazyModule {
  private val imiscParams = p(LinkNanParamsKey).imiscParams
  private val inner = LazyModule(new TLIMSIC(imiscParams, 8))
  private val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "cfg",
      sourceId = IdRange(0, 1 << mst.sourceBits)
    ))
  )
  private val clientNode = TLClientNode(Seq(clientParameters))
  inner.fromMem :*= TLBuffer() :*= clientNode

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle{
      val tls = Flipped(new TLULBundle(mst))
      val imsic = new ImsicBundle
    })

    private def connByName(sink:ReadyValidIO[Bundle], src:ReadyValidIO[Bundle]):Unit = {
      sink.valid := src.valid
      src.ready := sink.ready
      sink.bits := DontCare
      val recvMap = sink.bits.elements.map(e => (e._1.toLowerCase, e._2))
      val sendMap = src.bits.elements.map(e => (e._1.toLowerCase, e._2))
      for((name, data) <- recvMap) {
        if(sendMap.contains(name)) data := sendMap(name).asTypeOf(data)
      }
    }

    connByName(clientNode.out.head._1.a, io.tls.a)
    connByName(io.tls.d, clientNode.out.head._1.d)
    io.imsic.toCpu := inner.module.toCSR
    inner.module.fromCSR := io.imsic.fromCpu
  }
}

class ClusterPeriCtlBundle(implicit p:Parameters) extends ZJBundle {
  val imsic = new ImsicBundle
  val bootAddr = Output(UInt(64.W))
  val stop = Output(Bool())
  val defaultBootAddr = Input(UInt(64.W))
  val defaultEnable = Input(Bool())
  val coreId = Input(UInt((clusterIdBits - nodeAidBits).W))
}

class ClusterPeriBlock(tlParams: Seq[TilelinkParams], coreNum:Int)(implicit p:Parameters) extends Module {
  private val privateSeq = Seq.tabulate(coreNum)(i => Seq(
    ClusterPeriParams(s"imisc_$i", Seq((0x0000, 0x8000), (0x8000, 0x9000)), Some(i)),
    ClusterPeriParams(s"cpu_ctl_$i", Seq((0x9000, 0xA000)), Some(i))
  )).reduce(_ ++ _)

  private val sharedSeq = Seq(
    ClusterPeriParams("pll", Seq((0x1_0000, 0x2_0000)), None),
  )
  private val periSeq = privateSeq ++ sharedSeq
  private val periXBar = Module(new PeriXBar(tlParams, periSeq, coreNum))

  private val imsicSeq = Seq.tabulate(coreNum) { i=>
    val imsic = LazyModule(new ImsicWrapper(periXBar.io.downstream.head.params))
    val _imisc = Module(imsic.module)
    _imisc.suggestName(s"imisc_$i")
    (i, _imisc)
  }
  private val cpuCtlSeq = Seq.tabulate(coreNum) { i=>
    val cpuCtl = Module(new CpuCtrl(periXBar.io.downstream.head.params))
    cpuCtl.suggestName(s"cpu_ctl_$i")
    (i, cpuCtl)
  }
  private val pllCtl = Module(new ClusterPLL(periXBar.io.downstream.head.params))

  private val downstreams = periSeq.zip(periXBar.io.downstream)

  for((i, imisc) <- imsicSeq) {
    val cfg = downstreams.filter(_._1.name == s"imisc_$i").map(_._2).head
    imisc.io.tls <> cfg
  }

  for((i, cpuCtl) <- cpuCtlSeq) {
    val cfg = downstreams.filter(_._1.name == s"cpu_ctl_$i").map(_._2).head
    cpuCtl.tls <> cfg
  }

  pllCtl.tls <> downstreams.filter(_._1.name == s"pll").map(_._2).head

  val io = IO(new Bundle{
    val tls = MixedVec(tlParams.map(t => Flipped(new TLULBundle(t))))
    val cpu = Vec(coreNum, new ClusterPeriCtlBundle)
    val pllCfg = Output(Vec(8, UInt(32.W)))
    val pllLock = Input(Bool())
  })
  pllCtl.io.lock := io.pllLock
  io.pllCfg := pllCtl.io.cfg

  periXBar.cores.zip(io.cpu).foreach({case(a, b) => a := b.coreId})
  io.tls.zip(periXBar.io.upstream).foreach {case(a, b) => a <> b}

  for(i <- 0 until coreNum) {
    io.cpu(i).imsic <> imsicSeq(i)._2.io.imsic
    io.cpu(i).bootAddr := cpuCtlSeq(i)._2.io.cpuBootAddr
    io.cpu(i).stop := cpuCtlSeq(i)._2.io.cpuReset
    cpuCtlSeq(i)._2.io.defaultBootAddr := io.cpu(i).defaultBootAddr
    cpuCtlSeq(i)._2.io.defaultEnable := io.cpu(i).defaultEnable
  }
}
