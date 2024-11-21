package linknan.cluster

import chisel3._
import chisel3.util._
import SimpleL2.Configs.L2ParamKey
import SimpleL2.SimpleL2CacheDecoupled
import SimpleL2.chi.CHIBundleParameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import linknan.cluster.hub.AlwaysOnDomainBundle
import linknan.generator.TestIoOptionsKey
import linknan.utils._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{HasXSParameter, XSCoreParamsKey}
import xijiang.Node
import xijiang.router.base.DeviceIcnBundle
import xs.utils.ResetGen
import xs.utils.tl.{TLUserKey, TLUserParams}
import zhujiang.HasZJParams
import zhujiang.chi._

class ClusterSharedUnit(cioEdge: TLEdgeIn, l2EdgeIn: TLEdgeIn, node:Node)(implicit p:Parameters) extends LazyModule with HasZJParams
  with BindingScope with HasXSParameter {
  private val dcacheParams = p(XSCoreParamsKey).dcacheParametersOpt.get
  private val l2Params = p(L2ParamKey)
  private val l2cache = LazyModule(new SimpleL2CacheDecoupled()(p.alterPartial({
    case TLUserKey => TLUserParams(aliasBits = dcacheParams.aliasBitsOpt.getOrElse(0))
    case L2ParamKey => l2Params.copy(
      nrClients = node.cpuNum,
      alwaysWriteBackFull = true,
      chiBundleParams = Some(CHIBundleParameters(
        nodeIdBits = niw,
        addressBits = raw
      ))
    )
  })))
  private val l2xbar = LazyModule(new TLXbar)
  private val l2binder = LazyModule(new BankBinder(64 * (coreParams.L2NBanks - 1)))
  private val l2EccIntSink = IntSinkNode(IntSinkPortSimple(1, 1))
  private val l2param = p(L2ParamKey)
  private val cachePortNodes = Seq.fill(node.cpuNum)(TLClientNode(Seq(l2EdgeIn.master)))
  cachePortNodes.foreach(n => l2xbar.node :*= n)
  l2binder.node :*= l2xbar.node
  for(i <- 0 until l2param.nrSlice) l2cache.sinkNodes(i) :*= TLBuffer.chainNode(2, Some(s"l2_in_buffer")) :*= l2binder.node
  l2EccIntSink := l2cache.eccIntNode

  private val cioInNodes = Seq.fill(node.cpuNum)(TLClientNode(Seq(cioEdge.master)))
  private val cioXbar = LazyModule(new TLXbar)
  val cioOutNode = TLManagerNode(Seq(cioEdge.slave))
  cioInNodes.foreach(n => cioXbar.node :*= n)
  cioOutNode :*= cioXbar.node

  lazy val module = new Impl
  class Impl extends LazyRawModuleImp(this) with ImplicitClock with ImplicitReset {
    val io = IO(new Bundle{
      val core = Vec(node.cpuNum, Flipped(new CoreWrapperIO(cioEdge.bundle, l2EdgeIn.bundle)))
      val hub = Flipped(new AlwaysOnDomainBundle(node, cioOutNode.in.head._2.bundle))
    })
    private val csuEnable = if(p(TestIoOptionsKey).removeCore) true.B else io.hub.cpu.defaultCpuEnable.reduce(_ | _)
    private val allReset = io.hub.reset.asBool || !csuEnable
    private val resetSync = withClockAndReset(io.hub.clock, allReset.asAsyncReset) { ResetGen(dft = Some(io.hub.dft.reset))}
    childClock := io.hub.clock
    childReset := resetSync
    def implicitClock = childClock
    def implicitReset = childReset
    private val ioHubRc = Module(new TileLinkRationalMst(cioOutNode.in.head._2.bundle))
    private val l2HubRc = Module(new DevSideRationalCrossing(node))

    for(i <- 0 until node.cpuNum) {
      val core = io.core(i)
      val l2rc = Module(new TileLinkRationalSlv(l2EdgeIn.bundle))
      val iorc = Module(new TileLinkRationalSlv(cioEdge.bundle))
      l2rc.suggestName(s"l2rc_$i")
      iorc.suggestName(s"iorc_$i")
      l2rc.io.rc <> core.l2
      iorc.io.rc <> core.cio
      cachePortNodes(i).out.head._1 <> l2rc.io.tlm
      cioInNodes(i).out.head._1 <> iorc.io.tlm
      core.clock := io.hub.clock
      val coreReset = io.hub.reset.asBool || !io.hub.cpu.defaultCpuEnable(i)
      core.reset := withClockAndReset(io.hub.clock, coreReset.asAsyncReset) { ResetGen(dft = Some(io.hub.dft.reset)) }
      core.mhartid := io.hub.cpu.mhartid(i)
      core.reset_vector := io.hub.cpu.defaultBootAddr(i)
      io.hub.cpu.halt(i) := core.halt
      core.msip := io.hub.cpu.msip(i)
      core.mtip := io.hub.cpu.mtip(i)
      core.meip := io.hub.cpu.meip(i)
      core.seip := io.hub.cpu.seip(i)
      core.dbip := io.hub.cpu.dbip(i)
      core.imsic <> io.hub.imsic(i)
      io.hub.cpu.resetState(i) := core.reset_state
      io.hub.cpu.beu(i) := DontCare
      core.dft := io.hub.dft
    }
    ioHubRc.io.tls <> cioOutNode.in.head._1
    ioHubRc.io.tls.a.bits.address := cioOutNode.in.head._1.a.bits.address | (1L << (raw - 1)).U
    io.hub.cio <> ioHubRc.io.rc

    private val l2 = l2cache.module
    private val l2rcChi = Wire(new DeviceIcnBundle(node))
    private val txreq = Wire(Decoupled(new ReqFlit))
    private val txrsp = Wire(Decoupled(new RespFlit))
    private val txdat = Wire(Decoupled(new DataFlit))
    private val rxrsp = Wire(Decoupled(new RespFlit))
    private val rxdat = Wire(Decoupled(new DataFlit))
    private val rxsnp = Wire(Decoupled(new SnoopFlit))

    l2HubRc.io.rc <> io.hub.l2cache
    l2HubRc.io.chi <> l2rcChi

    l2rcChi.tx.req.get.valid := txreq.valid
    txreq.ready := l2rcChi.tx.req.get.ready
    l2rcChi.tx.req.get.bits := txreq.bits.asTypeOf(l2rcChi.tx.req.get.bits)

    l2rcChi.tx.resp.get.valid := txrsp.valid
    txrsp.ready := l2rcChi.tx.resp.get.ready
    l2rcChi.tx.resp.get.bits := txrsp.bits.asTypeOf(l2rcChi.tx.resp.get.bits)

    l2rcChi.tx.data.get.valid := txdat.valid
    txdat.ready := l2rcChi.tx.data.get.ready
    l2rcChi.tx.data.get.bits := txdat.bits.asTypeOf(l2rcChi.tx.data.get.bits)

    rxrsp.valid := l2rcChi.rx.resp.get.valid
    l2rcChi.rx.resp.get.ready := rxrsp.ready
    rxrsp.bits := l2rcChi.rx.resp.get.bits.asTypeOf(rxrsp.bits)

    rxdat.valid := l2rcChi.rx.data.get.valid
    l2rcChi.rx.data.get.ready := rxdat.ready
    rxdat.bits := l2rcChi.rx.data.get.bits.asTypeOf(rxdat.bits)

    rxsnp.valid := l2rcChi.rx.snoop.get.valid
    l2rcChi.rx.snoop.get.ready := rxsnp.ready
    rxsnp.bits := l2rcChi.rx.snoop.get.bits.asTypeOf(rxsnp.bits)

    connectByName(txreq, l2.io.chi.txreq)
    connectByName(txrsp, l2.io.chi.txrsp)
    connectByName(txdat, l2.io.chi.txdat)
    connectByName(l2.io.chi.rxrsp, rxrsp)
    connectByName(l2.io.chi.rxdat, rxdat)
    connectByName(l2.io.chi.rxsnp, rxsnp)
    txreq.bits.LPID := l2.io.chi.txreq.bits.lpID
    txreq.bits.SnoopMe := l2.io.chi.txreq.bits.snoopMe
    txdat.bits.FwdState := l2.io.chi.txdat.bits.fwdState
    l2.io.chi.rxdat.bits.fwdState := rxdat.bits.FwdState

    l2.io.chi_tx_rxsactive := true.B
    l2.io.chi_tx_linkactiveack := true.B
    l2.io.chi_rx_linkactivereq := true.B
    l2.io.nodeID := DontCare
  }
}
