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
import linknan.utils.connectByName
import org.chipsalliance.cde.config.Parameters
import xiangshan.{HasXSParameter, XSCoreParamsKey}
import xijiang.Node
import xijiang.router.base.DeviceIcnBundle
import xs.utils.ResetGen
import xs.utils.tl.{TLUserKey, TLUserParams}
import zhujiang.{DftWires, ZJRawModule}
import zhujiang.chi.{DataFlit, ReqFlit, RespFlit, SnoopFlit}
import zhujiang.tilelink.TLUBuffer

class L2Wrapper(l2EdgeIn: TLEdgeIn, node:Node)(implicit p:Parameters) extends LazyModule with BindingScope with HasXSParameter {
  private val l2cache = LazyModule(new SimpleL2CacheDecoupled)
  private val l2xbar = LazyModule(new TLXbar)
  private val l2binder = LazyModule(new BankBinder(64 * (coreParams.L2NBanks - 1)))
  private val l2EccIntSink = IntSinkNode(IntSinkPortSimple(1, 1))
  private val l2param = p(L2ParamKey)
  private val cachePortNode = Seq.fill(node.cpuNum)(TLClientNode(Seq(l2EdgeIn.master)))
  for(i <- 0 until node.cpuNum) {
    l2xbar.node :*= TLBuffer.chainNode(1, Some(s"core_${i}_cache_buffer")) :*= cachePortNode(i)
  }
  l2binder.node :*= l2xbar.node
  for(i <- 0 until l2param.nrSlice) l2cache.sinkNodes(i) :*= TLBuffer.chainNode(2, Some(s"l2_in_buffer")) :*= l2binder.node
  l2EccIntSink := l2cache.eccIntNode

  lazy val module = new Impl
  class Impl extends LazyRawModuleImp(this) {
    val io = IO(new Bundle {
      val clock = Input(Clock())
      val reset = Input(AsyncReset())
      val eccIntr = Output(Bool())
      val core = Vec(node.cpuNum, Flipped(new TLBundle(l2EdgeIn.bundle)))
      val hub = new DeviceIcnBundle(node)
      val dft = Input(new DftWires)
    })
    private val resetSync = withClockAndReset(io.clock, io.reset) { ResetGen(dft = Some(io.dft.reset))}
    childClock := io.clock
    childReset := resetSync

    private val l2 = l2cache.module
    private val txreq = Wire(Decoupled(new ReqFlit))
    private val txrsp = Wire(Decoupled(new RespFlit))
    private val txdat = Wire(Decoupled(new DataFlit))
    private val rxrsp = Wire(Decoupled(new RespFlit))
    private val rxdat = Wire(Decoupled(new DataFlit))
    private val rxsnp = Wire(Decoupled(new SnoopFlit))

    io.hub.tx.req.get.valid := txreq.valid
    txreq.ready := io.hub.tx.req.get.ready
    io.hub.tx.req.get.bits := txreq.bits.asTypeOf(io.hub.tx.req.get.bits)

    io.hub.tx.resp.get.valid := txrsp.valid
    txrsp.ready := io.hub.tx.resp.get.ready
    io.hub.tx.resp.get.bits := txrsp.bits.asTypeOf(io.hub.tx.resp.get.bits)

    io.hub.tx.data.get.valid := txdat.valid
    txdat.ready := io.hub.tx.data.get.ready
    io.hub.tx.data.get.bits := txdat.bits.asTypeOf(io.hub.tx.data.get.bits)

    rxrsp.valid := io.hub.rx.resp.get.valid
    io.hub.rx.resp.get.ready := rxrsp.ready
    rxrsp.bits := io.hub.rx.resp.get.bits.asTypeOf(rxrsp.bits)

    rxdat.valid := io.hub.rx.data.get.valid
    io.hub.rx.data.get.ready := rxdat.ready
    rxdat.bits := io.hub.rx.data.get.bits.asTypeOf(rxdat.bits)

    rxsnp.valid := io.hub.rx.snoop.get.valid
    io.hub.rx.snoop.get.ready := rxsnp.ready
    rxsnp.bits := io.hub.rx.snoop.get.bits.asTypeOf(rxsnp.bits)

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

    io.eccIntr := l2EccIntSink.in.head._1.head
    for(i <- 0 until node.cpuNum) {
      cachePortNode(i).out.head._1 <> io.core(i)
    }
  }
}

class ClusterSharedUnit(cioEdge: TLEdgeIn, l2EdgeIn: TLEdgeIn, node:Node)(implicit p:Parameters) extends ZJRawModule
  with ImplicitClock with ImplicitReset {
  val io = IO(new Bundle{
    val core = Vec(node.cpuNum, Flipped(new CoreWrapperIO(cioEdge.bundle, l2EdgeIn.bundle)))
    val hub = Flipped(new AlwaysOnDomainBundle(node, cioEdge.bundle))
  })
  val implicitReset = io.hub.reset
  val implicitClock = io.hub.clock
  private val dcacheParams = p(XSCoreParamsKey).dcacheParametersOpt.get
  private val l2Params = p(L2ParamKey)
  private val l2cache = LazyModule(new L2Wrapper(l2EdgeIn, node)(p.alterPartial({
    case TLUserKey => TLUserParams(aliasBits = dcacheParams.aliasBitsOpt.getOrElse(0))
    case L2ParamKey => l2Params.copy(
      nrClients = node.cpuNum,
      chiBundleParams = Some(CHIBundleParameters(
        nodeIdBits = niw,
        addressBits = raw
      ))
    )
  })))

  private val _l2cache = Module(l2cache.module)
  _l2cache.io.hub <> io.hub.l2cache
  _l2cache.io.dft := io.hub.dft
  _l2cache.io.clock := io.hub.clock
  _l2cache.io.reset := io.hub.reset

  for(i <- 0 until node.cpuNum) {
    val core = io.core(i)
    val cioBuffer = Module(new TLUBuffer(io.hub.cio(i).params))
    cioBuffer.suggestName(s"cioBuffer_$i")
    _l2cache.io.core(i) <> core.l2
    io.hub.cio(i) <> cioBuffer.io.out
    connectByName(cioBuffer.io.in.a, core.cio.a)
    connectByName(core.cio.d, cioBuffer.io.in.d)
    cioBuffer.io.in.a.bits.address := core.cio.a.bits.address | (1L << (raw - 1)).U
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
    core.imsic <> io.hub.imisc(i)
    io.hub.cpu.resetState(i) := core.reset_state
    io.hub.cpu.beu(i) := DontCare
    core.dft := io.hub.dft
  }
}
