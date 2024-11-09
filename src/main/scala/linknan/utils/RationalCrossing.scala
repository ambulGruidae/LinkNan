package linknan.utils

import chisel3._
import chisel3.util.ReadyValidIO
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.Node
import xijiang.router.base.{DeviceIcnBundle, IcnBundle}
import zhujiang.{ZJBundle, ZJModule}

class TileLinkRationalIO(params: TLBundleParameters) extends Bundle {
  val a = new RationalIO(new TLBundleA(params))
  val b = if(params.hasBCE) Some(Flipped(new RationalIO(new TLBundleB(params)))) else None
  val c = if(params.hasBCE) Some(new RationalIO(new TLBundleC(params))) else None
  val d = Flipped(new RationalIO(new TLBundleD(params)))
  val e = if(params.hasBCE) Some(new RationalIO(new TLBundleE(params))) else None
}

class TileLinkRationalMst(params: TLBundleParameters, direction:RationalDirection = Flexible) extends Module {
  val io = IO(new Bundle {
    val tls = Flipped(new TLBundle(params))
    val rc = new TileLinkRationalIO(params)
  })
  private val bce = params.hasBCE
  private val aSource = Module(new RationalCrossingSource(new TLBundleA(params), direction))
  private val bSink = if(bce) Some(Module(new RationalCrossingSink(new TLBundleB(params), direction.flip))) else None
  private val cSource = if(bce) Some(Module(new RationalCrossingSource(new TLBundleC(params), direction))) else None
  private val dSink = Module(new RationalCrossingSink(new TLBundleD(params), direction.flip))
  private val eSource = if(bce) Some(Module(new RationalCrossingSource(new TLBundleE(params), direction))) else None

  aSource.io.enq <> io.tls.a
  io.rc.a <> aSource.io.deq

  dSink.io.enq <> io.rc.d
  io.tls.d <> dSink.io.deq

  if(bce) {
    bSink.get.io.enq <> io.rc.b.get
    io.tls.b <> bSink.get.io.deq

    cSource.get.io.enq <> io.tls.c
    io.rc.c.get <> cSource.get.io.deq

    eSource.get.io.enq <> io.tls.e
    io.rc.e.get <> eSource.get.io.deq
  }
}

class TileLinkRationalSlv(params: TLBundleParameters, direction:RationalDirection = Flexible) extends Module {
  val io = IO(new Bundle {
    val rc = Flipped(new TileLinkRationalIO(params))
    val tlm = new TLBundle(params)
  })
  private val bce = params.hasBCE
  private val aSink = Module(new RationalCrossingSink(new TLBundleA(params), direction.flip))
  private val bSource = if(bce) Some(Module(new RationalCrossingSource(new TLBundleB(params), direction))) else None
  private val cSink = if(bce) Some(Module(new RationalCrossingSink(new TLBundleC(params), direction.flip))) else None
  private val dSource = Module(new RationalCrossingSource(new TLBundleD(params), direction))
  private val eSink = if(bce) Some(Module(new RationalCrossingSink(new TLBundleE(params), direction.flip))) else None

  aSink.io.enq <> io.rc.a
  io.tlm.a <> aSink.io.deq

  dSource.io.enq <> io.tlm.d
  io.rc.d <> dSource.io.deq

  if(bce) {
    bSource.get.io.enq <> io.tlm.b
    io.rc.b.get <> bSource.get.io.deq

    cSink.get.io.enq <> io.rc.c.get
    io.tlm.c <> cSink.get.io.deq

    eSink.get.io.enq <> io.rc.e.get
    io.tlm.e <> eSink.get.io.deq
  }
}

class IcnRationalIO(val node:Node)(implicit p:Parameters) extends ZJBundle {
  val i2dreq = if(node.ejects.contains("REQ") || node.ejects.contains("ERQ")) Some(new RationalIO(UInt(reqFlitBits.W))) else None
  val i2drsp = if(node.ejects.contains("RSP")) Some(new RationalIO(UInt(respFlitBits.W))) else None
  val i2ddat = if(node.ejects.contains("DAT")) Some(new RationalIO(UInt(dataFlitBits.W))) else None
  val i2dsnp = if(node.ejects.contains("SNP")) Some(new RationalIO(UInt(snoopFlitBits.W))) else None

  val d2ireq = if(node.injects.contains("REQ") || node.injects.contains("ERQ")) Some(Flipped(new RationalIO(UInt(reqFlitBits.W)))) else None
  val d2irsp = if(node.injects.contains("RSP")) Some(Flipped(new RationalIO(UInt(respFlitBits.W)))) else None
  val d2idat = if(node.injects.contains("DAT")) Some(Flipped(new RationalIO(UInt(dataFlitBits.W)))) else None
  val d2isnp = if(node.injects.contains("SNP")) Some(Flipped(new RationalIO(UInt(snoopFlitBits.W)))) else None
}

trait IcnRantionalHelper {
  private def connRV[T <: Data, K <:Data](sink:ReadyValidIO[T], src:ReadyValidIO[K]):Unit = {
    sink.valid := src.valid
    src.ready := sink.ready
    sink.bits := src.bits.asTypeOf(sink.bits)
  }

  def connSrc[T <: Data, K <:Data](rc: Option[RationalIO[T]], src:Option[ReadyValidIO[K]], direction:RationalDirection):Unit = {
    if(src.isDefined) {
      val srcRc = Module(new RationalCrossingSource(rc.get.bits0.cloneType, direction))
      connRV(srcRc.io.enq, src.get)
      rc.get <> srcRc.io.deq
    }
  }

  def connSink[T <: Data, K <:Data](rc: Option[RationalIO[T]], sink:Option[ReadyValidIO[K]], direction:RationalDirection):Unit = {
    if(sink.isDefined) {
      val sinkRc = Module(new RationalCrossingSink(rc.get.bits0.cloneType, direction.flip))
      sinkRc.io.enq <> rc.get
      connRV(sink.get, sinkRc.io.deq)
    }
  }
}
class IcnSideRationalCrossing(node:Node, direction:RationalDirection = Flexible)(implicit p:Parameters)
  extends ZJModule with IcnRantionalHelper {
  val io = IO(new Bundle{
    val chi = new DeviceIcnBundle(node)
    val rc = new IcnRationalIO(node)
  })
  connSrc(io.rc.i2dreq, io.chi.rx.req, direction)
  connSrc(io.rc.i2drsp, io.chi.rx.resp, direction)
  connSrc(io.rc.i2ddat, io.chi.rx.data, direction)
  connSrc(io.rc.i2dsnp, io.chi.rx.snoop, direction)

  connSink(io.rc.d2ireq, io.chi.tx.req, direction)
  connSink(io.rc.d2irsp, io.chi.tx.resp, direction)
  connSink(io.rc.d2idat, io.chi.tx.data, direction)
  connSink(io.rc.d2isnp, io.chi.tx.snoop, direction)
}

class DevSideRationalCrossing(node:Node, direction:RationalDirection = Flexible)(implicit p:Parameters)
  extends ZJModule with IcnRantionalHelper {
  val io = IO(new Bundle{
    val rc = Flipped(new IcnRationalIO(node))
    val chi = new IcnBundle(node)
  })
  connSink(io.rc.i2dreq, io.chi.tx.req, direction)
  connSink(io.rc.i2drsp, io.chi.tx.resp, direction)
  connSink(io.rc.i2ddat, io.chi.tx.data, direction)
  connSink(io.rc.i2dsnp, io.chi.tx.snoop, direction)

  connSrc(io.rc.d2ireq, io.chi.rx.req, direction)
  connSrc(io.rc.d2irsp, io.chi.rx.resp, direction)
  connSrc(io.rc.d2idat, io.chi.rx.data, direction)
  connSrc(io.rc.d2isnp, io.chi.rx.snoop, direction)
}