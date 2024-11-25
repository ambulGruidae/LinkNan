package linknan.cluster.power

import chisel3._

package object controller {
  val devActiveBits = 3

  object PowerMode {
    val powerModeBits = 2
    val OFF = 0.U(powerModeBits.W)
    val RET = 1.U(powerModeBits.W)
    val ON = 2.U(powerModeBits.W)
  }
}
