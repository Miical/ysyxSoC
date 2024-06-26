// See LICENSE for license details.
package sifive.blocks.devices.chiplink

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class SourceE(info: ChipLinkInfo) extends Module
{
  val io = IO(new Bundle {
    val e = Decoupled(new TLBundleE(info.edgeOut.bundle))
    val q = Flipped(Decoupled(UInt(info.params.dataBits.W)))
  })

  // Extract header fields
  val Seq(_, _, _, _, _, q_sink) = info.decode(io.q.bits)

  io.q.ready := io.e.ready
  io.e.valid := io.q.valid
  io.e.bits.sink := q_sink
}
