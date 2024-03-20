package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class GPIOIO extends Bundle {
  val out = Output(UInt(16.W))
  val in = Input(UInt(16.W))
  val seg = Output(Vec(8, UInt(8.W)))
}

class GPIOCtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val gpio = new GPIOIO
}

class gpio_top_apb extends BlackBox {
  val io = IO(new GPIOCtrlIO)
}

class gpioChisel extends Module {
  val io = IO(new GPIOCtrlIO)

  val led = RegInit(0.U(16.W))
  val switch = io.gpio.in
  val seg = RegInit(0.U(32.W))

  io.in.pready := true.B
  io.in.prdata := switch
  io.in.pslverr := false.B

  io.gpio.out := led

  def wdata_gen(org_data: UInt, data: UInt, mask: UInt): UInt = {
    Cat(Mux(mask(3), data(31, 24), org_data(31, 24)),
        Mux(mask(2), data(23, 16), org_data(23, 16)),
        Mux(mask(1), data(15, 8), org_data(15, 8)),
        Mux(mask(0), data(7, 0), org_data(7, 0)))
  }

  when (io.in.psel && io.in.penable && io.in.pwrite) {
    assert(io.in.paddr === 0x10002000L.U || io.in.paddr === 0x10002008L.U)
    val org_data = Mux(io.in.paddr === 0x10002000L.U, led, seg)
    val wdata = wdata_gen(org_data, io.in.pwdata, io.in.pstrb)
    when (io.in.paddr === 0x10002000L.U) {
      led := wdata
    } .otherwise {
      seg := wdata
    }
  }

  // printf("led: %x, switch: %x, seg: %x\n", led, switch, seg)

  // Segment display

  def seg_display(b: UInt): UInt = {
    val h = MuxLookup(b, "b0000000".U) (Seq (
      0.U  -> "b1000000".U,
      1.U  -> "b1111001".U,
      2.U  -> "b0100100".U,
      3.U  -> "b0110000".U,
      4.U  -> "b0011001".U,
      5.U  -> "b0010010".U,
      6.U  -> "b0000010".U,
      7.U  -> "b1111000".U,
      8.U  -> "b0000000".U,
      9.U  -> "b0010000".U,
      10.U -> "b0001000".U,
      11.U -> "b0000011".U,
      12.U -> "b1000110".U,
      13.U -> "b0100001".U,
      14.U -> "b0000110".U,
      15.U -> "b0001110".U
    ))
    h
  }

  for (i <- 0 until 8) {
    io.gpio.seg(i) := seg_display(seg(i*4+3, i*4).asUInt)
  }
}

class APBGPIO(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val node = APBSlaveNode(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      address       = address,
      executable    = true,
      supportsRead  = true,
      supportsWrite = true)),
    beatBytes  = 4)))

  private val outer = this

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val gpio_bundle = IO(new GPIOIO)

    val mgpio = Module(new gpioChisel)
    mgpio.io.clock := clock
    mgpio.io.reset := reset
    mgpio.io.in <> in
    gpio_bundle <> mgpio.io.gpio
  }
}
