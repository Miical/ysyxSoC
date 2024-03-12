package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SPIIO(val ssWidth: Int = 8) extends Bundle {
  val sck = Output(Bool())
  val ss = Output(UInt(ssWidth.W))
  val mosi = Output(Bool())
  val miso = Input(Bool())
}

class spi_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val spi = new SPIIO
    val spi_irq_out = Output(Bool())
  })
}

class flash extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

object SPIParameters {
  val SPI_START = 0x10001000L
  val Tx0 = SPI_START
  val Rx0 = Tx0
  val Tx1 = SPI_START + 0x04L
  val Rx1 = Tx1

  val CTRL = SPI_START + 0x10L
  val DIVIDER = SPI_START + 0x14L
  val SS = SPI_START + 0x18L
}

class APBSPI(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val spi_bundle = IO(new SPIIO)

    val mspi = Module(new spi_top_apb)
    mspi.io.clock := clock
    mspi.io.reset := reset
    mspi.io.in <> in
    spi_bundle <> mspi.io.spi

    // Flash XIP Asscess

    val state = RegInit(0.U(4.W))
    val normal :: send_addr :: set_divider :: set_ss :: set_ctrl :: wait_resp :: set_ss2 :: read_data :: return_data :: Nil = Enum(9)

    def byteRev(data: UInt): UInt = {
      val byte0 = data(7, 0)
      val byte1 = data(15, 8)
      val byte2 = data(23, 16)
      val byte3 = data(31, 24)
      Cat(byte0, byte1, byte2, byte3)
    }

    when (state =/= normal) {
      assert(!in.pwrite, "Flash XIP access should be read only")
      in.pready := false.B
      mspi.io.in.psel := false.B
      mspi.io.in.penable := false.B
      mspi.io.in.pstrb := 0xf.U
    }
    when (state === send_addr) {
      mspi.io.in.paddr := SPIParameters.Tx1.U
      mspi.io.in.pwrite := true.B
      mspi.io.in.psel := RegNext(!mspi.io.in.pready)
      mspi.io.in.penable := (RegNext(mspi.io.in.psel) && mspi.io.in.psel)
      mspi.io.in.pwdata := ((in.paddr & 0x00ffffffL.U) | 0x03000000L.U)
    } .elsewhen (state === set_divider) {
      mspi.io.in.paddr := SPIParameters.DIVIDER.U
      mspi.io.in.pwrite := true.B
      mspi.io.in.psel := RegNext(!mspi.io.in.pready)
      mspi.io.in.penable := (RegNext(mspi.io.in.psel) && mspi.io.in.psel)
      mspi.io.in.pwdata := 0x1.U
    } .elsewhen (state === set_ss) {
      mspi.io.in.paddr := SPIParameters.SS.U
      mspi.io.in.pwrite := true.B
      mspi.io.in.psel := RegNext(!mspi.io.in.pready)
      mspi.io.in.penable := (RegNext(mspi.io.in.psel) && mspi.io.in.psel)
      mspi.io.in.pwdata := 0x1.U
    } .elsewhen(state === set_ctrl) {
      mspi.io.in.paddr := SPIParameters.CTRL.U
      mspi.io.in.pwrite := true.B
      mspi.io.in.psel := RegNext(!mspi.io.in.pready)
      mspi.io.in.penable := (RegNext(mspi.io.in.psel) && mspi.io.in.psel)
      mspi.io.in.pwdata := 0x140.U
    } .elsewhen(state === wait_resp) {
      mspi.io.in.paddr := SPIParameters.CTRL.U
      mspi.io.in.pwrite := false.B
      mspi.io.in.psel := RegNext(!mspi.io.in.pready)
      mspi.io.in.penable := (RegNext(mspi.io.in.psel) && mspi.io.in.psel)
    } .elsewhen(state === set_ss2) {
      mspi.io.in.paddr := SPIParameters.SS.U
      mspi.io.in.pwrite := true.B
      mspi.io.in.psel := RegNext(!mspi.io.in.pready)
      mspi.io.in.penable := (RegNext(mspi.io.in.psel) && mspi.io.in.psel)
      mspi.io.in.pwdata := 0x0.U
    }
    .elsewhen(state === read_data) {
      mspi.io.in.paddr := SPIParameters.Rx0.U
      mspi.io.in.pwrite := false.B
      mspi.io.in.psel := RegNext(!mspi.io.in.pready)
      mspi.io.in.penable := (RegNext(mspi.io.in.psel) && mspi.io.in.psel)
    } .elsewhen(state === return_data) {
      in.prdata := byteRev(mspi.io.in.prdata)
      in.pready := true.B
    }

    val flash_req_fire = (state === normal && 0x30000000L.U <= in.paddr <= 0x3fffffffL.U && in.psel)
    val flash_send_addr_done = (state === send_addr && !mspi.io.in.psel)
    val flash_set_divider_done = (state === set_divider && !mspi.io.in.psel)
    val flash_set_ss_done = (state === set_ss && !mspi.io.in.psel)
    val flash_set_ctrl_done = (state === set_ctrl && !mspi.io.in.psel)
    val flash_wait_resp_done = (state === wait_resp && !mspi.io.in.psel && mspi.io.in.prdata(8) === 0.U)
    val flash_set_ss2_done = (state === set_ss2 && !mspi.io.in.psel)
    val flash_read_data_done = (state === read_data && !mspi.io.in.psel)
    val flash_return_data_done = (state === return_data && in.pready && in.psel)

    state := MuxLookup(state, normal) (Seq(
      normal -> Mux(flash_req_fire, send_addr, normal),
      send_addr -> Mux(flash_send_addr_done, set_divider, send_addr),
      set_divider -> Mux(flash_set_divider_done, set_ss, set_divider),
      set_ss -> Mux(flash_set_ss_done, set_ctrl, set_ss),
      set_ctrl -> Mux(flash_set_ctrl_done, wait_resp, set_ctrl),
      wait_resp -> Mux(flash_wait_resp_done, set_ss2, wait_resp),
      set_ss2 -> Mux(flash_set_ss2_done, read_data, set_ss2),
      read_data -> Mux(flash_read_data_done, return_data, read_data),
      return_data -> Mux(flash_return_data_done, normal, return_data)
    ))

  }
}
