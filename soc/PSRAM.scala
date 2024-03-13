package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class QSPIIO extends Bundle {
  val sck = Output(Bool())
  val ce_n = Output(Bool())
  val dio = Analog(4.W)
}

class psram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val qspi = new QSPIIO
  })
}

class psram extends BlackBox {
  val io = IO(Flipped(new QSPIIO))
}

class psramChisel extends RawModule {
  val io = IO(Flipped(new QSPIIO))

  val dout = Wire(UInt(4.W))
  val dout_en = Wire(Bool())
  dout := 0.U
  dout_en := false.B
  val din = TriStateInBuf(io.dio, dout, dout_en)

  withClockAndReset(io.sck.asClock, io.ce_n.asBool.asAsyncReset) {
    val psram_size = 0x100000
    val cmd_len = 8
    val addr_len = 24 / 4
    val read_delay_len = 7
    val data_len = 32 / 4


    val mem = Mem(psram_size, UInt(8.W))

    val cnt = RegInit(0.U(8.W))
    cnt := cnt + 1.U


    val cmd = RegInit(0.U(cmd_len.W))
    val addr = RegInit(0.U(24.W))
    val data = RegInit(0.U(32.W))

    val is_write = (cmd === 0x38.U)

    when (cnt < cmd_len.U) {
      cmd := (cmd << 1.U | din(0))
    }
    .elsewhen (cmd_len.U <= cnt && cnt < (cmd_len + addr_len).U) {
      addr := (addr << 4.U | din)
    }
    .otherwise {
      // Read
      when (!is_write) {
        when (cnt === (cmd_len + addr_len + 2).U) {
          data := (mem(addr) | (mem(addr + 1.U) << 8.U) | (mem(addr + 2.U) << 16.U) | (mem(addr + 3.U) << 24.U))
        }
        when ((cmd_len + addr_len + read_delay_len).U <= cnt) {
          dout := data(3, 0)
          dout_en := true.B
          data := data >> 4.U
        }
      }

      // Write
      .otherwise {
        when (!cnt(0)) {
          data := din
        } .otherwise {
          mem(addr + (cnt - (cmd_len + addr_len).U)(7, 1)) := Cat(din, data(3, 0))
          printf("write: addr: %x, data: %x\n", addr + (cnt - (cmd_len + addr_len).U)(7, 1), Cat(din, data(3, 0)))
        }
      }
    }

    printf("cnt: %d, addr: %x, cmd: %x, di: %b\n", cnt, addr, cmd, din)
  }
}

class APBPSRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val qspi_bundle = IO(new QSPIIO)

    val mpsram = Module(new psram_top_apb)
    mpsram.io.clock := clock
    mpsram.io.reset := reset
    mpsram.io.in <> in
    qspi_bundle <> mpsram.io.qspi
  }
}
