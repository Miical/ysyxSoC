package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SDRAMIO extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs  = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we  = Output(Bool())
  val a   = Output(UInt(13.W))
  val ba  = Output(UInt(2.W))
  val dqm = Output(UInt(2.W))
  val dq  = Analog(16.W)
}

class sdram_top_axi extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)))
    val sdram = new SDRAMIO
  })
}

class sdram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val sdram = new SDRAMIO
  })
}

class sdram extends BlackBox {
  val io = IO(Flipped(new SDRAMIO))
}

class sdramChisel extends RawModule {
  val io = IO(Flipped(new SDRAMIO))

  val dqout = Wire(UInt(16.W))
  val dqout_en = Wire(Bool())
  dqout := 0.U
  dqout_en := false.B
  val dqin = TriStateInBuf(io.dq, dqout, dqout_en)

  withClockAndReset(io.clk.asClock, (!io.cke).asAsyncReset) {
    val set_mode :: auto_refresh :: precharge :: active :: write :: read :: burst_terminate :: nop :: Nil = Enum(8)

    val mem = Mem(4 * 8192 * 512, UInt(16.W))

    val cmd = Cat(io.cs, io.ras, io.cas, io.we)
    val mode = RegInit(0.U(13.W))
    val cas_latency = mode(6, 4)
    val burst_length = mode(2, 0)

    val row = RegInit(0.U(log2Ceil(8192).W))

    val write_burst_cnt = RegInit(0.U(3.W))
    val write_burst_addr = RegInit(0.U(log2Ceil(4 * 8192 * 512).W))
    val read_burst_cnt = RegInit(0.U(3.W))
    val read_burst_addr = RegInit(0.U(log2Ceil(4 * 8192 * 512).W))

    val rdelay_data = Mem(16, UInt(16.W))
    val rdelay_en = Mem(16, Bool())
    rdelay_data(0) := 0.U
    rdelay_en(0) := false.B
    for (i <- 1 until 16) {
      rdelay_data(i) := rdelay_data(i - 1)
      rdelay_en(i) := rdelay_en(i - 1)
    }

    when (cmd === set_mode) {
      mode := io.a
      // printf("set mode: %x\n", io.a)
    } .elsewhen (cmd === active) {
      row := (io.ba * 8192.U) + io.a
      // printf("active row: %x\n", io.a)
    } .elsewhen (cmd === read) {
      val col = io.a
      val unit_addr = (row * 512.U) + io.a

      read_burst_addr := unit_addr
      read_burst_cnt := burst_length

      rdelay_data(0) := mem.read(unit_addr)
      rdelay_en(0) := true.B
    } .elsewhen (cmd === write) {
      val col = io.a
      val unit_addr = (row * 512.U) + io.a

      write_burst_addr := unit_addr
      write_burst_cnt := burst_length

      mem.write(unit_addr, dqin)
      // printf("write: %x\n", dqin)
    }

    when (write_burst_cnt =/= 0.U) {
      write_burst_cnt := write_burst_cnt - 1.U
      mem.write(write_burst_addr + (burst_length - write_burst_cnt + 1.U), dqin)
      // printf("write burst addr = %x, data = %x\n", write_burst_addr + (burst_length - write_burst_cnt + 1.U), dqin)
    }

    when (read_burst_cnt =/= 0.U) {
      read_burst_cnt := read_burst_cnt - 1.U
      rdelay_data(0) := mem.read(read_burst_addr + (burst_length - read_burst_cnt + 1.U))
      rdelay_en(0) := true.B
    }

    when (rdelay_en(cas_latency - 1.U)) {
      dqout := rdelay_data(cas_latency - 1.U)
      dqout_en := true.B
      // printf("read: %x\n", rdelay_data(cas_latency - 1.U))
    }

    // when (cmd =/= 7.U && cmd =/= 1.U) { printf("cmd = %x\n", cmd) }
  }
}

class AXI4SDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val beatBytes = 8
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
        address       = address,
        executable    = true,
        supportsWrite = TransferSizes(1, beatBytes),
        supportsRead  = TransferSizes(1, beatBytes),
        interleavedId = Some(0))
    ),
    beatBytes  = beatBytes)))

  private val outer = this

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_axi)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}

class APBSDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_apb)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}
