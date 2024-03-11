package ysyx

import chisel3._
import chisel3.util._


class bitrev extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class bitrevChisel extends RawModule { // we do not need clock and reset
  val io = IO(Flipped(new SPIIO(1)))

  withClockAndReset(io.sck.asClock, io.ss.asBool.asAsyncReset) {
    val bits = RegInit(0.U(8.W))
    val cnt = RegInit(0.U(4.W))
    io.miso := true.B
    cnt := cnt + 1.U


    when (cnt(3)) {
      io.miso := bits(cnt(2, 0))
      printf("out: cnt: %d, io.miso: %d\n", cnt, io.miso)
    }.otherwise {
      printf("cnt: %d, io.mosi: %d\n", cnt, io.mosi)
      bits := (bits | (io.mosi << (7.U - cnt(2, 0))))
    }
  }
}
