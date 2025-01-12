// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import Chisel._
import Chisel.ImplicitConversions._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.experimental._

case class FPUParams(
  fLen: Int = 64,
  divSqrt: Boolean = true,
  sfmaLatency: Int = 3,
  dfmaLatency: Int = 4
)

object FPConstants
{
  val RM_SZ = 3
  val FLAGS_SZ = 5
}
import FPConstants._

object POSITConstants
{
  val posit_size = 8
  val posit_exponent_size = 1
  val latency = 2
}
import POSITConstants._


trait HasFPUCtrlSigs {
  val ldst = Bool()
  val wen = Bool()
  val ren1 = Bool()
  val ren2 = Bool()
  val ren3 = Bool()
  val swap12 = Bool()
  val swap23 = Bool()
  val singleIn = Bool()
  val singleOut = Bool()
  val fromint = Bool()
  val toint = Bool()
  val fastpipe = Bool()
  val fma = Bool()
  val div = Bool()
  val sqrt = Bool()
  val wflags = Bool()
}


class FPUCtrlSigs extends Bundle with HasFPUCtrlSigs

class FPUDecoder(implicit p: Parameters) extends FPUModule()(p) {
  val io = new Bundle {
    val inst = Bits(INPUT, 32)
    val sigs = new FPUCtrlSigs().asOutput
  }

  val default =       List(X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X)
  val f =
    Array(FLW      -> List(Y,Y,N,N,N,X,X,X,X,N,N,N,N,N,N,N),
          FSW      -> List(Y,N,N,Y,N,Y,X,N,Y,N,Y,N,N,N,N,N),
          FMV_S_X  -> List(N,Y,N,N,N,X,X,Y,N,Y,N,N,N,N,N,N),
          FCVT_S_W -> List(N,Y,N,N,N,X,X,Y,Y,Y,N,N,N,N,N,Y),
          FCVT_S_WU-> List(N,Y,N,N,N,X,X,Y,Y,Y,N,N,N,N,N,Y),
          FCVT_S_L -> List(N,Y,N,N,N,X,X,Y,Y,Y,N,N,N,N,N,Y),
          FCVT_S_LU-> List(N,Y,N,N,N,X,X,Y,Y,Y,N,N,N,N,N,Y),
          FMV_X_S  -> List(N,N,Y,N,N,N,X,N,Y,N,Y,N,N,N,N,N),
          FCLASS_S -> List(N,N,Y,N,N,N,X,Y,Y,N,Y,N,N,N,N,N),
          FCVT_W_S -> List(N,N,Y,N,N,N,X,Y,Y,N,Y,N,N,N,N,Y),
          FCVT_WU_S-> List(N,N,Y,N,N,N,X,Y,Y,N,Y,N,N,N,N,Y),
          FCVT_L_S -> List(N,N,Y,N,N,N,X,Y,Y,N,Y,N,N,N,N,Y),
          FCVT_LU_S-> List(N,N,Y,N,N,N,X,Y,Y,N,Y,N,N,N,N,Y),
          FEQ_S    -> List(N,N,Y,Y,N,N,N,Y,Y,N,Y,N,N,N,N,Y),
          FLT_S    -> List(N,N,Y,Y,N,N,N,Y,Y,N,Y,N,N,N,N,Y),
          FLE_S    -> List(N,N,Y,Y,N,N,N,Y,Y,N,Y,N,N,N,N,Y),
          FSGNJ_S  -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,Y,N,N,N,N),
          FSGNJN_S -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,Y,N,N,N,N),
          FSGNJX_S -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,Y,N,N,N,N),
          FMIN_S   -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,Y,N,N,N,Y),
          FMAX_S   -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,Y,N,N,N,Y),
          FADD_S   -> List(N,Y,Y,Y,N,N,Y,Y,Y,N,N,N,Y,N,N,Y),
          FSUB_S   -> List(N,Y,Y,Y,N,N,Y,Y,Y,N,N,N,Y,N,N,Y),
          FMUL_S   -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,N,Y,N,N,Y),
          FMADD_S  -> List(N,Y,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y),
          FMSUB_S  -> List(N,Y,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y),
          FNMADD_S -> List(N,Y,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y),
          FNMSUB_S -> List(N,Y,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y),
          FDIV_S   -> List(N,Y,Y,Y,N,N,N,Y,Y,N,N,N,N,Y,N,Y),
          FSQRT_S  -> List(N,Y,Y,N,N,N,X,Y,Y,N,N,N,N,N,Y,Y))
  val d =
    Array(FLD      -> List(Y,Y,N,N,N,X,X,X,N,N,N,N,N,N,N,N),
          FSD      -> List(Y,N,N,Y,N,Y,X,N,N,N,Y,N,N,N,N,N),
          FMV_D_X  -> List(N,Y,N,N,N,X,X,X,N,Y,N,N,N,N,N,N),
          FCVT_D_W -> List(N,Y,N,N,N,X,X,N,N,Y,N,N,N,N,N,Y),
          FCVT_D_WU-> List(N,Y,N,N,N,X,X,N,N,Y,N,N,N,N,N,Y),
          FCVT_D_L -> List(N,Y,N,N,N,X,X,N,N,Y,N,N,N,N,N,Y),
          FCVT_D_LU-> List(N,Y,N,N,N,X,X,N,N,Y,N,N,N,N,N,Y),
          FMV_X_D  -> List(N,N,Y,N,N,N,X,N,N,N,Y,N,N,N,N,N),
          FCLASS_D -> List(N,N,Y,N,N,N,X,N,N,N,Y,N,N,N,N,N),
          FCVT_W_D -> List(N,N,Y,N,N,N,X,N,N,N,Y,N,N,N,N,Y),
          FCVT_WU_D-> List(N,N,Y,N,N,N,X,N,N,N,Y,N,N,N,N,Y),
          FCVT_L_D -> List(N,N,Y,N,N,N,X,N,N,N,Y,N,N,N,N,Y),
          FCVT_LU_D-> List(N,N,Y,N,N,N,X,N,N,N,Y,N,N,N,N,Y),
          FCVT_S_D -> List(N,Y,Y,N,N,N,X,N,Y,N,N,Y,N,N,N,Y),
          FCVT_D_S -> List(N,Y,Y,N,N,N,X,Y,N,N,N,Y,N,N,N,Y),
          FEQ_D    -> List(N,N,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y),
          FLT_D    -> List(N,N,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y),
          FLE_D    -> List(N,N,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y),
          FSGNJ_D  -> List(N,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,N,N),
          FSGNJN_D -> List(N,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,N,N),
          FSGNJX_D -> List(N,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,N,N),
          FMIN_D   -> List(N,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,N,Y),
          FMAX_D   -> List(N,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,N,Y),
          FADD_D   -> List(N,Y,Y,Y,N,N,Y,N,N,N,N,N,Y,N,N,Y),
          FSUB_D   -> List(N,Y,Y,Y,N,N,Y,N,N,N,N,N,Y,N,N,Y),
          FMUL_D   -> List(N,Y,Y,Y,N,N,N,N,N,N,N,N,Y,N,N,Y),
          FMADD_D  -> List(N,Y,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,Y),
          FMSUB_D  -> List(N,Y,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,Y),
          FNMADD_D -> List(N,Y,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,Y),
          FNMSUB_D -> List(N,Y,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,Y),
          FDIV_D   -> List(N,Y,Y,Y,N,N,N,N,N,N,N,N,N,Y,N,Y),
          FSQRT_D  -> List(N,Y,Y,N,N,N,X,N,N,N,N,N,N,N,Y,Y))

  val insns = fLen match {
    case 32 => f
    case 64 => f ++ d
  }
  val decoder = DecodeLogic(io.inst, default, insns)
  val s = io.sigs
  val sigs = Seq(s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                 s.swap23, s.singleIn, s.singleOut, s.fromint, s.toint,
                 s.fastpipe, s.fma, s.div, s.sqrt, s.wflags)
  sigs zip decoder map {case(s,d) => s := d}
}

class FPUCoreIO(implicit p: Parameters) extends CoreBundle()(p) {
  val inst = Bits(INPUT, 32)
  val fromint_data = Bits(INPUT, xLen)

  val fcsr_rm = Bits(INPUT, FPConstants.RM_SZ)
  val fcsr_flags = Valid(Bits(width = FPConstants.FLAGS_SZ))

  val store_data = Bits(OUTPUT, fLen)
  val toint_data = Bits(OUTPUT, xLen)

  val dmem_resp_val = Bool(INPUT)
  val dmem_resp_type = Bits(INPUT, 3)
  val dmem_resp_tag = UInt(INPUT, 5)
  val dmem_resp_data = Bits(INPUT, fLen)

  val valid = Bool(INPUT)
  val fcsr_rdy = Bool(OUTPUT)
  val nack_mem = Bool(OUTPUT)
  val illegal_rm = Bool(OUTPUT)
  val killx = Bool(INPUT)
  val killm = Bool(INPUT)
  val dec = new FPUCtrlSigs().asOutput
  val sboard_set = Bool(OUTPUT)
  val sboard_clr = Bool(OUTPUT)
  val sboard_clra = UInt(OUTPUT, 5)

  val keep_clock_enabled = Bool(INPUT)
}

class FPUIO(implicit p: Parameters) extends FPUCoreIO ()(p) {
  val cp_req = Decoupled(new FPInput()).flip //cp doesn't pay attn to kill sigs
  val cp_resp = Decoupled(new FPResult())
}

class FPResult(implicit p: Parameters) extends CoreBundle()(p) {
  val data = Bits(width = fLen+1)
  val exc = Bits(width = FPConstants.FLAGS_SZ)
}

class IntToFPInput(implicit p: Parameters) extends CoreBundle()(p) with HasFPUCtrlSigs {
  val rm = Bits(width = FPConstants.RM_SZ)
  val typ = Bits(width = 2)
  val in1 = Bits(width = xLen)
}

class FPInput(implicit p: Parameters) extends CoreBundle()(p) with HasFPUCtrlSigs {
  val rm = Bits(width = FPConstants.RM_SZ)
  val fmaCmd = Bits(width = 2)
  val typ = Bits(width = 2)
  val in1 = Bits(width = fLen+1)
  val in2 = Bits(width = fLen+1)
  val in3 = Bits(width = fLen+1)

  override def cloneType = new FPInput().asInstanceOf[this.type]
}

case class FType(exp: Int, sig: Int) {
  def ieeeWidth = exp + sig
  def recodedWidth = ieeeWidth + 1

  def qNaN = UInt((BigInt(7) << (exp + sig - 3)) + (BigInt(1) << (sig - 2)), exp + sig + 1)
  def isNaN(x: UInt) = x(sig + exp - 1, sig + exp - 3).andR
  def isSNaN(x: UInt) = isNaN(x) && !x(sig - 2)

  def classify(x: UInt) = {
    val sign = x(sig + exp)
    val code = x(exp + sig - 1, exp + sig - 3)
    val codeHi = code(2, 1)
    val isSpecial = codeHi === UInt(3)

    val isHighSubnormalIn = x(exp + sig - 3, sig - 1) < UInt(2)
    val isSubnormal = code === UInt(1) || codeHi === UInt(1) && isHighSubnormalIn
    val isNormal = codeHi === UInt(1) && !isHighSubnormalIn || codeHi === UInt(2)
    val isZero = code === UInt(0)
    val isInf = isSpecial && !code(0)
    val isNaN = code.andR
    val isSNaN = isNaN && !x(sig-2)
    val isQNaN = isNaN && x(sig-2)

    Cat(isQNaN, isSNaN, isInf && !sign, isNormal && !sign,
        isSubnormal && !sign, isZero && !sign, isZero && sign,
        isSubnormal && sign, isNormal && sign, isInf && sign)
  }

  // convert between formats, ignoring rounding, range, NaN
  def unsafeConvert(x: UInt, to: FType) = if (this == to) x else {
    val sign = x(sig + exp)
    val fractIn = x(sig - 2, 0)
    val expIn = x(sig + exp - 1, sig - 1)
    val fractOut = fractIn << to.sig >> sig
    val expOut = {
      val expCode = expIn(exp, exp - 2)
      val commonCase = (expIn + (1 << to.exp)) - (1 << exp)
      Mux(expCode === 0 || expCode >= 6, Cat(expCode, commonCase(to.exp - 3, 0)), commonCase(to.exp, 0))
    }
    Cat(sign, expOut, fractOut)
  }

  def recode(x: UInt) = hardfloat.recFNFromFN(exp, sig, x)
  def ieee(x: UInt) = hardfloat.fNFromRecFN(exp, sig, x)
}

object FType {
  val S = new FType(8, 24)
  val D = new FType(11, 53)

  val all = List(S, D)
}

trait HasFPUParameters {
  require(fLen == 32 || fLen == 64)
  val fLen: Int
  def xLen: Int
  val minXLen = 32
  val nIntTypes = log2Ceil(xLen/minXLen) + 1
  val floatTypes = FType.all.filter(_.ieeeWidth <= fLen)
  val minType = floatTypes.head
  val maxType = floatTypes.last
  def prevType(t: FType) = floatTypes(typeTag(t) - 1)
  val maxExpWidth = maxType.exp
  val maxSigWidth = maxType.sig
  def typeTag(t: FType) = floatTypes.indexOf(t)

  private def isBox(x: UInt, t: FType): Bool = x(t.sig + t.exp, t.sig + t.exp - 4).andR

  private def box(x: UInt, xt: FType, y: UInt, yt: FType): UInt = {
    require(xt.ieeeWidth == 2 * yt.ieeeWidth)
    val swizzledNaN = Cat(
      x(xt.sig + xt.exp, xt.sig + xt.exp - 3),
      x(xt.sig - 2, yt.recodedWidth - 1).andR,
      x(xt.sig + xt.exp - 5, xt.sig),
      y(yt.recodedWidth - 2),
      x(xt.sig - 2, yt.recodedWidth - 1),
      y(yt.recodedWidth - 1),
      y(yt.recodedWidth - 3, 0))
    Mux(xt.isNaN(x), swizzledNaN, x)
  }

  // implement NaN unboxing for FU inputs
  def unbox(x: UInt, tag: UInt, exactType: Option[FType]): UInt = {
    val outType = exactType.getOrElse(maxType)
    def helper(x: UInt, t: FType): Seq[(Bool, UInt)] = {
      val prev =
        if (t == minType) {
          Seq()
        } else {
          val prevT = prevType(t)
          val unswizzled = Cat(
            x(prevT.sig + prevT.exp - 1),
            x(t.sig - 1),
            x(prevT.sig + prevT.exp - 2, 0))
          val prev = helper(unswizzled, prevT)
          val isbox = isBox(x, t)
          prev.map(p => (isbox && p._1, p._2))
        }
      prev :+ (true.B, t.unsafeConvert(x, outType))
    }

    val (oks, floats) = helper(x, maxType).unzip
    if (exactType.isEmpty || floatTypes.size == 1) {
      Mux(oks(tag), floats(tag), maxType.qNaN)
    } else {
      val t = exactType.get
      floats(typeTag(t)) | Mux(oks(typeTag(t)), 0.U, t.qNaN)
    }
  }

  // make sure that the redundant bits in the NaN-boxed encoding are consistent
  def consistent(x: UInt): Bool = {
    def helper(x: UInt, t: FType): Bool = if (typeTag(t) == 0) true.B else {
      val prevT = prevType(t)
      val unswizzled = Cat(
        x(prevT.sig + prevT.exp - 1),
        x(t.sig - 1),
        x(prevT.sig + prevT.exp - 2, 0))
      val prevOK = !isBox(x, t) || helper(unswizzled, prevT)
      val curOK = !t.isNaN(x) || x(t.sig + t.exp - 4) === x(t.sig - 2, prevT.recodedWidth - 1).andR
      prevOK && curOK
    }
    helper(x, maxType)
  }

  // generate a NaN box from an FU result
  def box(x: UInt, t: FType): UInt = {
    if (t == maxType) {
      x
    } else {
      val nt = floatTypes(typeTag(t) + 1)
      val bigger = box(UInt((BigInt(1) << nt.recodedWidth)-1), nt, x, t)
      bigger | UInt((BigInt(1) << maxType.recodedWidth) - (BigInt(1) << nt.recodedWidth))
    }
  }

  // generate a NaN box from an FU result
  def box(x: UInt, tag: UInt): UInt = {
    val opts = floatTypes.map(t => box(x, t))
    opts(tag)
  }

  // zap bits that hardfloat thinks are don't-cares, but we do care about
  def sanitizeNaN(x: UInt, t: FType): UInt = {
    if (typeTag(t) == 0) {
      x
    } else {
      val maskedNaN = x & ~UInt((BigInt(1) << (t.sig-1)) | (BigInt(1) << (t.sig+t.exp-4)), t.recodedWidth)
      Mux(t.isNaN(x), maskedNaN, x)
    }
  }

  // implement NaN boxing and recoding for FL*/fmv.*.x
  def recode(x: UInt, tag: UInt): UInt = {
    def helper(x: UInt, t: FType): UInt = {
      if (typeTag(t) == 0) {
        t.recode(x)
      } else {
        val prevT = prevType(t)
        box(t.recode(x), t, helper(x, prevT), prevT)
      }
    }

    // fill MSBs of subword loads to emulate a wider load of a NaN-boxed value
    val boxes = floatTypes.map(t => UInt((BigInt(1) << maxType.ieeeWidth) - (BigInt(1) << t.ieeeWidth)))
    helper(boxes(tag) | x, maxType)
  }

  // implement NaN unboxing and un-recoding for FS*/fmv.x.*
  def ieee(x: UInt, t: FType = maxType): UInt = {
    if (typeTag(t) == 0) {
      t.ieee(x)
    } else {
      val unrecoded = t.ieee(x)
      val prevT = prevType(t)
      val prevRecoded = Cat(
        x(prevT.recodedWidth-2),
        x(t.sig-1),
        x(prevT.recodedWidth-3, 0))
      val prevUnrecoded = ieee(prevRecoded, prevT)
      Cat(unrecoded >> prevT.ieeeWidth, Mux(t.isNaN(x), prevUnrecoded, unrecoded(prevT.ieeeWidth-1, 0)))
    }
  }
}

abstract class FPUModule(implicit p: Parameters) extends CoreModule()(p) with HasFPUParameters

class FPToInt_posit(val size: Int, val exponent_max_size: Int) (implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  class Output extends Bundle {
    val in = new FPInput
    val lt = Bool()
    val store = Bits(width = fLen)
    val toint = Bits(width = xLen)
    val exc = Bits(width = FPConstants.FLAGS_SZ)
    override def cloneType = new Output().asInstanceOf[this.type]
  }
  val io = new Bundle {
    val in = Valid(new FPInput).flip
    val out = Valid(new Output)
  }
  /*
  TODO: am uitat sa fac conversia la 64 de biti e infuctie de tag
  */

  val in = RegEnable(io.in.bits, io.in.valid)
  val valid = Reg(next=io.in.valid)

  val posit_lt = Module(new posit.PositL(exponent_max_size, size))
  val posit_eq = Module(new posit.PositE(exponent_max_size, size))
  posit_lt.io.i_bits_1 := in.in1
  posit_lt.io.i_bits_2 := in.in2
  posit_eq.io.i_bits_1 := in.in1
  posit_eq.io.i_bits_2 := in.in2

  val tag = !in.singleOut // TODO typeTag
  val store = Cat(Fill(64-size,0.U),in.in1(size-1,0))
  /*
  if((64-size) > 0) {
    store := Cat(Fill(64-size,0.U),in.in1(size-1,0))
  } else {
    store := in.in1(size-1,0)
  }
  */
  val toint = Wire(init = store)
  val intType = Wire(init = tag)
  io.out.bits.store := (floatTypes.map(t => Fill(maxType.ieeeWidth / t.ieeeWidth, store(t.ieeeWidth - 1, 0))): Seq[UInt])(tag)
  io.out.bits.toint := toint
  io.out.bits.exc := Bits(0)

  //tODO: Calsificare de vazut si functia asta
  when (in.rm(0)) {
    val classify_out = 0.U
    toint := classify_out
    intType := 0
  }

  when (in.wflags) { // feq/flt/fle, fcvt
    toint := (~in.rm & Cat(posit_lt.io.o_result, posit_eq.io.o_result)).orR
    intType := 0

    when (!in.ren2) { // fcvt
      val conv = Module(new posit.PositPositInt(exponent_max_size, size))
      conv.io.i_bits := store
      //Devazut asta  TODO: in functie de sign
      //conv.io.signedOut := ~in.typ(0)
      toint := conv.io.o_bits
    }
  }

  io.out.valid := valid
  io.out.bits.lt := posit_lt.io.o_result
  io.out.bits.in := in
}

class IntToFP_posit(val latency: Int, val size: Int, val exponent_max_size: Int) (implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  val io = new Bundle {
    val in = Valid(new IntToFPInput).flip
    val out = Valid(new FPResult)
  }

  //TODO de vazut pentru 64 de biti
  val in = Pipe(io.in)
  val tag = !in.bits.singleIn // TODO typeTag

  val mux = Wire(new FPResult)
  mux.exc := Bits(0)
  mux.data := in.bits.in1

  val intValue = {
    val res = Wire(init = in.bits.in1.asSInt)
    for (i <- 0 until nIntTypes-1) {
      val smallInt = in.bits.in1((minXLen << i) - 1, 0)
      when (in.bits.typ.extract(log2Ceil(nIntTypes), 1) === i) {
        res := Mux(in.bits.typ(0), smallInt.zext, smallInt.asSInt)
      }
    }
    res.asUInt
  }

  val posit_conv = Module(new posit.PositIntPosit(exponent_max_size, size))
  posit_conv.io.i_bits := intValue
  


  when (in.bits.wflags) { // fcvt
    //mux.data := Cat(Fill(32 - size,0.U),posit_conv.io.o_bits)
    if((32-size) > 0) {
      mux.data := Cat(Fill(32-size,0.U),posit_conv.io.o_bits)
    } else {
      mux.data := posit_conv.io.o_bits
    }
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class FPToFP_posit(val latency: Int, val size: Int, val exponent_max_size: Int)(implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  val io = new Bundle {
    val in = Valid(new FPInput).flip
    val out = Valid(new FPResult)
    val lt = Bool(INPUT) // from FPToInt
  }

  val in = Pipe(io.in)


  val posit_sgnj = Module(new posit.PositSGNJ(exponent_max_size, size))
  val posit_sgnjn = Module(new posit.PositSGNJN(exponent_max_size, size))
  val posit_sgnjx = Module(new posit.PositSGNJX(exponent_max_size, size))
  posit_sgnj.io.i_bits_1 := in.bits.in1
  posit_sgnj.io.i_bits_2 := in.bits.in2
  posit_sgnjn.io.i_bits_1 := in.bits.in1
  posit_sgnjn.io.i_bits_2 := in.bits.in2
  posit_sgnjx.io.i_bits_1 := in.bits.in1
  posit_sgnjx.io.i_bits_2 := in.bits.in2


  val fsgnj = Cat(Fill(64-size,0.U),Mux(in.bits.rm(1), posit_sgnjx.io.o_bits, Mux(in.bits.rm(0), posit_sgnjn.io.o_bits, posit_sgnj.io.o_bits)))(31, 0)
  /*
  if((32-size) > 0) {
    fsgnj := Cat(Fill(32-size,0.U),Mux(in.bits.rm(1), posit_sgnjx.io.o_bits, Mux(in.bits.rm(0), posit_sgnjn.io.o_bits, posit_sgnj.io.o_bits)))
  } else {
    fsgnj := Mux(in.bits.rm(1), posit_sgnjx.io.o_bits, Mux(in.bits.rm(0), posit_sgnjn.io.o_bits, posit_sgnj.io.o_bits))
  }
  */


  val fsgnjMux = Wire(new FPResult)
  fsgnjMux.exc := UInt(0)
  fsgnjMux.data := fsgnj


  val posit_min = Module(new posit.PositMin(exponent_max_size, size))
  val posit_max = Module(new posit.PositMax(exponent_max_size, size))
  posit_min.io.i_bits_1 := in.bits.in1
  posit_min.io.i_bits_2 := in.bits.in2
  posit_max.io.i_bits_1 := in.bits.in1
  posit_max.io.i_bits_2 := in.bits.in2

  when (in.bits.wflags) { // fmin/fmax
    fsgnjMux.exc := 0.U
    if((32-size) > 0) {
      fsgnjMux.data := Cat(Fill(32-size,0.U),Mux(in.bits.rm(0), posit_max.io.o_bits, posit_min.io.o_bits))
    } else {
      fsgnjMux.data := Mux(in.bits.rm(0), posit_max.io.o_bits, posit_min.io.o_bits)
    }
    
  }

  val inTag = !in.bits.singleIn // TODO typeTag
  val outTag = !in.bits.singleOut // TODO typeTag
  val mux = Wire(init = fsgnjMux)
  /*
  TODO: de studiat ce se intampla pe aici
  for (t <- floatTypes.init) {
    when (outTag === typeTag(t)) {
      mux.data := Cat(fsgnjMux.data >> t.recodedWidth, maxType.unsafeConvert(fsgnjMux.data, t))
    }
  }

  when (in.bits.wflags && !in.bits.ren2) { // fcvt
    if (floatTypes.size > 1) {
      // widening conversions simply canonicalize NaN operands
      val widened = Mux(maxType.isNaN(in.bits.in1), maxType.qNaN, in.bits.in1)
      fsgnjMux.data := widened
      fsgnjMux.exc := maxType.isSNaN(in.bits.in1) << 4

      // narrowing conversions require rounding (for RVQ, this could be
      // optimized to use a single variable-position rounding unit, rather
      // than two fixed-position ones)
      for (outType <- floatTypes.init) when (outTag === typeTag(outType) && (typeTag(outType) == 0 || outTag < inTag)) {
        val narrower = Module(new hardfloat.RecFNToRecFN(maxType.exp, maxType.sig, outType.exp, outType.sig))
        narrower.io.in := in.bits.in1
        narrower.io.roundingMode := in.bits.rm
        narrower.io.detectTininess := hardfloat.consts.tininess_afterRounding
        val narrowed = sanitizeNaN(narrower.io.out, outType)
        mux.data := Cat(fsgnjMux.data >> narrowed.getWidth, narrowed)
        mux.exc := narrower.io.exceptionFlags
      }
    }
  }
  */

  io.out <> Pipe(in.valid, mux, latency-1)
}

class MulAddRecFNPipe_posit(val latency: Int, val size: Int, val exponent_max_size: Int) extends Module
{
    require(latency<=2) 

    val io = new Bundle {
        val validin = Bool(INPUT)
        val op = Bits(INPUT, 2)
        val a = Bits(INPUT, size + 1)
        val b = Bits(INPUT, size + 1)
        val c = Bits(INPUT, size + 1)
        val out = Bits(OUTPUT, size + 1)
        val exceptionFlags = Bits(OUTPUT, 5)
        val validout = Bool(OUTPUT)
    }

    val posit_add = Module(new posit.PositAdd(exponent_max_size, size))
    val posit_sub = Module(new posit.PositSub(exponent_max_size, size))
    val posit_mul = Module(new posit.PositMul(exponent_max_size, size))
    val b_neg = Wire(UInt(size.W))
    val c_neg = Wire(UInt(size.W))
    val posit_2comp_b = Module(new posit.PositTwoComp(exponent_max_size, size))
    val posit_2comp_c = Module(new posit.PositTwoComp(exponent_max_size, size))
    posit_2comp_b.io.i_bits := io.b(size-1,0)
    posit_2comp_c.io.i_bits := io.c(size-1,0)
    b_neg := posit_2comp_b.io.o_bits 
    c_neg := posit_2comp_c.io.o_bits
    val mul_result = Wire(UInt(size.W))
    val final_result_sub = Wire(UInt(size.W))
    val final_result_add = Wire(UInt(size.W))
    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    posit_mul.io.i_bits_1 := io.a(size-1,0)
    posit_mul.io.i_bits_2 := io.b(size-1,0)
    when(io.op(1) === 1.U) {
      posit_mul.io.i_bits_2 := b_neg
    }
    mul_result := posit_mul.io.o_bits
    val mul_sign = Wire(UInt(1.W))
    val c_sign = Wire(UInt(1.W))
    val different_sign = Wire(UInt(1.W))
    mul_sign := mul_result(size-1)
    c_sign := io.op(0) ^ io.c(size-1)
    different_sign := mul_sign ^ c_sign
    //posit_add.io.i_bits_1 := mul_result
    //posit_sub.io.i_bits_1 := mul_result
    posit_add.io.i_bits_2 := 0.U
    posit_sub.io.i_bits_2 := 0.U
    when( (different_sign ^ io.op(0)) === 1.U) {
      posit_add.io.i_bits_2 := c_neg
      posit_sub.io.i_bits_2 := c_neg
    } .otherwise {
      posit_add.io.i_bits_2 := io.c(size-1,0)
      posit_sub.io.i_bits_2 := io.c(size-1,0)
    }



    val valid_stage0 = Wire(Bool())
  
    val postmul_regs = if(latency>0) 1 else 0
    posit_add.io.i_bits_1 := Pipe(io.validin, mul_result, postmul_regs).bits
    posit_sub.io.i_bits_1 := Pipe(io.validin, mul_result, postmul_regs).bits
    //mulAddRecFNToRaw_postMul.io.roundingMode := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    valid_stage0                             := Pipe(io.validin, false.B, postmul_regs).valid
    
    //------------------------------------------------------------------------
    //------------------------------------------------------------------------

    val round_regs = if(latency==2) 1 else 0
    //roundRawFNToRecFN.io.invalidExc         := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.invalidExc, round_regs).bits
    final_result_sub                        := Pipe(valid_stage0, posit_sub.io.o_bits, round_regs).bits
    final_result_add                        := Pipe(valid_stage0, posit_add.io.o_bits, round_regs).bits
    io.validout                             := Pipe(valid_stage0, false.B, round_regs).valid

    
    io.out := 0.U
    when (different_sign === 1.U) {
      io.out := Cat(0.U(1.W), final_result_sub)
    } .otherwise {
      io.out := Cat(0.U(1.W), final_result_add)
    }
    io.exceptionFlags := 0.U
}


class FPUFMAPipe_posit(val latency: Int, val size: Int, val exponent_max_size: Int)
                (implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  require(latency>0)

  val io = new Bundle {
    val in = Valid(new FPInput).flip
    val out = Valid(new FPResult)
  }

  val valid = Reg(next=io.in.valid)
  val in = Reg(new FPInput)


  when (io.in.valid) {
    val one = UInt(1) << (size - 2)
    val zero = Fill(size, 0.U)
    val cmd_fma = io.in.bits.ren3
    val cmd_addsub = io.in.bits.swap23
    in := io.in.bits
    when (cmd_addsub) { in.in2 := one }
    when (!(cmd_fma || cmd_addsub)) { in.in3 := zero }
  }

  val fma = Module(new MulAddRecFNPipe_posit((latency-1) min 2, size, exponent_max_size))
  fma.io.validin := valid
  fma.io.op := in.fmaCmd
  fma.io.a := in.in1
  fma.io.b := in.in2
  fma.io.c := in.in3

  val res = Wire(new FPResult)
  //res.data := fma.io.out
  if((32-size) > 0) {
    res.data := Cat(Fill(32-size,0.U), fma.io.out)
  } else {
    res.data := fma.io.out
  }

  res.exc := fma.io.exceptionFlags

  io.out := Pipe(fma.io.validout, res, (latency-3) max 0)
}

class DivSqrt_posit(val latency: Int, val size: Int, val exponent_max_size: Int) extends Module
{
    //require(latency<=2) 

    val io = new Bundle {
        val inReady        = Bool(OUTPUT)
        val inValid        = Bool(INPUT)
        val sqrtOp         = Bool(INPUT)
        val a              = Bits(INPUT, size + 1)
        val b              = Bits(INPUT, size + 1)
        /*--------------------------------------------------------------------
        *--------------------------------------------------------------------*/
        val outValid_div   = Bool(OUTPUT)
        val outValid_sqrt  = Bool(OUTPUT)
        val out            = Bits(OUTPUT, size + 1)
        val exceptionFlags = Bits(OUTPUT, 5)
    }

    val posit_div = Module(new posit.PositDiv(exponent_max_size, size))
    val posit_sqrt = Module(new posit.PositSqrt(exponent_max_size, size))
    val cycle_number = RegInit(0.U(size.W))
    posit_div.io.i_bits_1 := io.a(size-1,0)
    posit_div.io.i_bits_2 := io.b(size-1,0)
    posit_sqrt.io.i_bits := io.a(size-1,0)
    //io.outValid_sqrt := 0.U
    //io.outValid_div := 0.U
    io.exceptionFlags := 0.U

    val final_result = Wire(UInt(size.W))
    final_result := 0.U
    when(io.sqrtOp) {
      final_result := posit_sqrt.io.o_bits
    } .otherwise {
      final_result := posit_div.io.o_bits
    }
    
    when(io.inValid) {
      cycle_number := 0
    } .otherwise {
      cycle_number := cycle_number + 1
    }
    val idle = (cycle_number === 0.U)
    val inReady = (cycle_number <= 1.U)
    val validout_sqrt = (cycle_number > 1.U)
    posit_sqrt.io.i_ready := inReady
    
    val valid_stage0 = Wire(Bool())
    io.inReady := ~valid_stage0 | inReady
  
    val postmul_regs = if(latency>0) 1 else 0
    //posit_add.io.i_bits_1 := Pipe(io.validin, mul_result, postmul_regs).bits
    //posit_sub.io.i_bits_1 := Pipe(io.validin, mul_result, postmul_regs).bits
    //mulAddRecFNToRaw_postMul.io.roundingMode := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    valid_stage0                             := Pipe(io.inValid, false.B, postmul_regs).valid
    
    //------------------------------------------------------------------------
    //------------------------------------------------------------------------

    val validout = Wire(Bool())
    val round_regs = if(latency>(log2Ceil(size)+2)) 1 else 0
    //roundRawFNToRecFN.io.invalidExc         := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.invalidExc, round_regs).bits
    //final_result_sub                        := Pipe(valid_stage0, posit_sub.io.o_bits, round_regs).bits
    //final_result_add                        := Pipe(valid_stage0, posit_add.io.o_bits, round_regs).bits
    validout                                  := Pipe(valid_stage0, false.B, round_regs).valid

    io.outValid_div  := validout && !io.sqrtOp
    io.outValid_sqrt := (validout || validout_sqrt) && io.sqrtOp && posit_sqrt.io.o_ready
    
    io.out := Cat(0.U(1.W), final_result)
    io.exceptionFlags := 0.U
}




@chiselName
class FPU(cfg: FPUParams)(implicit p: Parameters) extends FPUModule()(p) {
  val io = new FPUIO

  val useClockGating = coreParams match {
    case r: RocketCoreParams => r.clockGate
    case _ => false
  }
  val clock_en_reg = Reg(Bool())
  val clock_en = clock_en_reg || io.cp_req.valid
  val gated_clock =
    if (!useClockGating) clock
    else ClockGate(clock, clock_en, "fpu_clock_gate")

  val fp_decoder = Module(new FPUDecoder)
  fp_decoder.io.inst := io.inst
  val id_ctrl = fp_decoder.io.sigs

  val ex_reg_valid = Reg(next=io.valid, init=Bool(false))
  val ex_reg_inst = RegEnable(io.inst, io.valid)
  val ex_reg_ctrl = RegEnable(id_ctrl, io.valid)
  val ex_ra = List.fill(3)(Reg(UInt()))

  // load response
  val load_wb = Reg(next=io.dmem_resp_val)
  val load_wb_double = RegEnable(io.dmem_resp_type(0), io.dmem_resp_val)
  val load_wb_data = RegEnable(io.dmem_resp_data, io.dmem_resp_val)
  val load_wb_tag = RegEnable(io.dmem_resp_tag, io.dmem_resp_val)

  @chiselName class FPUImpl { // entering gated-clock domain

  val req_valid = ex_reg_valid || io.cp_req.valid
  val ex_cp_valid = io.cp_req.fire()
  val mem_cp_valid = Reg(next=ex_cp_valid, init=Bool(false))
  val wb_cp_valid = Reg(next=mem_cp_valid, init=Bool(false))
  val mem_reg_valid = RegInit(false.B)
  val killm = (io.killm || io.nack_mem) && !mem_cp_valid
  // Kill X-stage instruction if M-stage is killed.  This prevents it from
  // speculatively being sent to the div-sqrt unit, which can cause priority
  // inversion for two back-to-back divides, the first of which is killed.
  val killx = io.killx || mem_reg_valid && killm
  mem_reg_valid := ex_reg_valid && !killx || ex_cp_valid
  val mem_reg_inst = RegEnable(ex_reg_inst, ex_reg_valid)
  val wb_reg_valid = Reg(next=mem_reg_valid && (!killm || mem_cp_valid), init=Bool(false))

  val cp_ctrl = Wire(new FPUCtrlSigs)
  cp_ctrl <> io.cp_req.bits
  io.cp_resp.valid := Bool(false)
  io.cp_resp.bits.data := UInt(0)

  val ex_ctrl = Mux(ex_cp_valid, cp_ctrl, ex_reg_ctrl)
  val mem_ctrl = RegEnable(ex_ctrl, req_valid)
  val wb_ctrl = RegEnable(mem_ctrl, mem_reg_valid)

  // regfile
  val regfile = Mem(32, Bits(width = fLen+1))
  when (load_wb) {
    val wdata = Cat(0.U(1.W), load_wb_data)
    regfile(load_wb_tag) := wdata
    assert(consistent(wdata))
    if (enableCommitLog)
      printf("f%d p%d 0x%x\n", load_wb_tag, load_wb_tag + 32, load_wb_data)
  }

  val ex_rs = ex_ra.map(a => regfile(a))
  when (io.valid) {
    when (id_ctrl.ren1) {
      when (!id_ctrl.swap12) { ex_ra(0) := io.inst(19,15) }
      when (id_ctrl.swap12) { ex_ra(1) := io.inst(19,15) }
    }
    when (id_ctrl.ren2) {
      when (id_ctrl.swap12) { ex_ra(0) := io.inst(24,20) }
      when (id_ctrl.swap23) { ex_ra(2) := io.inst(24,20) }
      when (!id_ctrl.swap12 && !id_ctrl.swap23) { ex_ra(1) := io.inst(24,20) }
    }
    when (id_ctrl.ren3) { ex_ra(2) := io.inst(31,27) }
  }
  val ex_rm = Mux(ex_reg_inst(14,12) === Bits(7), io.fcsr_rm, ex_reg_inst(14,12))

  def fuInput(minT: Option[FType]): FPInput = {
    val req = Wire(new FPInput)
    val tag = !ex_ctrl.singleIn // TODO typeTag
    req := ex_ctrl
    req.rm := ex_rm
    req.in1 := ex_rs(0)
    req.in2 := ex_rs(1)
    req.in3 := ex_rs(2)
    req.typ := ex_reg_inst(21,20)
    req.fmaCmd := ex_reg_inst(3,2) | (!ex_ctrl.ren3 && ex_reg_inst(27))
    when (ex_cp_valid) {
      req := io.cp_req.bits
      when (io.cp_req.bits.swap23) {
        req.in2 := io.cp_req.bits.in3
        req.in3 := io.cp_req.bits.in2
      }
    }
    req
  }

  val sfma = Module(new FPUFMAPipe_posit(cfg.sfmaLatency, size = POSITConstants.posit_size, exponent_max_size = POSITConstants.posit_exponent_size))
  sfma.io.in.valid := req_valid && ex_ctrl.fma && ex_ctrl.singleOut
  sfma.io.in.bits := fuInput(None)

  val fpiu = Module(new FPToInt_posit(size = POSITConstants.posit_size, exponent_max_size = POSITConstants.posit_exponent_size))
  fpiu.io.in.valid := req_valid && (ex_ctrl.toint || ex_ctrl.div || ex_ctrl.sqrt || (ex_ctrl.fastpipe && ex_ctrl.wflags))
  fpiu.io.in.bits := fuInput(None)
  io.store_data := fpiu.io.out.bits.store
  io.toint_data := fpiu.io.out.bits.toint
  when(fpiu.io.out.valid && mem_cp_valid && mem_ctrl.toint){
    io.cp_resp.bits.data := fpiu.io.out.bits.toint
    io.cp_resp.valid := Bool(true)
  }

  val ifpu = Module(new IntToFP_posit(latency = POSITConstants.latency, size = POSITConstants.posit_size, exponent_max_size = POSITConstants.posit_exponent_size))
  ifpu.io.in.valid := req_valid && ex_ctrl.fromint
  ifpu.io.in.bits := fpiu.io.in.bits
  ifpu.io.in.bits.in1 := Mux(ex_cp_valid, io.cp_req.bits.in1, io.fromint_data)

  val fpmu = Module(new FPToFP_posit(latency = POSITConstants.latency, size = POSITConstants.posit_size, exponent_max_size = POSITConstants.posit_exponent_size))
  fpmu.io.in.valid := req_valid && ex_ctrl.fastpipe
  fpmu.io.in.bits := fpiu.io.in.bits
  fpmu.io.lt := fpiu.io.out.bits.lt

  val divSqrt_wen = Wire(init = false.B)
  val divSqrt_inFlight = Wire(init = false.B)
  val divSqrt_waddr = Reg(UInt(width = 5))
  val divSqrt_typeTag = Wire(UInt(width = log2Up(floatTypes.size)))
  val divSqrt_wdata = Wire(UInt(width = fLen+1))
  val divSqrt_flags = Wire(UInt(width = FPConstants.FLAGS_SZ))

  // writeback arbitration
  case class Pipe(p: Module, lat: Int, cond: (FPUCtrlSigs) => Bool, res: FPResult)
  val pipes = List(
    Pipe(fpmu, fpmu.latency, (c: FPUCtrlSigs) => c.fastpipe, fpmu.io.out.bits),
    Pipe(ifpu, ifpu.latency, (c: FPUCtrlSigs) => c.fromint, ifpu.io.out.bits),
    Pipe(sfma, sfma.latency, (c: FPUCtrlSigs) => c.fma && c.singleOut, sfma.io.out.bits)) ++
    (fLen > 32).option({
          val dfma = Module(new FPUFMAPipe_posit(cfg.dfmaLatency, size = POSITConstants.posit_size, exponent_max_size = POSITConstants.posit_exponent_size))
          dfma.io.in.valid := req_valid && ex_ctrl.fma && !ex_ctrl.singleOut
          dfma.io.in.bits := fuInput(None)
          Pipe(dfma, dfma.latency, (c: FPUCtrlSigs) => c.fma && !c.singleOut, dfma.io.out.bits)
        })
  def latencyMask(c: FPUCtrlSigs, offset: Int) = {
    require(pipes.forall(_.lat >= offset))
    pipes.map(p => Mux(p.cond(c), UInt(1 << p.lat-offset), UInt(0))).reduce(_|_)
  }
  def pipeid(c: FPUCtrlSigs) = pipes.zipWithIndex.map(p => Mux(p._1.cond(c), UInt(p._2), UInt(0))).reduce(_|_)
  val maxLatency = pipes.map(_.lat).max
  val memLatencyMask = latencyMask(mem_ctrl, 2)

  class WBInfo extends Bundle {
    val rd = UInt(width = 5)
    val single = Bool()
    val cp = Bool()
    val pipeid = UInt(width = log2Ceil(pipes.size))
    override def cloneType: this.type = new WBInfo().asInstanceOf[this.type]
  }

  val wen = Reg(init=Bits(0, maxLatency-1))
  val wbInfo = Reg(Vec(maxLatency-1, new WBInfo))
  val mem_wen = mem_reg_valid && (mem_ctrl.fma || mem_ctrl.fastpipe || mem_ctrl.fromint)
  val write_port_busy = RegEnable(mem_wen && (memLatencyMask & latencyMask(ex_ctrl, 1)).orR || (wen & latencyMask(ex_ctrl, 0)).orR, req_valid)
  ccover(mem_reg_valid && write_port_busy, "WB_STRUCTURAL", "structural hazard on writeback")

  for (i <- 0 until maxLatency-2) {
    when (wen(i+1)) { wbInfo(i) := wbInfo(i+1) }
  }
  wen := wen >> 1
  when (mem_wen) {
    when (!killm) {
      wen := wen >> 1 | memLatencyMask
    }
    for (i <- 0 until maxLatency-1) {
      when (!write_port_busy && memLatencyMask(i)) {
        wbInfo(i).cp := mem_cp_valid
        wbInfo(i).single := mem_ctrl.singleOut
        wbInfo(i).pipeid := pipeid(mem_ctrl)
        wbInfo(i).rd := mem_reg_inst(11,7)
      }
    }
  }

  val waddr = Mux(divSqrt_wen, divSqrt_waddr, wbInfo(0).rd)
  val wdouble = Mux(divSqrt_wen, divSqrt_typeTag, !wbInfo(0).single)
  val wdata = Mux(divSqrt_wen, divSqrt_wdata, (pipes.map(_.res.data): Seq[UInt])(wbInfo(0).pipeid))
  val wexc = (pipes.map(_.res.exc): Seq[UInt])(wbInfo(0).pipeid)
  when ((!wbInfo(0).cp && wen(0)) || divSqrt_wen) {
    assert(consistent(wdata))
    regfile(waddr) := wdata
    if (enableCommitLog) {
      printf("f%d p%d 0x%x\n", waddr, waddr + 32, ieee(wdata))
    }
  }
  when (wbInfo(0).cp && wen(0)) {
    io.cp_resp.bits.data := wdata
    io.cp_resp.valid := Bool(true)
  }
  io.cp_req.ready := !ex_reg_valid

  val wb_toint_valid = wb_reg_valid && wb_ctrl.toint
  val wb_toint_exc = RegEnable(fpiu.io.out.bits.exc, mem_ctrl.toint)
  io.fcsr_flags.valid := wb_toint_valid || divSqrt_wen || wen(0)
  io.fcsr_flags.bits :=
    Mux(wb_toint_valid, wb_toint_exc, UInt(0)) |
    Mux(divSqrt_wen, divSqrt_flags, UInt(0)) |
    Mux(wen(0), wexc, UInt(0))

  val divSqrt_write_port_busy = (mem_ctrl.div || mem_ctrl.sqrt) && wen.orR
  io.fcsr_rdy := !(ex_reg_valid && ex_ctrl.wflags || mem_reg_valid && mem_ctrl.wflags || wb_reg_valid && wb_ctrl.toint || wen.orR || divSqrt_inFlight)
  io.nack_mem := write_port_busy || divSqrt_write_port_busy || divSqrt_inFlight
  io.dec <> fp_decoder.io.sigs
  def useScoreboard(f: ((Pipe, Int)) => Bool) = pipes.zipWithIndex.filter(_._1.lat > 3).map(x => f(x)).fold(Bool(false))(_||_)
  io.sboard_set := wb_reg_valid && !wb_cp_valid && Reg(next=useScoreboard(_._1.cond(mem_ctrl)) || mem_ctrl.div || mem_ctrl.sqrt)
  io.sboard_clr := !wb_cp_valid && (divSqrt_wen || (wen(0) && useScoreboard(x => wbInfo(0).pipeid === UInt(x._2))))
  io.sboard_clra := waddr
  ccover(io.sboard_clr && load_wb, "DUAL_WRITEBACK", "load and FMA writeback on same cycle")
  // we don't currently support round-max-magnitude (rm=4)
  io.illegal_rm := io.inst(14,12).isOneOf(5, 6) || io.inst(14,12) === 7 && io.fcsr_rm >= 5

  if (cfg.divSqrt) {
    val divSqrt_killed = Reg(Bool())
    ccover(divSqrt_inFlight && divSqrt_killed, "DIV_KILLED", "divide killed after issued to divider")
    ccover(divSqrt_inFlight && mem_reg_valid && (mem_ctrl.div || mem_ctrl.sqrt), "DIV_BUSY", "divider structural hazard")
    ccover(mem_reg_valid && divSqrt_write_port_busy, "DIV_WB_STRUCTURAL", "structural hazard on division writeback")

    for (t <- floatTypes) {
      val tag = !mem_ctrl.singleOut // TODO typeTag
      val divSqrt = Module(new DivSqrt_posit(latency = POSITConstants.latency, size = POSITConstants.posit_size, exponent_max_size = POSITConstants.posit_exponent_size))
      divSqrt.io.inValid := mem_reg_valid && (mem_ctrl.div || mem_ctrl.sqrt) && !divSqrt_inFlight
      divSqrt.io.sqrtOp := mem_ctrl.sqrt
      divSqrt.io.a := fpiu.io.out.bits.in.in1
      divSqrt.io.b := fpiu.io.out.bits.in.in2

      when (!divSqrt.io.inReady) { divSqrt_inFlight := true } // only 1 in flight

      when (divSqrt.io.inValid && divSqrt.io.inReady) {
        divSqrt_killed := killm
        divSqrt_waddr := mem_reg_inst(11,7)
      }

      when (divSqrt.io.outValid_div || divSqrt.io.outValid_sqrt) {
        divSqrt_wen := !divSqrt_killed
        if ((32-POSITConstants.posit_size) > 0) {
          divSqrt_wdata := Cat(Fill(32-POSITConstants.posit_size,0.U),divSqrt.io.out)
        } else {
          divSqrt_wdata := divSqrt.io.out
        }
        divSqrt_flags := divSqrt.io.exceptionFlags
        divSqrt_typeTag := typeTag(t)
      }
    }
  } else {
    when (id_ctrl.div || id_ctrl.sqrt) { io.illegal_rm := true }
  }

  // gate the clock
  clock_en_reg := !useClockGating ||
    io.keep_clock_enabled || // chicken bit
    io.valid || // ID stage
    req_valid || // EX stage
    mem_reg_valid || mem_cp_valid || // MEM stage
    wb_reg_valid || wb_cp_valid || // WB stage
    wen.orR || divSqrt_inFlight || // post-WB stage
    io.dmem_resp_val // load writeback

  } // leaving gated-clock domain
  val fpuImpl = withClock (gated_clock) { new FPUImpl }

  def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    cover(cond, s"FPU_$label", "Core;;" + desc)
}
