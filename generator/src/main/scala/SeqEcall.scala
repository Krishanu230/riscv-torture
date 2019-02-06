package torture
import Rand._

class SeqEcall(xregs: HWRegPool) extends InstSeq
{
  // store the reg dum addr in ra and the store 1 at the start to distinguish bw ecalls
  override val seqname = "ecall"
  val tmp1 = reg_write_visible(xregs)
  insts += ADDI(tmp1, reg_read_zero(xregs), Imm(1))
  val ra = reg_write_ra(xregs)
  insts += LA(ra, BaseImm("xreg_output_data",0))
  insts += SW(tmp1, RegImm(ra,0))

  insts+= ECALL()

}
