package torture
import Rand._

class SeqEcall(xregs: HWRegPool) extends InstSeq
{

  override val seqname = "ecall"
  val do_not_use = reg_write_s0(xregs)
  //val tmp1 = reg_write_visible(xregs)
  //insts += ADDI(tmp1, reg_read_zero(xregs), Imm(1))
  //val tmp2 = reg_write_visible(xregs)
  //insts += LA(tmp2, BaseImm("xreg_output_data",0))
  //insts += SW(tmp1, RegImm(tmp2,0))//store imm 1
  val reg3 = reg_write_x3(xregs)
  insts += ADDI(reg3, reg_read_zero(xregs), Imm(1))
  insts+= ECALL()

}
