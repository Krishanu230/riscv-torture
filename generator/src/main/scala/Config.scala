package torture
package generator

import scopt.OptionParser
import java.io.FileWriter
import java.io.FileInputStream
import java.util.Properties
import scala.collection.JavaConversions._

class Config_obj
 {
  val privconfFile = "config/privconf.config"
  println("++++++***********************In config 2")



  val in2 = new FileInputStream(privconfFile)
  val config2 = new Properties()
  config2.load(in2)
  in2.close()
  println("opening propS from config2\n")
   val nseqs   = config2.getProperty("torture.generator.nseqs", "1000").toInt
   val memsize = config2.getProperty("torture.generator.memsize", "1024").toInt
   val fprnd   = config2.getProperty("torture.generator.fprnd", "0").toInt
   val use_amo = (config2.getProperty("torture.generator.amo", "true").toLowerCase == "true")
   val use_mul = (config2.getProperty("torture.generator.mul", "true").toLowerCase == "true")
   val use_div = (config2.getProperty("torture.generator.divider", "true").toLowerCase == "true")
   val mix     = config2.filterKeys(_ contains "torture.generator.mix").map { case (k,v) => (k.split('.')(3), v.toInt) }.asInstanceOf[Map[String,Int]]
   val vec     = config2.filterKeys(_ contains "torture.generator.vec").map { case (k,v) => (k.split('.').drop(3).reduce(_+"."+_), v) }.asInstanceOf[Map[String,String]]
   val segment = (config2.getProperty("torture.generator.segment", "true").toLowerCase == "true")
   val loop    = (config2.getProperty("torture.generator.loop", "true").toLowerCase == "true")
   val loop_size = config2.getProperty("torture.generator.loop_size", "256").toInt

  def printprops(): Unit = {
  println("nseqs = " + (nseqs) )
   println("memsize = " + (memsize) )
   println("fprnd = " + (fprnd) )
   println("use_amo  = " + (use_amo) )
   println("use mul = " + (use_mul) )
   println("use_div = " + (use_div) )
   println("--mix = "+ (mix))
   println("vec  : not printing")
   println("segment= "+ (segment))
   println("loop = " + (loop))
   println("loopsize = "+ (loop_size))
 }

 def apply(): Unit = {
       println("  in apply");
  }

}
