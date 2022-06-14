package spinalvs

import spinal.core.sim._
import spinal.core.{Component => Module, _}  // avoid namespace conflicts
import spinal.lib._

import java.io.File
import scala.collection.mutable
import scala.io.Source
import scala.swing._
import scala.swing.event._

object SpinalVS {
  val clock_period_ns = 10  // the period of synchronous clock in nano seconds
  lazy val simulationConfig: SpinalSimConfig =
    SimConfig.withWave.withConfig(SpinalConfig(
      defaultClockDomainFrequency = FixedFrequency(50 MHz)
    ))  // customize

  // remove clock from set
  def removeClock(s: mutable.Set[BaseType]): mutable.Set[BaseType] =
    s.filter(io => io.getDisplayName() != io.clockDomain.clock.getDisplayName())

  // type name in SpinalHDL
  def getClassIdentifier(sig: BaseType): String =  
    sig.getClass.getName.split('.').last.replace("$","")

  // connect signal name and type name
  def getNameClassStr(sig: BaseType): String =     
    s"${sig.getDisplayName}:${getClassIdentifier(sig)}"

  // generate array of signal names from a set
  def setIoName(names: Array[String], signals: mutable.Set[BaseType]): Unit =
    for (i <- names.indices)
      names(i) = signals.toSeq(i) match {  // get the column name of waveform table
        case v: BitVector => getNameClassStr(v) + s"[${v.getWidthStringNoInferation}]"
        case b: Bool => getNameClassStr(b)
        case _ => "<unsupported>"
      }

  // Read a "value change dump" file and parse waveform data from it.
  def parseVCD(vcdPath: String) = {
    val vcdFile = Source.fromFile(vcdPath)
    val vcdLines = vcdFile.mkString.split('\n')
    vcdFile.close()

    // a dynamic array of the map from id to value
    val sampleBuffer = mutable.ArrayBuffer(mutable.Map(""->BigInt(0)))
    val signalMap = mutable.Map("" -> "")  // name -> id

    // regular expressions to match a line of vcd file
    val signalPattern = """\s*\$var\s*wire\s*(\d+)\s+(\S+)\s+(\S+).+\$end\s*""".r
    val samplePattern = """b?([01]+)\s*(.+)\s*""".r
    val timestampPattern = """#(\d+)\s*""".r
    val timescalePattern = """\$timescale\s+(\d+) ?(.)s\s*.*""".r
    val resetPattern = """#170.*""".r
    //  .findFirstMatchIn(str).iterator.next().group(1).toInt

    val resetLine = vcdLines.indexWhere{
      case resetPattern(_*) => true
      case _ => false
    }  // simulation starts from the 170th timescale
    // divide the file into two parts according to the reset line number
    val sampleLines = vcdLines.drop(resetLine)
    val infoLines = vcdLines.dropRight(resetLine)

    // parse information of signals
    infoLines foreach {
      case signalPattern(width, id, name) =>
        sampleBuffer(0) += (id -> 0)
        signalMap += (name -> id)
      case timescalePattern(num, unit) => println("scale=" + num + unit + 's')
      case _ =>
    }
    // parse sample values of signals
    sampleLines foreach {
      case samplePattern(b, id) => sampleBuffer.last(id) = b.asBin
      case timestampPattern(num) =>
        if (num.toInt % clock_period_ns == 0)  // always at pos edge of clk
          sampleBuffer += sampleBuffer.last.clone()
      case _ =>
    }
    (signalMap, sampleBuffer)
  }

  // SpinalVS(new MyTopLevel)
  def apply[T <: Module](rtl: => T): Unit = {
    val compiledDut: SimCompiled[Module] = simulationConfig.compile(rtl)
    val vcdPath: String = simulationConfig._workspacePath +
      File.separator + compiledDut.report.toplevelName +
      File.separator + "test.vcd"  // default relative path
    val allIO: mutable.Set[BaseType] = compiledDut.dut.getAllIo  // set of I/O ports

    val outputSet: mutable.Set[BaseType] = allIO.filter(_.isOutput)  // set of output signals
    val inputSet: mutable.Set[BaseType] = removeClock(allIO).filter(_.isInput)  // set of input signals
    val inputNames = new Array[String](inputSet.size)
    val outputNames = new Array[String](outputSet.size)

    setIoName(inputNames, inputSet)
    setIoName(outputNames, outputSet)

    val inWaves: Array[Array[Any]] = Array.ofDim[Any](60, inputSet.size)
    val outWaves: Array[Array[Any]] = Array.ofDim[Any](60, outputSet.size)
    val inTab = new Table(inWaves, inputNames)  // display the wave data
    val outTab = new Table(outWaves, outputNames)

    // start spinal simulation
    def genWaves(): Unit = {
      compiledDut.doSim { dut =>
        val inputSeq = removeClock(dut.getAllIo).filter(_.isInput).toSeq
        dut.clockDomain.forkStimulus(clock_period_ns)
        inWaves.foreach { i =>  // for each clock cycle
          dut.clockDomain.waitSampling()  // clock++
          for (j <- i.indices)  // for each input signal
            if (i(j) != null)  // table cell is not empty
              inputSeq(j) match {  // match the signal type
                case v: BitVector => v #= i(j).toString.toLong
                case b: Bool      => b #= i(j).toString.toLong == 1
                case _            =>
              }
        }
        dut.clockDomain.waitSampling(2)  // flush after table
      }
      val (signalMap, sampleBuffer) = parseVCD(vcdPath)
      for (i <- outWaves.indices) {
        for (j <- outWaves(i).indices) {
          val name = outputNames(j).split(':').head
          outTab.update(i, j, sampleBuffer(i)(signalMap(name)))
        }
      }
    }

    // open the main frame of visual simulation
    new MainFrame {
      visible = true
      title = "Visual Simulation"

      contents = new BoxPanel(Orientation.Vertical) {
        contents += new Button("doSim") {
          reactions += {
            case ButtonClicked(_) => genWaves()
          }
        }
        contents += Swing.Glue
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new BoxPanel(Orientation.Vertical){
            contents += new Label("Input Signal Assignments")
            contents += new ScrollPane(inTab)
          }
          contents += new BoxPanel(Orientation.Vertical){
            contents += new Label("Output Signal Waveform")
            contents += new ScrollPane(outTab)
          }
        }
        border = Swing.EmptyBorder(8)
      }
    }
  }
}
