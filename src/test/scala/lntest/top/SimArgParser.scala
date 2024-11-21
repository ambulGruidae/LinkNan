package lntest.top

import linknan.generator.ArgParser.configParse
import linknan.generator._
import org.chipsalliance.cde.config.Parameters
import xs.utils.perf.DebugOptionsKey
import zhujiang.ZJParametersKey

object SimArgParser {
  def apply(args: Array[String]): (Parameters, Array[String]) = {
    val (configuration, stripCfgArgs) = configParse(sim = true)(args)

    var firrtlOpts = Array[String]()

    def parse(config: Parameters, args: List[String]): Parameters = {
      args match {
        case Nil => config

        case "--dramsim3" :: tail =>
          parse(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(UseDRAMSim = true)
          }), tail)

        case "--cpu-sync" :: tail =>
          parse(config.alter((site, here, up) => {
            case ZJParametersKey => up(ZJParametersKey).copy(cpuAsync = false)
          }), tail)

        case "--fpga-platform" :: tail =>
          parse(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(FPGAPlatform = true)
          }), tail)

        case "--enable-difftest" :: tail =>
          parse(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnableDifftest = true)
          }), tail)

        case "--basic-difftest" :: tail =>
          parse(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(AlwaysBasicDiff = true)
          }), tail)

        case "--no-cores" :: tail =>
          parse(config.alter((site, here, up) => {
            case TestIoOptionsKey => up(TestIoOptionsKey).copy(removeCore = true, keepImsic = false)
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnableDebug = true) // For ZhuJiang DontTouch IO
          }), tail)

        case "--no-csu" :: tail =>
          parse(config.alter((site, here, up) => {
            case TestIoOptionsKey => up(TestIoOptionsKey).copy(removeCsu = true, keepImsic = false)
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnableDebug = true)
          }), tail)

        case "--lua-scoreboard" :: tail =>
          parse(config.alter((site, here, up) => {
            case DebugOptionsKey => up(DebugOptionsKey).copy(EnableLuaScoreBoard = true)
          }), tail)

        case "--prefix" :: confString :: tail =>
          parse(config.alter((site, here, up) => {
            case PrefixKey => confString
          }), tail)

        case option :: tail =>
          firrtlOpts :+= option
          parse(config, tail)
      }
    }

    val cfg = parse(configuration, stripCfgArgs.toList)
    (cfg, firrtlOpts)
  }
}
