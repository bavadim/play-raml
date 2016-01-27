package bavadim.sbt

import java.io.File

import bavadim.RAMLRouteParser
import bavadim.compiler.RouteParser
import org.apache.commons.io.FileUtils
import play.api.PlayException
import play.routes.compiler.RoutesCompiler.RoutesCompilerTask
import play.routes.compiler.{RoutesCompilationError, RoutesGenerator}
import play.sbt.Play
import play.sbt.routes.{RoutesCompiler, _}
import sbt.Keys._
import sbt._

import scala.io.Codec
import scala.language.postfixOps

object PlayRamlPlugin extends AutoPlugin {
  import RoutesKeys._

  override def requires = Play

  override def trigger = allRequirements

  override lazy val projectSettings = RoutesCompiler.defaultSettings ++
    inConfig(Compile)(routesSettings) ++
    inConfig(Test)(routesSettings)

  def routesSettings = Seq(
    //redirect play generators
    routesCompilerTasks <<= Def.taskDyn {
      // Aggregate all the routes file tasks that we want to compile the reverse routers for.
      RoutesKeys.aggregateReverseRoutes.value.map {
        agg => RoutesKeys.routesCompilerTasks in(agg.project, configuration.value)
      }.join.map { aggTasks: Seq[Seq[RoutesCompilerTask]] =>

        // Aggregated tasks need to have forwards router compilation disabled and reverse router compilation enabled.
        val reverseRouterTasks = aggTasks.flatten.map { task =>
          task.copy(forwardsRouter = false, reverseRouter = true)
        }

        val routesSources = ((unmanagedResourceDirectories in Compile).value * "api.raml").get

        val thisProjectTasks = routesSources.map { file =>
          RoutesCompilerTask(file, routesImport.value, forwardsRouter = true,
            reverseRouter = generateReverseRouter.value, namespaceReverseRouter = namespaceReverseRouter.value)
        }

        thisProjectTasks ++ reverseRouterTasks
      }
    },
    sourceGenerators := Seq(Def.task[Seq[File]] {
        Compiler.compileRoutes(routesCompilerTasks.value, RoutesKeys.routesGenerator.value, (target in routes).value,
          streams.value.cacheDirectory, state.value.log)
      }.taskValue)
  )

  object Compiler {
    def compileRoutes(tasks: Seq[RoutesCompilerTask], generator: RoutesGenerator, generatedDir: File,
                      cacheDirectory: File, log: Logger): Seq[File] = {

      val (products, errors) = {
        val results = tasks.map { task =>
          compile(task, generator, generatedDir, new RAMLRouteParser(), log)
        }
        val errors = results.collect { case Left(e) => e }.flatten
        val files = results.collect { case Right(r) => r }.flatten
        (files, errors)
      }

      if (errors.nonEmpty) {
        val exceptions = errors.map {
          case RoutesCompilationError(source, message, line, column) =>
            reportCompilationError(log, RoutesCompilationException(source, message, line, column.map(_ - 1)))
        }
        throw exceptions.head
      }

      products
    }

    def compile(task: RoutesCompilerTask, generator: RoutesGenerator, generatedDir: File,
                parser: RouteParser, log: Logger): Either[Seq[RoutesCompilationError], Seq[File]] = {

      val namespace = Some("router")

      val routeFile = task.file.getAbsoluteFile
      val parsed = parser.parse(routeFile)
      parsed.right.map { rules =>
        val generated = generator.generate(task, namespace, rules)
        generated.map {
          case (filename, content) =>
            val file = new File(generatedDir, filename)
            FileUtils.writeStringToFile(file, content, implicitly[Codec].name)
            log.info(s"Writing file: $filename")
            file
        }
      }
    }

    private def reportCompilationError(log: Logger, error: PlayException.ExceptionSource) = {
      // log the source file and line number with the error message
      log.error(Option(error.sourceName).getOrElse("") + Option(error.line).map(":" + _).getOrElse("") + ": " + error.getMessage)
      Option(error.interestingLines(0)).map(_.focus).flatMap(_.headOption) foreach { line =>
        // log the line
        log.error(line)
        Option(error.position).foreach { pos =>
          // print a carat under the offending character
          val spaces = (line: Seq[Char]).take(pos).map {
            case '\t' => '\t'
            case x => ' '
          }
          log.error(spaces.mkString + "^")
        }
      }
      error
    }
  }
}
