package bavadim.compiler

import java.io.File

import org.apache.commons.io.FileUtils
import play.routes.compiler.RoutesCompiler.RoutesCompilerTask
import play.routes.compiler.{RoutesCompilationError, RoutesGenerator}

/**
 * @author vadim
 * @since 06.11.15
 */
object ExtendedRoutesCompiler {

  def compile(task: RoutesCompilerTask,
              nameExtension: String,
              parser: RouteParser,
              generator: RoutesGenerator,
              generatedDir: File): Either[Seq[RoutesCompilationError], Seq[File]] = {

    val namespace = Option(task.file.getName).filter(_.endsWith("." + nameExtension))
      .map(_.dropRight(("." + nameExtension).length)).orElse(Some("router"))

    val routeFile = task.file.getAbsoluteFile

    println(routeFile.getName)

    parser.parse(routeFile).right.map { rules =>
      val generated = generator.generate(task, namespace, rules)
      generated.map {
        case (filename, content) =>
          val file = new File(generatedDir, filename)
          FileUtils.writeStringToFile(file, content)
          file
      }
    }
  }
}
