package bavadim.compiler

import java.io.File

import org.apache.commons.io.FileUtils
import play.routes.compiler.{Rule, RoutesCompilationError}

/**
 * @author vadim
 * @since 06.11.15
 */
trait RouteParser {
  final def parse(routesFile: File): Either[Seq[RoutesCompilationError], List[Rule]] = {
    val routesContent = FileUtils.readFileToString(routesFile)
    parseContent(routesContent, routesFile)
  }

  protected def parseContent(routesContent: String, file: File): Either[Seq[RoutesCompilationError], List[Rule]]
}
