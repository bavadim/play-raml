package bavadim

import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets

import bavadim.compiler.RouteParser
import org.apache.commons.lang3.SystemUtils
import org.raml.model.{ActionType, Resource}
import org.raml.parser.loader.FileResourceLoader
import org.raml.parser.visitor.RamlDocumentBuilder
import play.routes.compiler._

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

/**
 * @author vadim
 * @since 02.11.15
 */
class RAMLRouteParser extends RouteParser {

  private object Elements extends RegexParsers {
    private def httpVerbs: Parser[String] = "get" | "post" | "put" | "delete" | "head" | "patch" | "options" | "GET" | "POST" | "PUT" | "PATCH" | "HEAD" | "DELETE" | "OPTIONS"

    private def isVerb(v: String) = v.toLowerCase == "get" | v.toLowerCase == "post" | v.toLowerCase == "put" |
      v.toLowerCase == "delete" | v.toLowerCase == "head" | v.toLowerCase == "patch" | v.toLowerCase == "options"

    private def call: Parser[HandlerCall] = opt("@") ~ absoluteMethod ~ opt(parameters) <~ repsep("""[^\r\n]*""" r, newLine) ^^ {
      case instantiate ~ absMethod ~ parameters =>
        val (packageParts, classAndMethod) = absMethod.splitAt(absMethod.size - 2)
        val packageName = packageParts.mkString(".")
        val className = classAndMethod(0)
        val methodName = classAndMethod(1)
        val dynamic = instantiate.isDefined
        HandlerCall(packageName, className, dynamic, methodName, parameters)
    }

    private def expression: Parser[String] = (multiString | string | parentheses | brackets | """[^),?=\n]""".r).+ ^^ {
      case p => p.mkString
    }

    private def parameterFixedValue: Parser[String] = "=" ~ ignoreWhiteSpace ~ expression ^^ {
      case a ~ _ ~ b => a + b
    }

    private def parameterDefaultValue: Parser[String] = "?=" ~ ignoreWhiteSpace ~ expression ^^ {
      case a ~ _ ~ b => a + b
    }

    private def parameterType: Parser[String] = ":" ~> ignoreWhiteSpace ~> simpleType

    private def simpleType: Parser[String] = {
      ((stableId <~ ignoreWhiteSpace) ~ opt(typeArgs)) ^^ {
        case sid ~ ta => sid.toString + ta.getOrElse("")
      } |
        (space("(") ~ types ~ space(")")) ^^ {
          case _ ~ b ~ _ => "(" + b + ")"
        }
    }

    private def typeArgs: Parser[String] = {
      (space("[") ~ types ~ space("]") ~ opt(typeArgs)) ^^ {
        case _ ~ ts ~ _ ~ ta => "[" + ts + "]" + ta.getOrElse("")
      } |
        (space("#") ~ identifier ~ opt(typeArgs)) ^^ {
          case _ ~ id ~ ta => "#" + id + ta.getOrElse("")
        }
    }

    private def types: Parser[String] = rep1sep(simpleType, space(",")) ^^ (_ mkString ",")

    private def stableId: Parser[String] = rep1sep(identifier, space(".")) ^^ (_ mkString ".")

    private def parameter: Parser[Parameter] = ((identifier | tickedIdentifier) <~ ignoreWhiteSpace) ~ opt(parameterType) ~ (ignoreWhiteSpace ~> opt(parameterDefaultValue | parameterFixedValue)) ^^ {
      case name ~ t ~ d => Parameter(name, t.getOrElse("String"), d.filter(_.startsWith("=")).map(_.drop(1)), d.filter(_.startsWith("?")).map(_.drop(2)))
    }

    private def parameters: Parser[List[Parameter]] = "(" ~> repsep(ignoreWhiteSpace ~> positioned(parameter) <~ ignoreWhiteSpace, ",") <~ ")"

    // Absolute method consists of a series of Java identifiers representing the package name, controller and method.
    // Since the Scala parser is greedy, we can't easily extract this out, so just parse at least 3
    private def absoluteMethod: Parser[List[String]] = namedError(javaIdent ~ "." ~ javaIdent ~ "." ~ rep1sep(javaIdent, ".") ^^ {
      case first ~ _ ~ second ~ _ ~ rest => first :: second :: rest
    }, "Controller method call expected")

    private def path: Parser[PathPattern] = {
      def singleComponentPathPart: Parser[DynamicPart] = ("{" ~> identifier <~ "}") ^^ {
        case name => DynamicPart(name, """[^/]+""", encode = true)
      }

      def staticPathPart: Parser[StaticPart] = ("""[\w]""".r +) ^^ {
        case chars => StaticPart(chars.mkString)
      }

      rep("/") ~> opt(rep1sep(singleComponentPathPart | staticPathPart, "/")) ^^ {
        case Some(p) => PathPattern(p)
        case None => PathPattern(Seq(StaticPart("/")))
      }
    }

    private def several[T](p: => Parser[T]): Parser[List[T]] = Parser { in =>
      import scala.collection.mutable.ListBuffer
      val elems = new ListBuffer[T]
      def continue(in: Input): ParseResult[List[T]] = {
        val p0 = p // avoid repeatedly re-evaluating by-name parser
        @scala.annotation.tailrec
        def applyp(in0: Input): ParseResult[List[T]] = p0(in0) match {
            case Success(x, rest) =>
              elems += x; applyp(rest)
            case Failure(_, _) => Success(elems.toList, in0)
            case err: Error => err
          }
        applyp(in)
      }
      continue(in)
    }

    private def ignoreWhiteSpace: Parser[Option[String]] = opt(whiteSpace)

    // This won't be needed when we upgrade to Scala 2.11, we will then be able to use JavaTokenParser.ident:
    // https://github.com/scala/scala/pull/1466
    private def javaIdent: Parser[String] =
      """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r

    private def tickedIdent: Parser[String] = """`[^`]+`""".r

    private def identifier: Parser[String] = namedError(javaIdent, "Identifier expected")

    private def tickedIdentifier: Parser[String] = namedError(tickedIdent, "Identifier expected")

    private def newLine: Parser[String] = namedError(("\r" ?) ~> "\n", "End of line expected")

    private def blankLine: Parser[Unit] = ignoreWhiteSpace ~> newLine ^^ { case _ => () }

    private def parentheses: Parser[String] = {
      "(" ~ several(parentheses | not(")") ~> """.""".r) ~ commit(")") ^^ {
        case p1 ~ charList ~ p2 => p1 + charList.mkString + p2
      }
    }

    private def brackets: Parser[String] = {
      "[" ~ several(parentheses | not("]") ~> """.""".r) ~ commit("]") ^^ {
        case p1 ~ charList ~ p2 => p1 + charList.mkString + p2
      }
    }

    private def string: Parser[String] = {
      "\"" ~ several(parentheses | not("\"") ~> """.""".r) ~ commit("\"") ^^ {
        case p1 ~ charList ~ p2 => p1 + charList.mkString + p2
      }
    }

    private def multiString: Parser[String] = {
      "\"\"\"" ~ several(parentheses | not("\"\"\"") ~> """.""".r) ~ commit("\"\"\"") ^^ {
        case p1 ~ charList ~ p2 => p1 + charList.mkString + p2
      }
    }

    private def space(s: String): Parser[String] = ignoreWhiteSpace ~> s <~ ignoreWhiteSpace

    private def namedError[A](p: Parser[A], msg: String): Parser[A] = Parser[A] { i =>
    p(i) match {
        case Failure(_, in) => Failure(msg, in)
        case o => o
      }
    }

    def controllerRef(ref: String) = parse(call, ref)
    def url(src: String) = parse(path, src)
  }

  override protected def parseContent(routesContent: String, file: File): Either[Seq[RoutesCompilationError], List[Rule]] = {
    def parseResources(m: Map[String, Resource], baseUrl: String, acc: List[Rule]): List[Rule] = {
      def constructRule(a: ActionType, path: PathPattern, r: Resource): Option[Route] =
        if (r.getAction(a) != null && r.getAction(a).getDescription != null) {
          val callB = Elements.controllerRef(r.getAction(a).getDescription.split("\n").head)
          if (callB.isEmpty) None
          else {
            val comments = r.getAction(a).getDescription.split("\n").tail.map(s => Comment(s)).toList
            Some(Route(HttpVerb(a.toString), path, callB.get, comments))
          }
        } else None

      def processRule(pathP: PathPattern, r: Resource, url: String): List[Rule] = {
        val l = List(
          constructRule(ActionType.DELETE, pathP, r),
          constructRule(ActionType.GET, pathP, r),
          constructRule(ActionType.HEAD, pathP, r),
          constructRule(ActionType.OPTIONS, pathP, r),
          constructRule(ActionType.PATCH, pathP, r),
          constructRule(ActionType.POST, pathP, r),
          constructRule(ActionType.PUT, pathP, r),
          constructRule(ActionType.TRACE, pathP, r)
        ).collect {
          case Some(v) => v
        }

        parseResources(r.getResources.asScala.toMap, url, l ++ acc)
      }

      if (m.isEmpty) acc
      else {
        m.flatMap { case (u, r) =>
          val url = baseUrl + u
          val pathP = Elements.url(url).get

          processRule(pathP, r, url)
        }
      }.toList
    }

    def fixSlashes(l: List[Rule]) = l.collect {
      case Route(verb, path, call, comments) =>
        val parts = path.parts.foldLeft(Seq[PathPart]()) {
          case (acc :+ StaticPart(v1), p) => acc :+ StaticPart(v1 + "/") :+ p
          case (acc :+ dynPart, StaticPart(v2)) => acc :+ dynPart :+ StaticPart("/" + v2)
          case (acc, e) => acc :+ e
        }
        Route(verb, PathPattern(parts), call, comments)
    }

    Try {
      if (SystemUtils.IS_OS_WINDOWS) {
        new RamlDocumentBuilder(new FileResourceLoader(file.getParentFile.getAbsolutePath)).build(
          new ByteArrayInputStream(routesContent.getBytes(StandardCharsets.UTF_8)), file.getAbsolutePath)
      } else {
        new RamlDocumentBuilder(new FileResourceLoader("")).build(
          new ByteArrayInputStream(routesContent.getBytes(StandardCharsets.UTF_8)), file.getAbsolutePath)
      }
    }.map { raml =>
      Right(fixSlashes(parseResources(raml.getResources.asScala.toMap, "", List())))
    }.recover {
      case e: Exception => Left(Seq(RoutesCompilationError(file, e.getMessage, None, None)))
    }.get
  }
}
