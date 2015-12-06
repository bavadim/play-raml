package bavadim.sbt

import java.io.File

import bavadim.compiler.Compiler
import play.routes.compiler.RoutesCompiler.RoutesCompilerTask
import play.sbt.routes.{RoutesCompiler, _}
import sbt.Keys._
import sbt._

import scala.language.postfixOps

object PlayRamlPlugin extends Plugin {
  lazy val testRoutesFilePath = SettingKey[String]("test-routes-path", "Path to routes file during testing")
  lazy val ramlList = TaskKey[Seq[RoutesCompilerTask]]("play-raml", "The raml routes files to compile")

  override lazy val projectSettings = RoutesCompiler.defaultSettings ++
    inConfig(Compile)(routesSettings) ++
    inConfig(Test)(routesSettings)

  def routesSettings = Seq(
    testRoutesFilePath := "conf/api.raml",
    //redirect play generators
    ramlList <<= Def.taskDyn {
      // Aggregate all the routes file tasks that we want to compile the reverse routers for.
      RoutesKeys.aggregateReverseRoutes.value.map {
        agg => RoutesKeys.routesCompilerTasks in(agg.project, configuration.value)
      }.join.map { aggTasks: Seq[Seq[RoutesCompilerTask]] =>

        // Aggregated tasks need to have forwards router compilation disabled and reverse router compilation enabled.
        val reverseRouterTasks = aggTasks.flatten.map { task =>
          task.copy(forwardsRouter = false, reverseRouter = true)
        }

        def routesSources = {
          val dirs = (unmanagedResourceDirectories in Compile).value
          (dirs * "raml").get ++ (dirs * "*.raml").get
        }

        val thisProjectTasks = routesSources.map { file =>
          RoutesCompilerTask(file, RoutesKeys.routesImport.value, forwardsRouter = true,
            reverseRouter = RoutesKeys.generateReverseRouter.value, namespaceReverseRouter = RoutesKeys.namespaceReverseRouter.value)
        }

        thisProjectTasks ++ reverseRouterTasks
      }
    },
    sourceGenerators <+= Def.task[Seq[File]] {
        Compiler.compileRoutes(ramlList.value, RoutesKeys.routesGenerator.value, (target in RoutesKeys.routes).value, 
          streams.value.cacheDirectory, state.value.log)
      }
  )

}
