import bintray.Keys._

lazy val commonSettings = Seq(
  version in ThisBuild := "0.2",
  organization in ThisBuild := "bavadim"
)

lazy val root = (project in file(".")).
  settings(commonSettings ++ bintrayPublishSettings: _*).
  settings(
    sbtPlugin := true,
    name := "play-raml",
    description := "Compile play routes from RAML file",
    homepage := some(url("https://github.com/bavadim/play-raml")),
    licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
    publishMavenStyle := false,
    repository in bintray := "sbt-raml-plugins",
    bintrayOrganization in bintray := None,
    libraryDependencies ++= Seq(
      "commons-io" % "commons-io" % "2.4",
      "com.typesafe.play" % "routes-compiler_2.10" % "2.4.3",
      "com.typesafe.play" %% "play" % "2.4.3",
      "org.raml" % "raml-parser" % "0.8.12" exclude("org.slf4j", "slf4j-log4j12"),
      "org.scalatest" %% "scalatest" % "3.0.0-M12" % Test),
    addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.3" exclude("org.slf4j", "slf4j-simple") exclude("org.slf4j", "slf4j-log4j12"))
  )
