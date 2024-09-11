import org.scalajs.jsenv.nodejs.NodeJSEnv

ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme := Some("semver-spec")

publish / skip := true

lazy val xml = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "xml",
    version := "0.0.7",
    scalaVersion := "3.5.0",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
      ),
    organization := "io.github.edadma",
    githubOwner := "edadma",
    githubRepository := name.value,
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      "com.lihaoyi" %%% "pprint" % "0.9.0" % "test",
    ),
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "char-reader" % "0.1.12",
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    licenses += "ISC" -> url("https://opensource.org/licenses/ISC"),
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )
  .nativeSettings(
  )
  .jsSettings(
    jsEnv := new NodeJSEnv(NodeJSEnv.Config().withExecutable("/home/ed/.nvm/versions/node/v18.20.4/bin/node")),
    //    Test / scalaJSUseMainModuleInitializer := true,
    //    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer := true,
  )
