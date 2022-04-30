// for the visualization part
lazy val circe = {
  val circeVersion = "0.14.1"

  Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic"
  ).map(_ % circeVersion)
}

lazy val munit = Seq("org.scalameta" %% "munit" % "0.7.29" % Test)

lazy val baseSettings = Seq(
  scalaVersion := "3.1.2",
  libraryDependencies ++= circe ++ munit,
  testFrameworks += new TestFramework("munit.Framework"),
  organization := "com.github.masseguillaume",
  homepage := Some(url("https://github.com/MasseGuillaume/porc-epic")),
  licenses := List(License.MIT),
  developers := List(
    Developer(
      "MasseGuillaume",
      "Guillaume Massé",
      "masgui@gmail.com",
      url("https://github.com/masseguillaume")
    )
  )
)

lazy val root = project.in(file("."))
  .settings(baseSettings)
  .aggregate(porcEpic, integration, bench)
  .dependsOn(porcEpic, integration, bench)

lazy val porcEpic = project
  .in(file("porc-epic"))
  .settings(baseSettings)

lazy val integration = project
  .in(file("integration"))
  .settings(baseSettings)
  .settings(
    libraryDependencies ++= Seq("org.typelevel" %% "cats-parse" % "0.3.7"),
    Test / parallelExecution := false,
    Test / fork := true,
    Test / javaOptions += "-Xmx4G",
    buildInfoPackage := "build",
    buildInfoKeys ++= Seq[BuildInfoKey](
      "porcupine" -> file("porcupine").getAbsolutePath
    )
  )
  .dependsOn(porcEpic)
  .enablePlugins(BuildInfoPlugin)

lazy val bench = project
  .settings(baseSettings)
  .dependsOn(porcEpic, integration)
  .enablePlugins(JmhPlugin)
