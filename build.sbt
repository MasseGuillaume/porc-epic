
inThisBuild(
  List(
    scalaVersion := "3.1.0",
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
)

// for the visualization part
lazy val circe = {
  val circeVersion = "0.14.1"

  Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
  ).map(_ % circeVersion)
}

lazy val munit = Seq("org.scalameta" %% "munit" % "0.7.29" % Test)

libraryDependencies ++= circe ++ munit

testFrameworks += new TestFramework("munit.Framework")


lazy val porcEpic = project.in(file("."))

lazy val integration = project.in(file("integration")).settings(
  libraryDependencies ++= Seq("org.typelevel" %% "cats-parse" % "0.3.7") ++ munit,
  Test / parallelExecution := false,
  buildInfoPackage := "build",
  buildInfoKeys ++= Seq[BuildInfoKey]("porcupine" -> file("porcupine").getAbsolutePath)
).dependsOn(porcEpic).enablePlugins(BuildInfoPlugin)

lazy val bench = project.dependsOn(porcEpic, integration).enablePlugins(JmhPlugin)