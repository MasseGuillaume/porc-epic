
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

libraryDependencies ++= circe ++
  Seq(
    "org.scalameta" %% "munit" % "0.7.29" % "it,test",
    // "org.typelevel" %% "cats-parse" % "0.3.7-10-5592775-SNAPSHOT" % IntegrationTest,
    "org.typelevel" %% "cats-parse" % "0.3.7" % IntegrationTest,
  )

testFrameworks += new TestFramework("munit.Framework")


enablePlugins(JmhPlugin)
configs(IntegrationTest)
Defaults.itSettings
IntegrationTest / parallelExecution := false

lazy val porcEpic = project.in(file("."))

lazy val bench = project.dependsOn()