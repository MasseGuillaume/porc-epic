
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

lazy val circe = {
  val circeVersion = "0.14.1"

  Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion)
}

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.11" % Test,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1" % Test
) ++ circe


