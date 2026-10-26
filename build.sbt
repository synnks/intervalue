organization := "com.synnks"
scalaVersion := "3.10.0"

val SpireVersion           = "0.18.0"
val MUnitVersion           = "1.3.6"
val MUnitScalaCheckVersion = "1.3.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "spire"            % SpireVersion,
  "org.scalameta" %% "munit"            % MUnitVersion           % Test,
  "org.scalameta" %% "munit-scalacheck" % MUnitScalaCheckVersion % Test
)

scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Wvalue-discard",
  "-Werror"
)

lazy val root = rootProject
  .settings(
    name := "intervalue"
  )
