val scala3Version = "3.7.4"

val catsVersion = "2.13.0"
lazy val root = project
  .in(file("."))
  .settings(
    name := "free-aggregate",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-kernel" % catsVersion,
      "org.typelevel" %% "cats-free" % catsVersion,
      "org.scalameta" %% "munit" % "1.1.0" % Test
    )
  )
