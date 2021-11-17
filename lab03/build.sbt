val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "lab03",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scala-lang.modules" % "scala-xml_3" % "2.0.1",
    libraryDependencies += "org.typelevel" % "cats-core_3" % "2.6.1"
  )
