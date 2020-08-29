name := "scala-pocker"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.12"

val osName: SettingKey[String] = SettingKey[String]("osName")

osName := (System.getProperty("os.name") match {
  case name if name.startsWith("Linux") => "linux"
  case name if name.startsWith("Mac") => "mac"
  case name if name.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
})

libraryDependencies += "org.openjfx" % "javafx-base" % "11.0.2" classifier osName.value
libraryDependencies += "org.openjfx" % "javafx-controls" % "11.0.2" classifier osName.value
libraryDependencies += "org.openjfx" % "javafx-fxml" % "11.0.2" classifier osName.value
libraryDependencies += "org.openjfx" % "javafx-graphics" % "11.0.2" classifier osName.value
libraryDependencies += "org.openjfx" % "javafx-media" % "11.0.2" classifier osName.value

libraryDependencies += "org.scalafx" %% "scalafx" % "14-R19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.1"
