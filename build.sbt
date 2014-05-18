name := "desktop-util"

version := "1.0"

organization := "me.ycy"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.ini4j" % "ini4j" % "0.5.2"
)
