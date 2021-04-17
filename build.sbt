name := "Grub"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.3"


libraryDependencies  ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scalactic" %% "scalactic" % "3.2.0",
  "org.knowm.xchart" % "xchart" % "3.8.0",
  "org.scalatest" %% "scalatest" % "3.2.0" % "test"
)