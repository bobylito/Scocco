name := "scocco"

organization := "bobylito"

version := "1.0-SNAPSHOT"

resolvers += "Christophs Maven Repo" at "http://maven.henkelmann.eu/"

libraryDependencies ++= Seq(
    "eu.henkelmann" % "actuarius_2.9.1" % "0.2.3",
    "commons-io" % "commons-io" % "2.4" 
)
