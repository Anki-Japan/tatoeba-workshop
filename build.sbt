scalaVersion := "2.11.6"
sbtVersion   := "0.13.8"

resolvers += "Atilika Open Source repository" at "http://www.atilika.org/nexus/content/repositories/atilika"

libraryDependencies ++= Seq(
  "commons-io"           % "commons-io" % "2.4",
  "org.atilika.kuromoji" % "kuromoji"   % "0.7.7"
)