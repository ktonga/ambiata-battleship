
scalaVersion := "2.11.7"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= deps

lazy val deps = Seq(
  "org.scalaz"     %% "scalaz-core"               % "7.1.3"
, "org.specs2"     %% "specs2-core"               % "3.0"    % "test"
, "org.specs2"     %% "specs2-scalacheck"         % "3.0"    % "test"
, "org.scalacheck" %% "scalacheck"                % "1.12.4" % "test"
, "org.scalaz"     %% "scalaz-scalacheck-binding" % "7.0.6"  % "test"
)

