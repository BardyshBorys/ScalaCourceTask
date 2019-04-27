name := "LohikaScalaCourses"

version := "0.1"

scalaVersion := "2.12.8"


(scalastyleConfig in Test) := baseDirectory.value / "scalastyle-test-config.xml"
(scalastyleConfigUrl in Test) := Some(url("http://www.scalastyle.org/scalastyle_config.xml"))

lazy val dependencies =
  new {
    val scalatestV = "3.0.4"
    val scalacheckV = "1.13.5"
    val scoptV = "3.7.1"

    val scalatest = "org.scalatest" %% "scalatest" % scalatestV
    val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckV
    val scopt = "com.github.scopt" %% "scopt" % scoptV
  }

lazy val commonDependencies = Seq(
  dependencies.scalatest % "test",
  dependencies.scalacheck % "test",
  dependencies.scopt
)

libraryDependencies ++= commonDependencies
