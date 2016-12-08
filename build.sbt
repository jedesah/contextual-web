lazy val root = (project in file(".")).
  settings(
    name := "contextual-website",
    version := "1.0-M1",
    scalaVersion := "2.11.8",
    scalacOptions += "-Xexperimental"
  )

unmanagedResourceDirectories in Compile += { baseDirectory.value / "ext" }

val raptureVersion = "2.0.0-M7"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
libraryDependencies += "com.propensive" %% "rapture" % raptureVersion
libraryDependencies += "com.propensive" %% "rapture-json-spray" % raptureVersion
libraryDependencies += "com.propensive" %% "rapture-http-jetty" % raptureVersion
