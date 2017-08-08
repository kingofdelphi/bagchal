name := "bagchal"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.102-R11"

unmanagedJars in Compile += {
  val ps = new sys.SystemProperties
  val jh = ps("java.home")
  Attributed.blank(file(jh) / "lib/ext/jfxrt.jar")
}

assemblyJarName in assembly := "bagchal.jar"

