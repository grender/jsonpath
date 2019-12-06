import _root_.io.gatling.build.MavenPublishKeys._
import _root_.io.gatling.build.license._

enablePlugins(AutomateHeaderPlugin, SonatypeReleasePlugin)

projectDevelopers := Seq(
  GatlingDeveloper("slandelle@gatling.io", "Stéphane Landelle", isGatlingCorp = true),
  GatlingDeveloper("nremond@gmail.com", "Nicolas Rémond", isGatlingCorp = false),
  GatlingDeveloper("grender.no8@gmail.com", "Alex Kopytov", isGatlingCorp = false)
)

licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))
headerLicense := ApacheV2License

useSonatypeRepositories := true

githubPath := "grender/jsonpath"

crossPaths := true

// Guthub package publish settings 

publishMavenStyle := true

// GitHub package repo isn't supporting javadoc and sources
publishArtifact in (Compile, packageDoc) := false
publishArtifact in (Compile, packageSrc) := false

externalResolvers += "GitHub grender Apache Maven Packages" at "https://maven.pkg.github.com/grender/jsonpath"
publishTo := Some("GitHub grender Apache Maven Packages" at "https://maven.pkg.github.com/grender/jsonpath")

sys.env.get("GITHUB_TOKEN") match {
  case None => {
    println("not found github token. not set credentials")
    credentials ++= Seq() // stupid hack to do nothing
  }
  case Some(githubToken)=>credentials += Credentials("GitHub Package Registry", "maven.pkg.github.com", "grender", githubToken)
}
