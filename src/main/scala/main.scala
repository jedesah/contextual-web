package contextual.website

import rapture.http._, httpBackends.jetty._, requestExtractors._
import rapture.dom._
import rapture.html._
import rapture.codec._
import rapture.core._
import rapture.uri._
import rapture.css._, dynamicCssReferencing._
import rapture.js._
import rapture.net._
import rapture.io._
import rapture.mime._
import rapture.time._
import rapture.json._, jsonBackends.spray._

import encodings.`UTF-8`.{name => _, _}

import htmlSyntax._
import dateFormats.longEuropean._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

object Main {

  def launch() = Future(main(Array()))

  def main(args: Array[String]): Unit = HttpServer.listen(80) { r =>
    log.info(s"Request received for path ${r.path} on ${r.serverName}")
    r match {
      case Path(^ / "scripts" / f) =>
        implicit val mime = MimeTypes.`text/javascript`
        uri"classpath:scripts/$f".input[Char]
      case Path(^ / "styles" / f) =>
        implicit val mime = MimeTypes.`text/css`
        uri"classpath:styles/$f".input[Char]
      case Path(^ / "content" / f) =>
        implicit val mime = MimeTypes.`text/plain`
        uri"classpath:content/$f".input[Char]
      case Path(^ / "images" / f) if f.endsWith("svg") =>
        implicit val mime = MimeTypes.`image/svg+xml`
        uri"classpath:images/$f".input[Byte]
      case Path(^ / "images" / f) if f.endsWith("png") =>
        implicit val mime = MimeTypes.`image/png`
        uri"classpath:images/$f".input[Byte]
      case Path(^ / "images" / f) if f.endsWith("jpg") =>
        implicit val mime = MimeTypes.`image/jpeg`
        uri"classpath:images/$f".input[Byte]
      case request if request.path.toString.startsWith("/_/api/") => try {
        implicit val mime = request.path.toString.split("\\.").last match {
          case "html" => MimeTypes.`text/html`
          case "css" => MimeTypes.`text/css`
          case "js" => MimeTypes.`text/javascript`
          case "gif" => MimeTypes.`image/gif`
          case "png" => MimeTypes.`image/png`
          case "jpg" => MimeTypes.`image/jpeg`
          case "svg" => MimeTypes.`image/svg+xml`
          case _ => MimeTypes.`application/octet-stream`
        }

        uri"classpath:api/${request.path.toString.drop(7)}".input[Byte]
      } catch {
        case e: Exception =>
          e.printStackTrace()
          throw e
        }
      case _ => Contextual.handle(r)
    }
  }
}

object Page {

  def cssLink(ss: String) = Link(rel = stylesheet, href = ^ / "styles" / s"$ss.css")

  def standard(title: String)(pageContent: AppliedElement[_ <: Html5.Flow, _ <: Html5.Flow, _ <: AttributeType]*): HtmlDoc = {
    HtmlDoc(
      Html(
        Head(
          Title(title),
          Meta(charset = encodings.`UTF-8`()),
          Meta(name = 'description, content = ""),
          Meta(name = 'author, content = ""),
          Meta(name = 'viewport, content = "width=device-width,initial-scale=1,maximum-scale=1,user-scalable=0"),
          Style(typ = "text/css")(Contextual.styles()),
          Script("(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');ga('create','UA-60044672-3','auto');ga('send','pageview');")
        ),
        Body(
          Div(cls = 'header)(
            H1("contextual"),
            H2("simple and typesafe interpolated strings, checked at compile-time")
          ),
          Div(cls = 'page)(pageContent: _*)
        )
      )
    )
  }

}

/**

Home
 - about
 - examples
 - API documentation
 - 

**/

case class Dsl(
  name: String,
  prefix: String,
  pkg: String,
  ivyGroup: String,
  ivyProject: String,
  version: String,
  reference: Option[String],
  examples: List[String]
)

object Contextual {
 
  object styles {
    val stylesheet = cssStylesheet"""
      @import url('https://fonts.googleapis.com/css?family=PT+Serif:400,400i,700|Roboto|Droid+Sans+Mono');
      $Body { padding: 0; margin: 0; font-family: Roboto; }
      .header { background: url(/images/contextual.jpg); width: 100vw; padding: 20vw 0 0 0;
          color: white; margin: 0; background-size: cover;
          max-height: 30vh; overflow: hidden; }
      a:link, a:visited, a:hover, a:active { color: #3d9930; text-decoration: none; }
      a:hover { text-decoration: underline; }
      $H1 { font-family: 'PT Serif'; font-size: 6vw; font-weight: 400;
          text-shadow: 0 0 1vw rgba(0, 0, 0, 0.8); margin: 0 0 0 10vw;
          }
      $H2 { font-family: 'PT Serif'; font-size: 2vw; font-weight: 400; font-style: italic;
          text-shadow: 0 0 1vw rgba(0, 0, 0, 0.8); margin: 0 0 0 0; padding: 1vw 0 1vw 10vw;
          background-color: rgba(0, 0, 0, 0.25);
          box-shadow: 0 0 2vw rgba(255, 255, 255, 0.5); }
      $H3 { font-family: 'PT Serif'; font-size: 24px; color: #d7182a; font-weight: 400;
          margin: 1.2em 0 0.6em; }
      $H3 $Code { font-size: 1em; }
      $H4 { font-family: 'PT Serif'; font-weight: 700; margin: 1.6em 0 -0.3em; padding-bottom: 0; }
      $Ul{ margin: 0.75em 0; padding: 0 1em; list-style: none; }
			$Li:before { content: ""; border-color: transparent #111; border-style: solid;
          border-width: 0.35em 0 0.35em 0.45em; display: block; height: 0; width: 0;
          left: -1em; top: 0.9em; position: relative; }
      .page { width: 80vw; max-width: 40em; margin: 5vw auto 5vw auto;
          }
      $P { text-align: justify; font-weight: 300; line-height: 1.8em; font-size: 15px; }
      .tags a { background-color: #3d9930; text-decoration: none; border-radius: 0.4em; padding: 0.2em 0.5em; line-height: 2em; color: white; opacity: 0.8;  }
      .tags a:hover { opacity: 1; text-decoration: none; } 
      
      $Code, $Pre { font-family: 'Droid Sans Mono'; font-size-adjust: 0.52; font-size: 15px; clear: both; }
      $Pre { border: solid 1px #dddddd; background-color: #eeeeee; padding: 4px 12px; }
			.inset { float: right; width: 24vw; margin: 0 -10vw 1vw 3vw; }
      .inset h3 { background-color: #d7182a; font-weight: bold; color: white; font-size: 1em;
          padding: 0.4vw 1vw; margin: 0.4vw 0 0; text-transform: uppercase;
          font-family: 'Droid Sans Mono'; }
      .inset p { padding: 0 1vw; line-height: 1.35em;
          text-align: left; font-size: 0.9em; }
      .inset h4 { margin: 0.8em 0 0.5em 0; padding: 0; font-size: 0.8em; text-transform: uppercase; border-bottom: solid 1px silver; font-family: 'Droid Sans Mono'; }
      @media screen and (max-height: 600px) {
        .header { padding: 5vw 0 0 0; }
      }
      @media screen and (max-width: 900px) {
        .inset { float: none; padding: 0; margin: 0; width: 100%; }
      }
    """

    def apply(): String = String(stylesheet)
  }

  def handle(r: HttpRequest): Response = r match {
    case Path(^ / "" | ^) =>
      Page.standard("Contextual: Home")(
        Div(cls = 'inset)(
          H3("Getting started"),
          H4("SBT"),
          Code(""""com.propensive" %% "contextual" % "0.12""""),
          H4("Import"),
          Code("""import contextual._"""),
          H4("Examples"),
          Div(cls = 'tags)((Interpolators.index.flatMap { dsl =>
            List(A(href = ^ / dsl)(Interpolators.all(dsl).name), Span(" "))
          }): _*),
          H4("Links"),
          Ul(
            Li(A(href = uri"https://github.com/propensive/contextual")("Source on GitHub")),
            Li(A(href = uri"https://gitter.im/propensive/contextual")("Gitter channel")),
            Li(A(href = ^ / "_" / "api" / "index.html")("API documentation"))
          )
        ),
        H3("About contextual"),
        P("""
          Contextual is a small Scala library which allows you to define your own string interpolators—prefixed string literals like """, Code("""uri"https://google.com""""), """ which determine how they should be evaluated, at runtime and at compile-time, while only writing very ordinary user code: no macros!
        """),
        P("""Contextual is still very new, so don't expect everything to work perfectly, yet!"""),
        H3("A simple example"),
        P("We can define a simple interpolator for URLs, ", Code("""url"""""), ", like this:"),
        Pre("""
          |import contextual._
          |
          |object UrlInterpolator extends Interpolator {
          |  def implementation(ctx: Contextual) = {
          |    val url = ctx.literals.head
          |    if(!checkValid(url.string)) url.abort(0, "not a valid URL")
          |    ctx.Implementation(url.string)
          |  }
          |}
          |
          |implicit class UrlStringContext(sc: StringContext) {
          |  val url = Prefix(UrlInterpolator, sc)
          |}""".stripMargin),
        P("and at the use site, it makes this possible:"),
        Pre("""
          |url"http://www.propensive.com/"""".stripMargin),
        H3("How does it work?"),
        P("""
          Scala offers the facility to implement custom string interpolators,
          and places no restrictions on implementing these using macros.
          Contextual provides a generalized macro for interpolating strings
          (with a prefix of your choice) that calls into a simple API for
          defining the compile-time checks and runtime implementation of the
          interpolated string."""),
        P("""This can be done without """, Em("you"), """ writing any macro code."""),
        H3("Concepts"),
        H4("Interpolators"),
        P("""
          An """, Code("Interpolator"), """ defines how an interpolated string should be understood at
          both compile-time, and runtime. Often, these are similar operations, but differ in
          how much is known about the holes; the expressions being interpolated amongst the fixed parts of
          the interpolated string. At runtime we have the evaluated substituted values 
          available, whereas at compile-time the values are unknown, though we do instead
          have access to certain meta-information about the substitutions, which allows some useful constraints to be placed on substitutions.
        """),
        H4("The ", Code("implementation"), " method"),
        P(Code("Interpolator"), "s have one abstract method which needs implementing to provide all he compile-time and runtime functionality:"),
        Pre("def implementation(ctx: Contextual): ctx.Implementation"),
        P(""),
        H4("The ", Code("Contextual"), " type"),
        P("""We represent the context of an interpolated string with the """, Code("Contextual"), """ type. It provides access to the fixed literal parts of the interpolated string, metadata about the holes, and the means to report errors during compilation, and to construct runtime implementations of the interpolated string."""),
        P("""Perhaps the most useful method of """, Code("Contextual"), """ is """, Code("parts"), """ which gives a sequence of tokens representing each section of the interpolated string: alternating """, Code("Literal"), " and ", Code("Hole"), """ objects which we can process to analyze the interpolated string.."""),
        H4("Attaching a prefix"),
        P("""In order to make a new string interpolator available through a prefix on a string, the Scala compiler needs to be able to "see" that prefix on Scala's built-in """, Code("StringContext"), """ object. This is very easily done by specifying a new """, Code("Prefix"), """ value with the desired name on an implicit class that wraps """, Code("StringContext"), """."""),
        P("""
          The """, Code("Prefix"), """ constructor takes only two parameters: the """, Code("Interpolator"), """ object (and it must be an """, B("object"), """, otherwise the macro will not be able to invoke it at compile time), and the """, Code("StringContext"), """ instance that we are extending.""")
      )

    case Path(^ / id) =>
      val interpolator = Interpolators.all(id)
      Page.standard(s"Contextual: Interpolator: $id")(
        Div(cls = 'inset)(
          H3("Package Information"),
          H4("SBT"),
          Code(interpolator.ivy.toString),
          H4("License"),
          "Apache 2.0",
          H4("Source code"),
          Ul(
            Li(
              A(href = Http.parse(interpolator.source))("GitHub")
            )
          )
        ),
        H3(s"${interpolator.name} (", Code(id), """"")"""),
        Div(
          H4("Features"),
          Div(
            Ul("",
              (interpolator.features.map { f => Li(f) }): _*
            )
          )
        ),
        H4("Examples"),
        Div(
          "",
          (interpolator.examples.map { eg =>
            Pre("import "+interpolator.pkg+"._\n"+interpolator.id+"\"\"\""+eg+"\"\"\"")
          }): _*
        ),
        interpolator.ref match {
          case Some(ref) => Div(
            H4("References"),
            Ul(
              Li(
                A(href = Http.parse(ref))(ref)
              )
            )
          )
          case None => Div("")
        }
      )
  }

}

object Interpolators {
  val index = uri"classpath:index.json".slurp[Char].as[Json].as[List[String]].sorted
  val all: Map[String, Interpolator] = index.map { idx =>
    val interpolator = uri"classpath:interpolators/$idx.json".slurp[Char].as[Json].as[Interpolator]
    (interpolator.id, interpolator)
  }.toMap
}

case class IvyRef(artifact: String, group: String, versions: List[String]) {
  override def toString = s""""$artifact" %% "$group" % "${versions.last}""""
}
case class Interpolator(id: String, name: String, pkg: String, examples: List[String], ivy: IvyRef, source: String, ref: Option[String], features: List[String], returnType: String, license: String, contexts: List[String] = Nil)

