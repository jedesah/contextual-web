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
            H1(A(href = ^)("contextual")),
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
      $H1 a:link, $H1 a:visited, $H1 a:hover, $H1 a:active { text-decoration: none; color: white; }
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
      .tags a { border: solid 1px #3d9930; text-decoration: none; padding: 0.1em 0.4em; line-height: 2em; color: #3d9930; opacity: 0.8;  }
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
          Code(""""com.propensive" %% "contextual" % "1.0.0""""),
          H4("Import"),
          Code("""import contextual._"""),
          H4("Examples"),
          Div(cls = 'tags)((Interpolators.index.flatMap { dsl =>
            List(A(href = ^ / dsl)(dsl), Span(" "))
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
          Contextual is a small Scala library for defining your own string interpolators—prefixed string literals like """, Code("""url"https://google.com""""), """ which determine how they are interpreted at compile-time, including any custom checks and compile errors that should be reported, while only writing very ordinary "user" code: no macros!
        """),
        H3("A simple example"),
        P("We can define a simple interpolator for URLs, ", Code("""url"""""), ", like this:"),
        Pre("""
          |import contextual._
          |
          |case class Url(url: String)
          |
          |object UrlInterpolator extends Interpolator {
          |  
          |  def contextualize(interpolation: StaticInterpolation) = {
          |    val lit@Literal(_, urlString) = interpolation.parts.head
          |    if(!checkValidUrl(urlString))
          |      interpolation.abort(lit, 0, "not a valid URL")
          |
          |    Nil
          |  }
          |
          |  def evaluate(interpolation: RuntimeInterpolation): Url =
          |    Url(interpolation.literals.head)
          |}
          |
          |implicit class UrlStringContext(sc: StringContext) {
          |  val url = Prefix(UrlInterpolator, sc)
          |}""".stripMargin),
        P("and at the use site, it makes this possible:"),
        Pre("""
          |scala> url"http://www.propensive.com/"
          |res: Url = Url(http://www.propensive.com/)
          |
          |scala> url"foobar"
          |<console>: error: not a valid URL
          |       url"foobar"
          |           ^""".stripMargin),
        H3("How does it work?"),
        P("""
          Scala offers the facility to implement custom string interpolators,
          and while these may be implemented with a simple method definition, the compiler imposes
          no restrictions on using macros. This allows the constant parts of an interpolated
          string to be inspected at compile-time, along with the types of the expressions
          substituted into it.
        """),
        P("""
          Contextual provides a generalized macro for interpolating strings
          (with a prefix of your choice) that calls into a simple API for
          defining the compile-time checks and runtime implementation of the
          interpolated string."""),
        P("""This can be done without """, Em("you"), """ writing any macro code."""),
        H3("Concepts"),
        H4(Code("Interpolator"), "s"),
        P("""
          An """, Code("Interpolator"), """ defines how an interpolated string should be understood, both at
          compile-time, and runtime. Often, these are similar operations, as both will work on the same
          sequence of constant literal parts to the interpolated string, but will differ in
          how much is known about the holes; that is, the expressions being interpolated amongst the constant parts of
          the interpolated string. At runtime we have the evaluated substituted values 
          available, whereas at compile-time the values are unknown, though we do
          have access to certain meta-information about the substitutions, which allows some useful constraints to be placed on substitutions.
        """),
        H4("The ", Code("contextualize"), " method"),
        P(Code("Interpolator"), "s have one abstract method which needs implementing to provide any compile-time checking or parsing functionality:"),
        Pre("def contextualize(interpolation: StaticInterpolation): Seq[Context]"),
        P("""The """, Code("contextualize"), """ method requires an implementation which inspects the literal parts and holes of the interpolated string. These are provided by the """, Code("parts"), """ member of the """, Code("interpolation"), """ parameter. """, Code("interpolation"), """ is an instance of """, Code("StaticInterpolation"), """, and also provides methods for reporting errors and warnings at compile-time.
        """),
        H4("The ", Code("evaluate"), " method"),
        P("""The runtime implementation of the interpolator would typically be provided by defining an implementation of """, Code("evaluate"), """. This method is not part of the subtyping API, so does not have to conform to an exact shape; it will be called with a single """, Code("Contextual[RuntimePart]"), """ parameter whenever an interpolator is expanded, but may take type parameters or implicit parameters (as long as these can be inferred), and may return a value of any type."""),
        H4("The ", Code("StaticInterpolation"), " and ", Code("RuntimeInterpolation"), " types"),
        P("""We represent the information about the interpolated string known at compile-time and runtime with the """, Code("StaticInterpolation"), """ and """, Code("RuntimeInterpolation"), """ types, respectively. These provide access to the constant literal parts of the interpolated string, metadata about the holes and the means to report errors and warnings at compile-time; and at runtime, the values substituted into the interpolated string, converted into a common "input" type. Normally """, Code("String"), """ would be chosen for the input type, but it's not required."""),
        P("""Perhaps the most useful method of the interpolation types is the """, Code("parts"), """ method which gives the sequence of parts representing each section of the interpolated string: alternating """, Code("Literal"), " values with either ", Code("Hole"), """s (at compile-time) or """, Code("Substitution"), """s at runtime."""),
        H4("Contexts"),
        P("""When checking an interpolated string containing some DSL, holes may appear in different contexts within the string. For example, in a XML interpolated string, a substitution may be inside a pair of (matching) tags, or as a parameter to an attribute, for example, """, Code("""xml"<tag attribute=$att>$content</tag>""""), ". In order for the XML to be valid, the string ", Code("att"), " must be delimited by quotes, whereas the string ", Code("code"), " does not require the quotes; both will require escaping. This difference is modeled with the concept of ", Code("Context"), "s: user-defined objects which represent the position within a parsed interpolated string where a hole is, and which may be used to distinguish between alternative ways of making a substitution."),
        P("""This idea is fundamental to any advanced implementation of the """, Code("contextualize"), """ method: besides performing compile-time checks, the method should return a sequence of """, Code("Context"), """s corresponding to each hole in the interpolated string. In the XML example above, this might be the sequence, """, Code("Seq(Attribute, Inline)"), """, referencing objects (defined at the same time as the """, Code("Interpolator"), """) which provide context to the substitutions of the """, Code("att"), " and ", Code("content"), """values."""),
        H4("Generalizing Substitutions"),
        P("""A typical interpolator will allow only certain types to be used as substitutions. This may include a few common types like """, Code("Int"), "s, ", Code("Boolean"), "s and ", Code("String"), """s, but Contextual supports ad-hoc extension with typeclasses, making it possible for user-defined types to be supported as substitutions, too. However, in order for the interpolator to understand how to work with arbitrary types, which may not yet have been defined, the interpolator must agree on a common interface for all substitutions. This is the """, Code("Input"), """ type, defined on the """, Code("Interpolator"), """, and every typeclass instance representing how a particular type should be embedded in an interpolated string must define how that type is converted to the common """, Code("Input"), """ type."""),
        P("""Often, it is easy and sufficient to use """, Code("String"), """ as the """, Code("Input"), """ type."""),
        H4("Embedding types"),
        P("""Different types are embedded by defining an implicit """, Code("Embedder"), """ typeclass instance, which specifies with a number of """, Code("Case"), """ instances how the type should be converted to the interpolator's """, Code("Input"), """ type. For example, given a hypothetical XML interpolator, """, Code("Symbol"), """s could be embedded using,"""),
        Pre("""implicit val embedSymbolsInXml = XmlInterpolator.embed[Symbol](
              |  Case(AttributeKey, AfterAtt)(_.name),
              |  Case(AttributeVal, InTag) { s => '"'s.name+'"' },
              |  Case(Content, Content)(_.name)
              |)""".stripMargin),
        P("""where the conversion to """, Code("String"), """s are defined for three different contexts, """, Code("AttributeKey"), ", ", Code("AttributeVal"), ", and ", Code("Content"), """. Whilst in the first two cases, the context changes, in the final case, the context is unchanged by making the substitution."""),
        H4("Attaching the interpolator to a prefix"),
        P("""Finally, in order to make a new string interpolator available through a prefix on a string, the Scala compiler needs to be able to "see" that prefix on Scala's built-in """, Code("StringContext"), """ object. This is very easily done by specifying a new """, Code("Prefix"), """ value with the desired name on an implicit class that wraps """, Code("StringContext"), """, as in the example above:"""),
        Pre("""
          |implicit class UrlStringContext(sc: StringContext) {
          |  val url = Prefix(UrlInterpolator, sc)
          |}""".stripMargin),
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
          ),
          H4("Defined contexts"),
          if(interpolator.contexts.isEmpty) P("None") else Ul("",
            interpolator.contexts.map { ctx => Li(Code(ctx)) }
          ),
          H4("Predefined substitution types"),
          if(interpolator.substitutionTypes.isEmpty) P("None") else Ul("",
            interpolator.substitutionTypes.map { ctx => Li(Code(ctx)) }
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
          (interpolator.examples.map { case Example(src, res) =>
            Pre("> import "+interpolator.pkg+"._\n> "+interpolator.id+"\"\"\""+src+"\"\"\"\nres: "+interpolator.returnType+s" = $res")
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

case class Example(source: String, result: String)

case class IvyRef(artifact: String, group: String, versions: List[String]) {
  override def toString = s""""$artifact" %% "$group" % "${versions.last}""""
}
case class Interpolator(id: String, name: String, pkg: String, examples: List[Example], ivy: IvyRef, source: String, ref: Option[String], features: List[String], returnType: String, license: String, contexts: List[String] = Nil, substitutionTypes: List[String] = Nil)

