// **Scocco** is a quick and dirty litterate programming tool for scala a la [Docco](http://jashkenas.github.com/docco/)
// 
// It is a quick-and-dirty, hundred-line-long, literate-programming-style
// documentation generator. It produces HTML
// that displays your comments alongside your code. Comments are passed through a 
// [Markdown](http://daringfireball.net/projects/markdown/syntax) parser 
// implemented by the [Actuarius library](https://github.com/chenkelmann/actuarius), 
// and code is passed through [Google prettify](http://code.google.com/p/google-code-prettify/) syntax highlighting.
// This page is the result of running Scocco against its own source file.
// 
// Using SBT : 
// 
//     sbt "run src/main/scala/Scocco.scala"
// 
// ...will generate an HTML documentation page for the named source files, 
// saving it into a `docs` folder.
// 
// The [source for Scocco](http://github.com/bobylito/scocco) is available on GitHub,
// and released under the MIT license.
// 
// Scocco can be used to process Scala.
// Only single-line comments are processed -- block comments are ignored.
// 
//### Partners in Crime:
// 
// * CoffeeScript (the original flavour) : [Jeremy Ashkenas](https://github.com/jashkenas/)'s
// [Docco](https://github.com/jashkenas/docco)
// 
// * Ruby : [Ryan Tomayko](http://github.com/rtomayko)'s 
// [Rocco](http://rtomayko.github.com/rocco/rocco.html)
// 
// * POSIX Shell scripts : Mr. Tomayko's
// [Shocco](http://rtomayko.github.com/shocco/)
// 
// * Python : [Nick Fitzgerald](http://github.com/fitzgen)'s 
// [Pycco](http://fitzgen.github.com/pycco/)
// 
// * Clojure : [Fogus](http://blog.fogus.me/)'s 
// [Marginalia](http://fogus.me/fun/marginalia/) 
// 
// * Lua : 
// [Robert Gieseke](https://github.com/rgieseke)'s [Locco](http://rgieseke.github.com/locco/).
// 
// * .NET [Don Wilson](https://github.com/dontangg)'s 
// [Nocco](http://dontangg.github.com/nocco/).
// 
//### The boring paperwork...

//First some imports 
import scala.collection.Set
import java.io.File
import scala.io.Source
import java.io.PrintWriter
import java.io.FileWriter
import java.io.BufferedWriter
import scala.util.parsing.combinator.RegexParsers

//### The subject

//Definition of the application. The whole application is contained in a single object.
object Scocco extends scala.App {
  
//What is necessary for actually parsing the markdown code
  import eu.henkelmann.actuarius.ActuariusTransformer
  val transformer = new ActuariusTransformer()
  def markdownToHtmlSnippet(input:String):String = transformer(input)
  
//This case class represents a part of code with its associated comments
  case class Section(comment: List[String], code: List[String], id : Int) {
//This function does not modify the current object
    def addSource(line: String): Section = {
      if (isStringComment(line)) {
        Section(line :: this.comment, this.code, this.id)
      } else {
        val n = line :: this.code
        Section(this.comment, n, this.id)
      }
    }
  }

  def generateDocumentation = { filename: String =>
    val f = readFile(filename)
    val sections = f.map({ file =>
      val sections = parse(file)
      
      // The use of *xml litterals* makes it very clean and simple 
      // to have html templates in the code
      val txt = <html>
                    <head>
                      <link rel="stylesheet" href="resources/docco.css"></link>
                      <link rel="stylesheet" href="resources/prettify.css"></link>
                      <script src="resources/prettify.js" type="text/javascript"></script>
                      <style>{"pre.prettyprint{border:none}"}</style>
                    </head>
                    <body onload="prettyPrint()" >
                      <table  cellpadding="0" cellspacing="0">
                        <tr><th class="docs"><h1>{ file.getName }</h1></th><th class="code"></th></tr>
                        { sections.reverse.map( x => template(x) ) }
                      </table>
                    </body>
                </html>
      export(file.getName()).map({ outStream =>
        outStream.write(txt.toString())
        outStream.close()
      }).getOrElse({
          //If the out file can't be write, output the standard out  
          println(txt)
      })
    })
    .getOrElse(sys.error("Impossible de lire le fichier : " + filename))
  }

  def readFile = { filename: String =>
    val f = new File(filename)
    if (f == null || !f.exists() || !f.canRead()) {
      None
    } else {
      Some(f)
    }
  }
  
  def ifDoesntExistMakeIt(f : File) = {
    if(!f.exists){
      f.mkdir
    }
  }
  
  def copyResourceToOut(resourcesFolder:File, filename : String){
    val file = this.getClass.getClassLoader.getResourceAsStream(filename)
    org.apache.commons.io.FileUtils.copyInputStreamToFile(file, new File(resourcesFolder, filename))
  }
  
  def export(filename : String) = {
    val doc = new File("docs")
    ifDoesntExistMakeIt(doc)
    
    val resourcesFolder = new File(doc, "resources")
    ifDoesntExistMakeIt(resourcesFolder)
    
    copyResourceToOut(resourcesFolder, "docco.css")
    copyResourceToOut(resourcesFolder, "prettify.css")
    copyResourceToOut(resourcesFolder, "prettify.js")
    
    val docFileName = filename.replaceAll("\\..+$", ".html")
    val out = new File(doc, docFileName)
    
    if(!out.exists()){
        out.createNewFile()
    }
    
    Some(new BufferedWriter(new FileWriter(out)))
  }
  
  def template(section:Section) = { 
    <tr id={section.id.toString()}>
      <td class="docs">
          <div class="pilwrap"> <a class="pilcrow" href={"#" + section.id.toString()} >&para;</a></div>
          { markdown(section.comment.reverse.map(clean).mkString("\n")) }
      </td>
      <td class="code">
          <div class="highlight">
              <pre class="prettyprint">{ section.code.reverse.mkString("\n") }</pre>
          </div>
      </td>
    </tr>
  }

  def clean(comment : String) = {
    comment.trim().replaceAll("^//", "")
  }

// Because the lib uses xml litterals, strings containing html have to be marked as not to be parsed
  def markdown(comment : String) = {
    scala.xml.Unparsed( markdownToHtmlSnippet( comment ))
  }

  def parse(file:File):List[Section] = {
    val lines = Source.fromFile(file).getLines
    lines.foldLeft(List[Section]())( {
      (parsed: List[Section], line: String) => 
          parsed match {
            case Nil => {
              val newLine = Section(Nil, Nil, 0)
              newLine.addSource(line) :: Nil
            }
            case Section(comment, Nil, _) :: tail => {
              parsed.head.addSource(line) :: tail
            }
            case Section(comment, code, id) :: tail => {
              if (isStringComment(line)) {
                val newLine = Section(line :: Nil, Nil, id+1)
                newLine :: parsed
              } else {
                Section(comment, line :: code, id) :: tail
              }
            }

          }
      })
  }

  def isStringComment = { (s: String) =>
    s.matches("^[ ]*//.+")
  }
  
// Let's start the whole thing!!
  args.map(generateDocumentation)
}