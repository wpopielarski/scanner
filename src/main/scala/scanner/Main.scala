package scanner

import java.nio.file.Paths
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.JavaConverters._
import java.nio.charset.Charset
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main extends App {
  def parseDir(args: Array[String]): Path = {
    args.toList match {
      case path::Nil =>
        val root = Paths.get(path)
        if (Files.exists(root))
          root
        else
          throw new IllegalArgumentException(s"Given path: $path does not exist.")
      case _ =>
        throw new IllegalArgumentException(s"Incorrect number of args. Must be one.")
    }
  }
 
  def scan(root: Path): List[Path] = {
    root match {
      case path if Files.isDirectory(path) =>
        Files.walk(path)
        .iterator().asScala
        .toList
        .filter(Files.isRegularFile(_))
      case file =>
        List(file)
    }
  }

  def storeAsString(content: List[String]): String = content.mkString(" ")
  
  def toWords(text: String): List[String] = {
    val result = text.foldLeft(List("")) { (z, e) =>
      z match {
        case _ :+ last if last == "" && !e.isLetter =>
          z
        case _ :+ last if last != "" && !e.isLetter =>
          z :+ ""
        case other :+ last =>
          other :+ (last + e)
      }
    }
    result match {
      case other :+ "" => other
      case result => result
    }
  }

  def storeAsMap(content: List[String]): Map[String, Int] = content
    .flatMap { toWords
    }.foldLeft(Map.empty[String, Int]) { (map, word) =>
    map.get(word) match {
      case Some(_) => map
      case None => map + (word -> 1)
    }
  }
  
  def storeInMemory[R](how: (List[String] => R))(files: List[Path]): Map[Path, R] = {
    files.map { file =>
      (file, Files.readAllLines(file, Charset.defaultCharset()).asScala.toList)
    }.map {
      case (file, listOfContent) =>
        (file, how(listOfContent))
    }.toMap
  }

  private val filesAndContent = (parseDir _ andThen scan _ andThen storeInMemory(storeAsMap))(args)
  println(filesAndContent)
  println(s"${filesAndContent.size} files found in ${args(0)}")

  def findWordInString(word: String, content: String): Int = {
    import scala.util.matching.Regex
    val regWord = ("""(^|\s)""" + Regex.quote(word) + """($|\s)""").r
    regWord.findFirstIn(content) match {
      case Some(_) => 1
      case None => 0
    }
  }

  def findWordInMap(word: String, content: Map[String, Int]): Int = content.get(word).getOrElse(0)
  
  def toPercentString(value: Double): String =
    "%.2f".format(value) + "%"

  def findWordsInFiles[R](findWord: (String, R) => Int)(words: Set[String], files: Map[Path, R]): Future[List[(String, String)]] = {
    val total = words.size
    Future.sequence(files.toList.map {
      case (path, content) => Future {
        val percent = words.toList.map {
          findWord(_, content)
        }.sum
        (path, if (total == 0) 0d else 100d * percent / total)
      }
    }).map { listOfPathsAndValues =>
      listOfPathsAndValues.filter { pathValue =>
        val (path, value) = pathValue 
        value > 0d
      }.sortBy(r => (-r._2, r._1)).map {
        case (path, value) => (path.toUri.getRawPath, toPercentString(value))
      }
    }
  }

  while(true) {
    val words = scala.io.StdIn.readLine("scanner >")
    val result = Await.result(findWordsInFiles(findWordInMap)(words.split("\\s+").toSet, filesAndContent), Duration.Inf)
    println(result.map(v => s"${v._1} ${v._2}").mkString("\n"))
  }

  case class Node(key: Char, kids: Array[Node] = Array.ofDim(26), value: Int = 0) {
    def insert(key: String): Unit = key.toSeq match {
      case Nil => ()
      case k +: tail =>
        if (kids(k - 'a') eq null)
          kids(k - 'a') = Node(key = k)
        if (tail.isEmpty)
          kids(k - 'a') = kids(k - 'a').copy(value = 1)
        kids(k - 'a').insert(tail.mkString)
    }
    
    def find(text: String): Int = text.toSeq match {
      case Nil =>
        value
      case l +: other =>
        if (kids(l - 'a') ne null)
          kids(l - 'a').find(other.mkString)
        else 0
    }
    
    override def toString = {
      s"""key: ${key}, value: ${value}, ${kids.map(v => if (v ne null) v.toString else "").mkString("[", "", "]")}"""
    }
  }
}
