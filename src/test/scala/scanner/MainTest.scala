package scanner

import org.scalatest.FlatSpec
import org.scalatest._
import java.nio.file.Paths
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class MainTest extends FlatSpec with Matchers {
  "parseDir" should "throw exception because no argument" in {
    assertThrows[IllegalArgumentException] {
      Main.parseDir(Array())
    }
  }

  it should "throw exception because args # > 1" in {
    assertThrows[IllegalArgumentException] {
      Main.parseDir(Array("anypath", "anypath"))
    }
  }
  
  it should "throw exception because path not exists" in {
    assertThrows[IllegalArgumentException] {
      Main.parseDir(Array("surely_non_existing_path"))
    }
  }
  
  "findWordInString" should "find word in content" in {
    assert(Main.findWordInString("word", "hello word") == 1)
  }

  it should "find word$ in content" in {
    assert(Main.findWordInString("word$", "hello word$ buddy") == 1)
  }

  it should "not find word in content" in {
    assert(Main.findWordInString("word", "hello wordword buddy") == 0)
  }
  
  "findWordInMap" should "find word in content" in {
    assert(Main.findWordInMap("word", Map("hello" -> 1, "word" -> 1)) == 1)
  }

  it should "find word$ in content" in {
    assert(Main.findWordInMap("word$", Map("hello" -> 1, "word$" -> 1, "buddy" -> 1)) == 1)
  }

  it should "not find word in content" in {
    assert(Main.findWordInMap("word", Map("hello" -> 1, "wordword" -> 1, "buddy" -> 1)) == 0)
  }
  
  "toPercentString" should "format with 2 decimals precision" in {
    assert(Main.toPercentString(100) == "100.00%")
  }
  
  "findWordsInFiles" should "find love and gravity in file1 and moon and gravity in file2" in {
    val file1 = Paths.get("file1")
    val file2 = Paths.get("file2")
    val files = Map(
        file1 -> "gravity of love",
        file2 -> "gravity of moon"
    )
    
    val actual = Await.result(Main.findWordsInFiles(Main.findWordInString)(Set("love", "moon", "gravity"), files), Duration.Inf)
    
    assert(actual == List((file1.toUri.getRawPath, "66.67%"), (file2.toUri.getRawPath, "66.67%")))
  }

  it should "find love and gravity in file1 and moon and gravity in file2 sorted with names" in {
    val file1 = Paths.get("file1")
    val file2 = Paths.get("file2")
    val files = Map(
        file2 -> "gravity of love",
        file1 -> "gravity of moon"
    )
    
    val actual = Await.result(Main.findWordsInFiles(Main.findWordInString)(Set("love", "moon", "gravity"), files), Duration.Inf)
    
    assert(actual == List((file1.toUri.getRawPath, "66.67%"), (file2.toUri.getRawPath, "66.67%")))
  }

  it should "find moon and gravity in file2 and gravity in file1 sorted with values" in {
    val file1 = Paths.get("file1")
    val file2 = Paths.get("file2")
    val files = Map(
        file1 -> "gravity of love",
        file2 -> "gravity of moon"
    )
    
    val actual = Await.result(Main.findWordsInFiles(Main.findWordInString)(Set("moon", "gravity"), files), Duration.Inf)
    
    assert(actual == List((file2.toUri.getRawPath, "100.00%"), (file1.toUri.getRawPath, "50.00%")))
  }

  it should "find moon in file2 only" in {
    val file1 = Paths.get("file1")
    val file2 = Paths.get("file2")
    val files = Map(
        file1 -> "gravity of love",
        file2 -> "gravity of moon"
    )
    
    val actual = Await.result(Main.findWordsInFiles(Main.findWordInString)(Set("moon"), files), Duration.Inf)
    
    assert(actual == List((file2.toUri.getRawPath, "100.00%")))
  }

  it should "not find any word" in {
    val file1 = Paths.get("file1")
    val file2 = Paths.get("file2")
    val files = Map(
        file1 -> "gravity of love",
        file2 -> "gravity of moon"
    )
    
    val actual = Await.result(Main.findWordsInFiles(Main.findWordInString)(Set("sun"), files), Duration.Inf)
    
    assert(actual == List())
  }

  "findWordsInFilesForMap" should "find love and gravity in file1 and moon and gravity in file2" in {
    val file1 = Paths.get("file1")
    val file2 = Paths.get("file2")
    val files = Map(
        file1 -> Map("gravity" -> 1, "of" -> 1, "love" -> 1),
        file2 -> Map("gravity" -> 1, "of" -> 1, "moon" -> 1)
    )
    
    val actual = Await.result(Main.findWordsInFiles(Main.findWordInMap)(Set("love", "moon", "gravity"), files), Duration.Inf)
    
    assert(actual == List((file1.toUri.getRawPath, "66.67%"), (file2.toUri.getRawPath, "66.67%")))
  }

  it should "find love and gravity in file1 and moon and gravity in file2 sorted with names" in {
    val file1 = Paths.get("file1")
    val file2 = Paths.get("file2")
    val files = Map(
        file1 -> Map("gravity" -> 1, "of" -> 1, "love" -> 1),
        file2 -> Map("gravity" -> 1, "of" -> 1, "moon" -> 1)
    )
    
    val actual = Await.result(Main.findWordsInFiles(Main.findWordInMap)(Set("love", "moon", "gravity"), files), Duration.Inf)
    
    assert(actual == List((file1.toUri.getRawPath, "66.67%"), (file2.toUri.getRawPath, "66.67%")))
  }

  it should "find moon and gravity in file2 and gravity in file1 sorted with values" in {
    val file1 = Paths.get("file1")
    val file2 = Paths.get("file2")
    val files = Map(
        file1 -> Map("gravity" -> 1, "of" -> 1, "love" -> 1),
        file2 -> Map("gravity" -> 1, "of" -> 1, "moon" -> 1)
    )
    
    val actual = Await.result(Main.findWordsInFiles(Main.findWordInMap)(Set("moon", "gravity"), files), Duration.Inf)
    
    assert(actual == List((file2.toUri.getRawPath, "100.00%"), (file1.toUri.getRawPath, "50.00%")))
  }

  it should "find moon in file2 only" in {
    val file1 = Paths.get("file1")
    val file2 = Paths.get("file2")
    val files = Map(
        file1 -> Map("gravity" -> 1, "of" -> 1, "love" -> 1),
        file2 -> Map("gravity" -> 1, "of" -> 1, "moon" -> 1)
    )
    
    val actual = Await.result(Main.findWordsInFiles(Main.findWordInMap)(Set("moon"), files), Duration.Inf)
    
    assert(actual == List((file2.toUri.getRawPath, "100.00%")))
  }

  it should "not find any word" in {
    val file1 = Paths.get("file1")
    val file2 = Paths.get("file2")
    val files = Map(
        file1 -> Map("gravity" -> 1, "of" -> 1, "love" -> 1),
        file2 -> Map("gravity" -> 1, "of" -> 1, "moon" -> 1)
    )
    
    val actual = Await.result(Main.findWordsInFiles(Main.findWordInMap)(Set("sun"), files), Duration.Inf)
    
    assert(actual == List())
  }

  "toWord" should "find words" in {
    val text = "Cat is wild."
    
    val actual = Main.toWords(text)
    
    assert(actual == List("Cat", "is", "wild"))
  }

  it should "find no words in empty" in {
    val text = ""
    
    val actual = Main.toWords(text)
    
    assert(actual == List())
  }

  it should "find word" in {
    val text = "Cat ."
    
    val actual = Main.toWords(text)
    
    assert(actual == List("Cat"))
  }

  it should "find no words" in {
    val text = ".."
    
    val actual = Main.toWords(text)
    
    assert(actual == List())
  }
  
  "Trie" should "construct tree" in {
    val text = List("peter", "piper", "pecked", "pickled", "peppers")
    val root = Main.Node(key = '#')
    text.foreach { s =>
      root.insert(s)
    }
    println(s"${root}")
    assert(root.find("peter") == 1)
    assert(root.find("piper") == 1)
    assert(root.find("pecked") == 1)
    assert(root.find("pickled") == 1)
    assert(root.find("peppers") == 1)
    assert(root.find("alice") == 0)
    assert(root.find("pete") == 0)
    assert(root.find("peters") == 0)
  }
}