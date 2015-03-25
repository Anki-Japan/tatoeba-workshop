package janki.workshop

import java.io._

import scala.collection.JavaConversions._

import org.apache.commons.io._

import org.atilika.kuromoji._

object Workshop extends App {

  def log(msg: String, spaces: Int = 0) = println(" " * spaces + msg)

  def diff(s1: Seq[_], s2: Seq[_]) = s1.size - s2.size

  def load(name: String): Seq[Array[String]] = {
    val is: InputStream = getClass.getClassLoader.getResourceAsStream(s"$name")
    IOUtils.readLines(is).map(_ split "\t" ).toSeq
  }

  case class Sentence(id: Long, lang: String, body: String, words: Seq[String] = Nil)
  case class Link    (from: Long, to: Long)

  log("Setup")
  val sentences = load("sentences.csv").map {arr => Sentence(arr(0).toLong, arr(1), arr(2))}
  val links     = load("links.csv")    .map {arr => Link    (arr(0).toLong, arr(1).toLong) }

  log("Step 1: Remove all languages but English and Japanese")
  val sentences1 = sentences.filter {s => s.lang == "eng" || s.lang == "jpn"}
  log(s"${diff(sentences, sentences1)} removed", 4)

  log("Step 2: Remove links from/to sentences that are not in scope")
  val validIds = sentences1.map {_.id}.toSet
  val links2   = links.filter   {case Link(from, to) => validIds(from) && validIds(to)}
  log(s"${diff(links, links2)} removed", 4)

  log("Step 3: Remove links whos \"from\" and \"to\" belong to same language")
  val languages = sentences1.map {sent => sent.id -> sent.lang}.toMap
  val links3    = links2.filter  {case Link(from, to) => languages(from) != languages(to)}
  log(s"${diff(links2, links3)} removed", 4)

  log("Step 4: Remove sentences who don't have links")
  val validIds4  = links3.flatMap    {l => Seq(l.from, l.to)}.toSet
  val sentences4 = sentences1.filter {s => validIds4(s.id)} 
  log(s"${diff(sentences1, sentences4)} removed", 4)

  log("Step 5: Add words to sentences")
  val tokenizer  = Tokenizer.builder.mode(Tokenizer.Mode.SEARCH).build

  val partsOfSpeech = Set(
    "名詞",    // めいし      - noun
    "形容詞",  // けいようし   - i-adjective
    "動詞",    // どうし      - verb
//  "助動詞",  // じょうどうし - auxiliary verb
//  "助詞",    // じょし      - particle
    "副詞"     // ふくし      - adverb
//  "連体詞"   // れんたいし   - pre-noun adjectival
  )

  def tokenize(str: String): Seq[String] = tokenizer.tokenize(str)
    .filter {t => partsOfSpeech(t.getPartOfSpeech.takeWhile(_ != ','))}
    .map    {_.getBaseForm}
    .filter {_ != null}
    .toSeq

  val sentences5 = sentences4.map {s => if (s.lang == "jpn") s.copy(words = tokenize(s.body)) else s}

  log("Step 6: Assert conditions")
  val fSentences: Seq[Sentence] = sentences5
  val fLinks    : Seq[Link    ] = links3

  log("Every link should have the \"to\" part", 2)
  val indexedLinks: Map[Long, Seq[Long]] = fLinks
    .groupBy {_.from}
    .map     {case (from, links) => from -> links.map {_.to}}
  log("Indexation is done", 4)
  val foulLink = indexedLinks.find {case (k, seq) => seq.isEmpty}
  log(s"Foul link found: $foulLink", 4)

  log("If exists a link from -> to, then a link to -> from must exist", 2)
  val foulLink2 = indexedLinks.find {case (k, links) => links.exists(l => !indexedLinks(l).contains(k))}
  log(s"Sentence doesn't match condition: $foulLink2", 4)

  log("Every sentence should be linked", 2)
  val allowedIds6 = indexedLinks.keys.toSet
  val foulSentence = fSentences.find(s => !allowedIds6(s.id))
  log(s"Foul sentence: $foulSentence", 4)

  log("Step 7: Save the work")
  val delimiter      = "\t"
  val wordsDelimiter = ";"

  val outDir       = new File("/Users/anatolii/Projects/JAnki/workshop/out")
  val sentencesOut = new File(outDir, "sentences")
  val linksOut     = new File(outDir, "links"    )

  def delimited(tokens: Any*) = tokens.mkString(delimiter)

  val lSentences: Seq[String] = fSentences.map {case Sentence(id, lang, body, words) =>
    delimited(id, lang, body, words.mkString(wordsDelimiter))
  }
  val lLinks    : Seq[String] = fLinks.map {case Link(from, to) => delimited(from, to)}

  FileUtils.writeLines(sentencesOut, lSentences)
  FileUtils.writeLines(linksOut    , lLinks    )

  log("Done")

}