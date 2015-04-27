case class Posting(docId: Int, tf: Int)

case class Result(docId: Int, doc: String, score: Double)

type Tokenizer = (String => Array[String])
type InvertedIndex = Map[String, List[Posting]]

case class SimpleTokenizer(regex: String = "[^a-z0-9äöüáéíóúãâêîôûàèìòùçñ]+") extends Tokenizer {
  val stopwords = io.Source.fromFile("../stopwords.txt").getLines.toSet
  def apply(s: String) = s.toLowerCase.split(regex).filter( !stopwords.contains(_))
}

class Index(val tokenizer: Tokenizer,
            val invertedIndex: InvertedIndex = Map.empty,
            val dataset: IndexedSeq[String] = Vector.empty) {
  def docCount(term: String) = invertedIndex.getOrElse(term, Nil).size
  def index(doc: String): Index = {
    val wordCounts = tokenizer(doc).groupBy(identity).mapValues(_.size)
    var newInverted = invertedIndex
    for((term, tf) <- wordCounts) {
      val newPostingList = Posting(dataset.size, tf) :: invertedIndex.getOrElse(term, Nil)
      newInverted += (term -> newPostingList)
    }
    new Index(tokenizer, newInverted, dataset :+ doc)
  }
}

class Searcher(index: Index) {
  def docNorm(docId: Int) = {
    val docTerms = tokenizer(index.dataset(docId))
    math.sqrt( docTerms.map( term => math.pow(idf(term), 2) ).sum )
  }

  def idf(term: String) =
    math.log(index.dataset.size.toDouble / index.docCount(term).toDouble)

  def searchOR(q: String, topK: Int = 10) = {
    val accums = new collection.mutable.HashMap[Int, Double].withDefaultValue(0D) //Map[docId -> Score]
    for (term <- index.tokenizer(q)) {
        for (posting <- index.invertedIndex.getOrElse(term, Nil)) {
            accums.put(posting.docId, accums(posting.docId) + posting.tf * math.pow(idf(term),2))
        }
    }
    accums.map(accumToResult).toSeq.sortWith(_.score > _.score).take(topK)
  }

  private def accumToResult(docIdAndScore: (Int, Double)): Result = {
    val (docId, score) = docIdAndScore
    Result(docId, index.dataset(docId), score / docNorm(docId))
  }
}

object IndexAndSearch extends App {
  def indexFromFile(filePath: String): Index = {
    val emptyIndex = new Index(SimpleTokenizer())
    val source = io.Source.fromFile(filePath)
    val index = source.getLines.foldLeft(emptyIndex) { (accIndex, line) => accIndex.index(line) }
    source.close()
    index
  }
  val searcher = new Searcher(indexFromFile(args(0)))
  while(true) {
    println("Ready for searching:")
    searcher.searchOR(scala.io.StdIn.readLine()).foreach(println)
  }
}