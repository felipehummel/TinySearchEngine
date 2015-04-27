type Tokenizer = (String => Array[String])
case class SimpleTokenizer(regex: String = "[^a-z0-9äöüáéíóúãâêîôûàèìòùçñ]+") extends Tokenizer {
  val stopwords = io.Source.fromFile("../stopwords.txt").getLines.toSet
  def apply(s: String) = s.toLowerCase.split(p).filter( !stopwords.contains(_))
}

case class Posting(docId: Int, tf: Int)

case class Result(docId: Int, doc: String, score: Double)

class Index(val tokenizer: Tokenizer) {
  var invertedIndex = Map[String, List[Posting]]().withDefaultValue(Nil)
  var dataset = List.empty[String] //Hold the documents contents

  def docCount(term: String) = invertedIndex(term).size
  def index(doc: String) {
    val wordCounts = tokenizer(doc).groupBy(identity).mapValues(_.size)
    for((term, tf) <- wordCounts) {
        invertedIndex += (term -> Posting(dataset.size, tf) :: invertedIndex(term))
    }
    dataset = doc :: dataset
  }
}

class Searcher(index: Index) {
  def tokenizer = index.tokenizer
  def docNorm(docId: Int) = {
    val docTerms = tokenizer(index.dataset(docId))
    math.sqrt( docTerms.map( term => math.pow(idf(term), 2) ).sum )
  }

  def idf(term: String) =
    math.log(index.dataset.size.toDouble / index.docCount(term).toDouble)

  def searchOR(q: String, topk: Int) = {
    val accums = new collection.mutable.HashMap[Int, Double].withDefaultValue(0D) //Map[docId -> Score]
    for (term <- tokenizer(q)) {
        for (posting <- index.invertedIndex(term)) {
            accums.put(posting.docId, accums(posting.docId) + posting.tf * math.pow(idf(term),2))
        }
    }
    accums.map(accumToResult).toSeq.sortWith( _.score > _.score).take(topk)
  }

  private def accumToResult(docIdAndScore: (Int, Double)): Result =
    Result(d._1, index.dataset(d._1), d._2 / docNorm(d._1))
}
object IndexAndSearch extends App {
  val index = new Index(SimpleTokenizer())
  io.Source.fromFile(args(0)).getLines.foreach(line => index.index(line))
  val searcher = new Searcher(index)
  while(true) {
      println("Ready for searching:")
      searcher.searchOR(readLine(), 10).foreach(println)
  }
}
