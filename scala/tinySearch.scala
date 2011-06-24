class Tokenizer(val p:String = "[^a-z0-9äöüáéíóúãâêîôûàèìòùçñ]+") {
    val stopwords = io.Source.fromFile("../stopwords.txt").getLines.toSet
    def tokenize(s:String) = s.toLowerCase.split(p).filter( !stopwords.contains(_))
}
case class Posting(docId:Int, var tf:Int)
case class Result(docId:Int, doc:String, score:Double)
class Index(val tokenizer: Tokenizer) {
    val invertedIndex = new collection.mutable.HashMap[String, List[Posting]]
    val dataset = new collection.mutable.ArrayBuffer[String] //Hold the documents contents
    def getDocCount(term:String) = invertedIndex.getOrElse(term, Nil).size
    def index(doc:String) { //dataset.size = current doc Id
        for(term <- tokenizer.tokenize(doc)) {
            val list = invertedIndex.getOrElse(term, Nil)
            if (list != Nil && list.head.docId == dataset.size)  //not the first time this term appears in the document
                list.head.tf += 1
            else    //first time of this term in the document 
                invertedIndex.put(term, Posting(dataset.size, 1) :: list)
        }
        dataset += doc
    }
}
class Searcher(val index:Index, val tokenizer:Tokenizer) {
    def docNorm(docId:Int) = math.sqrt(tokenizer.tokenize(index.dataset(docId)).foldLeft(0D)( (accum, t) => accum + math.pow(idf(t),2)))
    def idf(term:String) = math.log(index.dataset.size / index.getDocCount(term))
    def search(q:String, topk:Int) = {
        val accums = new collection.mutable.HashMap[Int, Double] //Map(docId -> Score)
        for(term <- tokenizer.tokenize(q)) 
            for(posting <- index.invertedIndex.getOrElse(term, Nil)) 
                accums.put(posting.docId, accums.getOrElse(posting.docId, 0D) + posting.tf * math.pow(idf(term),2))
        accums.map(d => Result(d._1, index.dataset(d._1), d._2 / docNorm(d._1))).toSeq.sortWith( _.score > _.score).take(topk)
    }  
}
object IndexAndSearch extends App {
    val index = new Index(new Tokenizer)
    io.Source.fromFile(args(0)).getLines.foreach(line => index.index(line))
    val searcher = new Searcher(index, new Tokenizer)
    while(true) {
        println("Ready for searching:")
        searcher.search(readLine(), 10).foreach(println)
    }
}
