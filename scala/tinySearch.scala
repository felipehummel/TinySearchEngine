class Tokenizer(val p:String = "[^a-z0-9äöüáéíóúãâêîôûàèìòùçñ]+") {
    val stopwords = io.Source.fromFile("../stopwords.txt").getLines.toSet
    def tokenize(s:String) = s.toLowerCase.split(p).filter( !stopwords.contains(_))
}
case class Posting(docId:Int, var tf:Int)
case class Result(docId:Int, doc:String, score:Float)
class Index(val tokenizer: Tokenizer) {
    val invertedIndex = new collection.mutable.HashMap[String, List[Posting]]
    val dataset = new collection.mutable.ArrayBuffer[String] //Hold the documents contents
    def getDocCount(term:String) = invertedIndex.getOrElse(term, Nil).size
    def index(doc:String) { //dataset.size = current doc Id
        for(term <- tokenizer.tokenize(doc)) {
            val list = invertedIndex.getOrElse(term, Nil)
            if (list != Nil && list.head.docId == dataset.size) 
                list.head.tf += 1
            else 
                invertedIndex.put(term, Posting(dataset.size, 1) :: list)
        }
        dataset += doc
    }
}
class Searcher(val index:Index, val tokenizer:Tokenizer) {
    def docNorma(docId:Int) : Float = 
        math.sqrt(tokenizer.tokenize(index.dataset(docId)).foldLeft(0F)( (accum, t) => accum + (idf(t)*idf(t)))).asInstanceOf[Float]
    def idf(term:String) = (math.log(index.dataset.size / index.getDocCount(term)).asInstanceOf[Float])
    def search(q:String, topk:Int) = {
        val accums = new collection.mutable.HashMap[Int, Float] //Map(docId -> Score)
        for(term <- tokenizer.tokenize(q); val list = index.invertedIndex.getOrElse(term, Nil)) 
            for(posting <- list) 
                accums.put(posting.docId, accums.getOrElse(posting.docId, 0F) + posting.tf * idf(term)*idf(term))
        accums.map(d => Result(d._1, index.dataset(d._1), d._2 / docNorma(d._1))).toSeq.sortWith( _.score > _.score).take(topk)
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
