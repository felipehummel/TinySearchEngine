object Search extends App {
    case class Posting(docId:Int, var tf:Int)
    case class SearchResult(docId:Int, doc:String, score:Float)
    val stopwords = Set("de", "o", "a", "com", "the")
    val invertedIndex = new collection.mutable.HashMap[String, List[Posting]]
    val dataset = new collection.mutable.ArrayBuffer[String]() //usa o dataset.size como docId
    def tokenize(s:String) = s.toLowerCase.split("[^a-z0-9äöüáéíóúãâêîôûàèìòùçñ]+").filter( !stopwords.contains(_))
    def getDocCount(term:String) = invertedIndex.getOrElse(term, Nil).size
    def index(doc:String) {
        dataset += doc
        for(term <- tokenize(doc)) {
            val list = invertedIndex.getOrElse(term, Nil)
            if (list != Nil && list.head.docId == dataset.size-1) 
                list.head.tf += 1
            else 
                invertedIndex.put(term, Posting(dataset.size-1, 1) :: list)
        }
    }
    def docNorma(docId:Int) : Float = 
        scala.math.sqrt(tokenize(dataset(docId)).foldLeft(0F)( (accum, t) => accum + (idf(t)*idf(t)))).asInstanceOf[Float]
    def idf(term:String) = (scala.math.log(dataset.size / getDocCount(term)).asInstanceOf[Float])
    def search(q:String, numResults:Int) = {
        val tokenizedQuery = tokenize(q)
        var accums = new collection.mutable.HashMap[Int, Float]
        for(i <- 0 until tokenizedQuery.size; val list = invertedIndex.getOrElse(tokenizedQuery(i), Nil)) 
            for(posting <- list) 
                accums.put(posting.docId, accums.getOrElse(posting.docId, 0F) + posting.tf * idf(tokenizedQuery(i))*idf(tokenizedQuery(i)))
        val results = accums.map(d => SearchResult(d._1, dataset(d._1), d._2 / docNorma(d._1))).toList
        results.sortWith( _.score > _.score).take(numResults)
    }
    scala.io.Source.fromFile(args(0)).getLines.foreach(line => index(line))
    while(true) {
        println("Pronto pra buscar:")
        search(readLine(), 10).foreach(println)
    }
}
