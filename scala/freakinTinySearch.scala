object Search extends App {
    val stopwords = io.Source.fromFile("../stopwords.txt").getLines.toSet
    val invertedIndex = new collection.mutable.HashMap[String, List[(Int,Int)]] //Posting = (docId, TF)
    val dataset = new collection.mutable.ArrayBuffer[String] //Hold the documents contents
    def tokenize(s:String) = s.toLowerCase.split("[^a-z0-9äöüáéíóúãâêîôûàèìòùçñ]+").filter(!stopwords.contains(_))
    def index(doc:String) { //dataset.size = current doc Id
        for(term <- tokenize(doc)) {
            val list = invertedIndex.getOrElse(term, Nil)
            if (list != Nil && list.head._1 == dataset.size) 
                invertedIndex.put(term, (list.head._1, list.head._2 + 1) :: list.tail)
            else 
                invertedIndex.put(term, (dataset.size, 1) :: list)
        }
        dataset += doc
    }
    def docNorma(docId:Int) = math.sqrt(tokenize(dataset(docId)).foldLeft(0D)( (accum, t) => accum + (math.pow(idf(t), 2))))
    def idf(term:String):Double = (scala.math.log(dataset.size / invertedIndex.getOrElse(term, Nil).size))
    def search(q:String, numResults:Int) = {
        val accums = new collection.mutable.HashMap[Int, Double] //Map(docId -> Score)
        for(term <- tokenize(q); val list = invertedIndex.getOrElse(term, Nil)) 
            for(posting <- list) 
                accums.put(posting._1, accums.getOrElse(posting._1, 0D) + posting._2 * math.pow(idf(term), 2))
        accums.map(d => (d._1, dataset(d._1), d._2 / docNorma(d._1))).toSeq.sortWith( _._3 > _._3).take(numResults)
    }
    io.Source.fromFile(args(0)).getLines.foreach(line => index(line))
    while(true) {
        println("Ready for searching:")
        search(readLine(), 10).foreach(println)
    }
}
