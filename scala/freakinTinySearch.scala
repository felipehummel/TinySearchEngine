object Search extends App {
    val stopwords = io.Source.fromFile("../stopwords.txt").getLines.toSet
    val invertedIndex = new collection.mutable.HashMap[String, List[(Int,Int)]] //Posting = (docId, TF)
    val dataset = new collection.mutable.ArrayBuffer[String] //Hold the documents contents
    def tokenize(s:String) = s.toLowerCase.split("[^a-z0-9äöüáéíóúãâêîôûàèìòùçñ]+").filter(!stopwords.contains(_))
    def index(doc:String) { //dataset.size = current doc Id
        for(term <- tokenize(doc)) {
            val list = invertedIndex.getOrElse(term, Nil)
            if (list != Nil && list.head._1 == dataset.size)  //not the first time this term appears in the document
                invertedIndex.put(term, (list.head._1, list.head._2 + 1) :: list.tail)
            else    //first time of this term in the document 
                invertedIndex.put(term, (dataset.size, 1) :: list)
        }
        dataset += doc
    }
    def docNorm(docId:Int) = math.sqrt(tokenize(dataset(docId)).foldLeft(0D)( (accum, t) => accum + (math.pow(idf(t), 2))))
    def idf(term:String):Double = (scala.math.log(dataset.size.toDouble / invertedIndex.getOrElse(term, Nil).size.toDouble))
    def searchOR(q:String, topk:Int) = {
        val accums = new collection.mutable.HashMap[Int, Double] //Map(docId -> Score)
        for(term <- tokenize(q); posting <- invertedIndex.getOrElse(term, Nil)) 
            accums.put(posting._1, accums.getOrElse(posting._1, 0D) + posting._2 * math.pow(idf(term), 2))
        accums.map(d => (d._1, dataset(d._1), d._2 / docNorm(d._1))).toSeq.sortWith(_._3 > _._3).take(topk)
    }
    io.Source.fromFile(args(0)).getLines.foreach(line => index(line))
    while(true) {
        println("Input your query:")
        searchOR(readLine(), 10).foreach(println)
    }
}
