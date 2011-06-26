Tiny Search
-----------

How many lines of code it takes to write a reasonable, understandable, full-text search engine?
The code in this repository can give an easy and fast overview on the Vector Space Model (tf-idf). 
Feel free to contribute with improvements and other language implementations.

Other languages
----------

Feel free to submit pull requests with implementations in any other languages. 
You can follow the same requirements of the Scala version:

- in-memory index;
- norms and IDF calculated online;
- default OR operator between query terms; 
- index a document per line from a single file.
- read stopwords from a file


Scala
---------
There are two Scala versions of the Vector Space Model. They are similar, except that "freakinTinySearch.scala" squeezes some more lines by getting rid of classes.

Warnings:

-   I only tested the Scala code with 2.9.
-   This is not intented for real world production code. It is just for fun and educational purposes.
-   The Scala code calculates document norm and term IDF on-the-fly while processing the query. This is far from optimal, but it makes things shorter.

    
    
