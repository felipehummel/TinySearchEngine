(ns tinysearch.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(def inverted-index (atom {}))
(def docs (atom []))

(defn add-posting [term tf doc-id]
  (swap! inverted-index update-in [term] conj {:doc-id doc-id :tf tf}))

(defn tokenize [doc]
  (str/split doc #"[^a-z0-9äöüáéíóúãâêîôûàèìòùçñ]+"))

(defn index-doc [doc]
  (swap! docs conj doc)
  (let [doc-id (dec (count @docs))]
    (->>
      (tokenize doc)
      (frequencies)
      (map (fn [[term tf]] (add-posting term tf doc-id)))
      (doall))))

(defn index-file [file]
  (->>
    (slurp file)
    (str/split-lines)
    (map index-doc)
    (doall)))

(defn posting-list [term]
  (get @inverted-index term))

(defn doc-content [doc-id]
  (nth @docs doc-id))

(defn idf [term]
  (Math/log
    (double
      (/ (count @docs) (count (posting-list term))))))

(defn doc-norm [doc-id]
  (->>
    (doc-content doc-id)
    (tokenize)
    (map (fn [term] (Math/pow (idf term)  2)))
    (reduce +)
    (Math/sqrt)))

(defn term-scores [{term :term postings :postings}]
  (->>
    postings
    (map (fn [p] (assoc p :score (* (:tf p) (math/expt (idf term) 2)))))))

(defn search-or [query]
  (let [splitted-query (tokenize query)]
    (->>
      splitted-query
      (map (fn [t] {:term t :postings (posting-list t)})) ; postings
      (mapcat term-scores)
      (group-by :doc-id)
      (map
        (fn [[doc-id postings]]
          {:doc-id doc-id
           :score (double (/ (reduce + (map :score postings)) (doc-norm doc-id)))}))
      (sort-by :score)
      (reverse))))

(defn search-loop []
  (while true
    (println "Type your search:")
    (->>
      (read-line)
      (search-or)
      (map (fn [result] (assoc result :doc (doc-content (:doc-id result)))))
      (map println)
      (doall))))

(defn index-and-search-loop [file]
  (index-file file)
  (search-loop))

(defn -main
  [& args]
  (index-and-search-loop (first args)))