(defproject tinysearch "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"] [org.clojure/math.numeric-tower "0.0.2"]]
  :main ^:skip-aot tinysearch.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
