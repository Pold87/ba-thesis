(defproject org-ba "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.reader "0.8.3"]
                 [dorothy "0.0.5"]
                 [rhizome "0.2.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [quil "1.7.0-SNAPSHOT"]
                 [net.mikera/imagez "0.3.1"]
                 [me.raynes/conch "0.5.0"]
                 [fipp "0.4.1"]
                 [me.raynes/fs "1.4.4"]]
  :main ^:skip-aot org-ba.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
