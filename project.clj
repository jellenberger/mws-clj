(defproject sparky "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [http-kit "2.1.14"]
                 [clj-http "0.7.8"]
                 [commons-codec "1.8"]]
  :main sparky.core
  :profiles {:uberjar {:aot :all}})
