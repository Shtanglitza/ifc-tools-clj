(defproject ifc-tools-clj "0.1.0-SNAPSHOT"
  :description "IFC library for Clojure"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[lein-tools-deps "0.4.5"]
            [s3-wagon-private "1.3.4"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:project]
                           :resolve-aliases [:test]}
  :repl-options {:init-ns ifc-tools-clj.step.core}
  :repositories [["shtanglitza" {:url "s3p://shtanglitza/clojure"
                                 :username :env/AWS_ACCESS_KEY_ID
                                 :passphrase :env/AWS_SECRET_ACCESS_KEY}]])