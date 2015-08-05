{:user
 {:plugins [[cider/cider-nrepl "0.9.0"]
            [lein-midje "3.1.3"]
            [lein-gorilla "0.3.4"]]
  :dependencies [[criterium "0.4.3"]
                 [spyscope "0.1.4"]
                 [midje "1.6.0" :exclusions [org.clojure/clojure]]
                 [cljfmt "0.2.0"]
                 #_[com.cemerick/piggieback "0.2.1"]
                 #_[org.clojure/tools.nrepl "0.2.10"]
                 #_[org.bodil/cljs-noderepl "0.1.11"]]
  ;:repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
  :injections [(require 'spyscope.core)]}}
