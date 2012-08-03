(defproject onlisp "0.0.1"
  :source-path "."
  :javac-options {:debug "true" :fork "true"}
  :repositories {"sonatype" "http://oss.sonatype.org/content/groups/public/"}
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :dev-dependencies [
                     [swank-clojure "1.4.0-SNAPSHOT" :exclusions [org.clojure/clojure]]
                    ]
  :jvm-opts ["-Djava.library.path=/usr/local/lib:/opt/local/lib:/usr/lib"]
  :extra-classpath-dirs ["."]
)
