(ns build
  "Build script for rechentafel.

   Tasks:
     clojure -T:build clean    — clean build artefacts
     clojure -T:build jar      — build the JAR (writes target/)
     clojure -T:build install  — install to local Maven repo
     clojure -T:build deploy   — deploy to Clojars (CLOJARS_USERNAME / CLOJARS_PASSWORD)
     clojure -T:build release  — create a GitHub release with the JAR (GITHUB_TOKEN)

   Versioning follows the replikativ convention:
     0.1.<commit-count>"
  (:refer-clojure :exclude [test])
  (:require [borkdude.gh-release-artifact :as gh]
            [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd])
  (:import [clojure.lang ExceptionInfo]))

(def org         "replikativ")
(def lib         'org.replikativ/rechentafel)
(def current-commit (b/git-process {:git-args "rev-parse HEAD"}))
(def version     (format "0.1.%s" (b/git-count-revs nil)))
(def class-dir   "target/classes")
(def basis       (b/create-basis {:project "deps.edn"}))
(def jar-file    (format "target/%s-%s.jar" (name lib) version))

(def src-dirs ["src" "resources"])

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (println "Writing pom.xml...")
  (b/write-pom
   {:class-dir class-dir
    :lib       lib
    :version   version
    :basis     basis
    :src-dirs  src-dirs
    :scm       {:url                 "https://github.com/replikativ/rechentafel"
                :connection          "scm:git:git://github.com/replikativ/rechentafel.git"
                :developerConnection "scm:git:ssh://git@github.com/replikativ/rechentafel.git"
                :tag                 (str "v" version)}
    :pom-data  [[:description
                 (str "Pure-Clojure spreadsheet interpreter — formulas, dynamic arrays, "
                      "LET/LAMBDA, tables, 3D refs. Cross-platform (Clojure + ClojureScript).")]
                [:url "https://github.com/replikativ/rechentafel"]
                [:licenses
                 [:license
                  [:name "Apache License, Version 2.0"]
                  [:url  "https://www.apache.org/licenses/LICENSE-2.0"]]]
                [:developers
                 [:developer
                  [:id    "whilo"]
                  [:name  "Christian Weilbach"]
                  [:email "ch_weil@topiq.es"]]]]})
  (println "Copying sources...")
  (b/copy-dir {:src-dirs   src-dirs
               :target-dir class-dir})
  (println (format "Building JAR: %s" jar-file))
  (b/jar {:class-dir class-dir
          :jar-file  jar-file}))

(defn install [_]
  (jar nil)
  (println "Installing to local Maven repo...")
  (b/install {:basis     basis
              :lib       lib
              :version   version
              :jar-file  jar-file
              :class-dir class-dir}))

(defn deploy
  "Deploy to Clojars.

   Requires environment variables:
     CLOJARS_USERNAME — Clojars username
     CLOJARS_PASSWORD — Clojars deploy token (not the account password)

   Get a token at https://clojars.org/tokens"
  [_]
  (jar nil)
  (println "Deploying to Clojars...")
  (dd/deploy {:installer :remote
              :artifact  jar-file
              :pom-file  (b/pom-path {:lib lib :class-dir class-dir})})
  (println (format "Deployed %s version %s" lib version)))

(defn fib [a b]
  (lazy-seq (cons a (fib b (+ a b)))))

(defn retry-with-fib-backoff [retries exec-fn test-fn]
  (loop [idle-times (take retries (fib 1 2))]
    (let [result (exec-fn)]
      (if (test-fn result)
        (do (println "Returned: " result)
            (if-let [sleep-ms (first idle-times)]
              (do (println "Retrying with remaining back-off times (in s): " idle-times)
                  (Thread/sleep (* 1000 sleep-ms))
                  (recur (rest idle-times)))
              result))
        result))))

(defn try-release []
  (try (gh/overwrite-asset {:org           org
                            :repo          (name lib)
                            :tag           version
                            :commit        current-commit
                            :file          jar-file
                            :content-type  "application/java-archive"
                            :draft         false})
       (catch ExceptionInfo e
         (assoc (ex-data e) :failure? true))))

(defn release
  "Create / refresh a GitHub release containing the built JAR.

   Requires GITHUB_TOKEN with `repo` scope."
  [_]
  (jar nil)
  (println "Creating GitHub release...")
  (let [ret (retry-with-fib-backoff 10 try-release :failure?)]
    (if (:failure? ret)
      (do (println "GitHub release failed!")
          (System/exit 1))
      (println (:url ret)))))
