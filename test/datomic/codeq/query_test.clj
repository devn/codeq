(ns datomic.codeq.query-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [datomic.api :as d]
            [datomic.codeq.core :as c]
            [datomic.codeq.git :as git]
            [datomic.codeq.query :as q])
  (:import [org.eclipse.jgit.api Git]
           [java.io File]))

(defn- temp-dir ^File []
  (let [d (File/createTempFile "codeq-query-test" "")]
    (.delete d) (.mkdirs d) d))

(defn- commit-all! [^Git git ^String msg]
  (.. git add (addFilepattern ".") (call))
  (.. git commit (setMessage msg)
      (setAuthor "Ada Lovelace" "ada@example.com")
      (setCommitter "Ada Lovelace" "ada@example.com") (call)))

(defn build-repo
  "Temp git repo: c1 defines greet (v1) and stable; c2 changes greet (v2),
   stable unchanged; c3 touches only a README (greet/stable blobs unchanged).
   Returns the repo dir."
  ^File []
  (let [dir (temp-dir)
        git (.. (Git/init) (setDirectory dir) (call))]
    (spit (io/file dir "core.clj")
          "(ns demo.core)\n(defn greet [] \"v1\")\n(defn stable [] :x)\n")
    (commit-all! git "init")
    (spit (io/file dir "core.clj")
          "(ns demo.core)\n(defn greet [] \"v2\")\n(defn stable [] :x)\n")
    (commit-all! git "change greet")
    (spit (io/file dir "README.md") "docs\n")
    (commit-all! git "docs")
    (.close git)
    dir))

(defn analyzed-db
  "Import + analyze the repo dir into a fresh mem db; return the db value."
  [^File dir]
  (let [conn  (c/ensure-db (str "datomic:mem://" (gensym "codeq")))
        grepo (git/open-repo (str dir))
        cms   (c/unimported-commits grepo (d/db conn) nil)]
    (c/import-git grepo conn "git@example.com:me/demo.git" "demo" cms)
    (c/run-analyzers grepo conn)
    (d/db conn)))

(deftest find-defs-matches-qualified-names
  (let [db (analyzed-db (build-repo))]
    (testing "substring resolves to def names (not the ns name)"
      (is (= ["demo.core/greet"] (q/find-defs db "greet")))
      (is (= ["demo.core/greet" "demo.core/stable"] (q/find-defs db "demo"))))
    (testing "no match -> empty"
      (is (= [] (q/find-defs db "nonexistent"))))))

(deftest fn-history-orders-versions-topologically
  (let [db   (analyzed-db (build-repo))
        hist (q/fn-history db "demo.core/greet")]
    (testing "one entry per distinct code version, oldest first"
      (is (= 2 (count hist)))
      (is (= ["init" "change greet"] (mapv :message hist)))
      (is (re-find #"v1" (:code (first hist))))
      (is (re-find #"v2" (:code (second hist)))))
    (testing "version maps carry the expected keys"
      (is (= #{:sha :code :commit :date :author :message :defop}
             (set (keys (first hist)))))
      (is (= "defn" (:defop (first hist))))
      (is (= "ada@example.com" (:author (first hist)))))))

(deftest fn-history-collapses-unchanged-versions
  (let [db (analyzed-db (build-repo))]
    (testing "a def unchanged across commits is a single version"
      (let [hist (q/fn-history db "demo.core/stable")]
        (is (= 1 (count hist)))
        (is (= "init" (:message (first hist))))))
    (testing "unknown name -> empty"
      (is (= [] (q/fn-history db "demo.core/nope"))))))
