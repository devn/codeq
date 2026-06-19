;;   Copyright (c) Metadata Partners, LLC and Contributors. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns datomic.codeq.query
  (:require [datomic.api :as d]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(defn find-defs
  "Sorted vector of distinct fully-qualified def names whose :code/name contains
   `pattern` (case-insensitive). Only names that are actually defined (have a
   :clj/def codeq) are returned, so the result is callable by `fn-history`."
  [db pattern]
  (let [needle (string/lower-case pattern)]
    (->> (d/q '[:find ?name
                :in $ ?needle
                :where
                [?cq :clj/def ?nm]
                [?nm :code/name ?name]
                [(clojure.string/lower-case ?name) ?lname]
                [(clojure.string/includes? ?lname ?needle)]]
              db needle)
         (map first)
         distinct
         sort
         vec)))

(defn fn-history
  "Evolution of a fully-qualified def name as version snapshots, oldest -> newest
   in topological (import) order, one map per distinct :code/sha. The introducing
   commit/tx for a version is the earliest among the codeqs carrying that
   :code/sha. Unknown name -> []."
  [db qualified-name]
  (->> (d/q '[:find ?sha ?text ?csha ?inst ?email ?msg ?defop ?tx
              :in $ ?name
              :where
              [?nm :code/name ?name]
              [?cq :clj/def ?nm]
              [?cq :codeq/code ?code]
              [?code :code/sha ?sha]
              [?code :code/text ?text]
              [?cq :codeq/file ?blob]
              [?blob :git/sha _ ?tx]
              [?tx :tx/commit ?commit]
              [?commit :git/sha ?csha]
              [?commit :commit/authoredAt ?inst]
              [?commit :commit/author ?auth]
              [?auth :email/address ?email]
              [(get-else $ ?commit :commit/message "") ?msg]
              [?cq :clj/defop ?defop]]
            db qualified-name)
       (group-by first)                              ;; group rows by ?sha
       (map (fn [[sha rows]]
              (let [e (apply min-key #(nth % 7) rows)] ;; earliest ?tx for this version
                {:sha sha
                 :code (nth e 1)
                 :commit (nth e 2)
                 :date (nth e 3)
                 :author (nth e 4)
                 :message (nth e 5)
                 :defop (nth e 6)
                 ::tx (nth e 7)})))
       (sort-by ::tx)
       (mapv #(dissoc % ::tx))))
