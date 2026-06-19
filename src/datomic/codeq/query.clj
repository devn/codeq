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
