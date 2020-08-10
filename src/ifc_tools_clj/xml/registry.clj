(ns ifc-tools-clj.xml.registry
  (:require [malli.core :as m]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [edamame.core :as edamame]))

(defn read-registry-edn
  [form]
  (edamame/parse-string
    form {:dispatch {\# {\" #(re-pattern %)}}}))


(def types
  (->> (slurp (.getPath (io/resource "types.edn")))
       read-registry-edn))

(def entities
  (->> (slurp (.getPath (io/resource "entities.edn")))
       read-registry-edn))

(def registry
  (merge
    m/predicate-registry
    m/class-registry
    m/comparator-registry
    m/base-registry
    types
    entities))

