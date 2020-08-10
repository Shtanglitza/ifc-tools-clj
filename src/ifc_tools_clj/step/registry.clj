(ns ifc-tools-clj.step.registry
  (:require [malli.core :as m]
            [clojure.java.io :as io]
            [ifc-tools-clj.step.util :as util]
            [edamame.core :as edamame]))


(defn read-registry-edn
  [form]
  (edamame/parse-string
    form {:dispatch {\# {\" #(re-pattern %)}}}))

(defn registry-from-edn
  [edn]
  (let [xf (map (fn [[k v]] [k (eval v)]))]
    (->> (slurp (io/resource edn))
         read-registry-edn
         (into {} xf))))

(def types (registry-from-edn "types.edn"))

(def entities (registry-from-edn "entities.edn"))


(def registry
  (merge
    (m/predicate-schemas)
    (m/class-schemas)
    (m/comparator-schemas)
    (m/base-schemas)
    ;m/default-registry
    types
    entities))

