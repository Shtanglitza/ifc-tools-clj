(ns ifc-tools-clj.xml.util
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [malli.core :as m]
            [clojure.core.match :refer [match]]))

(defn clean-str
  [s]
  (str/replace s #"\'|^.|.$" ""))

(defn is-number?
  [s]
  (re-find
    #"^[+-]?(\d+([.]\d*)?([e|E][+-]?\d+)?|[.]\d+(e[+-]?\d+)?)$"
    s))

(defn is-logical?
  [s]
  (some #{s} #{".T." ".F."}))

(defn is-enum-value?
  [s]
  ((and #(str/starts-with? % ".")
        #(str/ends-with? % "."))
   s))

(defn starts-with-hash?
  [s]
  (str/starts-with? s "#"))

(defn every-starts-with-hash?
  [coll]
  (every? starts-with-hash? coll))

(def logical-lookup {".T." :TRUE ".F." :FALSE})

(defn deserialize-param
  [p]
  (cond
    (= "$" p) p
    (is-number? p) (read-string p)
    (is-logical? p) (get logical-lookup p)
    (is-enum-value? p) (keyword (clean-str p))
    (starts-with-hash? p) ((comp read-string #(str/replace % #"#" "")) p)
    :else (clean-str p)))

(defn serialize-param
  [p]
  (cond
    (= "$" p) p
    (= "" p) "*"
    (number? p) p
    (starts-with-hash? p) p
    (some #{p} [:TRUE :FALSE]) (p (set/map-invert logical-lookup))
    (keyword? p) (str "." (name p) ".")
    (seq? p) (str "(" (str/join "," (map serialize-param p)) ")")
    :else (str "'" p "'")))

(defn get-schema
  "Get entity or type name with schema for given name string"
  [registry type-ref]
  (let [p (re-pattern (str "(?i)\\b" type-ref "\\b"))
        name-key-matched? (comp
                            (every-pred
                              keyword?
                              #(re-find p (name %)))
                            key)]
    (->> registry
         (filter name-key-matched?)
         first)))

(defn schema-descriptions*
  "Returns all descriptions from parent schema and schema children"
  ([registry entity-or-type-kw]
  (let [schema-form (m/form
                      entity-or-type-kw {:registry registry})]
    (schema-descriptions* schema-form)))
  ([schema-form]
   (->> (tree-seq vector? rest schema-form)
        (filter map?)
        (map :description))))

;(defn schema-descriptions
;  [registry entity-or-type]
;  (let [[entity-or-type-kw _] (get-schema registry entity-or-type)]
;    (schema-descriptions* registry entity-or-type-kw)))

(defn schema-elements*
  [registry schema]
   (match schema
    ([_ [name [_ {:description description} & params]]] :seq)
       [name description params]
    ([_ [name params]] :seq)
    [name
     (first
       (schema-descriptions*
         (m/form schema {:registry registry})))
     params]))

(defn schema-elements
  [registry entity-or-type]
  (let [[_ schema] (if (keyword? entity-or-type)
                        [entity-or-type (entity-or-type registry)]
                        (get-schema registry entity-or-type))
        [name desc params] (schema-elements* registry schema)]
    {:schema/name name
     :schema/desc desc
     :schema/param-elements params}))



(defn ffilter
  "Return first one which satisfies predicate function"
  [f coll]
  (first (filter f coll)))

(def kw-type->str-type
  (comp
    str/upper-case
    name))

(defn abs "(abs n) is the absolute value of n" [n]
  (cond
    (not (number? n)) (throw (IllegalArgumentException.
                               "abs requires a number"))
    (neg? n) (- n)
    :else n))


;Scratch