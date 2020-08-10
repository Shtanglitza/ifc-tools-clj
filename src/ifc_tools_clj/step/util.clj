(ns ifc-tools-clj.step.util
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [malli.core :as m]
            [clojure.core.match :refer [match]]
            [malli.util :as mu]))

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

(def ifc-instance?
  (m/validator
    [:map
     {:closed true}
     [:type-ref keyword?]
     [:params any?]]))

(defn proc-nested-instance-param
  [{:keys [:type-ref :params]}]
  (let [type-ref ((comp str/upper-case name) type-ref)]
    (str type-ref "(" params ")")))

(defn serialize-param
  [p]
  (cond
    (nil? p) nil
    (= "$" p) p
    (= "" p) "*"
    (number? p) p
    (starts-with-hash? p) p
    (some #{p} [:TRUE :FALSE]) (p (set/map-invert logical-lookup))
    (keyword? p) (str "." (name p) ".")
    (seq? p) (str "(" (str/join "," (map serialize-param p)) ")")
    (ifc-instance? p) (proc-nested-instance-param p)
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

;(defn schema-elements*
;  [registry schema]
;   (match schema
;          ([:map [name [_ {:description description} & params]]] :seq)
;          [name description params]
;
;          ([_ {:description description} & _] :seq)
;    [name
;     (first
;       (schema-descriptions*
;         (m/form schema {:registry registry})))
;     params]))


(defn schema-elements
  [registry schema-name]
  (let [[name schema]
        (if (keyword? schema-name)
          [schema-name (schema-name registry)]
          (get-schema registry schema-name))
        ;schema (schema-name registry)
        [name desc params]
        (match schema
               ([:map [name [_ {:description description} & params]]] :seq)
               [name description params]
               ([_ {:description description} & _] :seq)
               [name description nil]
               :else [name {:description :simple-type} nil])]
    {:schema/name name
     :schema/desc desc
     :schema/param-elements params}))




(defn entity-property
  [registry ifc-entity-kw]
  (-> (m/schema (ifc-entity-kw registry) {:registry registry})
      (mu/get-in [ifc-entity-kw])
      (m/properties)))

(defn type-property
  [registry ifc-type-kw]
  (-> (m/schema (ifc-type-kw registry) {:registry registry})
      (m/properties)))

(defn entity-or-type-property
  [registry entity-or-type-kw]
  ;(cond
  ;  (ifc-entity? entity-or-type-kw) (entity-property registry entity-or-type-kw)
  ;  (ifc-type? entity-or-type-kw)  (type-property registry entity-or-type-kw)))

  (if-let [prop (type-property registry entity-or-type-kw)]
    prop
    (entity-property registry entity-or-type-kw)))

;(defn e-or-p-desc
;  [registry entity-or-type-kw]
;  (:description
;    (entity-or-type-property
;      registry entity-or-type-kw)))

;(defn schema-elements
;  [registry entity-or-type]
;  (let [[name schema] (if (keyword? entity-or-type)
;                        [entity-or-type (entity-or-type registry)]
;                        (get-schema registry entity-or-type))
;        _ (println name schema)
;        desc  (e-or-p-desc registry name)
;        _ (println desc)
;        params (if (= desc :entity-declaration)
;                 (last (schema-elements* registry schema))
;                 nil)
;        ;[name desc params] (if (= (first schema) :map)
;        ;                     (schema-elements* registry schema)
;        ;                     [name (second schema) schema])
;        ]
;    {:schema/name name
;     :schema/desc desc
;     :schema/param-elements params}))


(def fkey (comp first keys))
(def fval (comp first vals))

(defn hash-id [id] (str "#" id))

(defn is-ifc-entity?
  "Check if map correspond to
  IFC entity definition"
  [registry m]
  (= (first
       (schema-descriptions*
         registry (fkey m)))
     :entity-declaration))

;(defn simple-type?
;  [registry e]
;  (= :simple-type
;    (first
;       (schema-descriptions*
;         registry e))))

(defn is-ifc-simple-type?
  [registry m]
  (= :simple-type
     (first
       (schema-descriptions*
         registry (fkey m)))))

(defn entity-or-type?
  [registry m]
  ((complement nil?)
   ((fkey m) registry)))


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


;Schema utils
(defn get-param-property
  [registry entity param-name]
  (-> (m/schema (entity registry) {:registry registry})
      (mu/get-in [entity param-name])
      (m/properties)))

;(defn get-param-description*
;  [registry entity param-name]
;  (:description
;    (get-param-property registry entity param-name)))

(defn schema-description
  [registry entity-or-type]
  (:schema/desc
    (schema-elements registry entity-or-type)))


(defn get-param-description
  [registry entity param-name]
  (->> (schema-elements registry entity)
       :schema/param-elements
       (map (juxt first last))
       (into {})
       param-name
       ((partial schema-description registry))))