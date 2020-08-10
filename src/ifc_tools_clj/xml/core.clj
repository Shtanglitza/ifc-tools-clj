(ns ifc-tools-clj.xml.core
  (:require [clojure.data.xml :as xml]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as numeric-tower]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [ifc-tools-clj.xml.registry :refer [registry]]))

(defn string->seq-of-numbers
  "Returns list of numbers that were separated by space in string, decimal separator: , (comma)."
  [s]
  (map #(edn/read-string (str/replace % "," ".")) (str/split s #" ")))

(defn string->data-struct
  "Receives a string of data and transforms it to clojure data structures:
   exponential > number > list of numbers > integer > boolean > string."
  [s]
  (cond
    ; nil
    (nil? s) ""
    ; list of numbers
    (and (str/includes? s " ") (not (re-find #"[a-zA-DF-Z.<>?/!@#$%^&*()_]" s)))
    (string->seq-of-numbers s)
    ; number
    (not (re-find #"[a-zA-DF-Z.<>?/!@#$%^&*()_]" s))
    (edn/read-string (str/replace s "," "."))
    ; boolean
    (= (str/lower-case s) "true") true
    (= (str/lower-case s) "false") false
    ; string
    :else (str s)))

(defn attr->data-struct
  [attr-type attr-string]
  (map #(if-not (nil? %) {attr-type (string->data-struct %)} nil) (str/split attr-string #" ")))

(defn get-attribute-data-type
  "Returns data type of the attribute. Dont remove it yet."
  [attr-def]
  (first
    (remove nil? (map #(if (vector? %)
                         (first %)
                         nil)
                      attr-def))))

(defn get-attr-def
  "Returns definition of attribute inside entity. Tag is name of the attribute,
   parent-element-schema is entity-schema."
  [tag par-el-schema]
  (if (= (first (nth (nth par-el-schema 1) 1)) :or)
    (nth (nth par-el-schema 1) 1)
    (some #(when (seq %) %)
          (map #(when (= (if (vector? %)
                         (first %)
                         %) tag) %)
               (nth (nth par-el-schema 1) 1)))))

(defn attr-type
  "Returns type of entity that attribute should be."
  [attr-def]
  (cond
    (keyword? (last attr-def)) (last attr-def)
    (keyword? (last (last attr-def))) (last (last attr-def))
    (keyword? (last (last (last attr-def)))) (last (last (last attr-def)))
    :else nil))

(defn ent-or-attr-schema
  "Returns schema of entity or attribute of an entity."
  [el par-el-schema]
  (cond
    ; has type attribute
    (contains? (into #{} (keys (:attrs el))) :type)
    ((keyword (:type (:attrs el))) registry)
    ; tag is in registry
    (contains? registry (:tag el))
    ((:tag el) registry)
    ; attribute is mentioned in the parent-elements schema
    (get-attr-def (:tag el) par-el-schema)
    (cond
      ; 1st type
      (keyword? (last (get-attr-def (:tag el) par-el-schema)))
      ((last (get-attr-def (:tag el) par-el-schema)) registry)
      ; 2nd type
      (keyword? (last (last (get-attr-def (:tag el) par-el-schema))))
      ((last (last (get-attr-def (:tag el) par-el-schema))) registry)
      ; 3rd type
      (vector? (last (last (get-attr-def (:tag el) par-el-schema))))
      (get-attr-def (:tag el) par-el-schema))))

(defn find-els-with-id
  "Returns seq of all elements with id"
  [root-el]
  (filter #(:id (:attrs %)) (tree-seq map? :content root-el)))

(defn find-el-with-id
  "Returns element with given id"
  [id els-with-id]
  (first (filter #(when (= (:id (:attrs %)) id) %) els-with-id)))

(defn clj-data-struct-call
  "This functions will be overloaded later in code."
  [x y z k]
  (println "This line should never be printed " x y z k))

(defn clj-struct-part-of
  "Creates clojure structure if element tag is in registry and
   when parent element is defined as list, set or vector."
  [gr-par-el-schema par-el par-el-schema el els-with-id xml-clj-struct]
  (if (contains? registry (:tag el))
    (if (vector? (attr-type (get-attr-def (:tag par-el) gr-par-el-schema)))
       (clj-data-struct-call par-el-schema
                             el
                             (ent-or-attr-schema el par-el-schema)
                             false
                             els-with-id
                             xml-clj-struct)
      {(attr-type (get-attr-def (:tag par-el) gr-par-el-schema))
       (clj-data-struct-call par-el-schema
                             el
                             (ent-or-attr-schema el par-el-schema)
                             false
                             els-with-id
                             xml-clj-struct)})
    nil))

(defn clj-struct-from-attr
  "Creates clojure data structures from clojure.xml.Element's attribute object based on IFC 4x2 schema"
  [par-el-schema el]
  (let [last-attr-def-el (last (get-attr-def (key el) par-el-schema))
        attr-type-val (attr-type (get-attr-def (key el) par-el-schema))
        attr-val (string->data-struct (val el))]
    {(key el)
     (if (keyword? last-attr-def-el)
       {last-attr-def-el
        (if (= (first (nth (nth (last-attr-def-el registry) 1) 1)) :enum)
          (keyword (str/upper-case attr-val))
          attr-val)}
       (if (vector? last-attr-def-el)
         (condp = (first last-attr-def-el)
           :list (into '() (attr->data-struct attr-type-val (val el)))
           :set (into #{} (attr->data-struct attr-type-val (val el)))
           :vector (into [] (attr->data-struct attr-type-val (val el)))
           attr-val)
         attr-val))}))

;(def collection-types {:map hash-map :set hash-set :list list :vector vector :enum vector :and and :or or})

;(def element-types #(:select-type :simple-type :enum-type :entity-declaration :collection-type))

(defn clj-data-struct
  "Creates clojure data structures from clojure.xml.Element object based on IFC 4x2 schema"
  [par-el-schema el el-schema part-of els-with-id xml-clj-struct]
  (let [attrs (:attrs el)
        content (:content el)
        c&a (concat content attrs)]
    ;(println (:tag element) ";" element-schema)
    ;; sub-element as wrapper
    (if (first (keep identity (map #(if (str/ends-with? (name (or (:tag %) :k)) "-wrapper") (:tag %) nil) content)))
      (string->data-struct (first (:content (first c&a))))
      ;; element type is map
      (if-not part-of
        (into {}
              (map #(if (contains? registry (:tag %))
                      ;; sub-element in registry
                      {(:tag %)
                       (clj-data-struct-call el-schema % (ent-or-attr-schema % el-schema) false els-with-id xml-clj-struct)}
                      (let [attr-def (get-attr-def (:tag %) el-schema)]
                        ;; sub-element is defined as attribute in element schema
                        (if-not (nil? attr-def)
                          {(:tag %)
                           ;; sub-element as attribute and type is list, set, vector or map
                           (condp = (if (vector? (last attr-def))
                                      (first (last attr-def))
                                      (nth attr-def 1))
                             :list (clj-data-struct-call el-schema % (ent-or-attr-schema % el-schema) :list els-with-id xml-clj-struct)
                             :set (clj-data-struct-call el-schema % (ent-or-attr-schema % el-schema) :set els-with-id xml-clj-struct)
                             :vector (clj-data-struct-call el-schema % (ent-or-attr-schema % el-schema) :vector els-with-id xml-clj-struct)
                             {(attr-type attr-def)
                              (clj-data-struct-call el-schema % (ent-or-attr-schema % el-schema) false els-with-id xml-clj-struct)})}
                          ;; sub-element as attribute and simple type
                          (if (instance? clojure.lang.MapEntry %)
                            (clj-struct-from-attr el-schema %)
                            ;; no conditions met, create element as map and continue
                            {(:tag %)
                             (clj-data-struct-call el-schema % nil false els-with-id xml-clj-struct)}))))
                   c&a))
        ;; element type is list, set or vector
        (condp = part-of
          :list (into '() (map #(clj-struct-part-of par-el-schema el el-schema % els-with-id xml-clj-struct) c&a))
          :set (into #{} (map #(clj-struct-part-of par-el-schema el el-schema % els-with-id xml-clj-struct) c&a))
          :vector (into [] (map #(clj-struct-part-of par-el-schema el el-schema % els-with-id xml-clj-struct) c&a)))))))

(defn clj-data-struct-call
  "Checks for id of an element and calls clj-data-struct."
  [par-el-schema el el-schema part-of els-with-id xml-clj-struct]
  ;(println (:tag element) ";" element-schema)
  (if (and (:href (:attrs el)) (= (:nil (:attrs el)) "true"))
    (let [el (find-el-with-id (:href (:attrs el)) els-with-id)]
      (clj-data-struct par-el-schema el el-schema part-of els-with-id xml-clj-struct))
    (clj-data-struct par-el-schema el el-schema part-of els-with-id xml-clj-struct)))

(def xml-data (slurp "test/ifc_tools_clj/samples/test3.ifcxml"))

(def xml-data-2 (str/replace xml-data "xsi:" ""))

(def xml-clj-struct (xml/parse-str xml-data-2))

(def els-with-id (find-els-with-id xml-clj-struct))

(def x {(:tag (nth (:content xml-clj-struct) 1))
        (clj-data-struct-call nil
                              (nth (:content xml-clj-struct) 1)
                              ((:tag (nth (:content xml-clj-struct) 1)) registry)
                              false
                              (find-els-with-id (nth (:content xml-clj-struct) 1))
                              xml-clj-struct)})

(pprint/pprint x)

;(pprint/pprint els-with-id)

;(pprint/pprint els-with-id)

;(pprint/pprint (find-el-with-id "i3" els-with-id xml-clj-struct))

(comment
  (require '[malli.core :as m])
  (require '[malli.generator :as mg])
  (require '[malli.error :as me])

  ;; Validate
  (m/validate
    (m/schema
      (:IfcProject registry)
      {:registry registry})
    x)

  ;; Validate with explained errors
  (-> (m/schema
        (:IfcProject registry)
        {:registry registry})
      (m/explain
        x)
      (me/humanize))

  ;; Generate example
  (mg/generate :IfcProject {:registry registry})
  )