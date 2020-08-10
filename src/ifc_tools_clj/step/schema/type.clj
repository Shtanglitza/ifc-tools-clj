(ns ifc-tools-clj.step.schema.type
  (:require [ifc-tools-clj.step.antlr :as parser]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.util :as mu]
            [clojure.core.match :refer [match]]
            ;[clojure.math.numeric-tower :as math]
            [ifc-tools-clj.step.schema.domain-rule :as dr]
            [ifc-tools-clj.step.util :as util]
            [edamame.core :as edamame]))

(def primitive-types
  {:REAL               ['double? {:description :simple-type}]
   :INTEGER            ['integer? {:description :simple-type}]
   :STRING             ['string? {:description :simple-type}]
   :BOOLEAN            ['boolean? {:description :simple-type}]
   :BINARY             [:re {:description :simple-type}
                        '#"[a-fA-F0-9]+"]
   :LOGICAL            [:enum {:description :simple-type}
                        :TRUE :FALSE :UNKNOWN]
   :NUMBER             ['number? {:description :simple-type}]
   :IfcPositiveInteger ['pos-int? {:description :simple-type}]})

(def types-registry (atom primitive-types))

(defn string-width
  ([_ [_[_[_[_[_[_[_ literal]]]]]]] _ ]
   (when literal
     `#(<= (count %) ~(read-string literal))))
  ([_ [_[_[_[_[_[_[_ literal]]]]]]] _ _]
   (when literal
     `#(= (count %) ~(read-string literal)))))


(defn simple-type-schema
  ([st]
   ((keyword st) primitive-types))
  ([st width]
   (if (empty? width)
     (simple-type-schema st)
     [:and {:description :simple-type}
      (simple-type-schema st)
      [:fn (apply string-width width)]])))

(defn enum-type-schema
  [[_ & enum-values]]
  (->> enum-values
       (filter seq?)
       (map second)
       (map keyword)
       (into [:enum {:description :enum-type}])))

(defn select-type-schema
  [select-values]
  (into [:or {:description :select-type}] select-values))

;(defn collection-type
;  ([ct more]
;   (let [{:keys [min max]} (first more)
;         is-distinct? (if (= ct "SET") true false)
;         into (when (= ct "SET") #{})
;         collection-type-sel (last more)]
;     (collection-type
;       collection-type-sel min max is-distinct? into)))
;  ([collection-type-sel min max is-distinct? into]
;   `(s/coll-of ~collection-type-sel
;              :min-count ~min
;              :max-count ~max
;              :distinct ~is-distinct?
;              :into ~into)))

(def collection-types
  {:SET :set
   :LIST :list
   :ARRAY :vector})

(defn collection-type-schema
  ([ct more]
   (let [{:keys [min max]} (first more)
         collection-type-sel (last more)]
     (collection-type-schema
       ((keyword ct) collection-types)
       collection-type-sel min max)))
  ([ct collection-type-sel min max]
   [ct {:description :collection-type
        :min min :max max} collection-type-sel]))

;(defn add-domain-rules
;  [type-sel domain-rules]
;  (if (vector? type-sel)
;    (let [[_ {desc :description} schema-body] type-sel
;          type-sel (if (= desc :simple-type) schema-body type-sel)
;          _ (println desc type-sel)
          ;]
    ;[:and {:description desc
    ;       :meta :with-domain-rules} type-sel domain-rules])
    ;[:and {:meta :with-domain-rules} type-sel domain-rules]))

(defn add-domain-rules
  [type-sel domain-rules]
  ;(let [type-sel-schema (type-sel @types-registry)]
    [:and
     type-sel
     ;(mu/get-in type-sel-schema [type-sel])
     domain-rules])

(defn transform-type-exp
  [exp]
  (match exp
         ([:typeDecl _ type-body _ _] :seq) (transform-type-exp type-body)

         ([:typeBody type-def _ type-sel _ & domain-rules] :seq)
         (let [type-def (transform-type-exp type-def)
               type-sel (transform-type-exp type-sel)]
           (if (not-empty domain-rules)
               (swap! types-registry
                      assoc
                      type-def
                      (add-domain-rules
                        type-sel (first (map transform-type-exp domain-rules))))
             (swap! types-registry
                    assoc type-def
                    ;[:map
                    ; [type-def
                      type-sel ;]]
)))

         ([:entityRef entity-ref] :seq) (keyword entity-ref)

         ([(:or :listType :arrayType :setType) ct & more] :seq)
         (collection-type-schema ct (keep transform-type-exp more))

         ([:enumType _ _ _ enum-values _] :seq)  (enum-type-schema enum-values)

         ([:selectType _ _ ([_ & select-values] :seq) _ ] :seq)
         (select-type-schema (keep transform-type-exp select-values))

         ([:simpleType ([_ st & width] :seq)] :seq)
         (simple-type-schema st width)

         ([:boundSpec _ bound-1 _ bound-2 _] :seq)
         {:min (transform-type-exp bound-1) :max (transform-type-exp bound-2)}

         ([:domainRules _ & rules] :seq)
         (first
           (keep dr/transform-domain-rule-exp rules))

         ([:typeDef type-def] :seq) (keyword type-def)

         ([:literal l] :seq) (read-string l)

         (:or "OF" "," ";" "?") nil

         :else (transform-type-exp (second exp))))


;(defn populate-edn-old
;  []
;  (->> (parser/parse-exp "resources/IFC4x2.exp")
;       (tree-seq seq? rest)
;       (filter #(= (first %) :typeDecl))
;       (map transform-type-exp)
;       (into {})
;       prn-str
;       (spit "resources/types.edn")))

(defn generate-registry!
  []
  (->> (parser/parse-exp "resources/IFC4x2.exp")
       (tree-seq seq? rest)
       (filter #(= (first %) :typeDecl))
       (map transform-type-exp)))

(defn populate-edn
  [types-registry]
  (->> types-registry
       pr-str
       (spit "resources/types.edn")))

(defn fetch-schema
  [types-registry entity-or-type]
  (let [reg (merge malli.core/default-registry
                   @types-registry)]
    (m/schema
      (edamame/parse-string
        (pr-str (entity-or-type reg))
        {:dispatch {\# {\" #(re-pattern %)}}})
      {:registry reg})))

(comment

  ;TODO za sutra
  (let [joj-fn? (m/fn-schema
                  :joj-fn?
                  (fn [arg] (< (util/abs (clojure.core/nth arg 1)) 60)))
        registry
        (merge
          {joj-fn? joj-fn?}
          ifc-tools-clj.step.registry/registry)]
    (m/validate joj-fn? [0 0]
                {:registry ifc-tools-clj.step.registry/registry}
                ))

  (->> (parser/parse-exp "resources/IFC4x2.exp")
       (tree-seq seq? rest)
       (filter #(= (first %) :typeDecl))
       ;(pr-str)
       ;(edn/read-string)
       ;(map (fn [[k schema]]
       ;       (s/register k (s/resolve-schema schema))))
       ;first
       (map transform-type-exp)
       (into {})
       ;(malli.edn/write-string)
       pr-str
       (spit "resources/type-attrs.edn")
       ;(edn/write-string)
       ;::IfcPlaneAngleMeasure
       ;(mg/generate)
       )

  :IfcNormalisedRatioMeasure [:map
                              [:IfcNormalisedRatioMeasure
                               [:IfcRatioMeasure
                                {:domain-rules [:fn (fn [arg] (clojure.core/and (<= 0.0 arg) (<= arg 1.0)))]}]]],


  )