(ns ifc-tools-clj.step.schema.type
  (:require [ifc-tools-clj.step.antlr :as parser]
            [clojure.string :as str]
            [malli.core :as m]
            [clojure.core.match :refer [match]]
            ;[clojure.math.numeric-tower :as math]
            ;[ifc-tools-clj.spec.domain-rule :as dr]
            ;[ifc-tools-clj.util :as util]
            ))

;(def hex-digit
;  (set "0123456789abcdef"))

;(defn hex-digit?
;  [x]
;  (contains? hex-digit x))



;(defn hex-str?
;  [s]
;  ;TODO make correct function for hex-str?
;  [:re #"[a-fA-F0-9]+"])


(def primitive-types
  {:REAL               'double?
   :INTEGER            'integer?
   :STRING             'string?
   :BOOLEAN            'boolean?
   :BINARY             [:re '#"[a-fA-F0-9]+"]
   :LOGICAL            [:enum :TRUE :FALSE :UNKNOWN]
   :NUMBER             'number?
   :IfcPositiveInteger 'pos-int?})

(defn string-width
  ([_ [_[_[_[_[_[_[_ literal]]]]]]] _ ]
   (when literal
     `#(<= (count %) ~(read-string literal))))
  ([_ [_[_[_[_[_[_[_ literal]]]]]]] _ _]
   (when literal
     `#(= (count %) ~(read-string literal)))))

;(defn simple-type
;  ([st-name]
;   (keyword (str *ns*) st-name))
;  ([st-name width]
;   (if (empty? width)
;     (simple-type st-name)
;     `(s/and ~(keyword (str *ns*) st-name)
;             ~(apply string-width width)))))

(defn simple-type-schema
  ([st]
   [:and {:description :simple-type}
    ((keyword st) primitive-types)])
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

;(defn select-type [select-values]
;  (let [select-values (interleave
;                        (map (comp keyword name) select-values)
;                        select-values)]
;    `(s/or ~@select-values)))

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

(defn add-domain-rules
  [type-sel domain-rules]
  (if domain-rules
    [:and {:description :with-domain-rules} type-sel domain-rules]
    type-sel))

(defn transform-type-exp
  [exp]
  (match exp
         ([:typeDecl _ type-body _ _] :seq) (transform-type-exp type-body)

         ([:typeBody type-def _ type-sel _ & domain-rules] :seq)
         (let [type-def (transform-type-exp type-def)
               type-sel (transform-type-exp type-sel)]
           (if (not-empty domain-rules)
             [type-def
              [:map
               [type-def
                (add-domain-rules
                  type-sel (first (map transform-type-exp domain-rules)))]]]
             [type-def
              [:map [type-def type-sel]]]))

         ([:entityRef entity-ref] :seq) (keyword entity-ref)

         ([(:or :listType :arrayType :setType) ct & more] :seq)
         (collection-type-schema ct (remove nil? (map transform-type-exp more)))

         ([:enumType _ _ _ enum-values _] :seq)  (enum-type-schema enum-values)

         ([:selectType _ _ ([_ & select-values] :seq) _ ] :seq)
         (select-type-schema (remove nil? (map transform-type-exp select-values)))

         ([:simpleType ([_ st & width] :seq)] :seq)
         (simple-type-schema st width)

         ([:boundSpec _ bound-1 _ bound-2 _] :seq)
         {:min (transform-type-exp bound-1) :max (transform-type-exp bound-2)}

         ([:domainRules _ & rules] :seq)
         nil
         ;TODO (remove nil? (map dr/transform-domain-rule-exp rules))

         ([:typeDef type-def] :seq) (keyword type-def)

         ([:literal l] :seq) (read-string l)

         (:or "OF" "," ";" "?") nil

         :else (transform-type-exp (second exp))))


(defn populate-edn
  []
  (->> (parser/parse-exp "resources/IFC4x2.exp")
       (tree-seq seq? rest)
       (filter #(= (first %) :typeDecl))
       (map transform-type-exp)
       (into {})
       prn-str
       (spit "resources/types.edn")))


(comment

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

  )