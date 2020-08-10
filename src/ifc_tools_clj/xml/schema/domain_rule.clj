(ns ifc-tools-clj.step.schema.domain-rule
  (:require [clojure.string :as str]
            [ifc-tools-clj.util :refer [abs]]
            [clojure.core.match :refer [match]]
            [malli.core :as m]))

(def comparators
  {"<" :<
   ">" :>
   "<=" :<=
   ">=" :>=
   "<>" :not=
   "=" :=
   ;":<>:" ":=:"
   })


(defn binary-exp-spec
  [op exp-1 exp-2]
  (concat '(fn [arg])
          `((~op ~exp-1 ~exp-2))))

(defn binary-exp-schema
  [op exp-1 exp-2]
  [:fn `(~op ~exp-1 ~exp-2)])

;(defn abs "(abs n) is the absolute value of n" [n]
;  (cond
;    (not (number? n)) (throw (IllegalArgumentException.
;                               "abs requires a number"))
;    (neg? n) (- n)
;    :else n))


(def std-func
  {"ABS" 'abs
   "SIZEOF" 'count
   })

(defn is-comp-op?
  [op]
  (some
    #{"<" ">" "<=" ">=" "<>" "=" ":<>:" ":=:"} [op]))

(def simple-factor-idx
  (comp
    read-string
    second
    #(re-find #"SELF\[(\d)\]" %)))

(defn transform-domain-rule-exp
  [exp]
  ;(println "domain rule - "  exp)
  (match [exp]
         [([:domainRule _ _ logical-expr] :seq)] (transform-domain-rule-exp logical-expr)
         [([:labelDef label] :seq)] label
         [([:stdConst "SELF"] :seq)] 'arg
         [([:literal (l :guard string?)] :seq)] (read-string l)
         [([:literal ([:aggregateLiteral & more] :seq)] :seq)] (map transform-domain-rule-exp
                                                                    (filter seq? more))
         [([:simpleFactor s] :seq)] (transform-domain-rule-exp s)
         [([:simpleFactor _ expr _] :seq)] (transform-domain-rule-exp expr)
         [([:simpleExpr term] :seq)] (transform-domain-rule-exp term)
         [([:simpleExpr expr op & terms-and-ops] :seq)]
         (concat '(fn [arg])
                 `((~(transform-domain-rule-exp op)
                     ~(transform-domain-rule-exp expr)
                     ~(mapcat transform-domain-rule-exp terms-and-ops))))
         [([:funcRef f params] :seq)] (cons (transform-domain-rule-exp f)
                                            (transform-domain-rule-exp params))
         [([:actualParams & more] :seq)] (remove nil? (map transform-domain-rule-exp more))
         [([:expression simple-exp] :seq)] (transform-domain-rule-exp simple-exp)
         [([:expression exp-1
            (op :guard is-comp-op?)
            exp-2] :seq)]
         ;(let [;exp-1 (transform-domain-rule-exp exp-1)
               ;exp-2 (transform-domain-rule-exp exp-2)]
           ;[(keyword op) [exp-1 exp-2]])
         (apply
           binary-exp-spec
           (map transform-domain-rule-exp [op exp-1 exp-2]))
         ;(concat '(fn [arg])
         ;                         `((~(transform-domain-rule-exp op)
         ;                             ~(transform-domain-rule-exp exp-1)
         ;                             ~(transform-domain-rule-exp exp-2))))
         [([:expression exp-1
            (op :guard #(some #{"IN" "LIKE"} [%])) exp-2] :seq)] (set (transform-domain-rule-exp
                                                                        exp-2))
         [([:stdFunc f] :seq)] (get std-func f)
         [([:interval _ exp-1 comp-op-1 exp-2 comp-op-2 exp-3 _] :seq)]
         (concat '(fn [arg])
                 `((and (~(transform-domain-rule-exp comp-op-1)
                          ~(transform-domain-rule-exp exp-1)
                          ~(transform-domain-rule-exp exp-2))
                        (~(transform-domain-rule-exp comp-op-2)
                          ~(transform-domain-rule-exp exp-2)
                          ~(transform-domain-rule-exp exp-3)))))
         [(s :guard [string? #(str/includes? % "SELF[")])] (let [i (- (simple-factor-idx s) 1)]
                                                             `(nth ~'arg ~i))
         [(op :guard [is-comp-op?])] (symbol op)
         ["OR"] 'or
         [([:element e] :seq)] (str/replace e #"\'" "")
         [(:or "OF" ";" "(" ")")] nil
         :else (transform-domain-rule-exp (second exp))))