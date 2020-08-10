(ns ifc-tools-clj.step.schema.entity
  (:require [ifc-tools-clj.spec.type :as stype]
            [ifc-tools-clj.antlr :as parser]
            [clojure.core.match :refer [match]]))



(defn transform-entity-exp
  [exp]
  (match exp
         ([:entityDecl entity-head entity-body _ _] :seq)
         (merge
           (transform-entity-exp entity-head)
           {:attrs (transform-entity-exp entity-body)})

         ([:entityHead _ entity-def sub-super _] :seq)
         (merge
           {:entity (transform-entity-exp entity-def)}
           (transform-entity-exp sub-super))

         ([:subSuper & type-decls] :seq)
         (apply merge (map transform-entity-exp type-decls))

         ([:supertypeDecl _ _ _ _ expr _] :seq)
         {:supertype-of (transform-entity-exp expr)}

         ([:subtypeDecl _ _ _ expr _] :seq)
         {:subtype-of (transform-entity-exp expr)}

         ([:choice & supertype-exprs] :seq)
         (remove nil? (map transform-entity-exp supertype-exprs))

         ([:attributes & clauses] :seq)
         (remove nil? (mapv transform-entity-exp clauses))

         ([(:or :entityDef :entityRef) entity] :seq) (keyword entity)

         ([:explDef & more] :seq)
         (apply merge (remove nil? (map transform-entity-exp more)))

         ([:attrDef attr-def] :seq) {:attr-name (keyword attr-def)}

         ([:collectionTypeSel coll] :seq)
         {:schema (stype/transform-type-exp coll)}

         ([:deriveClause _ & derived-attrs] :seq) nil ;{:derived-attrs (map emit-entity-decl-schema derived-attrs)}

         ([:inverseClause _ & inverse-attrs] :seq) nil  ;{:inverse-attrs (map emit-entity-decl-schema inverse-attrs)}

         (:or "ENTITY" "UNIQUE" "ABSTRACT" "SUPERTYPE" "SUBTYPE"
            "ONEOF" "OF" "," "(" ")" ";" ":") nil

         "OPTIONAL" {:optional? "$"}

         :else (transform-entity-exp (second exp))))


(defn entity-schema
  [attrs]
  (into
    [:map {:description :entity-declaration}]
    (map
      (fn [{:keys [:attr-name :optional? :schema]}]
        (if optional?
          [attr-name {:optional true} schema]
          [attr-name schema]))
      attrs)))

;TODO use mu/union for schemas
(defn collect-entity-schemas
  [m entity]
  (when-let [entity-data (get m entity)]
    (loop [[{:keys [:subtype-of]}] entity-data
           inherited [entity]]
      (if subtype-of
        (recur (get m subtype-of)
               (into inherited [subtype-of]))
        (entity-schema
          (mapcat
            #(get-in m [% 0 :attrs])
            (reverse inherited)))))))

(defn emit-entity-schemas
  [entity-map]
  (map
    (juxt
      key
      #(vector
         :map [(key %) (collect-entity-schemas entity-map (key %))]))
    entity-map))

(defn populate-edn
  []
  (->> (parser/parse-exp "resources/IFC4x2.exp")
       (tree-seq seq? rest)
       (filter #(= (first %) :entityDecl))
       (map transform-entity-exp)
       (group-by :entity)
       (emit-entity-schemas)
       (into {})
       prn-str
       (spit "resources/entities.edn")))


(comment

  )