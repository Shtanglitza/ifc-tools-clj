(ns ifc-tools-clj.step.core
  (:require [ifc-tools-clj.step.antlr :as parser]
            [ifc-tools-clj.step.util :as util]
            [ifc-tools-clj.step.registry :refer [entities types registry]]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [malli.core :as m])
  (:import (java.time.temporal ChronoUnit)
           (java.time LocalDateTime)))

(defn header
  [{:keys [name time-stamp author-or-collection
           organization-or-collection preprocessor-version
           originating-system authorisation]}]
  (str "ISO-10303-21;
  HEADER;
  FILE_DESCRIPTION((''),'2;1');
  FILE_NAME('" name "','"
       (or time-stamp
           (.truncatedTo
             (LocalDateTime/now)
             ChronoUnit/SECONDS)) "',('"
       author-or-collection "'),('"
       organization-or-collection "'),'"
       (or preprocessor-version "Ifc-tools by Shtanglitza") "','"
       originating-system "','"
       authorisation "');
  FILE_SCHEMA(('IFC4x2'));
  ENDSEC;

  DATA;"))

;TODO add parsing header in future
(defn transform-step-exp
  [exp]
  (match exp
         ([:file _ header data _] :seq)
         [;(transform-step-exp header)
          (transform-step-exp data)]

         ([:header _ _ file-description file-name file-schema _ _] :seq)
         nil ;[file-description file-name file-schema]

         ([:fileDescription _ _ description _ implementation _ _] :seq)
         nil ;[description implementation]

         ([:data _ _ & instances] :seq) (map transform-step-exp instances)

         ([:instance id _ constructor _] :seq)
         (assoc (transform-step-exp constructor) :id id)

         ([:constructor type-ref _ & params] :seq)
         (let [{:keys [:schema/name :schema/desc :schema/param-elements]}
                (util/schema-elements registry type-ref)
               params (keep transform-step-exp params)
               params (condp = desc
                        :entity-declaration (zipmap (map first param-elements) params)
                        :simple-type  (first params)
                        :collection-type (first params)
                        params)]
           {:type-ref name
            :params params})

         ([:collection _ & coll-values] :seq)
         (keep transform-step-exp coll-values)

         (id :guard util/starts-with-hash?)  id

         ([_ (value :guard [string?
                             (complement util/starts-with-hash?)])] :seq)
         (util/deserialize-param value)

         (:or ";" "," "(" ")" "ENDSEC") nil

         :else (transform-step-exp (second exp))))


(defn ifc-flat
  [ast]
  (->> ast
       transform-step-exp
       first
       (remove nil?)))

(defn find-roots
  [parsed]
  (let [ids (map :id parsed)
        id-refs (->> (mapcat (comp  :params) parsed)
                     (map second)
                     (mapcat  #(if (sequential? %) % [%]))
                     (filter util/starts-with-hash?)
                     )]
    (filter #(not-any? #{%} id-refs) ids)))


(defn process-params
  ([flat root-id]
   (let [{:keys [:id :type-ref :params]}
          (first (get flat root-id))]
     {type-ref
      (into {}
            (mapv
              (fn [[param-key param-value]]
                (process-params
                  flat
                  (util/get-param-description
                    registry type-ref param-key)
                  param-key param-value))
              params))}))
  ([flat param-desc k v]
   (match [param-desc k  v]
          [_ k (v :guard [util/starts-with-hash?])]
          [k (process-params flat v)]

          [_ k (v :guard [seq? util/every-starts-with-hash?])]
          [k (mapv (partial process-params flat) v)]

          ;[_ k (v :guard [seq? #(every? map? %)])]
          ;  [k (mapv
          ;     (fn [{:keys [:type-ref :params]}]
          ;       (process-params flat param-desc type-ref params))
          ;     v)])

          [_ k (v :guard [map?])]
          [k (apply (partial process-params flat param-desc)
                                              (vals v))]

          [_ _ "$"] nil

          :else {k v})))


(defn to-nested
  [entity-instances]
  (let [root-ids (find-roots entity-instances)]
    (map
      (partial
        process-params
        (group-by :id entity-instances))
      root-ids)))


;(defn is-child-ifc-entity?
;  [parent-entity param-name]
;  (= (util/get-param-description
;       registry parent-entity param-name)
;     :entity-declaration))

;(defn is-child-ifc-select-type?
;  [parent-entity param-name]
;  (= (util/get-param-description
;       registry parent-entity param-name)
;     :select-type))

(defn is-ifc-simple-type?
  [registry type]
  (= (util/schema-description registry type)
     :simple-type))


(defn- ifc-flatten
  [node-id counter m]
  (let [[type-ref parameters] ((juxt util/fkey util/fval) m)
        {:keys [:schema/name :schema/desc
                :schema/param-elements]}
         (util/schema-elements registry type-ref)
        proc-params (fn [[param-name _]]
                      [param-name (get parameters param-name "$")])
        params (into {} (map proc-params) param-elements)
        {:keys [:params :children]} (reduce-kv
                                      (fn [m k v]
                                        (cond
                                          (and (map? v)
                                               (is-ifc-simple-type? registry (util/fkey v)))
                                          (assoc-in m [:params k]
                                                    (zipmap [:type-ref :params]
                                                            (apply vec v)))
                                          (map? v)
                                          (-> m
                                              (assoc-in [:children (swap! counter inc)] v)
                                              (assoc-in [:params k] (util/hash-id @counter)))

                                          (and (vector? v)
                                               (every?
                                                 #(util/schema-description registry (util/fkey %))
                                                 v))
                                          (let [ids (repeatedly (count v) #(swap! counter inc))
                                                children  (into
                                                            (:children m)
                                                            (zipmap ids v))]
                                            (-> m
                                                (assoc-in [:children] children)
                                                (assoc-in [:params k] (map #(util/hash-id %) ids))))
                                          :else (assoc-in m [:params k] v)))
                                      {:params {}
                                       :children {}} params)
        node [{:id (util/hash-id node-id)
               :type-ref type-ref
               :params params}]]
    (if (not-empty children)
      (into node
            (mapcat
              #(ifc-flatten (first %) counter (second %))
              children))
      node)))

(def ifc-instance?
  (m/validator
    [:map
     {:closed true}
     [:type-ref keyword?]
     [:params any?]]))

(defn- serialize-ifc-row
  [{:keys [:id :type-ref :params]}]
  (let [param-elements (:schema/param-elements
                        (util/schema-elements
                          registry type-ref))
        type-ref ((comp str/upper-case name) type-ref)
        proc-param (comp
                     util/serialize-param
                     #(get params %)
                     first)
        params (str/join ","
                         (keep proc-param param-elements))]
    (str id " = " type-ref "(" params ");")))


;API


(defn ifc-deserialize
  ([s]
   (ifc-deserialize s :nested))
  ([s structure?]
   (let [parsed (parser/step-parser s)
         entity-instances (ifc-flat parsed)]
    (case structure?
      :flat entity-instances
      :nested (to-nested entity-instances)))))

(defn ifc-nested-to-flat-map
  [ifc-nested-map]
  (let [start-ids (range 1 (inc (count ifc-nested-map)))
        counter (atom (count ifc-nested-map))]
    (mapcat
      #(ifc-flatten %1 counter %2)
      start-ids ifc-nested-map)))

(defn ifc-serialize*
  [ifc-flat-map]
  (->> (map serialize-ifc-row ifc-flat-map)
       (cons (header {}))
       (#(conj (vec %) "ENDSEC;"))
       (#(conj % "END-ISO-10303-21;"))
       (str/join "\n")))

(defn ifc-serialize
  [ifc-nested-map]
  (let [ifc-flat (ifc-nested-to-flat-map ifc-nested-map)]
    (ifc-serialize* ifc-flat)))


(defn by-type
  [step-file type]
  (let [type (util/kw-type->str-type type)
        type-row? #(str/includes? % type)]
    (with-open [rdr (io/reader step-file)]
      (doall
        (->> (line-seq rdr)
             (filter type-row?)
             (map #(parser/parse-step-instance %))
             (map transform-step-exp))))))



(comment

  {:id "#12",
   :type-ref :IfcPerson,
   :params {:Identification "$",
            :FamilyName "Liebich",
            :GivenName "Thomas",
            :MiddleNames "$",
            :PrefixTitles "$",
            :SuffixTitles "$",
            :Roles "$",
            :Addresses "$"}}


  {:IfcMeasureWithUnit {:ValueComponent {:IfcPlaneAngleMeasure 0.017453293}, :UnitComponent {:IfcSIUnit {:UnitType :PLANEANGLEUNIT, :Name :RADIAN}}}}


  {:id "#1",
   :type-ref :IfcRelAggregates,
   :params {:Description "$",
            :GlobalId "2YBqaV_8L15eWJ9DA1sGmT",
            :Name "$",
            :OwnerHistory "$",
            :RelatedObjects ("#3"),
            :RelatingObject "#4"}}

  {:id "#3",
   :type-ref :IfcProject,
   :params {:RepresentationContexts ("#5"),
            :ObjectType "$",
            :GlobalId "0xScRe4drECQ4DMSqUjd6d",
            :Name "proxy with CSG",
            :Description "$",
            :UnitsInContext "#6",
            :OwnerHistory "#7",
            :LongName "$",
            :Phase "$"}}


  '([:GlobalId :IfcGloballyUniqueId]
   [:OwnerHistory {:optional true} :IfcOwnerHistory]
   [:Name {:optional true} :IfcLabel]
   [:Description {:optional true} :IfcText]
   [:ObjectType {:optional true} :IfcLabel]
   [:LongName {:optional true} :IfcLabel]
   [:Phase {:optional true} :IfcLabel]
   [:RepresentationContexts {:optional true} [:set {:description :collection-type, :min 1, :max nil} :IfcRepresentationContext]]
    [:UnitsInContext {:optional true} :IfcUnitAssignment])

  '(:RepresentationContexts :ObjectType :GlobalId :Name :Description :UnitsInContext :OwnerHistory :LongName :Phase)

  )