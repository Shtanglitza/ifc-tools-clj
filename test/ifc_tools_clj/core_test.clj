(ns ifc-tools-clj.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ifc-tools-clj.step.core :as step]
            [clojure.edn :as edn]))


;IFC STEP

(def load-ifc
  (comp step/ifc-deserialize slurp))

(def load-edn
  (comp edn/read-string slurp))

(defn roundtrip
  "Deserialize IFC step file into nested clojure data structure ifc-1,
  serialize it and deserialize it again into ifc-2, and then
  check equivalence between ifc-1 and ifc-2"
  [ifc-step-file]
  (let [ifc-1 (load-ifc ifc-step-file)
        ifc-2 (->> ifc-1
                   step/ifc-serialize
                   step/ifc-deserialize)]
    (is ifc-1 ifc-2)))

(deftest ifc-simple-deserialize-test
  (testing "Simple step file deserialization"
    (is (load-ifc "test/ifc_tools_clj/samples/project.ifc")
        (load-edn"test/ifc_tools_clj/samples/project.edn"))))

(deftest ifc-ser-deser-eq-test-1
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/project.ifc")))

(deftest ifc-ser-deser-eq-test-2
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/csg-primitive.ifc")))

(deftest ifc-ser-deser-eq-test-3
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/horizontal-alignment.ifc")))

(deftest ifc-ser-deser-eq-test-4
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/basin-advanced-brep.ifc")))

;(deftest ifc-ser-deser-eq-test-5
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/wall-with-opening-and-window.ifc")))

(deftest ifc-ser-deser-eq-test-6
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/Wall elemented case.ifc")))

(deftest ifc-ser-deser-eq-test-7
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/Basin faceted brep.ifc")))

(deftest ifc-ser-deser-eq-test-8
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/Basin tessellation.ifc")))

(deftest ifc-ser-deser-eq-test-9
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/Beam varying extrusion paths.ifc")))

(deftest ifc-ser-deser-eq-test-10
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/Brep model.ifc")))

;(def giant-ifc
;  (let [rdr (clojure.java.io/reader
;              "test/ifc_tools_clj/samples/20160125WestRiverSide Hospital - IFC4-Autodesk_Hospital_Sprinkle.ifc")
;        {:keys [:tree :parser]}
;        (clj-antlr.proto/parse
;          ifc-tools-clj.step.antlr/step-parser nil rdr)]))




;(with-open [rdr (clojure.java.io/reader file-path)]
;  (doseq [line (line-seq rdr)]
;    (if (head? line)
;      (process-header line)
;      (process-data line))))


;IFC XML

