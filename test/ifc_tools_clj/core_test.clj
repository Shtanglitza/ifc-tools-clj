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
    (is (load-ifc "test/ifc_tools_clj/samples/ifc/project.ifc")
        (load-edn "test/ifc_tools_clj/samples/project.edn"))))

;(deftest ifc-ser-deser-eq-test-1
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/ifc/air-terminal-element.ifc")))
;
;(deftest ifc-ser-deser-eq-test-2
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/ifc/air-terminal-library-object.ifc")))
;
(deftest ifc-ser-deser-eq-test-3
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/basin-advanced-brep.ifc")))

(deftest ifc-ser-deser-eq-test-4
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/basin-faceted-brep.ifc")))

(deftest ifc-ser-deser-eq-test-5
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/basin-tessellation.ifc")))

(deftest ifc-ser-deser-eq-test-6
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/bath-csg-solid.ifc")))

(deftest ifc-ser-deser-eq-test-7
 (testing "Serialization/Deserialization equivalence"
   (roundtrip "test/ifc_tools_clj/samples/ifc/beam-curved-i-shape-tessellated.ifc")))

;(deftest ifc-ser-deser-eq-test-8
; (testing "Serialization/Deserialization equivalence"
;   (roundtrip "test/ifc_tools_clj/samples/ifc/beam-extruded-solid.ifc")))

(deftest ifc-ser-deser-eq-test-9
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/beam-revolved-solid.ifc")))

(deftest ifc-ser-deser-eq-test-10
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/beam-standard-case.ifc")))

(deftest ifc-ser-deser-eq-test-11
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/beam-straight-i-shape-tessellated.ifc")))

(deftest ifc-ser-deser-eq-test-12
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/beam-varying-cardinal-points.ifc")))

(deftest ifc-ser-deser-eq-test-13
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/beam-varying-extrusion-paths.ifc")))

;(deftest ifc-ser-deser-eq-test-14
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/ifc/bbeam-varying-profiles.ifc")))

(deftest ifc-ser-deser-eq-test-15
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/bloss-curve.ifc")))

(deftest ifc-ser-deser-eq-test-16
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/brep-model.ifc")))

(deftest ifc-ser-deser-eq-test-17
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/column-straight-rectangle-tessellation.ifc")))

(deftest ifc-ser-deser-eq-test-18
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/construction-scheduling-task.ifc")))

(deftest ifc-ser-deser-eq-test-19
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/csg-primitive.ifc")))

(deftest ifc-ser-deser-eq-test-20
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/cube-advanced-brep.ifc")))

(deftest ifc-ser-deser-eq-test-21
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/extruded-solid.ifc")))

(deftest ifc-ser-deser-eq-test-22
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/grid-placement.ifc")))

(deftest ifc-ser-deser-eq-test-23
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/horizontal-alignment.ifc")))

(deftest ifc-ser-deser-eq-test-24
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/linear-placement.ifc")))

(deftest ifc-ser-deser-eq-test-25
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/mapped-shape-with-multiple-items.ifc")))

(deftest ifc-ser-deser-eq-test-26
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/mapped-shape-without-transformation.ifc")))

(deftest ifc-ser-deser-eq-test-27
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/mapped-shape-with-transformation.ifc")))

(deftest ifc-ser-deser-eq-test-28
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/OSM.ifc")))

(deftest ifc-ser-deser-eq-test-29
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/polygonal-face-tessellation.ifc")))

(deftest ifc-ser-deser-eq-test-30
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/project.ifc")))

;(deftest ifc-ser-deser-eq-test-31
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/ifc/reinforcing-assembly.ifc")))
;
;(deftest ifc-ser-deser-eq-test-32
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/ifc/reinforcing-stirrup.ifc")))

;(deftest ifc-ser-deser-eq-test-33
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/ifc/sectioned-solid.ifc")))

;(deftest ifc-ser-deser-eq-test-34
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/ifc/slab-openings.ifc")))

;(deftest ifc-ser-deser-eq-test-35
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/ifc/slab-standard-case.ifc")))

(deftest ifc-ser-deser-eq-test-36
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/slab-tessellated-unique-vertices.ifc")))

;(deftest ifc-ser-deser-eq-test-37
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/ifc/structural-curve-member.ifc")))

(deftest ifc-ser-deser-eq-test-38
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/surface-model.ifc")))

(deftest ifc-ser-deser-eq-test-39
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/terrain-and-alignment.ifc")))

(deftest ifc-ser-deser-eq-test-40
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/terrain-surface.ifc")))

(deftest ifc-ser-deser-eq-test-41
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/tessellated-item.ifc")))

;(deftest ifc-ser-deser-eq-test-42
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/ifc/tessellation-with-blob-texture.ifc")))

(deftest ifc-ser-deser-eq-test-43
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/tessellation-with-image-texture.ifc")))

(deftest ifc-ser-deser-eq-test-44
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/tessellation-with-individual-colors.ifc")))

;(deftest ifc-ser-deser-eq-test-45
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/ifc/tessellation-with-pixel-texture.ifc")))

(deftest ifc-ser-deser-eq-test-46
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/vertical-alignment.ifc")))

(deftest ifc-ser-deser-eq-test-47
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/wall-elemented-case.ifc")))

(deftest ifc-ser-deser-eq-test-48
  (testing "Serialization/Deserialization equivalence"
    (roundtrip "test/ifc_tools_clj/samples/ifc/wall-standard-case.ifc")))

;(deftest ifc-ser-deser-eq-test-49
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/ifc/wall-with-opening-and-window.ifc")))

;(deftest ifc-ser-deser-eq-test-50
;  (testing "Serialization/Deserialization equivalence"
;    (roundtrip "test/ifc_tools_clj/samples/ifc/20160125WestRiverSide Hospital - IFC4-Autodesk_Hospital_Sprinkle.ifc")))

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

