(ns ifc-tools-clj.step.antlr
  (:require [clj-antlr.core :as antlr]
            [clojure.java.io :as io]
            [clojure.pprint :as pp])
  (:import (org.antlr.v4.runtime ParserRuleContext
                                 Parser
                                 ;Token
                                 ;CommonTokenStream
                                 )))

(def ^Parser step-parser
  (antlr/parser
    (slurp (.getResourceAsStream
             (clojure.lang.RT/baseLoader)
             "STEP.g4"))))

(def exp-parser
  (antlr/parser
    (slurp (.getResourceAsStream
             (clojure.lang.RT/baseLoader)
             "Express.g4"))))

(defn parse-step
  [step-file]
  (try
    (step-parser
      (slurp step-file))
    (catch clj_antlr.ParseError e
      (pp/pprint e))))

(defn parse-step-instance
  [input]
  (try
    (antlr/parse step-parser {:root "instance"} input)
    (catch clj_antlr.ParseError e
      (pp/pprint e))))

(defn parse-exp
  [exp-file]
  (try
    (exp-parser
      (slurp exp-file))
    (catch clj_antlr.ParseError e
      (pp/pprint e))))

;(defn parser-step-chunk
;  [^Parser parser step-file chunk-size]
;  (with-open [rdr (io/reader step-file)]
;    (doall
;      (->> (line-seq rdr)))))