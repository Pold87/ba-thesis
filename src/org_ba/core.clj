(ns org-ba.core
  (:gen-class :main true)
  (:require [clojure.tools.reader.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [dorothy.core :as doro]
            [rhizome.viz :as rhi]
            [clojure.math.numeric-tower :as math]
            [quil.core :as quil]
            [clojure.java.shell :as shell]
            [me.raynes.conch :as conch]
            [me.raynes.conch.low-level :as conch-sh])
  (:import [javax.swing JPanel JButton JFrame JLabel]
           [java.awt.image BufferedImage BufferedImageOp]))

(defn read-lispstyle-edn
  "Read one s-expression from a file"
  [filename]
  (with-open [rdr (java.io.PushbackReader. (clojure.java.io/reader filename))]
    (edn/read rdr)))

(defmacro write->file
  "Writes body to the given file name"
  [filename & body]
  `(do
     (with-open [w# (io/writer ~filename)]
     (binding [*out* w#]
       ~@body))
  (println "Written to file: " ~filename)))

(defn read-objs
  "Read PDDL objects from a file and add type
  (e.g. 'table bed' -> (list table - furniture
                        bed - furniture))"
  [file object-type]
  (as-> (slurp file) objs
        (clojure.string/split objs #"\s")
        (map #(str % " - " object-type) objs)))



(defn create-pddl
  "Creates a PDDL file from a list of objects and locations"
  [objs-file objs-type]
  (str
   "(define (domain domainName)

  (:requirements
     :durative-actions
     :equality
     :negative-preconditions
     :numeric-fluents
     :object-fluents
     :typing)

  (:types\n"
   (pprint/cl-format nil "~{~&~5@T~a~}" (read-objs objs-file objs-type))
   ")

  (:constants

  )

  (:predicates

  )

  (:functions

  )

  (:durative-action actionName
     :parameters (?x - <objectType>)
     :duration (= ?duration #duration)
     :condition (at start <effects>)
     :effect (at end <effects>))
)"
   ))

(defn split-up
  "Split a PDDL type list (:types obj1.1 obj1.2 - objT1 obj2 - objT2 ...)
  into strings of subtypes and associated types,
  [[subytype1 subtype 2 ... - type][subtype1 subtype2 ...][type]"
  [coll]
  (let [coll (if (= :types (first coll))
               (rest coll)
               coll)]
    ;; REVIEW: insert (\w) for trimming?
    (re-seq #"((?:\s*\w+\s+)+)-\s+(\w+)\s*"
            (clojure.string/join " " coll))))


(defn types->hash-map-helper
  "Convert splitted type list (['<expr>' '<subtype1.1> <subtype1.2> ...' '<type1>']
  to a hash-map {'<type1>': ['<subtype1.1>' '<subtype1.2>' ...], '<type2>': ...}"
  [coll]
  (reduce (fn [h-map [_ objs obj-type]]
            (let [key-obj-type (keyword obj-type)
                  existing-vals (key-obj-type h-map)]
              (assoc h-map
                key-obj-type
                (concat existing-vals
                        (clojure.string/split objs #"\s")))))
          {}
          coll))

(defn types->hash-map
  "Splits types and converts them into a hash-map"
  [pddl-types]
  (types->hash-map-helper (split-up pddl-types)))

(defn map-entry->TikZ-seq
  "Converts a hashmap entry (:key [val1 val2 ...])
to a TikZ string (key -- { val1, val2 })"
  [entry]
  (str
   (name (key entry))
   " -- "
   "{" (clojure.string/join ", " (val entry)) "}"))

(defn hash-map->TikZ-out
  "Converts complete PDDL type hash-map to TikZ file"
  [h-map]
  (str
   "\\documentclass[tikz]{standalone}

\\usepackage[utf8]{inputenc}

\\usepackage{tikz}

\\usetikzlibrary{graphdrawing}
\\usetikzlibrary{graphs}
\\usegdlibrary{layered,trees}

\\begin{document}

\\begin{tikzpicture}

\\graph[layered layout, nodes={draw,circle,fill=blue!20,font=\\bfseries}]
{
  " (clojure.string/join ",\n  " (map map-entry->TikZ-seq h-map))
  "
};

\\end{tikzpicture}
\\end{document}"))

(defn types-map-entry->dot-language
  "Converts one hash-map entry
to the dot language"
  [entry]
  (str
   (name (key entry))
   " -> "
   "{" (clojure.string/join " " (val entry)) "}"))


(defn types-hash-map->dot-language
  "Converts a PDDL types hash-map
to the dot language notation"
  [pddl-types-map]
  (clojure.string/join "\n" (map types-map-entry->dot-language pddl-types-map)))

;;;; Read PDDL predicates and generate UML 'type' diagram



(defn get-types-in-predicate
  "Takes a PDDL predicate,
  e.g. '(at ?x - location ?y - object)
  and returns the involved types, e.g.
  '(location object)"
  [pddl-pred]
  (remove
   (fn [s]
     (let [first-char (first (name s))]
       (or (= \- first-char)
           (= \? first-char)))) (rest pddl-pred)))

(defn pddl-pred->hash-map-long
  "Takes a PDDL predicate, e.g.
  '(at ?x - location ?y - object) and returns a
  hash-map, that assigns the involved types
  to this predicate, e.g.
  {location [(at ?x - location ?y - object)],
   object [(at ?x - location ?y - object)]}"
  [pddl-pred]
  (reduce (fn [h-map pddl-type]
            (assoc h-map
              pddl-type
              (list pddl-pred)))
          {}
          (get-types-in-predicate pddl-pred)))


(pddl-pred->hash-map-long '(at ?x - location ?y - object))

;;; TODO: Create short version wiht prolog predicate style
;;; e.g. at/2
(defn all-pddl-preds->hash-map-long
  "Takes a list of PDDL predicates and
  returns a hash-map of types and the
  assigned predicate"
  [pddl-preds]
  (let [pddl-preds (if (= :predicates (first pddl-preds))
                     (rest pddl-preds)
                     pddl-preds)]
    (apply merge-with concat
           (map pddl-pred->hash-map-long pddl-preds))))

(defn hash-map->dot
  "Converts a hash-map to
  dot language for creating
  UML diagrams"
  [h-map]  
  (map (fn [map-entry]
         (str (key map-entry)
              "[label = \"{"
              (key map-entry)
              "|"
              (clojure.string/join "\\l" (val map-entry))
              "}\"]\n"))
       h-map))

(defn hash-map->dot-with-style
  "Adds dot template to
hash-map>dot"
  [h-map]
  (str
   "digraph hierarchy {
node[shape=record,style=filled,fillcolor=gray92]
edge[dir=back, arrowtail=empty]
\n"
   (clojure.string/join (hash-map->dot h-map))
   "}"))


(defn PDDL->dot-with-style
  "Adds dot template to
hash-map>dot"
  [preds types]
  (str
   "digraph hierarchy {
node[shape=record,style=filled,fillcolor=gray92]
edge[dir=back, arrowtail=empty]
\n"
   
   (clojure.string/join (hash-map->dot (all-pddl-preds->hash-map-long preds)))
   (types-hash-map->dot-language (types->hash-map types))
   
   "}"))

;;; Example for Predicate:
(def predicates 
  '(:predicates (at ?x - location ?y - object)
                (have ?x - object) 
                (hot ?x - object)
                (on ?f - furniture ?o - object)))

;;; Example invocation:
(hash-map->dot-with-style (all-pddl-preds->hash-map-long predicates))


(defn get-PDDL-construct
  "Takes a PDDL keyword and a PDDL domain/problem
file and returns all parts of the file that
belong to the PDDL keyword."
  [pddl-keyword pddl-file]
  (filter #(and (seq? %)
                (= (keyword pddl-keyword)
                   (first %)))
          (read-lispstyle-edn pddl-file)))


                                        ; TODO: Throw error if length != 1
(defn get-PDDL-predicates
  "Get all predicates in a PDDL file"
  [pddl-file]
  (first (get-PDDL-construct 'predicates pddl-file)))


                                        ; TODO: Throw error if length != 1
(defn get-PDDL-types
  "Get all types in a PDDL file"
  [pddl-file]
  (first (get-PDDL-construct 'types pddl-file)))

(defn PDDL->dot
  "Takes a complete PDDL file
and generates a UML type diagram"
  [pddl-file]
  (PDDL->dot-with-style (get-PDDL-predicates pddl-file)
                        (get-PDDL-types pddl-file)))

(defn PDDL->dot-commandline-input
  "Assumes that the PDDL input is
a string and 'reads' this string"
  [pddl-file]
  (print "The type is " (type pddl-file))
  (PDDL->dot (edn/read-string pddl-file)))


(defn PDDL->dot-file-input
  "Reads PDDL file"
  [pddl-file-name]
  (PDDL->dot pddl-file-name))

;;;; math helper functions

(defn sqr
  "Square of a number"
  [x]
  (* x x))

(defn round-places [number decimals]
  "Round to decimal places"
  (let [factor (math/expt 10 decimals)]
    (double (/ (math/round (* factor number)) factor))))

(defn euclidean-squared-distance
  "Computes the Euclidean squared distance between two sequences"
  [a b]
  (reduce + (map (comp sqr -) a b)))

(defn euclidean-distance
  "Computes the Euclidean distance between two sequences"
  [a b]
  (math/sqrt (euclidean-squared-distance a b)))

;;;; End math helper functions

(defn distance-delete
  [a b]
  10)

(defn calc-distance-good
  "Calculates the distance and writes
the calculated distances to a string
IS VERY GOOD !!!"
  [locations]
  (for [[ _ loc1 & xyz-1] locations
        [ _ loc2 & xyz-2] locations]
    ;; Euclidean distance rounded to 4 decimal places.
    (list 'distance loc1 loc2 (euclidean-distance xyz-1 xyz-2))))

(defn get-specified-predicates-in-pddl-file
  "Extracts all locations in the predicates part
(by the specified name) in a PDDL file"
  [pddl-file predicate-name]
  (filter #(and (seq? %)
                (= predicate-name (first %)))
          (get-PDDL-predicates pddl-file)))


(defn calc-distance
  "Calculate distances of PDDL objects"
  [locations]
  (for [[ _ loc1 & xyz-1] locations
        [ _ loc2 & xyz-2] locations]
    ;; Euclidean distance rounded to 4 decimal places.
    `(~'distance ~loc1 ~loc2
                 ~(euclidean-distance xyz-1 xyz-2))))

; LOOK UP: extended equality: 'hello = :hello

(defn add-part-to-PDDL
  "Takes a PDDL domain or problem
and add the specified part to the
specified position"
  [pddl-file position part]
  
  (map #(if (and (seq? %)
                 (= (keyword position) (first %)))
          (concat % part)
          %)
       (read-lispstyle-edn pddl-file)))

; Example invocation:
#_(pprint (add-part-to-PDDL "read-domain.pddl" 'predicates (calc-distance (get-specified-predicates-in-pddl-file "read-domain.pddl" 'location))))



; Was in main:
  #_(-> (PDDL->dot-file-input (first args))
      rhi/dot->image
      rhi/view-image)

#_(print (PDDL->dot-file-input (first args)))

;;; https://www.refheap.com/9034
(defn exit-on-close [sketch]
  (let [frame (-> sketch .getParent .getParent .getParent .getParent)]
    (.setDefaultCloseOperation frame javax.swing.JFrame/EXIT_ON_CLOSE)))

(defn -main
  "Runs the input/output scripts"
  [& args]
  ;; Write dot graph to file
  (doall
   (write->file "tmp.dot" (print (PDDL->dot-file-input (first args)))))
  (doall (conch-sh/stream-to-out
            (conch-sh/proc "dot" "-Tpng" "-o" "gen-graph.png" "tmp.dot") :out))

  (def img (ref nil))

  (defn setup []
    (quil/background 0)
    (dosync (ref-set img (quil/load-image "gen-graph.png"))))
  
  (def img-size
    (with-open [r (java.io.FileInputStream. "gen-graph.png")]
      (let [image (javax.imageio.ImageIO/read r)
            img-width (.getWidth image)
            img-height (.getHeight image)]
        [img-width img-height])))

  (defn draw []
    (quil/image @img 0 0))
  
  (exit-on-close
   (quil/sketch
    :title "PDDL UML Diagram"
    :setup setup
    :draw draw
    :size (vec img-size))))
