(ns friendly-xml-map.core
  (:require [clojure.xml :as xml]
            [schema.core :as s]
            [clojure.pprint :as pp]))

;; (defn foo
;;   "I don't do a whole lot."
;;   [x]
;;   (println x "Hello, World!"))

(def test-xml "<what attr-one=\"who\"><when>one</when></what>")

(def What
  {:attr-one s/Str
   :when s/Str})

(defn parse-str [xml-str]
  (xml/parse (java.io.ByteArrayInputStream. (.getBytes xml-str))))

(defn- is-primitive? [property-type]
  ;; (println (str "type of property-type = " (type property-type)))
  (and (= java.lang.Class (type property-type))
       (contains? #{java.lang.String java.lang.Long clojure.lang.Keyword} property-type)))

(defn- is-vector-of-primitives? [x]
  (and (sequential? x)
       (= 1 (count x))
       (is-primitive? (first x))))

(defn- get-key-value-from-xml-map [xml-map]
  (fn [[property-key property-type]]
    (println (str "analy map = " (with-out-str (pp/pprint {:property-key property-key
                                                           :property-type property-type
                                                           :attrs-of-map (:attrs xml-map)}))))
    (cond
      (is-primitive? property-type)
      (let [property-is-in-attrs (contains? (:attrs xml-map) property-key)]
        (if property-is-in-attrs
          [property-key (get (:attrs xml-map) property-key)]
          (let [single-content-matches (->> (get xml-map :content) ;; assuming single primitive content
                                        (filter #(= property-key (get % :tag))))]
            (if (not= 1 (count single-content-matches))
              (throw (Exception. "expecting exactly 1"))
              (let [val (-> single-content-matches first :content first)]
                [property-key val])))))
      
      (is-vector-of-primitives? property-type)
      (let [get-primitive-content (fn [x]
                                       (let [content (:content x)]
                                         (if (not= 1 (count content))
                                           (throw (Exception. "expected exactly 1"))
                                           (first content))))
            xform-vector-of-primitives (comp
                                        (filter #(= property-key (get % :tag)))
                                        (map get-primitive-content))]
        [property-key (into [] xform-vector-of-primitives (get xml-map :content))])

      )))


(defn friendly-map [xml-map schema]
  (let [key-val-vector (map (get-key-value-from-xml-map xml-map) schema)]
    (into {} key-val-vector)))
