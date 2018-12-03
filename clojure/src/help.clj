(ns help
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn input [file-name]
  (->> file-name io/resource slurp str/split-lines))
