(ns rt-in-weekend.util)

(defn flip [f & xs]
  (apply f (reverse xs)))