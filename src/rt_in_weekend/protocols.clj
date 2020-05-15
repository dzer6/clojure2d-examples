(ns rt-in-weekend.protocols)

(defprotocol ConvertibleProto
  (as-map [object]))

(defprotocol BuildableProto
  (build [object]))
