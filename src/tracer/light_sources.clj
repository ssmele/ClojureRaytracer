(ns tracer.light-sources)

(defrecord PointSource [loc])
(defrecord RectangeSource [x y dx dy])