(ns tracer.imaging
  (:import (java.awt Color)
           (java.awt.image BufferedImage)
           (javax.imageio ImageIO)))

(defn create-image [w h]
  "Creates image based on width and height."
  (new BufferedImage w h BufferedImage/TYPE_INT_RGB))

(defn set-pixel! [img x y rgb]
  "Imperatively sets the value of the given images location."
  (.setRGB img x y rgb))

(defn save-image [image where]
  "Saves given java image to the given file location."
  (let [file (new java.io.File where)]
    (ImageIO/write image "png" file)))

(defn RGB->Scalar [RGB]
  "Takes an RGB vector and converts it to a scalar."
  (.getRGB (Color. (int (* 255.99 (.x RGB)))
                   (int (* 255.99 (.y RGB)))
                   (int (* 255.99 (.z RGB))))))