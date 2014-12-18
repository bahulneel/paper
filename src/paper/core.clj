(ns paper.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.pldb :refer :all]
            [clojure.core.logic.fd :as fd]))

;; ## Material design

;; In material design everything is made up from paper and ink
(db-rel paper ^:index p)
(db-rel ink ^:index i)

;; Both paper and ink can have shape

;; shapes have a name and a description
(db-rel shape ^:index s d)

;; Paper can only be a rounded rectangle
(defn paper-shape [p s d]
  (all (paper p)
       (shape s d)
       (== s :rounded-rect)))

;; Ink can have any shape
(defn ink-shape [i s]
  (all (ink i)
       (shape s)))

;; Paper and ink can also have colour

;; colours have a name and a shade
(db-rel colour ^:index c s)

;; Paper can only be white
(defn paper-colour [p c s]
  (all (paper p)
       (colour c s)
       (== c :white)))

;; However, ink can be any colour
(defn ink-colour [i c s]
  (all (ink i)
       (colour c s)))

;; Paper is 3 dimensional
;;  - width and height must be positive
;;  - the depth is fixed at 1dp
(defn paper-dims [p w h d]
  (all (paper p)
       (fd/>= w 0)
       (fd/>= h 0)
       (fd/== d 1)))

;; Paper also lives in a 3-dimensional space
;;  - x and y can take any value
;;  - z must be positive
(defn paper-pos [p x y z]
  (all (paper p)
       (fd/>= 0)))

;; Properties
(db-rel seam p1 p2)

(db-rel on p-top p-bottom)
(db-rel over p-top p-bottom)
