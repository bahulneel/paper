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
       (fd/>= z 0)))

;; Object with the same z value exists on the same plane
(defn same-plane [p1 p2]
  (all (paper p1)
       (paper p2)
       (!= p1 p2)
       (fresh [p1-x p1-y p1-z
               p2-x p2-y p2-z]
         (paper-pos p1 p1-x p1-y p1-z)
         (paper-pos p2 p2-x p2-y p2-z)
         (fd/== p1-z p2-z))))

(defn different-plane [p1 p2]
  (all (paper p1)
       (paper p2)
       (!= p1 p2)
       (fresh [p1-x p1-y p1-z
               p2-x p2-y p2-z]
         (paper-pos p1 p1-x p1-y p1-z)
         (paper-pos p2 p2-x p2-y p2-z)
         (fd/!= p1-z p2-z))))

;; Or they can be above or below each other
(defn above [p1 p2]
  (all (paper p1)
       (paper p2)
       (!= p1 p2)
       (fresh [p1-x p1-y p1-z
               p2-x p2-y p2-z]
         (paper-pos p1 p1-x p1-y p1-z)
         (paper-pos p2 p2-x p2-y p2-z)
         (fd/> p1-z p2-z))))

(defn below [p1 p2]
  (all (paper p1)
       (paper p2)
       (!= p1 p2)
       (fresh [p1-x p1-y p1-z
               p2-x p2-y p2-z]
         (paper-pos p1 p1-x p1-y p1-z)
         (paper-pos p2 p2-x p2-y p2-z)
         (fd/< p1-z p2-z))))

;; They can intersect
(defn intersect [p1 p2]
  (all (paper p1)
       (paper p2)
       (!= p1 p2)
       (fresh [p1-x p1-y p1-z
               p1-w p1-h p1-d
               p2-x p2-y p2-z
               p2-w p2-h p2-d]
         (paper-dims p1 p1-w p1-h p1-d)
         (paper-dims p2 p2-w p2-h p2-d)
         (paper-pos p1 p1-x p1-y p1-z)
         (paper-pos p2 p2-x p2-y p2-z)
         (fresh [p1-x+w p1-y+h p2-x+w p2-y+h]
           (fd/+ p1-x p1-w p1-x+w)
           (fd/+ p1-y p1-h p1-y+h)
           (fd/+ p2-x p2-w p2-x+w)
           (fd/+ p2-y p2-h p2-y+h)
           (fd/< p1-x p2-x+w)
           (fd/< p2-x p1-x+w)
           (fd/< p1-y p2-y+h)
           (fd/< p2-y p1-y+h)))))

;; One can be inside another
(defn inside [p1 p2]
  (all (paper p1)
       (paper p2)
       (!= p1 p2)
       (fresh [p1-x p1-y p1-z
               p1-w p1-h p1-d
               p2-x p2-y p2-z
               p2-w p2-h p2-d]
         (paper-dims p1 p1-w p1-h p1-d)
         (paper-dims p2 p2-w p2-h p2-d)
         (paper-pos p1 p1-x p1-y p1-z)
         (paper-pos p2 p2-x p2-y p2-z)
         (fresh [p1-x+w p1-y+h p2-x+w p2-y+h]
           (fd/+ p1-x p1-w p1-x+w)
           (fd/+ p1-y p1-h p1-y+h)
           (fd/+ p2-x p2-w p2-x+w)
           (fd/+ p2-y p2-h p2-y+h)
           (fd/>= p1-x p2-x)
           (fd/>= p2-x+w p1-x+w)
           (fd/>= p1-y p2-y)
           (fd/>= p2-y+h p1-y+h)))))

;; Multiple material elements cannot occupy the same point in space simultaneously.
;;
;; In physics this is the Pauli exclusion principle
(defn pauli [p1 p2]
  (conde
   [(== p1 p2)]
   [(intersect p1 p2) (different-plane p1 p2)]
   [(!intersect p1 p2)]))

;; Containers and the objects they contain have a parent-child
;; relationship. Every object has a single parent, and may or may not
;; have one or more children
(db-rel parent container object)

;; Or it can be the root
(db-rel root object)

;; Objects can be placed on a container
(defn on-container [container object]
  (all (parent container object)
       (above object container)
       (inside object container)))

;; Materials may be at rest or raised
(db-rel at-rest p)
(db-rel raised p)

;; They also have a rest elevation
(db-rel rest-elevation p z)

;; If they live on a container they are raised to the height of the container
(defn rest-z [p z]
  (fresh [parent z-rest]
    (rest-elevation p z-rest)
    (conde [(on-container parent p) (fresh [c-x c-y c-z]
                                      (paper-pos c-x c-y c-z)
                                      (fd/+ c-z z-rest z))]
           [(root p) (z-rest z)])))

;; At rest paper adopts its rest elevation, if raised it it moves up by 6dp
(defn elevation [p z]
  (fresh [x y z-rest]
    (paper-pos p x y z)
    (rest-z p z-rest)
    (conde
     [(at-rest p) (== z z-rest)]
     [(raised p) (fd/+ z-rest 6 z)])))
