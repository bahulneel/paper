(ns paper.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.pldb :refer :all]
            [clojure.core.logic.fd :as fd]))

;; ### Devices
;; Each device has a type
(db-rel device d type)

;; A pixel depth
(db-rel pixel-depth d depth)

;; A screen which represents the from surface
(db-rel screen d w h d)

;; We can use the depth to move between px and dp
(defn pixel [d px dp]
  (fresh [px-depth]
    (device d)
    (pixel-depth d px-depth)
    (fd/* pixel-depth dp px)))

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

;; Or they can be over or under each other
(defn over [p1 p2]
  (all (paper p1)
       (paper p2)
       (!= p1 p2)
       (fresh [p1-x p1-y p1-z
               p2-x p2-y p2-z]
         (paper-pos p1 p1-x p1-y p1-z)
         (paper-pos p2 p2-x p2-y p2-z)
         (fd/> p1-z p2-z))))

(defn under [p1 p2]
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

;; Containers and the objects they contain have a parent-child
;; relationship. Every object has a single parent, and may or may not
;; have one or more children
(db-rel contains container object)

;; Materials may be at rest or raised
(db-rel at-rest p)
(db-rel raised p)

;; They also have a rest elevation
(db-rel rest-elevation p z)

;; At rest paper adopts its rest elevation, if raised it it moves up by 6dp
(defn elevation [p z]
  (fresh [x y z-rest]
    (paper-pos p x y z)
    (rest-elevation p z-rest)
    (conde
     [(at-rest p) (== z z-rest)]
     [(raised p) (fd/+ z-rest 6 z)])))

;; Multiple material elements cannot occupy the same point in space simultaneously.
;;
;; In physics this is the Pauli exclusion principle
(defn pauli [p1 p2]
  (conde
   [(== p1 p2)]
   [(fresh [c]
      (contains c p1)
      (contains c p2)
      (conde [(intersect p1 p2) (different-plane p1 p2)]
             [(!intersect p1 p2)]))]
   [(fresh [c1 c2]
      (contains c1 p1)
      (contains c2 p2)
      (!= c1 c2))]))

;; ## Layout
;; ### Arranging paper

;; Seams are created when two sheets of paper share the full length of a
;; common edge. Sheets joined by a seam generally move together.
(defn seem [p1 p2]
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
         (fd/== p1-z p2-z)
         (conde
          [(fd/== p1-x p2-x) (fd/== p1-w p2-w) (fresh [p1-y+h]
                                                 (fd/+ p1-y p1-h p1-y+h)
                                                 (fd/== p1-y+h p2-y))]
          [(fd/== p1-y p2-y) (fd/== p1-h p2-h) (fresh [p1-x+w]
                                                 (fd/+ p1-x p1-w p1-x+w)
                                                 (fd/== p1-x+w p2-x))]))))

;; ### Paper toolbars

;; A toolbar is a strip of paper used to present actions.
;;
;; Toolbars sit at the top a can be off-screren or on-screen
;;
;; Toolbars have a standard height, 56 dp on mobile and 64 dp on
;; desktop, but they can be taller. When taller, the actions can be
;; pinned to either the top or the bottom of the toolbar.
;; 56 dp on mobile and 64 dp on desktop
(defn bar-height [d h]
  (fresh [t]
    (device d t)
    (conde
     [(== :mobile) (fd/== h 54)]
     [(== :tablet) (fd/== h 54)]
     [(== :desktop) (fd/== h 64)])))

(defn valid-bar-height [d h]
  (fresh [height-inc step]
    (bar-height d height-inc)
    (fd/* height-inc step h)))

(defn toolbar [device p]
  (fresh [x y z
          h w d
          row-h]
    (device d)
    (valid-bar-height device h)
    (paper-pos p x y z)
    (paper-dims p w h d)
    (fd/<= y 0)
    (fd/== x 0)))

;; ## Baseline grids

;; All components align to an 8dp square baseline grid. Type aligns to a
;; 4dp baseline grid. Iconography in toolbars align to a 4dp square
;; baseline grid. This applies to mobile, tablet, and desktop.
(defn component-snap [x]
  (fresh [m]
    (fd/* 8 m x)))

(defn type-snap [x]
  (fresh [m]
    (fd/* 4 m x)))

;; ## Keylines and spacing

;; ### Horizontal keylines
(defn margin [d m]
  (fresh [t]
    (device d t)
    (conde [(== t :mobile) (fd/== m 16)]
           [(== t :tablet) (fd/== m 24)]
           [(== t :desktop) (fd/== m 24)]))) ; TODO check screen size
                                            ; instead for desktop
(defn associated-content [d m]
  (fresh [t]
    (device d t)
    (conde [(== t :mobile) (fd/== m 72)]
           [(== t :tablet) (fd/== m 80)]
           [(== t :desktop) (fd/== m 80)])))

(defn floating-action [d m]
  (fresh [t]
    (device d t)
    (conde [(== t :mobile) (fd/== m 32)]
           [(== t :tablet) (fd/== m 24)]
           [(== t :desktop) (fd/== m 24)])))

(defn side-menu-margin [d m]
  (fresh [t]
    (device d t)
    (conde [(== t :mobile) (fd/== m 56)]
           [(== t :tablet) (fd/>= m 0)]
           [(== t :desktop) (fd/>= m 0)])))

;; ### Ratio keylines
;; TODO

;; ### Incremental keylines
;; TODO

;; ### Touch targets
;; The minimum touch target size is 48dp.
(defn touch-target [p]
  (all (paper p)
       (fresh [w h d]
         (paper-dims p w h d)
         (fd/>= w 48)
         (fd/>= h 48))))

;; We can check if an
