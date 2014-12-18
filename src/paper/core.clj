(ns paper.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.pldb :refer :all]
            [clojure.core.logic.fd :as fd]))

;; ### Devices
;; Each device has a type
(db-rel device d)

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

;; We need axis to place out objects
(defn axis [x y z]
  (fd/in x y z (fd/interval -10000 10000)))

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

;; It has no parents
(db-rel base-object object)

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

;; All positions are relative to the sheet they live on, sometimes we may need the absolute value
(defn absolute-pos [p x y z]
  (fresh [p-x p-y p-z]
    (paper-pos p p-x p-z p-z)
    (conde [(fresh [c c-x c-y c-z]
              (contains c p)
              (absolute-pos c c-x c-z c-z)
              (fd/+ p-x c-x x)
              (fd/+ p-y c-y y)
              (fd/+ p-z c-z z))]
           [(base-object p)
            (fd/== x p-x)
            (fd/== y p-y)
            (fd/== z p-z)])))

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
  (device d)
  (conde
   [(== d :mobile) (fd/== h 54)]
   [(== d :tablet) (fd/== h 54)]
   [(== d :desktop) (fd/== h 64)]))

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
  (device d)
  (conde [(== d :mobile) (fd/== m 16)]
         [(== d :tablet) (fd/== m 24)]
         [(== d :desktop) (fd/== m 24)])) ; TODO check screen size
                                            ; instead for desktop
(defn associated-content [d m]
  (device d)
  (conde [(== d :mobile) (fd/== m 72)]
         [(== d :tablet) (fd/== m 80)]
         [(== d :desktop) (fd/== m 80)]))

(defn floating-action [d m]
  (device d)
  (conde [(== d :mobile) (fd/== m 32)]
         [(== d :tablet) (fd/== m 24)]
         [(== d :desktop) (fd/== m 24)]))

(defn side-menu-margin [d m]
  (device d)
  (conde [(== d :mobile) (fd/== m 56)]
         [(== d :tablet) (fd/>= m 0)]
         [(== d :desktop) (fd/>= m 0)]))

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

;; ## Visibility
;; A sheet is visible if its absolute position and size is in the screen
(defn visible [d p]
  (fresh [d-w d-h d-d
          p-x p-y p-z
          p-w p-h p-d]
    (screen d d-w d-h d-d)
    (absolute-pos p p-x p-y p-z)
    (paper-dims p p-w p-h p-d)
    (fd/<= p-z d-d)
    (fresh [p-x+w p-y+h]
      (fd/+ p-x p-w p-x+w)
      (fd/+ p-y p-h p-y+h)
      (fd/< p-x d-w)
      (fd/< 0 p-x+w)
      (fd/< p-y d-h)
      (fd/< 0 p-y+h))))

;; A sheet is on the screen is it fits completely onto the screen
(defn on-screen [d p]
  (fresh [d-w d-h d-d
          p-x p-y p-z
          p-w p-h p-d]
    (screen d d-w d-h d-d)
    (absolute-pos p p-x p-y p-z)
    (paper-dims p p-w p-h p-d)
    (trace-lvars :on-screen p-d)
    (fd/<= p-z d-d)
    (fresh [p-x+w p-y+h]
      (fd/+ p-x p-w p-x+w)
      (fd/+ p-y p-h p-y+h)
      (fd/<= p-x+w d-w)
      (fd/<= 0 p-x)
      (fd/<= p-y+h d-h)
      (fd/<= 0 p-y))))

(with-db
  (db [base-object :nav] [paper :nav] [device :mobile] [pixel-depth :mobile 1] [screen :mobile 800 600 10])
  (run 1 [p
          x y z
          w h pd]
    (fresh [d]
      (device d)
      (axis x y z)
      (axis w h pd)
      (paper p)
      (paper-dims p w h pd)
      (paper-pos p x y z)
      (on-screen d p))))
