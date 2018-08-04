(ns ^:figwheel-no-load boids.dev
  (:require
    [boids.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
