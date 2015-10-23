(ns disclojure.redblacktree
  (:use [clojure.core.match :only (match)]))

(defn rb-balance [tree]
  (match [tree]
         [(:or {:left {:color :red
                       :left {:color :red :left a :value x :right b}
                       :value y :right c}
                :value z :right d}

               {:left {:color :red                    
                       :left  a :value x
                       :right {:color :red :value y :left b :right c}}
                :value z :right d}

               {:left a :value x
                :right {:color :red
                        :left {:color :red
                               :left b :value y :right c}
                        :value z :right d}}

               {:left a :value x
                :right {:color :red
                        :left b :value y
                        :right {:color :red
                                :left c :value z :right d}}})]
         (rb-mk-tree :red
                     (rb-mk-tree :black a x b)
                     y
                     (rb-mk-tree :black c z d))

         :else tree))

(defn balance [tree]
  (match [tree]
         [(:or [:black [:red [:red a x b] y c] z d]
               [:black [:red a x [:red b y c]] z d]
               [:black a x [:red [:red b y c] z d]]
               [:black a x [:red b y [:red c z d]]])] [:red [:black a x b]
                                                            y
                                                            [:black c z d]]
               :else tree))

(defn insert [tree x]
  (let [ins (fn ins [tree]
              (match tree
                     nil [:red nil x nil]
                     [color a y b] (cond
                                    (< x y) (balance [color (ins a) y b])
                                    (> x y) (balance [color a y (ins b)])
                                    :else tree)))
        [_ a y b] (ins tree)]
    [:black a y b]))

(defn is-member? [tree x]
  (match tree
         nil false
         [_ a y b] (cond
                    (< x y) (recur a x)
                    (> x y) (recur b x)     
                    :else true)))
