(def x (reduce + 
               (distinct 
                 (concat 
                   (range 0 1000 3) 
                   (range 0 1000 5)))))
(println x)
