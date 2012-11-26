(defun modsList (aList aNum)
  (cond
    ((eq aNum 1000) aList)
    ((eq (mod aNum 3) 0) (modsList (cons aNum aList) (+ aNum 1)))
    ((eq (mod aNum 5) 0) (modsList (cons aNum aList) (+ aNum 1)))
    (t (modsList aList (+ aNum 1)))))

;sbcl --load ans.lisp
;(reduce #'+ (modsList '() 3))
