#| From Will Ness on https://stackoverflow.com/questions/20600559/lisp-prime-number
|#

(farey 1)
-> '(0 1)
(farey 4)
-> '(0 1/4 1/3 1/2 2/3 3/4 1)
(farey 0)
-> nil

(defun farey
       (n &optional
        (orig-n n)
        (out-list nil))
  (if (<= n 1)
      (if (> orig-n 0)
          (sort
           (remove-duplicates
            (append '(0) out-list '(1))
            :test #'equalp)
           #'<))
      (farey
       (- n 1) orig-n
       (append
        out-list
        (rest
         (mapcar
          #'(lambda (x)
              (/ x n)) (first-n-naturals n)))))))

(defun is-prime (n &optional (d (- n 1))) 
  (or (= d 1)
      (and (/= (rem n d) 0)
           (is-prime  n (- d 1)))))
