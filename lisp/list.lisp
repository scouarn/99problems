
;; P01 (*) Find the last box of a list.
(defun my-last (xs)
    (if (cdr xs)
        (my-last (cdr xs))
        xs
    ))


;; P02 (*) Find the last but one box of a list.
(defun my-but-last (xs)
    (if (cddr xs)
        (my-but-last (cdr xs))
        xs
    ))


;; P03 (*) Find the K'th element of a list.
(defun element-at (xs k)
    (if (= k 1)
        (car xs)
        (element-at (cdr xs) (- k 1))
    ))


;; P04 (*) Find the number of elements of a list.
(defun len (xs)
    (if xs
        (+ 1 (len (cdr xs)))
        0
    ))


;; P05 (*) Reverse a list.
(defun snoc (xs x)
    (if xs
        (cons (car xs) (snoc (cdr xs) x))
        (list x)
    ))

(defun rev (xs)
    (when xs
        (snoc (rev (cdr xs)) (car xs))
    ))

(defun rev_acc (xs acc)
    (if xs
        (rev_acc (cdr xs) (cons (car xs) acc))
        acc
    ))


;; P06 (*) Find out whether a list is a palindrome.
(defun palindrome? (xs)
    (equal xs (rev xs)))


;; P07 (**) Flatten a nested list structure.
(defun concat (xs ys)
    (if xs
        (cons (car xs)
              (concat (cdr xs) ys))
        ys
    ))

(defun flatten (xs)
    (when xs
        (concat (car xs)
                (flatten (cdr xs))
    )))


;; P08 (**) Eliminate consecutive duplicates of list elements.
(defun compress (xs)
    (if (cdr xs)
        (if (equal (car xs) (cadr xs))
            (compress (cdr xs))
            (cons (car xs) (compress (cdr xs))))
        xs))
