(defun trim (n &rest list)
  (cond ((null list) nil)
    ((zerop n) list)
    (T (cons (trim (1- n) (cdr list))))))

(defun subobj (&rest list)
  (cond ((null list) nil)
    ((char-equal (car list) #\{)
      (cons (cons (cons (car list) (subobj (cdr list)))) (subobj (trim (list-length (subobj (cdr list)) (cdr list))))))
    ((char-equal (car list) #\[)
      (cons (cons (cons (car list) (subarray (cdr list)))) (subobj (trim (list-length (subarray (cdr list)) (cdr list))))))
    ((char-equal (car list) #\}) #\})
    (T 
      (cons (car list) (subobj (cdr list))))))

(defun subarray (&rest list)
  (cond ((null list) nil)
    ((char-equal (car list) #\[)
      (cons (cons (car list) (subarray (cdr list))) (subarray (trim (list-length (subarray (cdr list)) (cdr list))))))
    ((char-equal (car list) #\{)
      (cons (cons (car list) (subobj (cdr list))) (subarray (trim (list-length (subobj (cdr list)) (cdr list))))))
    ((char-equal (car list) #\]) #\})
    (T (cons (car list) (subarray (cdr list))))))

(defun unify (&rest list)
  (cond ((null list) nil)
  ((char-equal (car list) #\")
    (cons (cons (unify-quotes (cdr list))) (unify (trim (list-length (unify-quotes (cdr list)) (cdr list))))))
  ((numberp (car list)) 
    (cons (cons (car list) (unify-numbers (cdr list))) (unify (trim (list-length (unify-numbers (cdr list)) (cdr list))))))
  (T (cons (car list) (unify (cdr list))))))

(defun unify-quotes (&rest list)
  (cond ((null list) nil)
  ((char-equal (car list) #\") nil)
  (T (cons (car list) (unify-quotes (cdr list))))))

(defun unify-numbers (&rest list)
    (cond ((null list) nil)
    (or (numberp (car list)) (char-equal (car list) #\.) (char-equal (car list) #\E))
      (cons (car list) (unify-numbers (cdr list)))
    (T nil)))

(defun jsonparse (JSON)
  (cond 
    (stringp JSON)
      trasformala in lista e se ha '{ all'inizio concatena jsonobj
    (stringp JSON)
      trasformala in lista e se ha '[ all'inizio concatena jsonarray
    (cond 
      (and (char-equal (first JSON) #\{) (stringp (second JSON)) (char-equal (third JSON) #\:))
        (cond 
          (and (or (stringp (fourth JSON)) (numberp (fourth JSON))) (char-equal (fifth JSON) #\,))
            (cons (cons (second JSON) (fourth JSON)) (jsonparse (cons #\{ (trim 5 JSON))))
          (char-equal (fourth JSON) #\{)
            (cons (cons (second JSON) (jsonparse (subobj (cdr (third JSON)))) (jsonparse (cons #\{ (trim (+ (list-length (subobj (cdr (third JSON)))) 1) JSON)))))
          (char-equal (fourth JSON) #\[)
            (cons (cons (second JSON) (jsonparse (subarray (cdr (third JSON)))) (jsonparse (cons #\{ (trim (+ (list-length (subarray (cdr (third JSON)))) 1) JSON)))))
          (char-equal (second JSON) #\})
            nil
          T
            (error 'stringa json non valida))
      (char-equal (first JSON) #\[)
        (cond 
          (or (stringp (second JSON)) (numberp (second JSON)))
            (cons (second JSON) (jsonparse (cons #\[ (trim 3 JSON))))
          (char-equal (second JSON) #\{)
            (cons (jsonparse (subobj (cdr (second JSON)))) (jsonparse (cons #\{ (trim (+ (list-length (subarray (cdr (third JSON)))) 1) JSON))))
          (char-equal (second JSON) #\[)
            (cons (jsonparse (subarray (cdr (second JSON)))) (jsonparse (cons #\{ (trim (+ (list-length (subobj (cdr (third JSON)))) 1) JSON))))
          (char-equal (second JSON) #\])
            nil
          T
            (error 'stringa json non valida))
      T
        (error 'stringa json non valida))
  (null JSON)
    nil
  T
    (error 'stringa json non valida)))