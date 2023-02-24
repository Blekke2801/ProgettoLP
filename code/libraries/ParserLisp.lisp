(defun trim (n list)
  (cond 
    (null list) 
      nil
    (zerop n)  
      list
    T 
      (cons (trim (1- n) (rest list)))))

(defun subobj (list)
  (cond 
      (null list) 
        nil
      (char-equal (first list) #\{)
        (cons (cons (cons (first list) (subobj (rest list)))) (subobj (trim (list-length (subobj (rest list)) (rest list)))))
      (char-equal (first list) #\[)
        (cons (cons (cons (first list) (subarray (rest list)))) (subobj (trim (list-length (subarray (rest list)) (rest list)))))
      (char-equal (first list) #\}) 
        #\}
      T 
        (cons (first list) (subobj (rest list)))))

(defun subarray (list)
  (cond ((null list) nil)
    ((char-equal (first list) #\[)
      (cons (cons (first list) (subarray (rest list))) (subarray (trim (list-length (subarray (rest list)) (rest list))))))
    ((char-equal (first list) #\{)
      (cons (cons (first list) (subobj (rest list))) (subarray (trim (list-length (subobj (rest list)) (rest list))))))
    ((char-equal (first list) #\]) #\})
    (T (cons (first list) (subarray (rest list))))))

(defun unify (String)
  (cond 
    (stringp String)
      (unify (coerce String 'list))
    (null String) 
      nil
    (char-equal (first String) #\")
      (list (coerce (unify-quotes (rest String)) 'string) (unify (trim (+ (list-length (unify-quotes (rest String))) 1) (rest String))))
    (numberp (first String)) 
      (list (first String) (coerce (unify-numbers (rest String)) 'float) (unify (trim (list-length (unify-numbers (rest String))) (rest String))))
    T 
      (cons (first String) (unify (rest String)))))

(defun unify-quotes (list)
  (cond 
    (null list) 
      nil
    (char-equal (first list) #\") 
      nil
    T 
      (cons (first list) (unify-quotes (rest list)))))

(defun unify-numbers (list)
    (cond 
      (null list) 
        nil
      (or (numberp (first list)) (char-equal (first list) #\.) (char-equal (first list) #\E))
        (cons (first list) (unify-numbers (rest list)))
      T 
        nil))

(defun jsonparse (JSON)
  (cond 
    (stringp JSON)
      (let (x (remove #\linefeed (remove #\Tab (remove #\Space (unify JSON)))))
        (cond 
          (char-equal (first x) #\{)
            (cons "jsonobj" (jsonparse x))
          (char-equal (first x) #\[)
            (cons "jsonarray" (jsonparse x))
          T
            (error "stringa json non valida")))
    (and (char-equal (first JSON) #\{) (stringp (second JSON)) (char-equal (third JSON) #\:))
      (cond 
        (and (or (stringp (fourth JSON)) (numberp (fourth JSON))) (char-equal (fifth JSON) #\,))
          (cons (cons (second JSON) (fourth JSON)) (jsonparse (cons #\{ (trim 5 JSON))))
        (char-equal (fourth JSON) #\{)
          (cons (cons (second JSON) (jsonparse (subobj (rest (third JSON)))) (jsonparse (cons #\{ (trim (1+ (list-length (subobj (rest (third JSON))))) JSON)))))
        (char-equal (fourth JSON) #\[)
          (cons (cons (second JSON) (jsonparse (subarray (rest (third JSON)))) (jsonparse (cons #\{ (trim (1+ (list-length (subarray (rest (third JSON))))) JSON)))))
        T
          (error "stringa json non valida"))
    (char-equal (first JSON) #\[)
      (cond 
        (or (stringp (second JSON)) (numberp (second JSON)))
          (cons (second JSON) (jsonparse (cons #\[ (trim 3 JSON))))
        (char-equal (second JSON) #\{)
          (cons (jsonparse (subobj (rest (second JSON)))) (jsonparse (cons #\{ (trim (1+ (list-length (subarray (rest (third JSON))))) JSON))))
        (char-equal (second JSON) #\[)
          (cons (jsonparse (subarray (rest (second JSON)))) (jsonparse (cons #\{ (trim (1+ (list-length (subobj (rest (third JSON))))) JSON))))
        (char-equal (second JSON) #\])
          nil
        T
          (error "stringa json non valida"))
    (or (and (char-equal (first JSON) #\{) (char-equal (second JSON) #\})) (and (char-equal (first JSON) #\[) (char-equal (second JSON) #\])) (null JSON))
    nil
  T
    (error "stringa json non valida")))

  (defun jsonaccess (list &rest target)
    (cond 
      (and (string-equal (first list) "jsonobj") (stringp (car target)))
        (apply )
      (and (string-equal (first list) "jsonarray") (integerp (car target)) (and (< (car target) (1- (list-length list))) (>= (car target) 0)))

      ))