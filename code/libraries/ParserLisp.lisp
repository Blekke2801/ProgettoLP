(defun flatten (x)
  (cond ((null x) x)
        ((atom x) (list x))
        (t (append (flatten (first x))
                   (flatten (rest x))))))

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
      (list (concatenate 'string (unify-quotes (rest String))) (unify (trim (+ (list-length (unify-quotes (rest String))) 1) (rest String))))
    (numberp (first String)) 
      (if (null (search #\. (unify-numbers (rest String)))))
        (list (first String) (coerce (unify-numbers (rest String)) 'float) (unify (trim (list-length (unify-numbers (rest String))) (rest String))))
      (list (first String) (parse-integer (concatenate 'string (unify-numbers (rest String)))) (unify (trim (list-length (unify-numbers (rest String))) (rest String))))
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
      (cons (nth (1+ (search (car target) (flatten (cdr list)))) (cdr list)) (jsonaccess list (cdr target)))
    (and (string-equal (first list) "jsonarray") (integerp (car target)) (and (< (car target) (1- (list-length list))) (>= (car target) 0)))
      (cons (nth target (cdr list)) (jsonaccess list (cdr target)))
    (or (null target) (null (cdr list)) (null list))
      nil
    T
      (error "impossibile effettuare jsonaccess")))
  
(defun file-read (file)
(with-open-file (stream file)
  (let ((contents (make-string (file-length stream))))
    (read-sequence contents stream)
    contents)))

;funzione jsonread

(defun jsonread (Path)
  (jsonparse (file-read Path)))


(defun deparseobj (list)
  (cond
    (and (not (null (car list))) (null (cdr list)))
      (if 
        (listp (car list)) 
          (cond 
            (string-equal (car (cdr (car list))) "jsonobj")
              (list (car (car list)) #\: #\Space  #\{ (deparseobj (cdr (car list))) #\})
            (string-equal (car (cdr (car list))) "jsonarray")
              (list (car (car list)) #\: #\Space  #\[ (deparsearray (cdr (car list))) #\})
            T
              error "lista non valida"
          )
      (list (car (car list)) #\: #\Space (cdr (car list)) #\}))
    (not (null (cdr list)))
      (if 
          (listp (car list)) 
            (cond 
              (string-equal (car (cdr (car list))) "jsonobj")
                (list (car (car list)) #\: #\Space  #\{ (deparseobj (cdr (car list)))  #\, #\Space (deparseobj (cdr list)))
              (string-equal (car (cdr (car list))) "jsonarray")
                (list (car (car list)) #\: #\Space  #\[ (deparsearray (cdr (car list)))  #\, #\Space (deparseobj (cdr list)))
              T
                error "lista non valida"
            )
          (list (car (car list)) #\: #\Space (cdr (car list)) #\, #\Space (deparseobj (cdr list))))
    (null list)
      #\}
    T 
     error "lista non valida"
  ))

(defun deparsearray (list)
  (cond
    (and (not (null (car list))) (null (cdr list)))
      (if 
        (listp (car list)) 
          (cond 
            (string-equal (car (car list)) "jsonobj")
              (list #\{ (deparseobj (cdr (car list))) #\])
            (string-equal (car (car list)) "jsonarray")
              (list #\[ (deparsearray (cdr (car list))) #\])
            T
              error "lista non valida"
          )
        (list (car list) #\]))
    (not (null (cdr list)))
      (if 
          (listp (car list)) 
            (cond 
              (string-equal (car (car list)) "jsonobj")
                (list #\{ (deparseobj (cdr (car list))) #\, #\Space (deparsearray (cdr list)))
              (string-equal (car (car list)) "jsonarray")
                (list #\[ (deparsearray (cdr (car list))) #\, #\Space (deparsearray (cdr list)))
              T
                error "lista non valida"
            )
        (list (car list)  #\, #\Space (deparsearray (cdr list))))
    (null list)
      #\]
    T 
     error "lista non valida"
  ))


(defun jsondeparser (list)
  (cond 
    (string-equal "jsonobj" (car list))
      (cons #\{ (deparseobj (cdr list)))
    (string-equal "jsonarray" (car list))
      (cons #\[ (deparsearray (cdr list)))
    T
      error "lista non valida"))

(defun jsondump (Obj Path)
  (with-open-file (stream Path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream (concatenate 'string (jsondeparser Obj)))) Path)