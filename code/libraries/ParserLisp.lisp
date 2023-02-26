(defun flatten (x)
  (cond ((null x) x)
        ((atom x) (list x))
        (t (append (flatten (first x))
                   (flatten (rest x))))))

(defun trim (n list)
  (cond 
    ((null list) 
      nil)
    ((zerop n)  
      list)
    ((< n (list-length list)) 
      (trim (1- n) (cdr list)))
    (T
      (error "errore trim"))))

(defun subobj (list)
  (cond 
      ((null list) 
        nil)
      ((char-equal (first list) #\{)
        (cons (cons (first list) (subobj (rest list))) (subobj (trim (list-length (subobj (rest list))) (rest list)))))
      ((char-equal (first list) #\[)
        (cons (cons (first list) (subarray (rest list))) (subobj (trim (list-length (subarray (rest list))) (rest list)))))
      ((char-equal (first list) #\}) 
        #\})
      (T 
        (cons (first list) (subobj (rest list))))))

(defun subarray (list)
  (cond ((null list) nil)
    ((char-equal (first list) #\[)
      (cons (cons (first list) (subarray (rest list))) (subarray (trim (list-length (subarray (rest list))) (rest list)))))
    ((char-equal (first list) #\{)
      (cons (cons (first list) (subobj (rest list))) (subarray (trim (list-length (subobj (rest list))) (rest list)))))
    ((char-equal (first list) #\]) #\])
    (T (cons (first list) (subarray (rest list))))))

(defun unify (String)
  (cond 
    ((stringp String)
      (unify (coerce String 'list)))
    ((null String) 
      nil)
    ((char-equal (first String) #\")
      (list (concatenate 'string (unify-quotes (rest String))) (unify (trim (+ (list-length (unify-quotes (rest String))) 1) (rest String)))))
    ((numberp (first String)) 
    (let (x String)
      (if (null (search #\. (unify-numbers (rest x))))
        (list (first x) (coerce (unify-numbers (rest x)) 'float) (unify (trim (list-length (unify-numbers (rest x))) (rest x))))
      (list (first x) (parse-integer (concatenate 'string (unify-numbers (rest x)))) (unify (trim (list-length (unify-numbers (rest x))) (rest x)))))))
    (T 
      (cons (first String) (unify (rest String))))))

(defun unify-quotes (list)
  (cond 
    ((null list) 
      nil)
    ((char-equal (first list) #\") 
      nil)
    (T 
      (cons (first list) (unify-quotes (rest list))))))

(defun unify-numbers (list)
    (cond 
      ((null list) 
        nil)
      ((or (numberp (first list)) (char-equal (first list) #\.) (char-equal (first list) #\E))
        (cons (first list) (unify-numbers (rest list))))
      (T 
        nil)))

(defun jsonparse (JSON)
  (cond 
    ((stringp JSON)
      (let ((x (remove #\linefeed (remove #\Tab (remove #\Space (unify JSON))))))
        (cond 
          ((char-equal (first x) #\{)
            (cons "jsonobj" (jsonparse x)))
          ((char-equal (first x) #\[)
            (cons "jsonarray" (jsonparse x)))
          (T
            (error "stringa json non valida")))))
    ((and (char-equal (first JSON) #\{) (stringp (second JSON)) (char-equal (third JSON) #\:))
    (let (x JSON)
      (cond 
        ((and (or (stringp (fourth x)) (numberp (fourth x))) (char-equal (fifth X) #\,))
          (cons (cons (second x) (fourth x)) (jsonparse (cons #\{ (trim 5 x)))))
        ((char-equal (fourth x) #\{)
          (cons (cons (second x) (jsonparse (subobj (rest (third x))))) (jsonparse (cons #\{ (trim (1+ (list-length (subobj (rest (third x))))) x)))))
        ((char-equal (fourth x) #\[)
          (cons (cons (second x) (jsonparse (subarray (rest (third X))))) (jsonparse (cons #\{ (trim (1+ (list-length (subarray (rest (third x))))) x)))))
        (T
          (error "stringa json non valida")))))
    ((char-equal (first JSON) #\[)
      (let (x JSON)
        (cond 
          ((or (stringp (second x)) (numberp (second JSON)))
            (cons (second x) (jsonparse (cons #\[ (trim 3 x)))))
          ((char-equal (second x) #\{)
            (cons (jsonparse (subobj (rest (second x)))) (jsonparse (cons #\{ (trim (1+ (list-length (subarray (rest (third x))))) x)))))
          ((char-equal (second x) #\[)
            (cons (jsonparse (subarray (rest (second x)))) (jsonparse (cons #\{ (trim (1+ (list-length (subobj (rest (third x))))) x)))))
          ((char-equal (second x) #\])
            nil)
          (T
            (error "stringa json non valida")))))
    ((or (and (char-equal (first JSON) #\{) (char-equal (second JSON) #\})) (and (char-equal (first JSON) #\[) (char-equal (second JSON) #\])) (null JSON))
      nil)
  (T
    (error "stringa json non valida"))))

(defun jsonaccess (list &rest target)
  (cond 
    ((and (string-equal (first list) "jsonobj") (stringp (car target)))
      (cons (nth (1+ (search (car target) (flatten (cdr list)))) (cdr list)) (jsonaccess list (cdr target))))
    ((and (string-equal (first list) "jsonarray") (integerp (car target)) (and (< (car target) (1- (list-length list))) (>= (car target) 0))))
      (cons (nth target (cdr list)) (jsonaccess list (cdr target)))
    ((or (null target) (null (cdr list)) (null list))
      nil)
    (T
      (error "impossibile effettuare jsonaccess"))))
  
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
    ((and (not (null (car list))) (null (cdr list)))
      (let (x list)
        (if 
          (listp (car x)) 
            (let (y x)
              (cond 
                ((string-equal (car (cdr (car y))) "jsonobj")
                  (list (car (car y)) #\: #\Space  #\{ (deparseobj (cdr (car y))) #\}))
                ((string-equal (car (cdr (car y))) "jsonarray")
                  (list (car (car y)) #\: #\Space  #\[ (deparsearray (cdr (car y))) #\}))
                (T
                  (error "lista non valida"))
              ))
        (list (car (car x)) #\: #\Space (cdr (car x)) #\}))))
    ((not (null (cdr list)))
      (let (x list)
        (if 
          (listp (car x)) 
            (let (y x)
              (cond 
                ((string-equal (car (cdr (car y))) "jsonobj")
                  (list (car (car y)) #\: #\Space  #\{ (deparseobj (cdr (car y))) #\, #\Space (deparseobj (cdr y))))
                ((string-equal (car (cdr (car y))) "jsonarray")
                  (list (car (car y)) #\: #\Space  #\[ (deparsearray (cdr (car y))) #\, #\Space (deparseobj (cdr y))))
                (T
                  (error "lista non valida"))
              ))
        (list (car (car x)) #\: #\Space (cdr (car x)) #\}))))
    ((null list)
      #\})
    (T 
     (error "lista non valida"))
  ))

(defun deparsearray (list)
  (cond
    ((and (not (null (car list))) (null (cdr list)))
      (let (x list)
        (if 
          (listp (car x)) 
            (let (y x)
              (cond 
                ((string-equal (car (cdr (car y))) "jsonobj")
                  (list (car (car y)) #\: #\Space  #\{ (deparseobj (cdr (car y))) #\]))
                ((string-equal (car (cdr (car y))) "jsonarray")
                  (list (car (car y)) #\: #\Space  #\[ (deparsearray (cdr (car y))) #\]))
                (T
                  (error "lista non valida"))
              ))
        (list (car (car x)) #\: #\Space (cdr (car x)) #\]))))
    ((not (null (cdr list)))
      (let (x list)
        (if 
          (listp (car x)) 
            (let (y x)
              (cond 
                ((string-equal (car (cdr (car y))) "jsonobj")
                  (list (car (car y)) #\: #\Space  #\{ (deparseobj (cdr (car y))) #\, #\Space (deparsearray (cdr y))))
                ((string-equal (car (cdr (car y))) "jsonarray")
                  (list (car (car y)) #\: #\Space  #\[ (deparsearray (cdr (car y))) #\, #\Space (deparsearray (cdr y))))
                (T
                  (error "lista non valida"))
              ))
        (list (car (car x)) #\: #\Space (cdr (car x)) #\}))))
    ((null list)
      #\])
    (T 
     (error "lista non valida"))
  ))


(defun jsondeparser (list)
  (cond 
    ((string-equal "jsonobj" (car list))
      (cons #\{ (deparseobj (cdr list))))
    ((string-equal "jsonarray" (car list))
      (cons #\[ (deparsearray (cdr list))))
    (T
      (error "lista non valida"))))

(defun jsondump (Obj Path)
  (with-open-file (stream Path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream (concatenate 'string (jsondeparser Obj)))) Path)