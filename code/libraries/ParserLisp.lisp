(defun flatten (x)
  (cond ((null x) x)
        ((atom x) (list x))
        (t (append (flatten (first x))
                   (flatten (cdr x))))))

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
    ((eq (first list) #\{)
     (cons (cons (first list) (subobj (cdr list))) (subobj (trim (list-length (subobj (cdr list))) (cdr list)))))
    ((eq (first list) #\[)
     (cons (cons (first list) (subarray (cdr list))) (subobj (trim (list-length (subarray (cdr list))) (cdr list)))))
    ((eq (first list) #\}) 
     #\})
    (T 
     (cons (first list) (subobj (cdr list))))))

(defun subarray (list)
  (cond ((null list) nil)
	((eq (first list) #\[)
	 (cons (cons (first list) (subarray (cdr list))) (subarray (trim (list-length (subarray (cdr list))) (cdr list)))))
	((eq (first list) #\{)
	 (cons (cons (first list) (subobj (cdr list))) (subarray (trim (list-length (subobj (cdr list))) (cdr list)))))
	((eq (first list) #\]) #\])
	(T (cons (first list) (subarray (cdr list))))))

(defun unify (String)
  (cond 
    ((stringp String)
     (unify (coerce String 'list)))
    ((null String) 
     nil)
    ((eq (first String) #\")
     (list (concatenate 'string (unify-quotes (cdr String))) (unify (trim (+ (list-length (unify-quotes (cdr String))) 1) (cdr String)))))
    ((numberp (first String)) 
     (let (x String)
       (if (null (search #\. (unify-numbers (cdr x))))
	      (list (first x) (parse-integer (concatenate 'string (unify-numbers (cdr x)))) (unify (trim (list-length (unify-numbers (cdr x))) (cdr x)))))
       (list (first x) (coerce (unify-numbers (cdr x)) 'float) (unify (trim (list-length (unify-numbers (cdr x))) (cdr x))))))
    (T 
     (cons (first String) (unify (cdr String))))))

(defun unify-quotes (list)
  (cond 
    ((null list) 
     nil)
    ((eq (first list) #\") 
     nil)
     
    (T 
     (cons (first list) (unify-quotes (cdr list))))))

(defun unify-numbers (list)
  (cond 
    ((null list) 
     nil)
    ((or (numberp (first list)) (eq (first list) #\.) (eq (first list) #\E))
     (cons (first list) (unify-numbers (cdr list))))
    (T 
     nil)))

;jsonparse

(defun jsonparse (JSON)
  (cond 
    ((null JSON)
      (error "stringa json vuota"))
    ((stringp JSON)
     (let ((x (remove nil (remove #\linefeed (remove #\Tab (remove #\Space (flatten (unify JSON))))))))
       (cond 
         ((char-equal (first x) #\{)
          (cons 'JSONOBJ (jsonparse x)))
         ((char-equal (first x) #\[)
          (cons 'JSONARRAY (jsonparse x)))
         (T
          (error "stringa json non valida preconversione")))))
    ((and (char-equal (first JSON) #\{) (stringp (second JSON)) (char-equal (third JSON) #\:))
     (let (x JSON)
       (cond 
         ((and (or (stringp (fourth x)) (numberp (fourth x))) (char-equal (fifth X) #\,))
          (cons (cons (second x) (fourth x)) (jsonparse (cons #\{ (trim 5 x)))))
         ((and (or (stringp (fourth x)) (numberp (fourth x))) (char-equal (fifth X) #\}))
          (cons (second x) (fourth x)))
         ((char-equal (fourth x) #\{)
          (cons (cons (second x) (jsonparse (subobj (cdddr x)))) (jsonparse (cons #\{ (trim (1+ (list-length (subobj (cdddr x)))) x)))))
         ((char-equal (fourth x) #\[)
          (cons (cons (second x) (jsonparse (subarray (cdddr x)))) (jsonparse (cons #\{ (trim (1+ (list-length (subarray (cdddr x)))) x)))))
         (T
          (error "stringa json non valida obj")))))
    ((char-equal (first JSON) #\[)
     (let (x JSON)
       (cond 
         ((and (or (stringp (second x)) (numberp (second x)) (char-equal (third X) #\,)))
          (cons (second x) (jsonparse (cons #\[ (trim 3 x)))))
         ((and (or (stringp (second x)) (numberp (second x)) (char-equal (third X) #\])))
          (cons (second x) (jsonparse (cons #\[ (trim 3 x)))))
         ((char-equal (second x) #\{)
          (cons (jsonparse (subobj (cddr x))) (jsonparse (cons #\{ (trim (1+ (list-length (subarray (cddr x)))) x)))))
         ((char-equal (second x) #\[)
          (cons (jsonparse (subarray (cddr x))) (jsonparse (cons #\{ (trim (1+ (list-length (subobj (cddr x)))) x)))))
         ((char-equal (second x) #\])
          nil)
         (T
          (error "stringa json non valida array")))))
    ((or  (and (char-equal (first JSON) #\{) (char-equal (second JSON) #\})) 
          (and (char-equal (first JSON) #\[) (char-equal (second JSON) #\])))
        nil)
    (T
     (error "stringa json non valida totale"))))

;jsonaccess 

(defun jsonaccess (list target)
  (cond 
    ((and (eq (first list) 'JSONOBJ) (stringp (car target)))
     (cons (nth (1+ (search (car target) (flatten (cdr list)))) (cdr list)) (jsonaccess list (cdr target))))
    ((and (eq (first list) 'JSONARRAY) (integerp (car target)) (and (< (car target) (1- (list-length list))) (>= (car target) 0))))
    (cons (nth target (cdr list)) (jsonaccess list (cdr target)))
    ((or (null target) (null (cdr list)) (null list))
     nil)
    (T
     (error "impossibile effettuare jsonaccess"))))


;funzione jsonread

(defun jsonread (Path)
  (jsonparse (let (file Path)
    (with-open-file (stream file)
      (let ((contents (make-string (file-length stream))))
        (read-sequence contents stream)
        contents)))))


(defun deparseobj (list)
  (cond
    ((and (not (null (car list))) (null (cdr list)))
     (let (x list)
       (if 
        (listp (car x)) 
        (let (y x)
          (cond 
            ((eq (car (cdr (car y))) 'JSONOBJ)
             (list (car (car y)) #\: #\Space  #\{ (deparseobj (cdr (car y))) #\}))
            ((eq (car (cdr (car y))) 'JSONARRAY)
             (list (car (car y)) #\: #\Space  #\[ (deparsearray (cdr (car y))) #\}))
            (T
             (error "lista obj non valida utlima"))
            ))
        (list (car (car x)) #\: #\Space (cdr (car x)) #\}))))
    ((not (null (cdr list)))
     (let (x list)
       (if 
        (listp (car x)) 
        (let (y x)
          (cond 
            ((eq (car (cdr (car y))) 'JSONOBJ)
             (list (car (car y)) #\: #\Space  #\{ (deparseobj (cdr (car y))) #\, #\Space (deparseobj (cdr y))))
            ((eq (car (cdr (car y))) 'JSONARRAY)
             (list (car (car y)) #\: #\Space  #\[ (deparsearray (cdr (car y))) #\, #\Space (deparseobj (cdr y))))
            (T
             (error "lista obj non valida non ultima"))
            ))
        (list (car (car x)) #\: #\Space (cdr (car x)) #\}))))
    ((null list)
     #\})
    (T 
     (error "lista obj non valida totale"))
    ))

(defun deparsearray (list)
  (cond
    ((and (not (null (car list))) (null (cdr list)))
     (let (x list)
       (if 
        (listp (car x)) 
        (let (y x)
          (cond 
            ((eq (car (cdr (car y))) 'JSONOBJ)
             (list (car (car y)) #\: #\Space  #\{ (deparseobj (cdr (car y))) #\]))
            ((eq (car (cdr (car y))) 'JSONARRAY)
             (list (car (car y)) #\: #\Space  #\[ (deparsearray (cdr (car y))) #\]))
            (T
             (error "lista array non valida ultima"))
            ))
        (list (car (car x)) #\: #\Space (cdr (car x)) #\]))))
    ((not (null (cdr list)))
     (let (x list)
       (if 
        (listp (car x)) 
        (let (y x)
          (cond 
            ((eq (car (cdr (car y))) 'JSONOBJ)
             (list (car (car y)) #\: #\Space  #\{ (deparseobj (cdr (car y))) #\, #\Space (deparsearray (cdr y))))
            ((eq (car (cdr (car y))) 'JSONARRAY)
             (list (car (car y)) #\: #\Space  #\[ (deparsearray (cdr (car y))) #\, #\Space (deparsearray (cdr y))))
            (T
             (error "lista array non valida non ultima"))
            ))
        (list (car (car x)) #\: #\Space (cdr (car x)) #\}))))
    ((null list)
     #\])
    (T 
     (error "lista array non valida totale"))
    ))


(defun jsondeparser (list)
  (cond 
    ((eq 'JSONOBJ (car list))
     (cons #\{ (deparseobj (cdr list))))
    ((eq 'JSONARRAY (car list))
     (cons #\[ (deparsearray (cdr list))))
    (T
     (error "lista non valida"))))

(defun jsondump (Obj Path)
  (with-open-file (stream Path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream (concatenate 'string (jsondeparser Obj)))) Path)
