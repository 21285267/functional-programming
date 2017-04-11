#lang racket

(require srfi/1)
(require srfi/13)
(require 2htdp/image)
(require (prefix-in srfi: srfi/1))
;;images

;;
(define key(bitmap "C:/Users/User/Desktop/Folder/functional-programming/img/Key.bmp"))
(display key)

(define Zombie(bitmap "C:/Users/User/Desktop/Folder/functional-programming/img/Zombie.bmp"))
(display Zombie)
;;C:/Users/User/Desktop/Folder/functional-programming




;different objects you have in the game
(define objects '((1 "silver dagger")
                  (1 "a gold coin")
                  (1 "key")
                  (2 "gold sword")
                  (3 "boots")
                  (4 "soda")
                  (5 "water bottle")
                  (6 "bag of coins")
                  (7 "gold apple")
                  (8 "banana")))



(define monster '((8" Big foot")
                  (3 "Zombie" )))



;different rooms that you can go to 
(define descriptions '((1 "You are in the lobby.")
                       (2 "You are in the hallway.")
                       (3 "You are in a swamp.")
                       (4 "You are in the kitchen")
                       (5 "You are in the bathroom")
                       (6 "You are in the ballroom")
                       (7 "You are in the utility room")
                       (8 "You are in the box room")
                       (9 "You are in the music room")
                       (10"You are in the kings room")))

(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
;different actions the user can do an exmple is pick up different items
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define help '(((help) help) ((help) help)))
(define health '(((health) health) ((health) health)))
(define actions `(,@look ,@quit ,@pick ,@put ,@inventory,@help,@health))

;additional actions you can take to go to a specific room
(define decisiontable `((1 ((south) 2),@actions)
                        (2 ((east) 3) ((south) 6) ((west) 4) ,@actions)
                        (3 ((west) 2) ((run) 1),@actions)
                        (4 ((east) 2) ,@actions)
                        (5 ((north) 6) ((east) 7),@actions)
                        (6 ((north) 6) ((south) 5),@actions)
                        (7 ((west) 5) ((east) 9) ((south) 8),@actions)
                        (8 ((north) 7) ((south) 10) ((run) 7),@actions)
                        (9 ((west) 7),@actions)
                        (10((north) 8),@actions)))
;this is going to refference the different objects in the data base that the user will
;find in the different rooms
;;;monsterrr;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define monsterdb (make-hash))
(define inventorydb1 (make-hash))

(define (add-monster db1 id monster)
  (if (hash-has-key? db1 id)
      (let ((record (hash-ref db1 id)))
        (hash-set! db1 id (cons monster record)))
      (hash-set! db1 id (cons monster empty))))

(define (add-monsters db1)
  (for-each
   (lambda (r) 
     (add-monster db1 (first r) (second r))) monster))

(add-monsters monsterdb)

(define (display-monster db1 id)
  (when (hash-has-key? db1 id)
    (let* ((record (hash-ref db1 id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'bag)
            (printf "You are carrying ~a.\n" output)
            (printf "You can see ~a.\n" output))))))





;;;monsterrr;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define objectdb (make-hash))
(define inventorydb (make-hash))

(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

(define (add-objects db)
  (for-each
   (lambda (r) 
     (add-object db (first r) (second r))) objects))

(add-objects objectdb)

(define (display-objects db id)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'bag)
            (printf "You are carrying ~a.\n" output)
            (printf "You can see ~a.\n" output))))))

(define (remove-object-from-room db id str) ;;change the name to db
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item) 
             (printf "I don't see that item in the room!\n"))
            (else
             (printf "Added ~a to your bag.\n" (first item))
             (add-object inventorydb 'bag (first item))
             (hash-set! db id result)))))
  ;; Here I have removed the define in order to make the code shorter and remove duplicaitons
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "You are not carrying that item!\n"))
            (else
             (printf "Removed ~a from your bag.\n" (first item))
             (add-object objectdb id (first item))
             (hash-set! db 'bag result))))))
             
(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))
;; For this defenition I had to change the name to remove-object-from-room in order to make the invetory
;; database link together 
(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room inventorydb id item)))

(define (display-inventory)
  (display-objects inventorydb 'bag))

(define (slist->string l)
  (string-join (map symbol->string l)))

(define (get-directions id)
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (get-description id)
  (car (assq-ref descriptions id)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))


(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))

(define (display-description id)
  (printf "~a\n" (get-description id)))

(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (when description
      (display-description id)
      (display-objects objectdb id)
      (display-monster monsterdb id))
    ;;Displaying the monster from defenition so if define use this
    (printf "> ")
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (printf "huh? I didn't understand that!\n")
               (loop id #f))
              ((eq? response 'look)
               (get-directions id)
               (loop id #t))
              ((eq? response 'pick)
               (pick-item id input)
               (loop id #f))
              ((eq? response 'drop)
               (put-item id input)
               (loop id #f))
              ((eq? response 'inventory)
               (display-inventory)
               (loop id #f))
               ((eq? response 'help)
                ;; Displays Help text on the screen
                (display-help)
                (loop id #f))

            

                ((eq? response 'health)
                ;; Displays health text on the screen
                (display-health)
                (loop id #f))
              ((eq? response 'quit)
               (printf "So Long, and Thanks for All the Fish...\n")
               (exit)))))))


(define (display-help)
  (printf "\nHELP\n
Keywords: help drop pick look health
User ''run'' in emergency!!!!!!
"))


(define (display-health)
  (printf "\nYou have 3 Hearts\n"))

(startgame 1)
