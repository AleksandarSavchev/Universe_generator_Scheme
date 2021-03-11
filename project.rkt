#lang racket/gui
(require 2htdp/image)

(define-syntax cons-stream
  (syntax-rules ()
                ((cons-stream h t)
                 (cons h (delay t)))))

(define head car)

(define (tail s)
  (force (cdr s)))

(define zero '( (0 . "Хигс бозон") (0 . "W бозон") (0 . "Z бозон") (0 . "топ кварк") (0 . "топ антикварк")
                (0 . "дънен кварк") (0 . "дънен антикварк") (0 . "глуон") (0 . "тау лептон")
                (0 . "антитау лептон") (0 . "чаровен кварк") (0 . "чаровен антикварк") (0 . "фотон")
                (0 . "мюон") (0 . "анти мюон")  (0 . "позитрон") (0 . "електрон") (0 . "неутрино")
                (0 . "антинеутрино") (0 . "долен кварк") (0 . "долен антикварк") (0 . "странен кварк")
                (0 . "странен антикварк") (0 . "горен кварк") (0 . "горен антикварк")))

(define (add-to name x l)
  (cond ((null? l) l)
        ((string=? name (cdar l)) (cons (cons (+ (caar l) x) name) (cdr l)))
        (else (cons (car l) (add-to name x (cdr l))))))

(define default (add-to "Хигс бозон" 1 zero))

(define (print l)
  (define (iter l)
    (cond ((null? l) (display ";") (newline))
          ((= (caar l) 0) (iter (cdr l)))
          (else (display ", ") (display (cdar l)) (display ": ") (display (caar l)) (iter (cdr l)))))
  (cond ((null? l) (newline))
        ((= (caar l) 0) (print (cdr l)))
        (else (display (cdar l)) (display ": ") (display (caar l)) (iter (cdr l)))))

(define (image-print l)
  (cond ((null? l) (newline))
        ((= (caar l) 0) (image-print (cdr l)))
        (else (display (image (cdar l))) (image-print (add-to (cdar l) -1 l)))))

(define (get-num name l)
  (cond ((null? l) 0)
        ((string=? name (cdar l)) (caar l))
        (else (get-num name (cdr l)))))

(define (combine l1 l2)
  (if (null? l1) l1
      (cons (cons (+ (caar l1) (caar l2)) (cdar l1)) (combine (cdr l1) (cdr l2)))))

(define (make-custom-system l)
  (if (null? l) zero
      (combine (add-to (cdar l) (caar l) zero) (make-custom-system (cdr l)))))

(define (same? l1 l2)
  (if (null? l1) #t
      (and (= (caar l1) (caar l2)) (same? (cdr l1) (cdr l2)))))

(define (stable? l)
  (if (and (= (get-num "Хигс бозон" l) 0)
           (= (get-num "W бозон" l) 0)
           (= (get-num "Z бозон" l) 0)
           (= (get-num "топ кварк" l) 0)
           (= (get-num "топ антикварк" l) 0)) true
                                              false))

(define (image string)
  (cond ((string=? string "Хигс бозон") (scale 0.25 (bitmap/file "images/higgs.png")))
        ((string=? string "W бозон") (scale 0.25 (bitmap/file "images/W.png")))
        ((string=? string "Z бозон") (scale 0.25 (bitmap/file "images/Z.png")))
        ((string=? string "топ кварк") (scale 0.25 (bitmap/file "images/top.png")))
        ((string=? string "топ антикварк") (scale 0.25 (bitmap/file "images/antitop.png")))
        ((string=? string "дънен кварк") (scale 0.25 (bitmap/file "images/bottom.png")))
        ((string=? string "дънен антикварк") (scale 0.25 (bitmap/file "images/antibottom.png")))
        ((string=? string "глуон") (scale 0.25 (bitmap/file "images/gluon.png")))
        ((string=? string "тау лептон") (scale 0.25 (bitmap/file "images/tau.png")))
        ((string=? string "антитау лептон") (scale 0.25 (bitmap/file "images/antitau.png")))
        ((string=? string "чаровен кварк") (scale 0.25 (bitmap/file "images/charm.png")))
        ((string=? string "чаровен антикварк") (scale 0.25 (bitmap/file "images/anticharm.png")))
        ((string=? string "фотон") (scale 0.25 (bitmap/file "images/photon.png")))
        ((string=? string "мюон") (scale 0.25 (bitmap/file "images/muon.png")))
        ((string=? string "анти мюон") (scale 0.25 (bitmap/file "images/antimuon.png")))
        ((string=? string "позитрон") (scale 0.25 (bitmap/file "images/positron.png")))
        ((string=? string "електрон") (scale 0.25 (bitmap/file "images/electron.png")))
        ((string=? string "неутрино") (scale 0.25 (bitmap/file "images/neutrino.png")))
        ((string=? string "антинеутрино") (scale 0.25 (bitmap/file "images/antineutrino.png")))
        ((string=? string "долен кварк") (scale 0.25 (bitmap/file "images/down.png")))
        ((string=? string "долен антикварк") (scale 0.25 (bitmap/file "images/antidown.png")))
        ((string=? string "странен кварк") (scale 0.25 (bitmap/file "images/strange.png")))
        ((string=? string "странен антикварк") (scale 0.25 (bitmap/file "images/antistrange.png")))
        ((string=? string "горен кварк") (scale 0.25 (bitmap/file "images/up.png")))
        ((string=? string "горен антикварк") (scale 0.25 (bitmap/file "images/antiup.png")))))

(define (change l)
  (if (null? l) l
      (cons (cons (caar l) (image (cdar l))) (change (cdr l)))))

(define (X-colapse)
  (if (> (random 1000000) 433) zero
      (let ((x (random 1000000))
            (new (add-to "Хигс бозон" -1 zero)))
        (cond ((<= x 648000) (add-to "дънен кварк" 1 (add-to "дънен антикварк" 1 new)))
              ((and (> x 648000) (<= x 789000)) (add-to "W бозон" 2 new))
              ((and (> x 789000) (<= x 877200)) (add-to "глуон" 2 new))
              ((and (> x 877200) (<= x 947600)) (add-to "тау лептон" 1 (add-to "антитау лептон" 1 new)))
              ((and (> x 947600) (<= x 980300)) (add-to "чаровен кварк" 1 (add-to "чаровен антикварк" 1 new)))
              ((and (> x 980300) (<= x 996200)) (add-to "Z бозон" 2 new))
              ((and (> x 996200) (<= x 998430)) (add-to "фотон" 2 new))
              ((and (> x 998430) (<= x 999540)) (add-to "фотон" 1 (add-to "Z бозон" 1 new)))
              ((and (> x 999540) (<= x 999784)) (add-to "мюон" 1 (add-to "анти мюон" 1 new)))
              (else (add-to "топ кварк" 1 (add-to "топ антикварк" 1 new)))))))

(define (W-colapse)
  (if (= (random 2) 1) zero
      (let ((x (random 3))
            (new (add-to "W бозон" -1 zero)))
        (cond ((= x 1) (add-to "позитрон" 1 (add-to "неутрино" 1 new)))
              ((= x 2) (add-to "анти мюон" 1 (add-to "неутрино" 1 new)))
              (else (add-to "антитау лептон" 1 (add-to "неутрино" 1 new)))))))

(define (Z-colapse)
  (if (= (random 2) 1) zero
      (let ((x (random 1000))
            (new (add-to "Z бозон" -1 zero)))
        (cond ((<= x 206) (add-to "неутрино" 1 (add-to "антинеутрино" 1 new)))
              ((and (> x 206) (<= x 240)) (add-to "електрон" 1 (add-to "позитрон" 1 new)))
              ((and (> x 240) (<= x 274)) (add-to "мюон" 1 (add-to "анти мюон" 1 new)))
              ((and (> x 274) (<= x 308)) (add-to "тау лептон" 1 (add-to "антитау лептон" 1 new)))
              ((and (> x 308) (<= x 460)) (add-to "долен кварк" 1 (add-to "долен антикварк" 1 new)))
              ((and (> x 460) (<= x 612)) (add-to "странен кварк" 1 (add-to "странен антикварк" 1 new)))
              ((and (> x 612) (<= x 764)) (add-to "дънен кварк" 1 (add-to "дънен антикварк" 1 new)))
              ((and (> x 764) (<= x 882)) (add-to "горен кварк" 1 (add-to "горен антикварк" 1 new)))
              (else (add-to "топ кварк" 1 (add-to "чаровен кварк" 1 (add-to "чаровен антикварк" 1 new))))))))

(define (Top-colapse)
  (if (> (random 10000) 1295) zero
      (let ((x (random 3))
            (new (add-to "топ кварк" -1 zero)))
        (cond ((= x 1) (add-to "W бозон" 1 (add-to "долен кварк" 1 new)))
              ((= x 2) (add-to "W бозон" 1 (add-to "странен кварк" 1 new)))
              (else (add-to "W бозон" 1 (add-to "дънен кварк" 1 new)))))))

(define (Topanti-colapse)
  (if (> (random 10000) 1295) zero
      (let ((x (random 3))
            (new (add-to "топ антикварк" -1 zero)))
        (cond ((= x 1) (add-to "W бозон" 1 (add-to "долен антикварк" 1 new)))
              ((= x 2) (add-to "W бозон" 1 (add-to "странен антикварк" 1 new)))
              (else (add-to "W бозон" 1 (add-to "дънен антикварк" 1 new)))))))

(define (step l)
  (define (iter colapse n l)
    (if (= n 0) zero
        (combine (colapse) (iter colapse (- n 1) l))))
  (combine l (combine (iter X-colapse (get-num "Хигс бозон" l) l)
                      (combine (iter W-colapse (get-num "W бозон" l) l)
                               (combine (iter Z-colapse (get-num "Z бозон" l) l)
                                        (combine (iter Top-colapse (get-num "топ кварк" l) l)
                                                 (iter Topanti-colapse (get-num "топ антикварк" l) l)))))))

(define (stream l)
  (if (stable? l) (list l)
      (cons-stream l (stream (step l)))))

(define (read stream)
  (define (iter n stream last)
    (cond ((same? (head stream) last) (iter (+ n 1) (tail stream) last))
          ((stable? (head stream)) (sleep 0.5) (display n) (display ". ") (print (head stream)) (display "Stable state has been reached!"))
          (else (sleep 0.5) (display n) (display ". ")  (print (head stream)) (iter (+ n 1) (tail stream) (head stream)))))
  (display "1. ")
  (print (head stream))
  (if (stable? (head stream)) (display "Stable state has been reached!")
      (iter 2 (tail stream) (head stream))))

(define (image-read stream)
  (define (iter n stream last)
    (cond ((same? (head stream) last) (iter (+ n 1) (tail stream) last))
          ((stable? (head stream)) (sleep 0.5) (display n) (display ". ") (image-print (head stream)) (display "Stable state has been reached!"))
          (else (sleep 0.5) (display n) (display ". ")  (image-print (head stream)) (iter (+ n 1) (tail stream) (head stream)))))
  (display "1. ")
  (image-print (head stream))
  (if (stable? (head stream)) (display "Stable state has been reached!")
      (iter 2 (tail stream) (head stream))))

(define (simulate . ls)
  (define (iter l)
    (if (null? l) zero
        (combine (make-custom-system (car l)) (iter (cdr l)))))
  (if (null? ls) (read (stream default))
      (read (stream (iter ls)))))

(define (image-simulate . ls)
  (define (iter l)
    (if (null? l) zero
        (combine (make-custom-system (car l)) (iter (cdr l)))))
  (if (null? ls) (image-read (stream default))
      (image-read (stream (iter ls)))))

;Примерни правилно написани тестове:
;(simulate)
;(simulate '((1 . "Хигс бозон") (1 . "W бозон") (1 . "Z бозон") (1 . "топ кварк") (1 . "топ антикварк")))
;(simulate '((1 . "Хигс бозон")) '((1 . "W бозон")) '((1 . "Z бозон")) '((1 . "топ кварк")) '((1 . "топ антикварк")))
;(image-simulate)
;(image-simulate '((10 . "Хигс бозон")))
;(image-simulate '((1 . "Хигс бозон") (1 . "W бозон") (1 . "Z бозон") (1 . "топ кварк") (1 . "топ антикварк")))
;(image-simulate '((1 . "Хигс бозон")) '((1 . "W бозон")) '((1 . "Z бозон")) '((1 . "топ кварк")) '((10 . "електрон")))