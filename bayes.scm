#lang scheme
(require test-engine/scheme-tests)

;; bayes : float float float -> float
;; given the priors, figure out the posterior
(define (bayes p a<-x a<-~x)
  (/ (* a<-x p)
     (+ (* a<-x p)
        (* a<-~x
           (- 1 p)))))



(define INITIAL-RED-BAG-PROBABILITY 0.5)
(define RED-BAG-RED-CHIPS-RATIO 0.7)
(define BLUE-BAG-RED-CHIPS-RATIO 0.3)

;; calculate-bag-chances : number number -> number
;; Solution to [1].
;; Receives how many red and blue chips are taken from a bag
;; and returns the probability that it is the red bag that was chosen.
(define (calculate-bag-chances red-chips blue-chips)
  (local [;; calculate-chances : number (number -> number) number -> number
          ;; iterate every chip calculating the new bag prob.
          (define (calculate-chances bag-probability
                                     function-for-new-prob
                                     chips)
            (cond
              [(zero? chips) bag-probability]
              [else
               (calculate-chances (function-for-new-prob bag-probability)
                                  function-for-new-prob
                                  (sub1 chips))]))]
    (calculate-chances (calculate-chances INITIAL-RED-BAG-PROBABILITY
                                          (lambda (bag-probability)
                                            (bayes
                                             bag-probability
                                             (- 1 RED-BAG-RED-CHIPS-RATIO)
                                             (- 1 BLUE-BAG-RED-CHIPS-RATIO)))
                                          blue-chips)
                       (lambda (bag-probability)
                         (bayes bag-probability
                                RED-BAG-RED-CHIPS-RATIO
                                BLUE-BAG-RED-CHIPS-RATIO))
                       red-chips)))

(check-within (calculate-bag-chances 8 4) 0.96 0.01)


;; [1] http://books.google.com/books?id=1u8fYyoTzMYC&pg=PA12&lpg=PA12&source=bl&ots=6Eqy6C4nPA&sig=wqvsXAuLr9wWhI2S_6fOc8mRzgc&hl=en&ei=KCNnS8OJEpCPlAfXz7iUCg&sa=X&oi=book_result&ct=result&resnum=1&ved=0CAkQ6AEwAA#v=onepage



;;You have 5 unmarked bags with 100 beads each. Bags #1-4 contain 4 red beads and 96 black beads; bag #5 contains 7 red beads and 93 black beads. You randomly select one of the five bags and remove three beads without looking inside the bag. One is red, and the other two are black. What is the probability that you drew the beads from bag #5?

(define INITIAL-#5-BAG-PROBABILITY 0.2) ;5 bags

;; calculate-bag-chances-plus : number number number number -> number
;; calculate the odds of the bag retrieved being the #5 bag. Receives 
;; a list representing the number and type of chips (true = black, false = red)
;; the black chip ratio for the #5 bag and the black chip ratio for the rest of
;; the bags, and the #5 bag probability and the initial number of chips for every bag
(define (calculate-bag-chances-plus chips
                                    black-chip-ratio-#5
                                    black-chip-ratio-others
                                    bag-#5-probability
                                    bag-chip-count)
  (cond
    [(empty? chips) bag-#5-probability]
    [else
     (calculate-bag-chances-plus
      (rest chips)
      (calculate-new-ratio black-chip-ratio-#5
                           bag-chip-count
                           (first chips))
      (calculate-new-ratio black-chip-ratio-others
                           bag-chip-count
                           (first chips))
      (cond
        [(false? (first chips)) ;; a red chip
         (bayes bag-#5-probability
                (- 1 black-chip-ratio-#5)
                (- 1 black-chip-ratio-others))]
        [else ;; a black chip
         (bayes bag-#5-probability
                black-chip-ratio-#5
                black-chip-ratio-others)])
      (sub1 bag-chip-count))]))


(check-within (calculate-bag-chances-plus (list false false false)
                                          0.93
                                          0.96
                                          0.2
                                          100)
              0.6862 0.0001)

(check-within (calculate-bag-chances-plus (list false false
                                                false false false)
                                          0.93
                                          0.96
                                          0.2
                                          100)
              1.0 0.0001)

;; test to see if there is no diff in the order of the chips taken

(check-within (calculate-bag-chances-plus (list true true false)
                                          0.93
                                          0.96
                                          0.2
                                          100)
              0.2910 0.0001)


(check-within (calculate-bag-chances-plus (list true false true)
                                          0.93
                                          0.96
                                          0.2
                                          100)
              0.2910 0.0001)

(check-within (calculate-bag-chances-plus (list false true true)
                                          0.93
                                          0.96
                                          0.2
                                          100)
              0.2910 0.0001)

;; calculate-new-ratio : number number boolean -> number
;; calculate a new ratio based on the color of the chip
(define (calculate-new-ratio black-chip-ratio
                             bag-chip-count
                             is-a-black-chip?)
  (let* ([black-chips (* black-chip-ratio bag-chip-count)]
         [red-chips (- bag-chip-count black-chips)])
    (cond
      [(false? is-a-black-chip?)
       (- 1 (/ (sub1 red-chips) (sub1 bag-chip-count)))]
      [else
       (/ (sub1 black-chips) (sub1 bag-chip-count))])))

(check-within (calculate-new-ratio 0.7 100 true)
              0.6969 0.0001)

(check-within (calculate-new-ratio 0.7 100 false)
              0.7070 0.0001)

(test)