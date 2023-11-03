#lang scheme
;Tanner Benbrook
;the function to add two lists
(define (poly_add apol bpol)
   ;checks for if one list is null you should return the other
   (if (null? apol) bpol
          (if (null? bpol) apol
              ;call the function to add the first two inner lists and then call the reverse and remove_leading_zero function
              ;to take off unnecessary zero polynomials at the end
              (cons (remove_leading_zero (reverse (addition_inner_list (car apol) (car bpol)))) (poly_add (cdr apol) (cdr bpol))))))

;the function to subtract two lists
(define (poly_sub apol bpol)
     ;checks for if one list is null you should return the other
     (if (null? apol) bpol
          (if (null? bpol) apol
              ;call the function to subtract the first two inner lists and then call the reverse and remove_leading_zero function
              ;to take off unnecessary zero polynomials at the end
              (cons (remove_leading_zero (reverse (sub_inner_list (car apol) (car bpol)))) (poly_sub (cdr apol) (cdr bpol))))))

;method to do addition on each individual inner list
;implemented this before Dr. Cheng went over it in class
(define (addition_inner_list  L1 L2)
  (if (null? L1) L2
      (if (null? L2) L1
      (cons (+ (car L1) (car L2)) (addition_inner_list (cdr L1) (cdr L2))))))

;method to do subtraction on each individual inner list
(define (sub_inner_list  L1 L2)
  (if (null? L1) (map - L2)
      (if (null? L2) L1
      ;subtract the first element and recursively call
      ;for every new first element in the list
      (cons (- (car L1) (car L2)) (sub_inner_list (cdr L1) (cdr L2))))))

;method to reverse the inner lists to take off trailing zero's
(define (remove_leading_zero reverse_L)
  ;base case for if there is a sublist with all zero's
  ;you should replace that with the empty list
  (if (null? reverse_L)
      '()
      ;the reversed list is passed in and if you have a leading zero, that means the original
      ;list has a trailing zero (if the list does not have a leading zero, that means the original
      ;list does not have a trailing zero and you should just return the list).
      ;You will just recursively call cdr on the rest of the list since
      ;you do not want to return the zero's at the beginning of the reversed lists
      ;(or the end of the original list)
      (if (= (car reverse_L) 0) (remove_leading_zero(cdr reverse_L)) (reverse reverse_L))))

;this function takes the partial derivative with respect to x
(define (before_removed_null_lists L1)
  ;if the inner list you are looking at is null,
  ;then you should return the empty  list.
  (if (null? L1)
      '()
;the 0 is used as the multiplying index to compute the derivatives.
;(represents the powers of x)
(cons (shift_inner_list(mult_inner_list (car L1) 0)) (before_removed_null_lists (cdr L1)))))

;this function takes off the empty lists
(define (poly_derx apol)
  (take_off_empty (reverse (before_removed_null_lists apol))))
;function to take off empty lists
(define (take_off_empty L1)
      (if (null? (car L1)) (take_off_empty(cdr L1)) (reverse L1)))

;multiply the inner lists
;implemented this before Dr. Cheng went over it in class
(define (mult_inner_list list n)
  (if (null? list) list
  ;recursively multiply each element in the list
  (cons (* (car list) n) (mult_inner_list (cdr list) (+ n 1)))))

;shifts the multiplied elements to the left with the
;index list (symbolizing the derivative)
(define (shift_inner_list list)
  (if (null? list)
      '()
  ;return everything except the first element in sub lists
  ;(this serves as a left shift of the whole list)
  (cdr list)))
;this function multiplies two lists of polynomials
(define (poly_mul apol bpol)
  (if (null? apol)
     '()
      (if (null? bpol)
          '()
          ;this part multiplies the inner lists keeping track of the correct shifting
          ;that needs to be done. This is why the empty list is appended to the front
          ;of the recursive lists
          (poly_add (part_mult (car apol) bpol) (cons '() (poly_mul (cdr apol) bpol))))))

;this function multiplies the numbers of two inner lists
(define (part_mult L1 L2)
  (cond
    ((null? L2) '())
    (else (cons(mulup L1 (car L2)) (part_mult L1 (cdr L2))))))

;this is from Dr. Cheng
(define (smup a L1)
  (if (null? L1) '()
      (cons (* a (car L1))
            (smup a (cdr L1)))))

;this is from Dr. Cheng
(define (mulup L1 L2)
  (if (null? L1) '()
      (if (null? L2) '()
          (addition_inner_list (smup (car L1) L2)
                               (cons 0 (mulup (cdr L1) L2)
                                     )))))