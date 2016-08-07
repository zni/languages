;; definition for a*c*b
(defconstant +dfa-def-acb+ '((0 1 2)
                             (#\a #\b #\c)
                             ((0 #\c 1)
                              (0 #\a 0)
                              (0 #\b 2)
                              (1 #\c 1)
                              (1 #\b 2))
                             (2)
                             0
                             -1))

(defun states (dfa)
  (first dfa))

(defun alphabet (dfa)
  (second dfa))

(defun transitions (dfa)
  (third dfa))

(defun final-states (dfa)
  (fourth dfa))

(defun initial-state (dfa)
  (fifth dfa))

(defun error-state (dfa)
  (sixth dfa))

(defun make-transition-table (dfa)
  "Generate the transition table for a given DFA definition."
  (flet ((out-state (s)
           (first s))
         (sym (s)
           (second s))
         (in-state (s)
           (third s)))
    (let* ((num-states (length (states dfa)))
           (alpha (alphabet dfa))
           (alpha-size (length alpha))
           (transition-table (make-array `(,num-states ,alpha-size)
                                         :initial-element (error-state dfa))))
      (progn
        (dolist (trans (transitions dfa))
          (setf (aref transition-table
                      (out-state trans)
                      (position (sym trans) alpha))
                (in-state trans)))
        transition-table))))

(defun next-state-gen (transition-table alphabet error-state)
  "Generates the next-state closure for the matcher. The closure takes
   a character and state."
    (flet ((translate-char (char)
             (let ((index (position char alphabet)))
               (if index
                   index
                   -1))))
      (lambda (char state)
        (let ((char-index (translate-char char)))
          (if (= char-index -1)
              error-state
              (aref transition-table state char-index))))))

(defun make-dfa-matcher (dfa)
  "Make a matcher from a DFA definition.  Returns a closure that takes
   a string to match on."
  (let* ((table (make-transition-table dfa))
         (final-states (final-states dfa))
         (initial-state (initial-state dfa))
         (error-state (error-state dfa))
         (alphabet (alphabet dfa))
         (next-state (next-state-gen table alphabet error-state)))
    (flet ((error-statep (state)
             (= state error-state))
           (final-statep (state)
             (not (null (member state final-states)))))
      (lambda (string)
        (let ((state initial-state))
          (loop for n across string
             until (error-statep state)
             do (setf state (funcall next-state n state)))
          (final-statep state))))))
