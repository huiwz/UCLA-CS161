;;INPUT:Given a list TREE
;;OUTPUT:return the traversal path of depth first search
;;from left to right
;;This function uses the normal DFS recursion strategy, starting
;;from left side to right side
(defun DFS (TREE)
  (cond ((null TREE) nil)
	((atom TREE) (cons TREE nil))
	(t (append (DFS (car TREE)) (DFS (cdr TREE))))
  )
)

;;INPUT:Given a list TREE
;;OUTPUT:return a list consists of all the atom elements in TREE
;;This function will go through each single element in the list
;;and test if it's an atom
(defun getElement (TREE)
  (cond ((null TREE) nil)
	((atom (car TREE)) (cons (car TREE) (getElement (cdr TREE))))
	(t (getElement (cdr TREE)))
   )
)

;;INPUT:Given an input TREE
;;OUTPUT: if it's an atom, return the list of this single atom
;;if it's a list, return this list
(defun openList (TREE)
  (cond ((atom TREE) (cons TREE nil))
	((listp TREE) TREE)
   )
)

;;INPUT:Given a list TREE and depth to search
;;OUTPUT:which returns the search order with a given depth
;;This function apply the usual DFS approach, except that
;;it ends when the variable depth hits zero
(defun varDFS (TREE depth)
  (cond ((null TREE) nil)
	((<= depth 1) TREE)
	((atom TREE) (cons TREE nil))
	(t (varDFS (append (openList (car TREE)) (varDFS (cdr TREE) 2)) (- depth 1)))
   )
)

(varDFS '((A (B)) C (D)) 0)

(varDFS '(A (B C) (D) (E (F G))) 3)

;;INPUT:Given a tree TREE and the Depth factor depth
;;OUTPUT:Return the top-level list of the terminal nodes in the
;;order that they would be visited by a left-to-right depth-first
;;iterative-deepening search
;;This function will recursively varDFS and append all the resulting
;;lists together
(defun DFID (TREE depth)
  (cond ((< depth 1) nil)
	(t (append (DFID TREE (- depth 1)) (getElement(varDFS TREE depth))))
	)
  )

(DFID '(a (b c) (d) (e (f g))) 2)

; FINAL-STATE takes a single argument (S), the current state, and returns T if
; it is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (cond ((and (equal (car s) 3) (and (equal (cadr s) 3) (equal (car (cddr s)) nil))) t)
	(t nil)
   )
  )

(final-state '(t))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), a number of
; missionaries to move (M), and a number of cannibals to move (C). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (let* ((missionary (- (car s) m))
	 (cannibal (- (cadr s) c))
	 (side (car (cddr s)))
	 )
    (cond ((> 0 missionary) nil)
	  ((> 0 cannibal) nil)
	  ((and (> cannibal missionary) (not (equal missionary 0))) nil)
	  ((and (< cannibal missionary) (not (equal missionary 3))) nil)
	  (t (cons (list missionary cannibal (not side)) nil))
     )
      )
  )

(next-state '(3 3 nil) 3 0)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.
(defun succ-fn (s)
  (append (append (next-state (s 2 0)) (next-state (s 0 2))) (append (append (next-state (s 1 0)) (next-state (s 1 1))) (append (next-state (s 0 1)) (next-state (s 1 1)))))
  )

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by MC-DFS (STATES). It returns T if S is a member of
; STATES and NIL otherwise.
(defun on-path (s states)
  (cond ((null states) nil)
	(t (or (equal (car states) s) (on-path s (cdr states))))
   )
  )

(on-path '(1 1 t) '((1 2 t) (2 2 t) (3 3 t)))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: the path
; from from the initial state to the current state (PATH), and the legal
; successor states to the last state on PATH (STATES). PATH is a first-in
; first-out list of states; that is, the first element is the initial state for
; the current search and the last element is the most recent state explored.
; MULT-DFS does a depth-first search on each element of STATES in turn. If any
; of those searches reaches the final state, MULT-DFS returns the complete path
; from the initial state to the goal state. Otherwise, it returns NIL.
(defun mult-dfs (states path)
  (cond ((null states) nil)
	((mc-dfs (car states) path) (mc-dfs (car states) path))
	(t (mult-dfs (cdr states) path))
   )
  )

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond ((final-state s) (append path (list s)))
	((not (on-path s path)) (append path (list s)))
	(t nil)
   )
  )

