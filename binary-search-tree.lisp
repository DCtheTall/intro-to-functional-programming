;; Binary search tree functions
;; On nodes of the form (value left-child right-child)


;; Create a new node with a particular value
(defun node (val)
  (cons val (cons nil (cons nil nil))))


;; Select left child of a node
(defun left-child (n) (car (cdr n)))


;; Select right child of a node
(defun right-child (n) (car (cdr (cdr n))))


;; Compare two trees to see if they are equal
(defun bst-equal (tree1 tree2)
  (cond ((null tree1) (null tree2))
        ((null tree2) nil)
        ((eq (car tree1) (car tree2))
          (and (bst-equal (left-child tree1) (left-child tree2))
               (bst-equal (right-child tree1) (right-child tree2))))
        (t nil)))


;; Returns if a tree contains a subtree
(defun bst-contains (subtree tree)
  (cond ((null subtree) t)
        ((null tree) nil)
        ((eq (car subtree) (car tree))
          (bst-equal subtree tree))
        ((< (car subtree) (car tree))
          (bst-equal subtree (left-child tree)))
        (t (bst-equal subtree (right-child tree)))))


;; Search a BST for a value
(defun bst-search (value tree)
  (cond ((null tree) nil)
        ((eq value (car tree)) t)
        ((< value (car tree))
          (bst-search value
                      (left-child tree)))
        (t (bst-search value
                       (right-child tree)))))


;; Concat two lists
(defun concat (left right)
  (if (null left)
      right
      (cons (car left) (concat (cdr left) right))))


;; Construct a list from a binary tree's values in order
(defun bst-in-order (n)
  (if (null n)
      nil
      (concat (bst-in-order (left-child n))
              (concat (cons (car n) nil)
                      (bst-in-order (right-child n))))))


;; Invert a binary tree
(defun bst-invert (n)
  (if (null n)
      nil
      (cons (car n)
            (cons (bst-invert (right-child n))
                  (cons (bst-invert (left-child n)) nil)))))


;; Create a list of a binary tree's values in descending order
(defun bst-desc-order (n) (bst-inorder (bst-invert n)))
