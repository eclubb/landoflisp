(defvar *nodes* '((living-room (You are in the living room.
                                A wizard is snoring loudly on the couch.))
                  (garden (You are in a beautiful garden.
                           There is a well in front of you.))
                  (attic (You are in the attic.
                          There is a giant welding torch in the corner.))))

(defvar *edges* '((living-room (garden west door) (attic upstairs ladder))
                  (garden (living-room east door))
                  (attic (living-room downstairs ladder))))

(defvar *objects* '(whiskey bucket frog chain))
(defvar *object-locations* '((whiskey living-room)
                             (bucket living-room)
                             (frog garden)
                             (chain garden)))

(defvar *location* 'living-room)

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  `(There is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
              (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
             `(You see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs)))))

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction (cdr (assoc *location* *edges*)) :key #'cadr)))
        (if next
          (progn (setf *location* (car next))
                 (look))
          '(You cannot go that way.))))

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
          (push (list object 'body) *object-locations*)
          `(You are now carrying the ,object))
         (t '(You cannot get that.))))

(defun inventory ()
    (append '(items -) (objects-at 'body *objects* *object-locations*)))

(princ (look))
