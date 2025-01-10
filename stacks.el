;;; stacks.el -- Implementing stacks in text buffers
;;  Inspired by [[https://nicholas.carlini.com/writing/2025/regex-chess.html]]. Here, stacks buffers are in org-mode.

(defun push (str &optional stack)
  (let ((stack-regexp
	 (format "* %s\n" (or stack "STACK"))))
    (with-current-buffer (get-buffer "test")
      (goto-char (point-min))
      (if (search-forward-regexp stack-regexp
				 nil
				 t)
	  (replace-match
	   (concat stack-regexp (format "%s\n" str)))
	)
      )
    )
  )

(defun pop (&optional stack)
  (let ((stack-regexp
	 (format "* %s\n" (or stack "STACK"))))
    (with-current-buffer (get-buffer "test")
      (goto-char (point-min))
      (if (search-forward-regexp
	   (concat stack-regexp
		   "\\([[:word:]]+\\)\n")
	   nil
	   t)
	  (let ((str (match-string 1)))
	    (replace-match stack-regexp)
	    str)
	nil)
      )
    )
  )

(defun get-wordreg (&optional buf)
  (with-current-buffer (get-buffer (or buf "test"))
    (goto-char (point-min))
    (if (search-forward-regexp "* WORDREG \\([[:word:]]+\\)\n"
			       nil
			       t)
	(match-string 1)
      nil)))

(defun set-wordreg (mark &optional buf)
  (with-current-buffer (get-buffer (or buf "test"))
    (goto-char (point-min))
    (if (search-forward-regexp "* WORDREG \\([[:word:]]+\\)\n"
			       nil
			       t)
	(replace-match (format "* WORDREG %s\n" mark)))))
  

;; Interpreters
(defun next ()
  (let ((cur (string-to-number (get-wordreg))))
    (set-wordreg (1+ cur))
    (with-current-buffer (get-buffer-create "dict")
      (goto-line cur)
      (goto (string-trim-right (thing-at-point 'line t)))
      )
    )
  )

(defun goto (word)
  (with-current-buffer (get-buffer-create "dict")
    (goto-char (point-min))
    (if (search-forward-regexp (format "^** %s \\([[:word:]]+\\)\n" word) nil t)
	(let ((wtype (match-string 1)))
	  (set-text-properties 0 (length wtype) nil wtype)
	  (cond
	   ((string= wtype "primitive")
	    (while (not (string= "\n" (thing-at-point 'line t)))
	      (let ((instr (string-trim-right (thing-at-point 'line t))))
		;; (with-current-buffer (get-buffer-create "*EXECTRACE*")
		;;   (insert (format "> %s\n" instr))
		;;   )
		(eval (car (read-from-string instr)))
		)
	      (forward-line)
	      )
	    ;; Should jump back to the NEXT word
	    )
	   ((string= wtype "secondary")
	    (push (get-wordreg) "CALL")
	    (set-wordreg (line-number-at-pos))
	    )
	   (t nil)
	   )
	  wtype)
      ;; Not a word, push as a constant onto stack
      (let ((res nil))
	(push word)
	res))
    )
  )

;; Example arithmetic function
(defun stck-plus ()
  (let ((x (pop))
	(y (pop)))
    (if (and x y)
	(push (+ (string-to-number x)
		 (string-to-number y))))
    )
  )

(goto "FIVE")
(next)
