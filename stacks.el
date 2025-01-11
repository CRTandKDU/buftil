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

(defun buftil--get-variable (var &optional buf)
  (with-current-buffer (get-buffer (or buf "test"))
    (goto-char (point-min))
    (if (search-forward-regexp
	 (concat (format "* %s " var) "\\([[:word:]]+\\)\n") nil t)
	(match-string 1)
      nil)))

(defun buftil--set-variable (var mark &optional buf)
  (with-current-buffer (get-buffer (or buf "test"))
    (goto-char (point-min))
    (if (search-forward-regexp
	 (concat (format "* %s " var) "\\([[:word:]]+\\)\n") nil t)
	(replace-match (format "* %s %s\n" var mark)))
    )
  )

(defun get-wordreg (&optional buf) (buftil--get-variable "WORDREG" buf))
(defun set-wordreg (mark &optional buf) (buftil--set-variable "WORDREG" mark buf))

(defun create-header ()
  (with-current-buffer (get-buffer-create "dict")
    ;; This is called by a primitive code in dictionary: keep calling line
    (let ((keep-data (line-number-at-pos)))
      (goto-char (point-min))
      (if (search-forward-regexp "* END\n" nil t)
	  (let ((word (instr-pop)))
	    (replace-match (format "** %s secondary\nCOLON\n* END\n" word) t)))
      (goto-line keep-data)
      )
    ))

(defun create-footer () (create-body "SEMI\n"))

(defun create-body (instr)
  (with-current-buffer (get-buffer-create "dict")
    ;; This is called by a primitive code in dictionary: keep calling line
    (let ((keep-data (line-number-at-pos)))
      (goto-char (point-min))
      (if (search-forward-regexp "* END\n" nil t)
	  (replace-match (format "%s\n* END\n" instr) t))
      (goto-line keep-data)
      )
    ))
  
(defun create-line-number ()
  (let ((lineno nil))
    (with-current-buffer (get-buffer-create "dict")
    ;; This is called by a primitive code in dictionary: keep calling line
    (let ((keep-data (line-number-at-pos)))
      (goto-char (point-min))
      (if (search-forward-regexp "* END\n" nil t)
	  (setq lineno (1- (line-number-at-pos))))
      (goto-line keep-data)
      )
    (debug lineno)
    lineno
    )
    ))

;; Interpreters: inner
(defun next ()
  
  (let ((cur (string-to-number (get-wordreg))))
    (set-wordreg (1+ cur))
    (with-current-buffer (get-buffer-create "dict")
      (goto-line cur)
      (goto (string-trim-right (thing-at-point 'line t)))
      )
    )
  )

(defun execute (word wtype wmode)
  (cond
   ;; Primitive in underlying language, emacs-lisp
   ((string= wtype "primitive")
    (while (not (string= "\n" (thing-at-point 'line t)))
      (let ((instr (string-trim-right (thing-at-point 'line t))))
	;; (with-current-buffer (get-buffer-create "*EXECTRACE*")
	;;   (insert (format "> %s\n" instr))
	;;   )
	(eval (car (read-from-string instr)))
	)
      (forward-line)
      ))
   ;; Secondary: consecutive words, one per line
   ((string= wtype "secondary")
    (push (get-wordreg) "CALL")
    (set-wordreg (line-number-at-pos)))
   ))

(defun goto (word)
  (with-current-buffer (get-buffer-create "dict")
    (goto-char (point-min))
    (if (search-forward-regexp
	 (format "^** %s \\([[:word:]]+\\)\\( [[:word:]]+\\)?\n" word) nil t)
	(let ((wtype (match-string 1))
	      (wmode (match-string 2))
	      (mode (buftil--get-variable "MODE")))
	  (set-text-properties 0 (length wtype) nil wtype)
	  (set-text-properties 0 (length wmode) nil wmode)
	  ;;
	  (cond
	   ((string= "EXECUTION" mode)
	    (execute word wtype wmode))
	   ;;
	   ((string= "COMPILE" mode)
	    (if (string= " immediate" wmode)
		(execute word wtype wmode)
	      (create-body word)))
	   ;;
	   (t nil)
	   )
	  wtype)
      ;; Not a word
      (let ((res nil)
	    (mode (buftil--get-variable "MODE")))
	(cond
	   ((string= "EXECUTION" mode)
	    (push word))
	   ;;
	   ((string= "COMPILE" mode)
	    (create-body word))
	   ;;
	   (t nil)
	   )
	res))
    )
  )

;; Interpreters: outer
(defvar buftil-stack-instruction nil "TIL Instruction stack")

(defun instr-pop ()
  (let ((res (car buftil-stack-instruction)))
    (setq buftil-stack-instruction (cdr buftil-stack-instruction))
    res))

(defun til-outer () 
  (let ((instr (instr-pop)))
    (while instr
      (set-wordreg "OUTER")
      (goto instr)
      (while (not (string= "OUTER" (get-wordreg)))
	(next)
	)
      (setq instr (instr-pop)))
    )
  )

(defun til-repl ()
  (interactive)
  (let ((inline (read-string "Enter FORTH line: ")))
    (buftil--set-variable "MODE" "EXECUTION")
    (setq buftil-stack-instruction (split-string inline))
    (til-outer)))

