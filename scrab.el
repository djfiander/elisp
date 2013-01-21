(defvar djf-words-dictionary 
  (if (file-exists-p "~/Dropbox/scrabble-words")
      "~/Dropbox/scrabble-words"))

(defvar djf-words-buf (get-buffer-create " DJF words"))

(with-current-buffer djf-words-buf
  (insert-file-contents djf-words-dictionary nil nil nil t))

(defun djf-find-matches (pattern)
  (with-current-buffer djf-words-buf
    (list-matching-lines (concat "^" pattern "$")  0)))

(defun scrab (pattern)
  (interactive "sPattern: ")
  (djf-find-matches pattern))

(defun scrab-flush (letters)
  (interactive "sLetters: ")
  (with-current-buffer (get-buffer "*Occur*")
    (let (saved-read-only buffer-read-only)
      (unwind-protect
	  (progn
	    (setq buffer-read-only nil)
	    (flush-lines (concat "\\([" letters "]\\).*\\1")))
	(setq buffer-read-only saved-read-only)))))

(provide 'scrab)
