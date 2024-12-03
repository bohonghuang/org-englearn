;; -*- lexical-binding: t; -*-

(require 'pdf-tools)
(require 'org-pdftools)

(require 'org-englearn)

(defcustom org-englearn-capture-pdf-view-empty-threshold 0.025
  "A distance beyond which the selected text is no longer part of the context."
  :group 'org-englearn
  :type 'float)

;;;###autoload
(defun org-englearn-capture-pdf-view ()
  (interactive)
  (org-pdftools-setup-link)
  (cl-assert (pdf-view-active-region-p))
  (let ((pdf-view-active-region (copy-tree pdf-view-active-region))
        (org-pdftools-markup-pointer-function (cl-constantly nil))
        (org-pdftools-get-desc-function (let ((function org-pdftools-get-desc-function))
                                          (lambda (&rest args) (apply function (cl-subseq args 0 2)))))
        (text (org-englearn-remove-redundant-delimiters-in-string
               (cl-first (pdf-view-active-region-text)))))
    (cl-flet ((active-region-text () (cl-first (pdf-view-active-region-text)))
              (active-region-sentence ()
                (let ((context (org-englearn-remove-redundant-delimiters-in-string
                                (cl-first (pdf-view-active-region-text)))))
                  (with-temp-buffer
                    (insert context)
                    (goto-char (point-min))
                    (let ((case-fold-search nil))
                      (re-search-forward text))
                    (let ((beg (save-excursion
                                 (condition-case nil (backward-sentence)
                                   (error (goto-char (point-min))))
                                 (point)))
                          (end (save-excursion
                                 (condition-case nil (forward-sentence)
                                   (error (goto-char (point-max))))
                                 (point))))
                      (buffer-substring-no-properties beg end))))))
      (if (string-match-p org-englearn-capture-sentence-regexp text)
          (progn
            (org-capture-string (active-region-sentence) "E")
            (org-englearn-capture-process-expression-buffer text))
        (cl-macrolet ((save-active-region-parts (&rest body)
                        (cl-with-gensyms (region original)
                          `(let* ((,region (cl-first pdf-view-active-region))
                                  (,original (cl-copy-list ,region)))
                             (unwind-protect (progn . ,body)
                               (cl-replace ,region ,original)))))
                      (search-sentence-bound
                        ((component &rest target) &optional (threshold 'org-englearn-capture-pdf-view-empty-threshold))
                        (cl-with-gensyms (region original-region previous-string string pos pos-text pos-target)
                          (let ((regexp (rx (char ?? ?! ?.) (or (+ blank) eol))))
                            `(save-active-region-parts
                              (cl-loop with ,region = (cl-first pdf-view-active-region)
                                       with ,pos-target = (,component ,region)
                                       for ,pos from (,component ,region) ,@target by 0.005
                                       for ,previous-string = (active-region-text) then ,string
                                       for ,string = (active-region-text)
                                       for ,pos-text = ,pos then (if (string= ,string ,previous-string)
                                                                     ,pos-text (setf ,pos-target ,pos))
                                       until (> (abs (- ,pos ,pos-text)) ,threshold)
                                       until (string-match-p ,regexp (active-region-text))
                                       do (setf (,component ,region) ,pos)
                                       finally (cl-return ,pos-target)))))))
          (let ((region (cl-first pdf-view-active-region))
                (y-start (search-sentence-bound (cl-second downto 0.0)))
                (y-end (search-sentence-bound (cl-fourth to 1.0))))
            (when (= (cl-shiftf (cl-second region) y-start) y-start)
              (cl-decf (cl-second region) org-englearn-capture-pdf-view-empty-threshold)
              (setf (cl-first region) (search-sentence-bound (cl-first downto 0.0)))
              (let ((text (active-region-text)))
                (save-active-region-parts
                 (setf (cl-first region) (search-sentence-bound (cl-third to 1.0))
                       (cl-second region) (search-sentence-bound (cl-second downto 0.0))
                       (cl-first region) (search-sentence-bound (cl-first downto 0.0)))
                 (when (> (length (active-region-text)) (length text))
                   (setf pdf-view-active-region (copy-tree pdf-view-active-region))))))
            (when (= (cl-shiftf (cl-fourth region) y-end) y-end)
              (cl-incf (cl-fourth region) org-englearn-capture-pdf-view-empty-threshold)
              (setf (cl-third region) (search-sentence-bound (cl-third to 1.0)))
              (let ((text (active-region-text)))
                (save-active-region-parts
                 (setf (cl-third region) (search-sentence-bound (cl-third downto 0.0))
                       (cl-fourth region) (search-sentence-bound (cl-fourth to 1.0))
                       (cl-third region) (search-sentence-bound (cl-third to 1.0)))
                 (when (> (length (active-region-text)) (length text))
                   (setf pdf-view-active-region (copy-tree pdf-view-active-region))))))
            (let ((sentence (active-region-sentence)))
              (org-capture-string sentence "E")
              (org-englearn-capture-process-vocabulary-buffer text sentence))))))))

(provide 'org-englearn-pdf-view)
;;; org-englearn-pdf-view.el ends here
