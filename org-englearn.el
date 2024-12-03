;;; org-englearn.el --- English learning workflow with Org -*- lexical-binding: t -*-

;; Commentary:
;; This package provides an efficient English learning workflow, combining with org-mode, org-capture, org-roam.

(require 'cl-lib)
(require 'rx)
(require 'hydra)
(require 'go-translate)
(require 'org)
(require 'org-capture)
(require 'org-roam)

(defcustom org-englearn-file-meanings (expand-file-name "org-roam/english-learning/meanings.org" org-directory)
  "Path to the file for English meanings."
  :group 'org-englearn
  :type 'file)

(defcustom org-englearn-file-inbox (expand-file-name "org-capture/english.org" org-directory)
  "Path to the file as a inbox of English vocabulary and difficult sentences."
  :group 'org-englearn
  :type 'file)

(defcustom org-englearn-file-vocabularies (expand-file-name "org-roam/english-learning/vocabularies.org" org-directory)
  "Path to the file for English vocabularies."
  :group 'org-englearn
  :type 'file)

(defcustom org-englearn-file-expressions (expand-file-name "org-roam/english-learning/expressions.org" org-directory)
  "Path to the file for English expressions."
  :group 'org-englearn
  :type 'file)

(defcustom org-englearn-translation-engines (make-instance 'gt-youdao-dict-engine)
  "The translation engines used by `go-translate'."
  :group 'org-englearn
  :type 'sexp)

(defun org-englearn-trim-string (string)
  (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" string))

(defun org-englearn-fill-heading ()
  (if (region-active-p)
      (atomic-change-group
        (let* ((sentence-begin (region-beginning))
               (sentence-end (region-end))
               last-mark
               (first-loop t))
          (goto-char sentence-begin)
          (while (progn
                   (when (not (eq (point) last-mark))
                     (set-mark (setq last-mark (point)))
                     (forward-sexp)
                     (let* ((phrase (buffer-substring-no-properties (region-beginning) (region-end)))
                            (element (cl-ecase (car (read-multiple-choice
                                                     "What's the form of this word in heading?"
                                                     `((?s "n.(sth./sb./sp.)")
                                                       (?n "n.(noun)")
                                                       (?v "v.(verb)")
                                                       (?p "prep.")
                                                       (?k ,phrase)
                                                       (?. "...")
                                                       (?  "(empty)"))))
                                       (?s (cl-ecase (car (read-multiple-choice "Some?"
                                                                                '((?t "sth.")
                                                                                  (?p "sp.")
                                                                                  (?b "sb.")
                                                                                  (?o "one's")
                                                                                  (?s "oneself"))))
                                             (?t "=sth.=")
                                             (?p "=sp.=")
                                             (?b "=sb.=")
                                             (?o "=one's=")
                                             (?s "=oneself=")))
                                       (?n "=n.=")
                                       (?v "=v.=")
                                       (?p "=prep.=")
                                       (?k phrase)
                                       (?. "=...=")
                                       (?  "")))
                            (insertion (concat (if (or (string-empty-p element) first-loop)
                                                   "" " ")
                                               element)))
                       (save-excursion
                         (org-back-to-heading)
                         (end-of-line)
                         (insert insertion)
                         (let ((inc (length insertion)))
                           (cl-incf sentence-begin inc)
                           (cl-incf sentence-end inc)
                           (cl-incf last-mark inc))))
                     (ignore-errors
                       (forward-sexp)
                       (backward-sexp))
                     (setq first-loop nil)
                     (and (< (point) sentence-end)))))
          (deactivate-mark)
          (goto-char sentence-end)
          (insert "_")
          (goto-char sentence-begin)
          (insert "_")))
    (error "No mark set!")))

(defun org-englearn-move-capture-timestamp ()
  (org-back-to-heading)
  (forward-line 1)
  (let ((beg (point))
        timestamp)
    (end-of-line)
    (setq timestamp (buffer-substring-no-properties beg (point)))
    (delete-region beg (point))
    (delete-char 1)
    (goto-char (point-max))
    (forward-line -1)
    (end-of-line)
    (insert " " timestamp)))

(defun org-englearn-remove-redundant-delimiters-in-string (string)
  (replace-regexp-in-string "[[:space:]\n]+" " " string))

(defun org-englearn-translate-to-kill-ring (text)
  (gt-start
   (make-instance
    'gt-translator
    :taker (make-instance
            'gt-taker
            :text (org-englearn-remove-redundant-delimiters-in-string text)
            :langs '(en zh))
    :engines org-englearn-translation-engines
    :render (make-instance 'gt-kill-ring-render))))

;;;###autoload
(defun org-englearn-org-roam-node-insert ()
  (interactive)
  (setq-local org-roam-capture-templates `(("m" "Meaning"
                                            entry "* ${title}" :if-new (file ,org-englearn-file-meanings) :unnarrowed t :immediate-finish t)))
  (add-hook 'org-capture-after-finalize-hook #'org-englearn-capture-heading-by-id-hook -5 t)
  (unwind-protect (org-roam-node-insert)
    (remove-hook 'org-capture-after-finalize-hook #'org-englearn-capture-heading-by-id-hook t)
    (kill-local-variable 'org-roam-capture-templates)))

(defun org-englearn-capture-process-vocabulary-buffer (text context)
  (search-forward text)
  (set-mark (- (point) (length text)))
  (org-englearn-translate-to-kill-ring context)
  (activate-mark)
  (org-englearn-fill-heading)
  (org-englearn-move-capture-timestamp)
  (goto-char (point-max))
  (forward-line -2)
  (end-of-line)
  (insert " \\\\")
  (forward-line 1)
  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command)
  (yank)
  (org-back-to-heading)
  (forward-line 1)
  (indent-for-tab-command)
  (insert "- ")
  (let ((beg (point)))
    (forward-line 1)
    (beginning-of-line)
    (delete-char 1)
    (insert "-")
    (org-indent-item)
    (goto-char beg)
    (org-englearn-org-roam-node-insert)
    (beginning-of-line)
    (re-search-forward (rx line-start (* space) "- "))
    (pcase (completing-read "What's its part?" '("adj." "adv." "n." "v." "vt." "vi." "prep.")
                            nil nil nil nil nil)
      (`"adv." (insert "adv. ")
       (end-of-line)
       (insert "地"))
      (`"adj." (insert "adj. ")
       (end-of-line)
       (insert "的"))
      (x (insert x " ")))))

(defun org-englearn-capture-process-expression-buffer (text)
  (org-englearn-move-capture-timestamp)
  (org-back-to-heading)
  (delete-blank-lines)
  (search-forward text)
  (beginning-of-line)
  (delete-horizontal-space)
  (insert "- " )
  (org-englearn-translate-to-kill-ring text)
  (let ((title (read-string "Title: ")))
    (end-of-line)
    (insert " \\\\")
    (forward-line 1)
    (beginning-of-line)
    (open-line 1)
    (indent-for-tab-command)
    (yank)
    (org-back-to-heading)
    (end-of-line)
    (insert title)))

;;;###autoload
(defun org-englearn-capture-process-region (&optional beg end)
  (interactive)
  (let* ((beg (or beg (if (region-active-p) (region-beginning) (mark))))
         (end (or end (if (region-active-p) (region-end) (point))))
         (_ (goto-char beg))
         (cap (buffer-substring-no-properties beg end))
         (beg (save-excursion
                (unless (> (point) (mark)) (exchange-point-and-mark))
                (backward-sentence)
                (point)))
         (end (save-excursion
                (unless (< (point) (mark)) (exchange-point-and-mark))
                (forward-sentence)
                (point)))
         (sentence (buffer-substring-no-properties beg end)))
    (org-englearn-capture-process-vocabulary-buffer cap sentence)))

(defun org-englearn-capture-heading-by-id-hook ()
  (when-let* ((roam-capture (org-roam-capture-p))
              (roam-list (plist-get org-capture-plist :org-roam))
              (ins-link (eq (plist-get roam-list :finalize) 'insert-link)))
    (with-current-buffer (org-capture-get :buffer)
      (goto-char (point-min))
      (re-search-forward (rx line-start "* " (literal (plist-get roam-list :link-description)) (* space) line-end))
      (org-roam-capture--put :id (org-id-get-create)))))

(cl-pushnew
 `("E" "English Vocabulary" entry (file ,org-englearn-file-inbox) "* %?\n%U\n\n  %i\n  %a" :kill-buffer t)
 org-capture-templates :test #'string= :key #'car)

(defconst org-englearn-capture-sentence-regexp (rx (or ?? ?. ?!) (optional ?\") (* space) line-end))

;;;###autoload
(defun org-englearn-capture (&optional beg end)
  (interactive)
  (let* ((beg (or beg (if (region-active-p) (region-beginning) (mark))))
         (end (or end (if (region-active-p) (region-end) (point))))
         (cap (org-englearn-remove-redundant-delimiters-in-string (buffer-substring-no-properties beg end))))
    (if (string-match-p org-englearn-capture-sentence-regexp cap)
        (progn
          (org-capture-string cap "E")
          (org-englearn-capture-process-expression-buffer cap))
      (let* ((beg (save-excursion
                    (unless (> (point) (mark)) (exchange-point-and-mark))
                    (backward-sentence)
                    (point)))
             (end (save-excursion
                    (unless (< (point) (mark)) (exchange-point-and-mark))
                    (forward-sentence)
                    (point)))
             (sentence (org-englearn-remove-redundant-delimiters-in-string (buffer-substring-no-properties beg end))))
        (deactivate-mark)
        (org-capture-string sentence "E")
        (org-englearn-capture-process-vocabulary-buffer cap sentence)))))

;;;###autoload
(defun org-englearn-process-inbox ()
  (interactive)
  (find-file org-englearn-file-inbox)
  (org-map-entries
   (lambda ()
     (setq org-map-continue-from (org-element-property
                                  :begin
                                  (org-element-at-point)))
     (org-narrow-to-element)
     (org-fold-show-subtree)
     (cl-ecase (cl-first (read-multiple-choice
                          "Which category does it belong to?"
                          '((?v "vocabulary")
                            (?e "expression"))))
       (?v
        (save-excursion
          (org-back-to-heading)
          (forward-line 1)
          (cl-assert (org-list-has-child-p (line-beginning-position) (org-list-struct))))
        (with-current-buffer
            (prog1 (find-file-noselect
                    (if-let* ((title (cl-fifth (org-heading-components)))
                              (node (org-roam-node-from-title-or-alias title)))
                        (org-roam-node-file node)
                      org-englearn-file-vocabularies))
              (org-cut-subtree))
          (goto-char (point-max))
          (yank)
          (org-englearn-process-new-heading)
          (org-id-get-create)))
       (?e
        (save-excursion
          (org-back-to-heading)
          (forward-line 1)
          (cl-assert (not (org-list-has-child-p (line-beginning-position) (org-list-struct)))))
        (with-current-buffer
            (prog1 (find-file-noselect org-englearn-file-expressions)
              (org-cut-subtree))
          (goto-char (point-max))
          (yank)
          (org-englearn-process-new-heading))))
     (widen))))

(defun org-englearn-process-new-heading ()
  (let ((titles (org-map-entries (lambda () (cl-fifth (org-heading-components)))))
        (title (cl-fifth (org-heading-components)))
        (item-title nil)
        (item nil))
    (when (cl-find title titles :test #'string-equal-ignore-case)
      (org-back-to-heading)
      (re-search-forward (rx line-start (* space) "- "))
      (save-excursion
        (let ((beg (point)))
          (end-of-line)
          (setq item-title (org-englearn-trim-string (buffer-substring-no-properties beg (point))))))
      (backward-char 2)
      (org-mark-element)
      (setq item (buffer-substring-no-properties (mark) (point)))
      (deactivate-mark)
      (org-cut-subtree)
      (condition-case nil
          (progn
            (re-search-backward (rx "* " (literal title) (* space) line-end))
            (save-restriction
              (org-narrow-to-subtree)
              (condition-case nil
                  (progn
                    (re-search-forward (rx line-start (* space) "- " (literal item-title)))
                    (cl-assert (org-list-has-child-p (line-beginning-position) (org-list-struct)))
                    (widen)
                    (org-end-of-item)
                    (let ((insert-point (point)))
                      (insert item)
                      (goto-char insert-point)
                      (forward-line 1)
                      (beginning-of-line)
                      (delete-region insert-point (point))))
                (search-failed
                 (re-search-forward (rx line-start (* space) "- "))
                 (widen)
                 (org-end-of-item-list)
                 (insert item)))))
        (search-failed
         (goto-char (point-max))
         (yank))))))

(defun org-englearn--org-roam-node-equal-by-id (a b)
  (string-equal (org-roam-node-id a) (org-roam-node-id b)))

(defun org-englearn-org-roam--alias-add (alias)
  (org-roam-property-add "ROAM_ALIASES" alias))

;;;###autoload
(defun org-englearn-org-roam-node-merge (source destination)
  (interactive
   (list (org-roam-node-read nil nil nil t "Source node: ")
         (or (and (eq major-mode 'org-mode) (org-roam-node-at-point))
             (org-roam-node-read nil nil nil t "Target node: "))))
  (cl-loop with aliases = (cons (org-roam-node-title source) (org-roam-node-aliases source))
           with source-link = (concat "id:" (org-roam-node-id source))
           and target-link = (concat "id:" (org-roam-node-id destination))
           for backlink in (org-roam-backlinks-get source)
           for node = (org-roam-backlink-source-node backlink)
           for point = (org-roam-backlink-point backlink)
           for file = (org-roam-node-file node)
           do (with-current-buffer (find-file-noselect file)
                (save-excursion
                  (goto-char point)
                  (cl-destructuring-bind (start . end) (org--bounds-of-link-at-point)
                    (cl-assert (string-equal (org--link-at-point) source-link))
                    (replace-regexp-in-region (regexp-quote source-link) target-link start end)
                    (cl-destructuring-bind (newstart . newend) (org--bounds-of-link-at-point)
                      (cl-assert (= start newstart))
                      (cl-assert (= end newend))))))
           finally
           (let ((marker (with-current-buffer (find-file-noselect (org-roam-node-file destination))
                           (save-excursion
                             (goto-char (org-roam-node-point destination))
                             (cl-assert (org-englearn--org-roam-node-equal-by-id destination (org-roam-node-at-point)))
                             (point-marker)))))
             (with-current-buffer (find-file-noselect (org-roam-node-file source))
               (save-excursion
                 (goto-char (org-roam-node-point source))
                 (cl-assert (org-englearn--org-roam-node-equal-by-id source (org-roam-node-at-point)))
                 (cl-assert (string-equal (org-roam-node-title source) (cl-fifth (org-heading-components))))
                 (org-cut-subtree)))
             (with-current-buffer (marker-buffer marker)
               (goto-char marker)
               (cl-assert (org-englearn--org-roam-node-equal-by-id destination (org-roam-node-at-point)))
               (cl-assert (string-equal (org-roam-node-title destination) (cl-fifth (org-heading-components))))
               (mapc #'org-englearn-org-roam--alias-add aliases)))))

;;;###autoload
(defun org-englearn-org-roam-alias-add ()
  (interactive)
  (let* ((node (cond
                ((and (eq major-mode 'org-mode) (org--link-at-point))
                 (save-window-excursion
                   (org-open-at-point)
                   (org-roam-node-at-point)))
                ((and (eq major-mode 'org-mode) (org-roam-node-at-point))
                 (org-roam-node-at-point))
                (t (org-roam-node-read))))
         (alias (read-string (format "Alias to \"%s\": " (org-roam-node-title node)))))
    (cl-assert (null (org-roam-node-from-title-or-alias alias)))
    (with-current-buffer (find-file-noselect (org-roam-node-file node))
      (save-excursion
        (goto-char (org-roam-node-point node))
        (cl-assert (org-englearn--org-roam-node-equal-by-id node (org-roam-node-at-point)))
        (org-englearn-org-roam--alias-add alias)))))

(provide 'org-englearn)
;;; org-englearn.el ends here
