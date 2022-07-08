;;; -*- lexical-binding: t; -*-
;; Funzione che stampa gli autori
(defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
    (if (> (length authors-list) 1)
        (format "%s et al." (plist-get (nth 0 authors-list) :name))
      (plist-get (nth 0 authors-list) :name)))

(defun my-search-print-fn (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title)
                      (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (entry-authors (concatenate-authors (elfeed-meta entry :authors)))
           (title-width (- (window-width) 10
                           elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title 100
                          :left))
           (entry-score (elfeed-format-column (number-to-string (elfeed-score-scoring-get-score-from-entry entry)) 10 :left))
           (authors-column (elfeed-format-column entry-authors 40 :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")

      (insert (propertize title-column
                          'face title-faces 'kbd-help title) " ")
      (insert (propertize authors-column
                          'kbd-help entry-authors) " ")
      (insert entry-score " ")))

(defun elfeed-entry-to-arxiv ()
    "Fetch an arXiv paper into the local library from the current elfeed entry."
    (interactive)
    (let* ((link (elfeed-entry-link elfeed-show-entry))
           (match-idx (string-match "arxiv.org/abs/\\([0-9.]*\\)" link))
           (matched-arxiv-number (match-string 1 link)))
      (when matched-arxiv-number
        (message "Going to arXiv: %s" matched-arxiv-number)
        (arxiv-get-pdf-add-bibtex-entry matched-arxiv-number robo/main-bib-library robo/main-pdfs-library-path))))

(defun concatenate-authors-full (authors-list)
  "Given AUTHORS-LIST, list of plists; return string of all authors
concatenated."
  (mapconcat
   (lambda (author) (plist-get author :name))
   authors-list ", "))

(defun elfeed--show-format-author (author)
  "Format author plist for the header."
  (cl-destructuring-bind (&key name uri email &allow-other-keys)
      author
    (cond ((and name uri email)
           (format "%s <%s> (%s)" name email uri))
          ((and name email)
           (format "%s <%s>" name email))
          ((and name uri)
           (format "%s (%s)" name uri))
          ((name)
           (format "%s" name))
          (name name)
          (email email)
          (uri uri)
          ("[unknown]"))))

(defun elfeed-show-refresh--mail-style ()
  "Update the buffer to match the selected entry, using a mail-style."
  (interactive)
  (let* ((inhibit-read-only t)
         (title (elfeed-entry-title elfeed-show-entry))
         (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (authors (elfeed-meta elfeed-show-entry :authors))
         (authors2 (concatenate-authors-full (elfeed-meta elfeed-show-entry :authors)))
         (link (elfeed-entry-link elfeed-show-entry))
         (doi (elfeed-meta elfeed-show-entry :doi))
         (tags (elfeed-entry-tags elfeed-show-entry))
         (tagsstr (mapconcat #'symbol-name tags ", "))
         (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
         (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (type (elfeed-entry-content-type elfeed-show-entry))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (feed-title (elfeed-feed-title feed))
         (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
    (erase-buffer)
    (insert (format (propertize "Title: %s\n" 'face 'message-header-name)
                    (propertize title 'face 'message-header-subject)))
    ;; Concatenate the authors
    (when elfeed-show-entry-author
          (insert
           (format (propertize "Authors: %s\n" 'face 'message-header-name)
                   (propertize authors2 'face 'message-header-to))))
    (insert (format (propertize "Date: %s\n" 'face 'message-header-name)
                    (propertize nicedate 'face 'message-header-other)))
    (insert (format (propertize "Feed: %s\n" 'face 'message-header-name)
                    (propertize feed-title 'face 'message-header-other)))
    (when tags
      (insert (format (propertize "Tags: %s\n" 'face 'message-header-name)
                      (propertize tagsstr 'face 'message-header-other))))
    (insert (propertize "Link: " 'face 'message-header-name))
    (elfeed-insert-link link link)
    (insert "\n")
    (unless (string= doi link)
      (insert (propertize "DOI: " 'face 'message-header-name))
      (elfeed-insert-link doi doi)
      (insert "\n")
     )
    (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
             do (insert (propertize "Enclosure: " 'face 'message-header-name))
             do (elfeed-insert-link (car enclosure))
             do (insert "\n"))
    (insert "\n")
    (if content
        (if (eq type 'html)
            (elfeed-insert-html content base)
          (insert content))
      (insert (propertize "(empty)\n" 'face 'italic)))
    (goto-char (point-min))))


(setq elfeed-search-print-entry-function #'my-search-print-fn)

(defun entry-has-doi (_ xml entry)
  ;; Cerca il doi dall'atom se presente e lo inserisce, altrimenti è nil
    (setf (elfeed-meta entry :doi)
          (xml-query '(link :href) xml)))

(add-hook 'elfeed-new-entry-parse-hook #'entry-has-doi)
(provide 'elfeed-arxiv-aspect)

