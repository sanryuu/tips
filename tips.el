;;; package --- 
;;; Commentary:
;;; Code:
(require 'json)
(require 'qiita)
(require 'package)
(setq inhibit-startup-message t)
(package-initialize)

(setq response-emacs (qiita:api-tag-items "emacs"))

(defun tips-insert-qiita-item (qiita-item-list)
  (mapcar 
   '(lambda (x)
      (insert
       (concat 
        (plist-get x :stock)
        "[["
        (plist-get x :url)
        "]["
        (plist-get x :title)
        "]]"
        "\n"))) 
   qiita-item-list))


(defun url-http-get (url)
  "Send ARGS to URL as a GET request."
  (with-temp-buffer
    (call-process "curl" nil (current-buffer) nil
                  "-H" "Content-type: application/json"
                  "-s" 
                  url)
  (buffer-substring-no-properties
     (point-min) (point-max))))

(setq loading-tips-melpa-dl-url "http://melpa.org/download_counts.json")
(setq loading-tips-melpa-archive-url "http://melpa.org/archive.json")


(defun tips-get-desc (package-name archive-data)
  (cdr 
   (assoc 'desc
          (cdr (cdr (assoc package-name archive-data))))))

(setq tips-boder 30000)

(defun tips-filter (tips-boder download-number-set)
  (let (filtered-set)
      (mapcar 
       '(lambda (n)
          (if (> (cdr n) tips-boder)
              (add-to-list 'filtered-set n)
            ))
       download-number-set)
      filtered-set))

(defun tips-choice-package (tips-choice-num download-set)
  (let ((loop-count 0)
        (tips-display-list ())
     (random-item))
  (while (< loop-count tips-choice-num)
    (setq loop-count (1+ loop-count))
    (setq random-item (nth (random (length download-set)) download-set))
    (add-to-list 
     'tips-display-list
     random-item)
    (setq download-set (delete random-item download-set)))
  tips-display-list))
     
(defun tips-create-desc-set (download-set archive-data)
  (let (package-desc-set)
    (mapcar '(lambda (n)
               (add-to-list
                'package-desc-set
                (cons
                 (car n)
                 (tips-get-desc (car n) archive-data))))
            download-set)
    package-desc-set))

(setq tips-choice-num 15)

(defun tips-insert-package-info-part (name-desc-set)
  (mapcar
   '(lambda (n)
      (insert 
       (concat "- "
               (symbol-name (car n))
               ": "
               (cdr n)
               "\n")))
   name-desc-set))

(defun tips-insert-package-info ()
  (let ((tips-melpa-download-data
         (json-read-from-string
          (url-http-get loading-tips-melpa-dl-url)))
        (tips-melpa-archive-data
         (json-read-from-string
          (url-http-get loading-tips-melpa-archive-url)))
        filtered-download-data
        choiced-package
        package-set
        name-desc-set)
    (setq filtered-download-data 
          (tips-filter tips-boder tips-melpa-download-data))
    (setq choiced-package
          (tips-choice-package tips-choice-num filtered-download-data))
    (setq name-desc-set
          (tips-create-desc-set choiced-package tips-melpa-archive-data))
    (tips-insert-package-info-part name-desc-set)))

(defun tips-init ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*tips*"))
  (org-mode)
  (tips-insert-qiita-item (qiita:api-tag-items "emacs"))
  (tips-insert-package-info))


(provide 'tips)

;;; tips.el ends here
