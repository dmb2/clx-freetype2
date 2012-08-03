
(in-package #:clx-truetype)

(defvar *font-dirs* #+unix (list "/usr/share/fonts/TTF/"
                                 (namestring (merge-pathnames ".fonts/" (user-homedir-pathname))))
        #+macos (list "/Library/Fonts/")
        "List of directories, which contain TrueType fonts.")

;;(pushnew (xlib:font-path *display*) *font-dirs*)
(defun cache-font-file (pathname)
  "Caches font file."
  (ignore-errors  
   (zpb-ttf:with-font-loader (font pathname)
     (multiple-value-bind (hash-table exists-p)
         (gethash (zpb-ttf:family-name font) *font-cache*
                  (make-hash-table :test 'equal))
       (setf (gethash (zpb-ttf:subfamily-name font) hash-table)
             pathname)
       (unless exists-p
         (setf (gethash (zpb-ttf:family-name font) *font-cache*)
               hash-table))))))

(defun ttf-pathname-test (pathname)
  (string-equal "ttf" (pathname-type pathname)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +font-cache-filename+ 
    #.(merge-pathnames "font-cache.sexp"
                       (merge-pathnames ".fonts/" (user-homedir-pathname)))))

(defun cache-fonts ()
  "Caches fonts from @refvar{*font-dirs*} directories."
  (clrhash *font-cache*)
  (dolist (font-dir *font-dirs*)
    (fad:walk-directory font-dir #'cache-font-file :if-does-not-exist :ignore
                        :test #'ttf-pathname-test))
  (cl-store:store *font-cache* +font-cache-filename+))

(defun get-font-families ()
  "Returns cached font families."
  (let ((result (list)))
    (maphash (lambda (key value)
               (declare (ignorable value))
               (push key result)) *font-cache*)
    (nreverse result)))

(defun get-font-subfamilies (font-family)
  "Returns font subfamilies for current @var{font-family}. For e.g. regular, italic, bold, etc."
  (let ((result (list)))
    (maphash (lambda (family value)
               (declare (ignorable family))
               (when (string-equal font-family family)
                 (maphash (lambda (subfamily pathname)
                            (declare (ignorable pathname))
                            (push subfamily result)) value)
                 (return-from get-font-subfamilies 
                   (nreverse result)))) *font-cache*)
    (nreverse result)))

;; family ->
;;   subfamily -> filename
;;   subfamily -> filename
(eval-when (:load-toplevel :execute)
  (defparameter *font-cache*
    (if (fad:file-exists-p +font-cache-filename+)
        (cl-store:restore +font-cache-filename+)
        (make-hash-table :test 'equal))
    "Hashmap for caching font families, subfamilies and files."))

