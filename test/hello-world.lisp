;;;;
;;;; REPL playing

(defpackage #:clx-truetype-test
  (:nicknames :xft-test)
  (:use #:cl #:xft)
  (:export show-window))

(in-package :clx-truetype-test)

(defparameter *display* (xlib:open-default-display))
;;(defparameter *display* (xlib:open-default-display "192.168.1.101:0.0"))

(defparameter *screen* (xlib:display-default-screen *display*))
(defparameter *root* (xlib:screen-root *screen*))

(defun show-window ()
  (let* ((black (xlib:screen-black-pixel *screen*))
         (white (xlib:screen-white-pixel *screen*))
         (window
           (xlib:create-window :parent *root* :x 0 :y 0 :width 640 :height 480 
                          :class :input-output
                          :background white
                          :event-mask '(:key-press :key-release :exposure :button-press
                                        :structure-notify)))
         (grackon (xlib:create-gcontext
                   :drawable window
		   :foreground black
		   :background white))
         (font (make-instance 'font :family "Times New Roman" :subfamily "Bold Italic"
                              :size 36 :antialias t)))
    (unwind-protect
         (progn
           (xlib:map-window window)
           (setf (xlib:gcontext-foreground grackon) black)
           (xlib:event-case (*display* :force-output-p t
                                  :discard-p t)
             (:exposure ()
                        (xlib:clear-area window :width (xlib:drawable-width window)
                                         :height (xlib:drawable-height window))
                        (draw-text window grackon font "The quick brown fox jumps over the lazy dog." 100 100)
                        (when (= 0 (random 2))
                          (rotatef (xlib:gcontext-foreground grackon) (xlib:gcontext-background grackon)))
                        (draw-text window grackon font "Съешь же ещё этих мягких французских булок, да выпей чаю." 100 (+ 100 (baseline-to-baseline window font)))
                        (setf (font-antialias font) (= 0 (random 2)))
                        (if (= 0 (random 2))
                            (setf (font-subfamily font) "Regular")
                            (setf (font-subfamily font) "Italic"))
                        (draw-text window grackon font "Жебракують філософи при ґанку церкви в Гадячі, ще й шатро їхнє п’яне знаємо." 100 (+ 100 (* 2 (baseline-to-baseline window font))))
                        (draw-text window grackon font "Press space to exit. Нажмите пробел для выхода." 100 (+ 100 (* 3 (baseline-to-baseline window font))))
                        nil)
             (:button-press () t)
             (:key-press (code state) (char= #\Space (xlib:keycode->character *display* code state)))))
      (progn
        (xlib:free-gcontext grackon)
        (xlib:destroy-window window)
        (xlib:display-force-output *display*)))))
