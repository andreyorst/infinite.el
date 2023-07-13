;;; infinite.el --- Infinite Window System -*- lexical-binding: t -*-
;;
;; Author: Andrey Listopadov
;; Homepage: https://gitlab.com/andreyorst/infinite.el
;; Package-Requires: ((emacs "26.1"))
;; Keywords: windowing
;; Prefix: infinite
;; Version: 0.0.2
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with isayt.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Open up a new empty space and create movable windows in it.  New
;; windows can be created with `infinite-visit-file' or
;; `infinite-open-buffer'.  When splitting, window tries to find an
;; empty space nearby automatically.
;;
;; Highly experimental and WIP.
;;
;;; Code:

(defvar infinite--base-frame nil)
(defvar infinite--frames nil)
(defvar infinite-gap 10)
(defvar infinite--default-pixel-width 560)
(defvar infinite--default-pixel-height 720)

(defun infinite--track-mouse (event)
  (interactive "e")
  (track-mouse
    (catch 'done
      (while t
        (let ((beginning-position (mouse-pixel-position))
              new-event)
          (setq new-event (read-event))
          (if (not (eq (event-basic-type new-event) 'mouse-movement))
              (when (eq (event-basic-type new-event) 'mouse-1)
                (throw 'done nil))
            (let ((current-position (mouse-pixel-position)))
              (setq x-pointer-shape 'hand)
              (setq track-mouse 'dragging)
              (dolist (f infinite--frames)
                (let* ((p (frame-position f))
                       (x (car p))
                       (y (cdr p)))
                  (let ((new-x (- (cadr beginning-position)
                                  (cadr current-position)))
                        (new-y (- (cddr beginning-position)
                                  (cddr current-position))))
                    (modify-frame-parameters
                     f
                     `((user-position . t)
                       (left . (+ ,(- x new-x)))
                       (top . (+ ,(- y new-y)))))))))))))))



(defun infinite--delete-frame (&optional f)
  (interactive)
  (let ((frame (or f (selected-frame))))
    (when (memq frame infinite--frames)
      (setq infinite--frames (remq frame infinite--frames))
      (delete-frame frame))))


(defun infinite--get-open-space ()
  (let ((xpos 0))
    (dolist (f infinite--frames)
      (setq xpos (max xpos (+ (car (frame-position f)) (frame-pixel-width f)))))
    (+ xpos 5)))

(defun infinite--space-occupied-p (p1x p1y p2x p2y)
  (catch 'yes
    (dolist (f infinite--frames)
      (let* ((pos (frame-position f))
             (p3x (car pos))
             (p3y (cdr pos))
             (p4x (+ p3x (frame-pixel-width f)))
             (p4y (+ p3y (frame-pixel-height f))))
        (when (rectangle-overlap-p
               p1x p1y
               p2x p2y
               p3x p3y
               p4x p4y)
          (throw 'yes f))))))

(defun rectangle-overlap-p ( p1x p1y p2x p2y
                             p3x p3y p4x p4y)
  ;;  p1__________
  ;;   |          |
  ;;   |     p3___|______
  ;;   |      |   |      |
  ;;   |      |   |      |
  ;;   |______|___|      |
  ;;          |   p2     |
  ;;          |__________|
  ;;                     p4
  ;;
  (not (or (< p2y p3y) (> p1y p4y) (< p2x p3x) (> p1x p4x))))

(defun infinite--find-free-space (frame)
  (catch 'pos
    (let ((f frame)
          (direction 'right))
      (while t
        (let* ((pos (frame-position f))
               (x (car pos))
               (y (cdr pos))
               (pos (pcase direction
                      ('right
                       (cons (+ x (frame-pixel-width f) infinite-gap) y))
                      ('left
                       (cons (- x infinite--default-pixel-width infinite-gap) y))
                      ('below
                       (cons x (+ y (frame-pixel-height f) infinite-gap)))
                      ('above
                       (cons x (- y infinite--default-pixel-height infinite-gap))))))
          (if-let ((new-f (infinite--space-occupied-p
                           (car pos) (cdr pos)
                           (+ (car pos) infinite--default-pixel-width)
                           (+ (cdr pos) infinite--default-pixel-height))))
              (setq f new-f
                    direction (pcase direction
                                ('right 'below)
                                ('below 'left)
                                ('left 'below)
                                ('above 'right)))
            (throw 'pos pos)))))))

(defun infinite--open-side-window (window &rest _)
  (let* ((new-pos (infinite--find-free-space (window-frame window))))
    (frame-selected-window
     (infinite--make-frame (car new-pos) (cdr new-pos)))))


(defun infinite--make-frame (x y &optional buffer)
  (let ((nframe (make-frame
                 `((left . ,x) (top . ,y)
                   (width . 80) (height . 42)
                   (parent-frame . ,infinite--base-frame)
                   (child-frame-border-width . 2)
                   (drag-with-header-line . t)
                   (drag-internal-border . t)
                   (minibuffer . nil)))))
    (setq infinite--default-pixel-width
          (frame-pixel-width nframe)
          infinite--default-pixel-height
          (frame-pixel-height nframe))
    (push nframe infinite--frames)
    (select-frame nframe)
    (when buffer
      (switch-to-buffer buffer))
    (let ((window (frame-root-window nframe)))
      (set-window-parameter
       window
       'header-line-format
       " %b")
      (set-window-parameter
       window
       'split-window
       #'infinite--open-side-window))
    nil))

(define-derived-mode infinite-mode fundamental-mode
  "Infinite"
  "Major mode for Infinite window system.
Don't call manually, instead use `infinite-start'."
  (setq infinite--base-frame (window-frame (get-buffer-window))
        infinite--frames nil
        mode-line-format nil))

(define-key infinite-mode-map [down-mouse-1] #'infinite--track-mouse)

;;;###autoload
(defun infinite-start ()
  "Create infinite space that can be panned with mouse and spawn windows on it."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer " *infinite*")
  (read-only-mode)
  (infinite-mode))

(defun infinite-open-buffer (buffer)
  (interactive "B")
  (infinite--make-frame (infinite--get-open-space) 10 buffer))

(defun infinite-visit-file (filename &rest _)
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename)))
    (message "%S" value)
    (infinite--make-frame (infinite--get-open-space) 10 value)))

(provide 'infinite)
