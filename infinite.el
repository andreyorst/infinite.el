;;; infinite.el --- Infinite Window System -*- lexical-binding: t -*-
;;
;; Author: Andrey Listopadov
;; Homepage: https://gitlab.com/andreyorst/infinite.el
;; Package-Requires: ((emacs "26.1"))
;; Keywords: frames mouse convenience
;; Prefix: infinite
;; Version: 0.0.3
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

(defcustom infinite-gap 10
  "Gap in pixels between windows."
  :group 'infinite
  :type 'integer
  :package-version '(infinite "0.0.3"))

(defvar infinite--base-frame nil)
(defvar infinite--frames nil)
(defvar infinite--default-pixel-width 560)
(defvar infinite--default-pixel-height 720)

(defvar infinite--header-line-format
  '(:eval (concat
           " "
           (propertize
            "🆇"
            'face '(:foreground "red")
            'pointer 'hand
            'help-echo "close window"
            'local-map (let ((map (make-sparse-keymap)))
                         (define-key map [header-line mouse-1] 'infinite-delete-frame)
                         map))
           " "
           (propertize
            "🅼"
            'face (if (frame-parameter (selected-frame) 'infinite-maximized)
                      '(:foreground "blue")
                    '(:foreground "green"))
            'pointer 'hand
            'help-echo (if (frame-parameter (selected-frame) 'infinite-maximized)
                           "unmaximize window"
                         "maximize window")
            'local-map (let ((map (make-sparse-keymap)))
                         (define-key map [header-line mouse-1]
                                     (if (frame-parameter (selected-frame) 'infinite-maximized)
                                         #'infinite-unmaximize-frame
                                       #'infinite-maximize-frame))
                         map))
           " %b")))

(defun infinite--this-window (&rest _)
  (selected-window))

(defun infinite-maximize-frame (event)
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (frame (window-frame window))
         (frame-width (frame-pixel-width frame))
         (frame-height (frame-pixel-height frame))
         (pos (frame-position frame))
         (parent (frame-parent frame))
         (parent-window (frame-root-window parent))
         (parent-window-width (window-pixel-width parent-window))
         (parent-window-height (window-pixel-height parent-window)))
    (dolist (f infinite--frames)
      (unless (eq f frame)
        (make-frame-invisible f t)))
    (set-window-parameter
     window
     'split-window
     #'infinite--this-window)
    (modify-frame-parameters
     frame
     `((infinite-maximized . t)
       (infinite-previous-position . ,pos)
       (infinite-previous-size . ,(cons frame-width frame-height))
       (drag-with-header-line . nil)
       (left . 0)
       (top . 0)))
    (set-frame-size frame parent-window-width parent-window-height t)))

(defun infinite-unmaximize-frame (event)
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (frame (window-frame window))
         (pos (frame-parameter frame 'infinite-previous-position))
         (size (frame-parameter frame 'infinite-previous-size)))
    (modify-frame-parameters
     frame
     `((infinite-maximized . nil)
       (infinite-previous-position . nil)
       (drag-with-header-line . t)
       (left . ,(car pos))
       (top . ,(cdr pos))))
    (set-window-parameter
     window
     'split-window
     #'infinite--open-side-window)
    (set-frame-size frame (car size) (cdr size) t)
    (dolist (f infinite--frames)
      (unless (eq f frame)
        (make-frame-visible f)))))

(defun infinite--track-mouse (_)
  "Track mouse dragging events.
Moves all visible child frames that were opened in the infinite
buffer."
  (interactive "e")
  (track-mouse
    (catch 'done
      (while t
        (let ((beginning-position (mouse-pixel-position))
              event)
          (setq event (read-event))
          (pcase (event-basic-type event)
            ('mouse-movement
             (let ((current-position (mouse-pixel-position)))
               (setq track-mouse 'dragging)
               (dolist (f infinite--frames)
                 (let ((p (frame-position f))
                       (dx (- (cadr beginning-position)
                              (cadr current-position)))
                       (dy (- (cddr beginning-position)
                              (cddr current-position))))
                   (modify-frame-parameters
                    f
                    `((user-position . t)
                      (left . (+ ,(- (car p) dx)))
                      (top . (+ ,(- (cdr p) dy)))))))))
            ('mouse-1 (throw 'done nil))))))))

(defun infinite--track-wheel (event)
  "Track mouse scrolling EVENT.
Moves all visible child frames that were opened in the infinite
buffer."
  (interactive "e")
  (let ((delta
         (pcase (event-basic-type event)
           ('wheel-right (cons -10 0))
           ('wheel-left (cons 10 0))
           ('wheel-up (cons 0 -10))
           ('wheel-down (cons 0 10)))))
    (dolist (f infinite--frames)
      (let ((p (frame-position f))
            (dx (car delta))
            (dy (cdr delta)))
        (modify-frame-parameters
         f
         `((user-position . t)
           (left . (+ ,(- (car p) dx)))
           (top . (+ ,(- (cdr p) dy)))))))))

(defun infinite-delete-frame (event)
  "Delete currently selected frame based on EVENT.
Removes it from the list of infinite frames."
  (interactive "e")
  (let ((frame (window-frame (posn-window (event-start event)))))
    (when (memq frame infinite--frames)
      (setq infinite--frames (remq frame infinite--frames))
      (handle-delete-frame `(delete-frame (,frame))))))

(defun infinite--space-occupied-p (p1x p1y p2x p2y)
  "Check if space is already occupied by another frame.
P1X P1Y P2X P2Y are the top left and bottom right x and y
components of a rectangle.  See `infinite--rectangle-overlap-p'
for more info."
  (catch 'yes
    (dolist (f infinite--frames)
      (let* ((pos (frame-position f))
             (p3x (car pos))
             (p3y (cdr pos))
             (p4x (+ p3x (frame-pixel-width f)))
             (p4y (+ p3y (frame-pixel-height f))))
        (when (infinite--rectangle-overlap-p
               p1x p1y p2x p2y
               p3x p3y p4x p4y)
          (throw 'yes f))))))

(defun infinite--rectangle-overlap-p
    ( p1x p1y p2x p2y
      p3x p3y p4x p4y)
  "Check if two rectangles overlap.
P1X P1Y P2X P2Y correspond to coordinates of the first rectangle
top left and bottom right x and y components respectively.  P3X
P3Y P4X P4Y are the same for the second rectangle.

 P1__________
  |          |
  |     P3___|______
  |      |   |      |
  |      |   |      |
  |______|___|      |
         |   P2     |
         |__________|
                    P4"
  (not (or (< p2y p3y) (> p1y p4y) (< p2x p3x) (> p1x p4x))))

(defun infinite--find-free-space (&optional frame)
  "Find nearest free space for a given FRAME.
If FRAME is not provided starts the search from the top left
corner."
  (catch 'pos
    (let (f direction)
      (setq f frame direction 'right)
      (while t
        (let* ((pos (and f (frame-position f)))
               (x (or (car pos) infinite-gap))
               (y (or (cdr pos) infinite-gap))
               (pos (pcase direction
                      ('right
                       (cons (+ x (if f (frame-pixel-width f) 0) infinite-gap) y))
                      ('left
                       (cons (- x (if f infinite--default-pixel-width 0) infinite-gap) y))
                      ('below
                       (cons x (+ y (if f (frame-pixel-height f) 0) infinite-gap)))
                      ('above
                       (cons x (- y (if f infinite--default-pixel-height 0) infinite-gap)))))
               (colliding-frame
                (infinite--space-occupied-p
                 (car pos) (cdr pos)
                 (+ (car pos) infinite--default-pixel-width)
                 (+ (cdr pos) infinite--default-pixel-height))))
          (when (null colliding-frame) (throw 'pos pos))
          (setq direction
                (if (null f) 'right
                  (pcase direction
                    ('right 'below)
                    ('below 'left)
                    ('left 'below)
                    ('above 'right))))
          (setq f colliding-frame))))))

(defun infinite--open-side-window (window &rest _)
  "Open a new window to the side of the given WINDOW."
  (let* ((new-pos (infinite--find-free-space (window-frame window))))
    (frame-selected-window
     (infinite--make-frame (car new-pos) (cdr new-pos)))))

(defun infinite--new-window (&rest _)
  "Open a new window."
  (let* ((new-pos (infinite--find-free-space)))
    (frame-selected-window
     (infinite--make-frame (car new-pos) (cdr new-pos)))))

(defun infinite--make-frame (x y &optional buffer norecord _)
  "Make frame that obeys infinite rules.
Frame is positioned at X Y coordinates, and may optionally
contain a given BUFFER with the respect to the NORECORD
parameter."
  (let ((nframe (make-frame
                 `((left . ,x) (top . ,y)
                   (width . 80) (height . 42)
                   (parent-frame . ,infinite--base-frame)
                   (child-frame-border-width . 2)
                   (drag-with-header-line . t)
                   (drag-internal-border . t)
                   (unsplittable . t)
                   (undecorated . t)
                   (minibuffer . nil)))))
    (setq infinite--default-pixel-width
          (frame-pixel-width nframe)
          infinite--default-pixel-height
          (frame-pixel-height nframe))
    (push nframe infinite--frames)
    (select-frame nframe)
    (when buffer
      (switch-to-buffer buffer norecord))
    (let ((window (frame-root-window nframe)))
      (set-window-parameter
       window
       'header-line-format
       infinite--header-line-format)
      (set-window-parameter
       window
       'split-window
       #'infinite--open-side-window))
    nil))

(defvar-keymap infinite-mode-map
  :doc "The key map used by `infinite-mode'."
  "<down-mouse-1>" #'infinite--track-mouse
  "<wheel-left>"   #'infinite--track-wheel
  "<wheel-right>"  #'infinite--track-wheel
  "<wheel-up>"     #'infinite--track-wheel
  "<wheel-down>"   #'infinite--track-wheel
  "<remap> <find-file>" #'infinite-visit-file
  "<remap> <switch-to-buffer>" #'infinite-open-buffer)

(define-derived-mode infinite-mode fundamental-mode
  "Infinite"
  "Major mode for Infinite window system.
Don't call manually, instead use `infinite-start'."
  :keymap infinite-mode-map
  (setq infinite--base-frame (window-frame (get-buffer-window))
        infinite--frames nil
        mode-line-format nil)
  (make-local-variable 'minor-mode-overriding-map-alist)
  (push `(pixel-scroll-precision-mode . ,infinite-mode-map)
        minor-mode-overriding-map-alist)
  (let ((window (frame-root-window)))
    (set-window-parameter
     window
     'split-window
     #'infinite--new-window)))

;;;###autoload
(defun infinite-start ()
  "Create infinite space that can be panned with mouse and spawn windows on it."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer " *infinite*")
  (read-only-mode)
  (infinite-mode))

(defun infinite-open-buffer (buffer-or-name &optional norecord _)
  "Display buffer BUFFER-OR-NAME in the new window.

If optional argument NORECORD is non-nil, do not put the buffer
at the front of the buffer list, and do not make the window
displaying it the most recently selected one."
  (interactive "B")
  (let ((pos (infinite--find-free-space)))
    (infinite--make-frame (car pos) (cdr pos) buffer-or-name norecord)))

(defun infinite-visit-file (filename &optional wildcards)
  "Edit file FILENAME.
Switch to a window visiting file FILENAME, creating one in the
infinite buffer.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files.  You can
suppress wildcard expansion by setting ‘find-file-wildcards’ to nil."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let* ((value (find-file-noselect filename nil nil wildcards))
         (values (if (listp value) value (list value))))
    (dolist (value values)
      (let ((pos (infinite--find-free-space)))
        (infinite--make-frame (car pos) (cdr pos) value)))))

(provide 'infinite)
;;; infinite.el ends here
