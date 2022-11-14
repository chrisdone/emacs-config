;;; record-gif.el --- Record Emacs frame to a gif.

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar record-gif-directory temporary-file-directory)
(defvar record-gif-window nil)
(defvar record-gif-dimensions nil)
(defvar record-gif-frame-rate 35)

(defun record-gif-choose-window ()
  "Choose the window to take screenshots of."
  (interactive)
  (let ((out (shell-command-to-string "xwininfo -display :0")))
    (when (string-match
           "Window id: \\([^ ]+\\)"
           out)
      (setq record-gif-window (match-string 1 out))
      (when (string-match
             "Width: \\([0-9]+\\)"
             out)
        (let ((width (string-to-number (match-string 1 out))))
          (when (string-match
                 "Height: \\([0-9]+\\)"
                 out)
            (let ((height (string-to-number (match-string 1 out))))
              (setq record-gif-dimensions
                    (format "%dx%d" width height))
              (message "Window chosen: %s (%s)"
                       record-gif-window
                       record-gif-dimensions))))))))

(defun record-gif-choose-directory ()
  "Choose the directory to save gifs into."
  (interactive)
  (make-directory
   (setq record-gif-directory (ido-read-directory-name "Directory: " record-gif-directory))
   t))

(defun record-gif-snapshot ()
  "Take a snapshot of the current frame."
  (interactive)
  (when (and record-gif-directory
             record-gif-window
             record-gif-dimensions)
    (shell-command-to-string
     (format
      "import -window %s -crop %s+13+0 +repage %s/%f.png"
      record-gif-window
      record-gif-dimensions
      record-gif-directory
      (time-to-seconds (current-time))))))

(defun record-gif-preview ()
  "Preview the animation of the snapshots so far."
  (interactive)
  (shell-command-to-string
   (format "animate -delay %d %s/*.png"
           record-gif-frame-rate
           record-gif-directory)))

(defun record-gif-save ()
  "Save the animation to file."
  (interactive)
  (let ((name (ido-read-file-name "Output filename: " record-gif-directory)))
    (shell-command-to-string
     (format "convert -delay %d %s/*.png %s"
             record-gif-frame-rate
             record-gif-directory
             name))
    (when (y-or-n-p "Open output GIF in buffer?")
      (let ((buffer (find-file name)))
        (with-current-buffer buffer
          (set (make-local-variable 'image-animate-loop)
               t)
          (image-toggle-animation))))))

(provide 'record-gif)
