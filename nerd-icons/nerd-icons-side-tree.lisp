;; -*- mode: Lisp; coding: utf-8-unix; -*-
;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(in-package side-tree)

(defun get-nerd-icon (path &optional opened-p)
  (let ((pane (window-text-pane (current-window))))
    (multiple-value-bind (sym color)
        (nerd-icons:file-to-icon-name path)
      (when (and opened-p (eq sym 'nerd-icons:custom-folder-oct))
        (setq sym 'nerd-icons:custom-folder-open))
      (values (get sym :char)
              (list 'face (make-face nil
                                     :font (gp:find-best-font
                                            pane
                                            (gp:make-font-description
                                             :family nerd-icons:*nerd-font-family*
                                             :size (gp:font-description-attribute-value
                                                    (gp:font-description (capi:simple-pane-font pane))
                                                    :size))) 
                                     :foreground (color:get-color-translation color)))))))

(defmethod get-icon :around ((kind (eql :project)) path)
  (let ((pane (window-text-pane (current-window))))
    (values (get 'nerd-icons:cod-repo :char)
            (list 'face (make-face nil
                                   :font (gp:find-best-font
                                          pane
                                          (gp:make-font-description
                                           :family nerd-icons:*nerd-font-family*
                                           :size (gp:font-description-attribute-value
                                                  (gp:font-description (capi:simple-pane-font pane))
                                                  :size))) 
                                   :foreground (editor::face-foreground (editor::find-face 'project-name-face)))))))

(defmethod get-icon :around ((kind (eql :directory)) path)
  (get-nerd-icon path))
(defmethod get-icon :around ((kind (eql :directory-opened)) path)
  (get-nerd-icon path t))
(defmethod get-icon :around ((kind (eql :file)) path)
  (get-nerd-icon path))
