;; -*- mode: Lisp; coding: utf-8-unix; -*-
;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(in-package side-tree)

(defun get-nerd-icon (path &optional opened-p)
  (let ((pane (car *windows-plist*)))
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

(defmethod get-icon :around ((kind (eql :project)) &key &allow-other-keys)
  (let ((pane (car *windows-plist*)))
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

(defmethod get-icon :around ((kind (eql :directory)) &key truename &allow-other-keys)
  (get-nerd-icon truename))
(defmethod get-icon :around ((kind (eql :directory-opened)) &key truename &allow-other-keys)
  (get-nerd-icon truename t))
(defmethod get-icon :around ((kind (eql :file)) &key truename &allow-other-keys)
  (get-nerd-icon truename))

(defmethod get-icon :around ((kind (eql :lisp-definition)) &key dspec &allow-other-keys)
  (let ((pane (car *windows-plist*)))
    (macrolet ((ret (sym fg)
                 `(values (get ',sym :char)
                          (list 'face (make-face nil
                                                 :font (gp:find-best-font
                                                        pane
                                                        (gp:make-font-description
                                                         :family nerd-icons:*nerd-font-family*
                                                         :size (gp:font-description-attribute-value
                                                                (gp:font-description (capi:simple-pane-font pane))
                                                                :size))) 
                                                 :foreground ',fg)))))
      (case (dspec:dspec-class dspec)
        ((or package defpackage)                                                 (ret nerd-icons:cod-symbol-namespace nerd-icons:light-orange))
        ((or function defun)                                                     (ret nerd-icons:md-function nerd-icons:blue))
        ((or variable defvar defparameter define-symbol-macro)                   (ret nerd-icons:cod-symbol-field nerd-icons:light-blue))
        (defconstant                                                             (ret nerd-icons:cod-symbol-constant nerd-icons:yellow))
        ((or defgeneric method method-combination defmethod)                     (ret nerd-icons:cod-symbol-method nerd-icons:purple))
        (defmacro                                                                (ret nerd-icons:cod-symbol-misc nerd-icons:light-green))
        ((or defclass define-condition)                                          (ret nerd-icons:cod-symbol-class nerd-icons:light-yellow))
        ((or capi:define-interface capi:define-layout)                           (ret nerd-icons:cod-symbol-interface nerd-icons:light-orange))
        ((or (or type deftype) structure defstruct structure-class defsystem)    (ret nerd-icons:cod-symbol-structure nerd-icons:yellow))
        ((or defcommand define-editor-variable)                                  (ret nerd-icons:cod-terminal nerd-icons:cyan))
        (t                                                                       (ret nerd-icons:cod-code :white))))))
