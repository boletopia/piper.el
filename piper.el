;;; piper.el --- Text-to-Speech interface using Piper TTS -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name.

;; Author: Your Name <your.email@example.com>
;; URL: https://github.com/yourusername/piper.el
;; Keywords: convenience
;; Version: 0.1.0

;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Text-to-Speech interface for Emacs using Piper.
;; This package allows you to convert text to speech using Piper's TTS engine.
;;
;; Usage:
;; - Place `piper.el` in your Emacs `load-path`.
;; - Configure the model path and Piper binary path via customization.
;; - Run `M-x piper-run` to transcribe text at point.
;;
;; Version 0.1
;; - initial release

;;; Code:

(require 'cl-lib)

;;; User-facing options

(defgroup piper ()
  "Text-to-Speech interface using Piper."
  :group 'external)

(defcustom piper-binary-path "/home/yizhe/Src/piper/piper/piper"
  "Path to the Piper binary."
  :type 'file
  :group 'piper)

(defcustom piper-model-path "/home/yizhe/Src/piper/piper/models/en_US-lessac-high.onnx"
  "Path to the Piper model file."
  :type 'file
  :group 'piper)

(defcustom piper-aplay-parameters "-r 22050 -f S16_LE -t raw -"
  "Parameters for the `aplay` command used to play the raw audio output."
  :type 'string
  :group 'piper)

(defcustom piper-temp-file
  (concat (temporary-file-directory) "piper-output.raw")
  "Temporary file used for Piper's raw audio output."
  :type 'file
  :group 'piper)

(defcustom piper-insert-text-at-point t
  "Whether to read the text at point for TTS."
  :type 'boolean
  :group 'piper)

(defcustom piper-show-process-output nil
  "Whether to show Piper's process output in a dedicated buffer."
  :type 'boolean
  :group 'piper)

(defcustom piper-tooltip-height 200
  "Height of the tooltip text when `piper-mode` is active, in units of 1/10 point."
  :type 'integer
  :group 'piper)

;;; Internal variables

(defvar piper--stdout-buffer-name "*piper-stdout*")
(defvar piper--stderr-buffer-name "*piper-stderr*")
(defvar piper--original-tooltip-height nil
  "Stores the original `showkey-tooltip-height` value to restore when `piper-mode` is disabled.")

;;; Utility functions

(defun piper--get-text ()
  "Get text to send to Piper for TTS."
  (if piper-insert-text-at-point
      (if (use-region-p)
          (buffer-substring-no-properties (region-beginning) (region-end))
        (read-string "Enter text for TTS: "))
    (read-string "Enter text for TTS: ")))

(defun piper--build-command (text)
  "Build the Piper command for the given TEXT."
  (let ((escaped-text (replace-regexp-in-string "'" "'\\''" text)))
    (list "sh" "-c"
          (format "echo '%s' | %s --model %s --output-raw 2>/dev/null | aplay %s"
                  escaped-text
                  (shell-quote-argument piper-binary-path)
                  (shell-quote-argument piper-model-path)
                  piper-aplay-parameters))))

(defun piper--run-process (text)
  "Run the Piper process with the given TEXT."
  (let ((command (piper--build-command text)))
    (make-process
     :name "piper-process"
     :command command
     :connection-type nil
     :buffer (if piper-show-process-output
                 (get-buffer-create piper--stdout-buffer-name)
               nil)
     :stderr (get-buffer-create piper--stderr-buffer-name)
     :sentinel (lambda (_process event)
                 (if (string= event "finished\n")
                     (message "Piper process finished successfully.")
                   (message "Piper process failed: %s" event))))))

;;; Main functions

(defun piper-run ()
  "Run Piper to convert text to speech."
  (interactive)
  (let ((text (piper--get-text)))
    (message "Sending text to Piper for TTS...")
    (piper--run-process text)))

(defun piper-speak-letter (char)
  "Speak the given CHAR using Piper."
  (piper--run-process (string char)))

;;;###autoload
(defun piper-file (file)
  "Run Piper to convert the contents of FILE to speech."
  (interactive "fSelect file: ")
  (let ((text (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))))
    (message "Sending file contents to Piper for TTS...")
    (piper--run-process text)))

(defun piper--speak-current-char ()
  "Speak the character just typed."
  (let ((char (char-before)))
    (when (and char (characterp char))
      (piper-speak-letter char))))

(provide 'piper)
;;; piper.el ends here
