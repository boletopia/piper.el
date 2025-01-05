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

(defcustom piper-install-directory "/home/yizhe/Src/piper/piper/"
  "Base directory where Piper is installed.
This directory should contain the Piper binary and model files.
User should download or compile their relevent binary from
https://github.com/rhasspy/piper"
  :type 'directory
  :group 'piper)

(defcustom piper-model-name "en_US-lessac-high"
  "Name of the Piper model to use (without file extension).
The model file must exist in the `models` subdirectory of `piper-install-directory`."
  :type 'string
  :group 'piper)

(defcustom piper-binary-path (concat piper-install-directory "piper")
  "Path to the Piper binary.
This path is derived from `piper-install-directory` by default."
  :type 'file
  :group 'piper)

(defcustom piper-model-path
  (concat piper-install-directory "models/" piper-model-name ".onnx")
  "Path to the Piper model file.
This path is dynamically derived from `piper-install-directory` and `piper-model-name`."
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

(defcustom piper-fifo-mode t
  "If non-nil, use a First-In-First-Out (FIFO) model to stop Piper processes.
When a new process is started, the oldest process in the queue is stopped automatically."
  :type 'boolean
  :group 'piper)

;;; Internal variables

(defvar piper--process nil
  "Reference to the current Piper process, if any.")

(defvar piper--process-queue nil
  "Queue to track all currently running Piper processes.")

(defvar piper--stdout-buffer-name "*piper-stdout*"
  "Buffer name for Piper's stdout.")

(defvar piper--stderr-buffer-name "*piper-stderr*"
  "Buffer name for Piper's stderr.")

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

(defun piper--enqueue-process (process)
  "Add PROCESS to the Piper process queue."
  (push process piper--process-queue))

(defun piper--dequeue-process ()
  "Remove and return the oldest process in the Piper process queue."
  (let ((oldest-process (car (last piper--process-queue))))
    (setq piper--process-queue (butlast piper--process-queue))
    oldest-process))

(defun piper--stop-oldest-process ()
  "Stop the oldest running Piper process in the queue."
  (let ((oldest-process (piper--dequeue-process)))
    (when (and oldest-process (process-live-p oldest-process))
      (delete-process oldest-process)
      (message "Stopped the oldest Piper process: %s" (process-name oldest-process)))))

(defun piper--confirm-and-stop-if-needed ()
  "Check if a Piper process is running and confirm with the user to stop it.
If `piper-fifo-mode` is enabled, automatically stop the oldest process."
  (when piper--process-queue
    (if piper-fifo-mode
        (piper--stop-oldest-process)
      (if (yes-or-no-p "A Piper process is already running. Stop it?")
          (piper--stop-oldest-process)
        (error "Cannot start a new Piper process while another is running.")))))

(defun piper--confirm-multiple-processes ()
  "Ask the user to confirm if they want to allow multiple Piper processes to run.
Returns t if the user agrees, and nil otherwise."
  (yes-or-no-p "A Piper process is already running. Do you want to allow another process to run?"))

(defun piper--run-process (text)
  "Run the Piper process with the given TEXT."
  (when (and piper--process-queue (not (piper--confirm-multiple-processes)))
    (error "Cannot start a new Piper process while another is running."))
  (let ((command (piper--build-command text))
        (process nil))
    (setq process
          (make-process
           :name "piper-process"
           :command command
           :connection-type nil
           :buffer (if piper-show-process-output
                       (get-buffer-create piper--stdout-buffer-name)
                     nil)
           :stderr (get-buffer-create piper--stderr-buffer-name)
           :sentinel (lambda (process event)
                       (if (string= event "finished\n")
                           (progn
                             (message "Piper process finished successfully.")
                             (setq piper--process-queue
                                   (delq process piper--process-queue)))
                         (progn
                           (message "Piper process failed: %s" event)
                           (setq piper--process-queue
                                 (delq process piper--process-queue)))))))
    (piper--enqueue-process process)
    (message "Piper process started: %s" (process-name process))))

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

;;; Stop Piper process

(defun piper-stop ()
  "Stop the oldest running Piper process, if any.
If multiple processes are running, it uses the FIFO model and stops the oldest."
  (interactive)
  (if piper--process-queue
      (let ((process (piper--dequeue-process))) ;; Get the oldest process from the queue
        (when (process-live-p process)
          (delete-process process)
          (message "Stopped Piper process: %s" (process-name process))))
    (message "No Piper process is currently running.")))


(provide 'piper)
;;; piper.el ends here
