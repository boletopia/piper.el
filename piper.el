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
  "Queue of Piper and `aplay` processes being managed.")

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
        (or (thing-at-point 'sentence t) ;; Try to get the current sentence
            (thing-at-point 'word t)    ;; Fallback to the current word
            (read-string "Enter text for TTS: "))) ;; Fallback to user input
    (read-string "Enter text for TTS: ")))

(defun piper--generate-unique-filename (text)
  "Generate a unique filename for the given TEXT using its hash.
The file is stored in the system's temporary directory."
  (let ((hash (secure-hash 'md5 text)))
    (concat (file-name-as-directory (temporary-file-directory))
            "piper-audio-" hash ".raw")))

(defun piper--build-command (text)
  "Build the Piper command for the given TEXT.
The raw audio is saved to a unique file based on the text."
  (let* ((escaped-text (replace-regexp-in-string "'" "'\\''" text))
         (unique-filename (piper--generate-unique-filename text))) ;; Generate unique filename
    ;; If the file already exists, skip generation and replay it
    (if (file-exists-p unique-filename)
        (progn
          (message "Audio file already exists: %s. Replaying instead." unique-filename)
          (piper--replay-audio-file unique-filename) ;; Replay the existing file
          nil) ;; Return nil to indicate no new command is needed
      ;; Otherwise, generate the file and play it
      (list "sh" "-c"
            (format "echo '%s' | %s --model %s --output-raw | tee %s | aplay %s"
                    escaped-text
                    (shell-quote-argument piper-binary-path)
                    (shell-quote-argument piper-model-path)
                    (shell-quote-argument unique-filename)
                    piper-aplay-parameters)))))

(defun piper--enqueue-process (process type)
  "Add PROCESS to the `piper--process-queue` with TYPE metadata.
TYPE should be either 'piper or 'aplay."
  (setq piper--process-queue
        (append piper--process-queue (list (cons process type)))))

(defun piper--dequeue-process (process)
  "Remove PROCESS from the `piper--process-queue`."
  (setq piper--process-queue
        (cl-remove-if (lambda (entry) (eq (car entry) process))
                      piper--process-queue)))

(defun piper--stop-oldest-process (&optional type)
  "Stop the oldest running process in the queue.
If TYPE is specified, stop only the oldest process of that type ('piper or 'aplay)."
  (let ((oldest-entry (if type
                          (cl-find-if (lambda (entry) (eq (cdr entry) type)) piper--process-queue)
                        (car piper--process-queue))))
    (when oldest-entry
      (let ((process (car oldest-entry)))
        (when (and process (process-live-p process))
          (delete-process process)
          (message "Stopped the oldest %s process: %s"
                   (if type (symbol-name type) "Piper or aplay")
                   (process-name process)))
        (piper--dequeue-process process)))))

(defun piper--confirm-multiple-processes (&optional type)
  "Ask the user to confirm if they want to allow multiple processes to run.
If TYPE is specified, check only for processes of that type ('piper or 'aplay).
Returns t if the user agrees, and nil otherwise."
  (let ((running (cl-find-if (lambda (entry)
                               (or (null (cdr entry))  ;; Handle nil type gracefully
                                   (eq (cdr entry) (or type 'piper))))
                             piper--process-queue)))
    (if running
        (yes-or-no-p (format "A %s process is already running. Do you want to allow another process to run?"
                             (or (symbol-name (cdr running)) "unknown")))
      t))) ;; If no process of the specified type is running, allow it by default.

(defun piper--start-aplay-process (filename)
  "Start the `aplay` process to play the given FILENAME and add it to the queue."
  (let ((process
         (start-process "piper-aplay-process" nil "aplay"
                        "-r" "22050" "-f" "S16_LE" "-t" "raw" filename))) ;; Use filename directly
    ;; Add the `aplay` process to the queue
    (piper--enqueue-process process 'aplay)
    ;; Set up a sentinel for cleanup
    (set-process-sentinel
     process
     (lambda (process event)
       (if (string= event "finished\n")
           (message "Playback finished successfully.")
         (message "Playback failed or was terminated: %s" event))
       ;; Remove the `aplay` process from the queue
       (piper--dequeue-process process)))
    (message "Started playback with `aplay`: %s" filename)))

;; Something is still wrong with this play queue thing
(defun piper--run-process (text)
  "Run the Piper process with the given TEXT, or replay an existing audio file if it exists."
  (when (and piper--process-queue (not (piper--confirm-multiple-processes)))
    (error "Cannot start a new Piper process while another is running."))
  (let* ((command (piper--build-command text)) ;; Build the command
         (stdout-buffer (get-buffer-create piper--stdout-buffer-name))
         (stderr-buffer (get-buffer-create piper--stderr-buffer-name))
         process)
    ;; If the command is nil, it means the file was replayed and no new process is needed
    (if (not command)
        (message "Replayed existing audio file, no new process started.")
      ;; Otherwise, proceed with running the Piper process
      (progn
        ;; Clear buffers before starting a new process
        (with-current-buffer stdout-buffer (erase-buffer))
        (with-current-buffer stderr-buffer (erase-buffer))
        ;; Create and start the Piper process
        (setq process
              (make-process
               :name "piper-process"
               :command command
               :connection-type nil
               :buffer (if piper-show-process-output stdout-buffer nil)
               :stderr stderr-buffer
               :sentinel
               (lambda (process event)
                 (if (string= event "finished\n")
                     (progn
                       (message "Piper process finished successfully. Audio saved to: %s"
                                piper-temp-file)
                       ;; Start `aplay` for playback
                       (piper--start-aplay-process piper-temp-file))
                   (progn
                     (message "Piper process failed or was terminated: %s" event)
                     ;; Delete the temporary file if the process failed or was terminated
                     (when (and piper-temp-file (file-exists-p piper-temp-file))
                       (delete-file piper-temp-file)
                       (message "Deleted temporary audio file: %s" piper-temp-file)))
                   ;; Remove the Piper process from the queue
                   (piper--dequeue-process process))))))
        ;; Add the Piper process to the queue
        (piper--enqueue-process process 'piper)
        (message "Piper process started: %s" (process-name process)))))

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

(defun piper-stop (&optional type)
  "Stop the first running process in the queue.
If TYPE is specified, stop only processes of that type ('piper or 'aplay)."
  (interactive)
  (if (not piper--process-queue)
      (message "No processes are currently running.")
    (let ((target (if type
                      ;; Find the first process of the specified type
                      (cl-find-if (lambda (entry) (eq (cdr entry) type))
                                  piper--process-queue)
                    ;; Default: stop the first process in the queue
                    (car piper--process-queue))))
      (if (not target)
          (message "No %s process is currently running." (or type "specified"))
        (let ((process (car target))
              (process-type (cdr target)))
          (when (and process (process-live-p process))
            ;; Terminate the process
            (delete-process process)
            (message "Stopped %s process: %s"
                     (if (eq process-type 'piper) "Piper" "aplay")
                     (process-name process)))
          ;; Remove the stopped process from the queue
          (piper--dequeue-process process))))))
(provide 'piper)
;;; piper.el ends here
