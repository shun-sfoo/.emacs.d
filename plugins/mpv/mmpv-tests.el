;;; mmpv-tests.el --- Tests for mmpv  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'mmpv)

(ert-deftest mmpv-test-timestamp-parsing ()
  "Test SRT timestamp parsing."
  (should (= (mmpv-parse-timestamp "00:00:01,500") 1.5))
  (should (= (mmpv-parse-timestamp "00:01:30,000") 90.0))
  (should (= (mmpv-parse-timestamp "01:00:00,000") 3600.0))
  (should (= (mmpv-parse-timestamp "00:00:00,100") 0.1))
  (should (= (mmpv-parse-timestamp "00:00:00.500") 0.5))
  (should (null (mmpv-parse-timestamp "invalid"))))

(ert-deftest mmpv-test-timestamp-with-spaces ()
  "Test timestamp parsing with extra spaces."
  (should (= (mmpv-parse-timestamp " 00:00:01,500 ") 1.5))
  (should (= (mmpv-parse-timestamp "00:00:01,500\r\n") 1.5)))

(ert-deftest mmpv-test-subtitle-structure ()
  "Test subtitle structure creation."
  (let ((sub (mmpv--subtitle-create :start 1.5 :end 3.0 :text "Hello")))
    (should (= (mmpv--subtitle-start sub) 1.5))
    (should (= (mmpv--subtitle-end sub) 3.0))
    (should (string= (mmpv--subtitle-text sub) "Hello"))))

(ert-deftest mmpv-test-subtitle-equality ()
  "Test subtitle equality comparison."
  (let ((sub1 (mmpv--subtitle-create :start 1.5 :end 3.0 :text "Hello"))
        (sub2 (mmpv--subtitle-create :start 1.5 :end 3.0 :text "Hello"))
        (sub3 (mmpv--subtitle-create :start 2.0 :end 3.0 :text "Hello")))
    (should (mmpv--subtitle-equal sub1 sub2))
    (should (not (mmpv--subtitle-equal sub1 sub3)))))

(ert-deftest mmpv-test-srt-parsing-basic ()
  "Test basic SRT parsing."
  (let* ((content "1\n00:00:01,000 --> 00:00:03,000\nHello World\n\n2\n00:00:04,000 --> 00:00:06,000\nGoodbye\n")
         (entries (mmpv-parse-srt-from-string content)))
    (should (= (length entries) 2))
    (should (= (mmpv--subtitle-start (car entries)) 1.0))
    (should (= (mmpv--subtitle-end (car entries)) 3.0))
    (should (string= (mmpv--subtitle-text (car entries)) "Hello World"))))

(ert-deftest mmpv-test-srt-parsing-multiline ()
  "Test SRT parsing with multiline text."
  (let* ((content "1\n00:00:01,000 --> 00:00:03,000\nLine one\nLine two\n\n")
         (entries (mmpv-parse-srt-from-string content)))
    (should (= (length entries) 1))
    (should (string= (mmpv--subtitle-text (car entries)) "Line one\nLine two"))))

(ert-deftest mmpv-test-srt-parsing-windows-line-endings ()
  "Test SRT parsing with Windows line endings."
  (let* ((content "1\r\n00:00:01,000 --> 00:00:03,000\r\nHello\r\n\r\n")
         (entries (mmpv-parse-srt-from-string content)))
    (should (= (length entries) 1))
    (should (= (mmpv--subtitle-start (car entries)) 1.0))
    (should (string= (mmpv--subtitle-text (car entries)) "Hello"))))

(ert-deftest mmpv-test-srt-parsing-edge-cases ()
  "Test SRT parsing edge cases."
  (should (= (length (mmpv-parse-srt-from-string "")) 0))
  (should (= (length (mmpv-parse-srt-from-string "invalid content")) 0))
  (should (= (length (mmpv-parse-srt-from-string "1\ninvalid --> invalid\nText\n\n")) 0)))

(ert-deftest mmpv-test-find-subtitle-at-time ()
  "Test finding subtitle at specific time."
  (let* ((entries (list (mmpv--subtitle-create :start 0.0 :end 2.0 :text "First")
                        (mmpv--subtitle-create :start 2.0 :end 4.0 :text "Second")
                        (mmpv--subtitle-create :start 4.0 :end 6.0 :text "Third")))
         (session (mmpv-session--create :subtitle-entries entries)))
    (cl-letf (((symbol-value 'mmpv-session) session))
      (should (string= (mmpv--subtitle-text (mmpv-find-subtitle-at-time 1.0)) "First"))
      (should (string= (mmpv--subtitle-text (mmpv-find-subtitle-at-time 3.0)) "Second"))
      (should (null (mmpv-find-subtitle-at-time 10.0))))))

(ert-deftest mmpv-test-callback-structure ()
  "Test callback structure."
  (let ((cb (mmpv--callback-create :id 42 :fn #'ignore)))
    (should (= (mmpv--callback-id cb) 42))
    (should (functionp (mmpv--callback-fn cb)))))

(ert-deftest mmpv-test-session-creation ()
  "Test session creation with defaults."
  (let ((session (mmpv-session--create :socket-path "/tmp/test.sock")))
    (should (string= (mmpv-session-socket-path session) "/tmp/test.sock"))
    (should (null (mmpv-session-process session)))
    (should (null (mmpv-session-socket-proc session)))
    (should (= (mmpv-session-last-time session) -1.0))
    (should (= (mmpv-session-subtitle-last-index session) -1))))

(ert-deftest mmpv-test-debug-macro ()
  "Test debug macro expansion."
  (let ((mmpv-debug nil))
    (should (null (mmpv--debug "test"))))
  (let ((mmpv-debug t))
    (should (string-match-p "test" (mmpv--debug "test")))))

(ert-deftest mmpv-test-with-session-macro ()
  "Test with-session macro."
  (let ((mmpv-session nil))
    (should (null (mmpv--with-session t))))
  (let ((mmpv-session (mmpv-session--create)))
    (should (mmpv--with-session t))))

(ert-deftest mmpv-test-cmd-id-generation ()
  "Test command ID generation."
  (let ((mmpv-session (mmpv-session--create)))
    (should (= (mmpv--next-id) 1))
    (should (= (mmpv--next-id) 2))
    (should (= (mmpv--next-id) 3))))

(ert-deftest mmpv-test-cmd-id-returns-nil-without-session ()
  "Test command ID returns nil without session."
  (let ((mmpv-session nil))
    (should (null (mmpv--next-id)))))

(provide 'mmpv-tests)

;;; mmpv-tests.el ends here
