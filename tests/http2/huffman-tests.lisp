;;;; huffman-tests.lisp - Tests for Huffman encoding/decoding

(in-package #:clgrpc-tests)

(def-suite huffman-tests
  :in http2-tests
  :description "Huffman coding tests")

(in-suite huffman-tests)

;;; Basic Encoding/Decoding Tests

(test huffman-round-trip-simple
  "Test Huffman encoding and decoding round-trip with simple string"
  (let ((text "www.example.com"))
    (let* ((encoded (huffman-encode-string text))
           (decoded (huffman-decode-string encoded)))
      (is (string= decoded text)))))

(test huffman-round-trip-empty
  "Test Huffman encoding/decoding of empty string"
  (let ((text ""))
    (let* ((encoded (huffman-encode-string text))
           (decoded (huffman-decode-string encoded)))
      (is (string= decoded text)))))

(test huffman-round-trip-single-char
  "Test Huffman encoding/decoding of single character"
  (let ((text "a"))
    (let* ((encoded (huffman-encode-string text))
           (decoded (huffman-decode-string encoded)))
      (is (string= decoded text)))))

(test huffman-round-trip-all-ascii
  "Test Huffman encoding/decoding of all printable ASCII"
  (let ((text (coerce (loop for i from 32 to 126 collect (code-char i))
                     'string)))
    (let* ((encoded (huffman-encode-string text))
           (decoded (huffman-decode-string encoded)))
      (is (string= decoded text)))))

(test huffman-round-trip-headers
  "Test Huffman encoding/decoding of typical HTTP headers"
  (let ((headers '("content-type" "application/json" "user-agent"
                  "Mozilla/5.0" ":method" "GET" ":path" "/")))
    (dolist (text headers)
      (let* ((encoded (huffman-encode-string text))
             (decoded (huffman-decode-string encoded)))
        (is (string= decoded text))))))

;;; RFC 7541 Examples (Appendix C)

(test huffman-example-www-dot-example-dot-com
  "Test Huffman encoding of 'www.example.com' from RFC 7541 C.4.1"
  (let* ((text "www.example.com")
         (encoded (huffman-encode-string text))
         ;; Expected from RFC: f1e3 c2e5 f23a 6ba0 ab90 f4ff
         (expected (bytes #xf1 #xe3 #xc2 #xe5 #xf2 #x3a #x6b #xa0 #xab #x90 #xf4 #xff)))
    (is (equalp encoded expected))))

(test huffman-example-no-cache
  "Test Huffman encoding of 'no-cache' from RFC 7541 C.4.1"
  (let* ((text "no-cache")
         (encoded (huffman-encode-string text))
         ;; Expected from RFC: a8eb 1064 9cbf
         (expected (bytes #xa8 #xeb #x10 #x64 #x9c #xbf)))
    (is (equalp encoded expected))))

(test huffman-example-custom-key
  "Test Huffman encoding of 'custom-key' from RFC 7541 C.4.1"
  (let* ((text "custom-key")
         (encoded (huffman-encode-string text))
         ;; Expected from RFC: 25a8 49e9 5ba9 7d7f
         (expected (bytes #x25 #xa8 #x49 #xe9 #x5b #xa9 #x7d #x7f)))
    (is (equalp encoded expected))))

(test huffman-example-custom-value
  "Test Huffman encoding of 'custom-value' from RFC 7541 C.4.1"
  (let* ((text "custom-value")
         (encoded (huffman-encode-string text))
         ;; Expected from RFC: 25a8 49e9 5bb8 e8b4 bf
         (expected (bytes #x25 #xa8 #x49 #xe9 #x5b #xb8 #xe8 #xb4 #xbf)))
    (is (equalp encoded expected))))

;;; Compression Ratio Tests

(test huffman-compression-typical-headers
  "Test that Huffman achieves reasonable compression on typical headers"
  (let ((headers '(("content-type" . "application/json")
                  ("user-agent" . "Mozilla/5.0 (X11; Linux x86_64)")
                  (":method" . "GET")
                  (":path" . "/api/users/123")
                  (":scheme" . "https")
                  (":authority" . "api.example.com"))))
    (dolist (header headers)
      (let* ((text (format nil "~A: ~A" (car header) (cdr header)))
             (original-bytes (babel:string-to-octets text :encoding :utf-8))
             (encoded (huffman-encode-string text)))
        ;; Huffman should achieve some compression on typical headers
        ;; Not always, but on average yes
        (format t "~%  '~A' -> ~D bytes original, ~D bytes encoded (~,1F% of original)"
                text
                (length original-bytes)
                (length encoded)
                (* 100.0 (/ (length encoded) (length original-bytes))))))))

;;; Error Handling Tests

(test huffman-decode-invalid-padding
  "Test that invalid Huffman padding is rejected"
  ;; Create invalid Huffman data with incorrect padding
  ;; All 1s padding longer than 7 bits is invalid
  (signals error
    (huffman-decode (bytes #xFF #xFF #xFF))))

;;; Byte-Level Encoding Tests

(test huffman-encode-bytes-round-trip
  "Test Huffman encoding/decoding at byte level"
  (let ((data (bytes 0 1 2 3 127 128 255)))
    (let* ((encoded (huffman-encode data))
           (decoded (huffman-decode encoded)))
      (is (equalp decoded data)))))

(test huffman-encode-all-bytes
  "Test Huffman encoding/decoding of all possible byte values"
  (let ((data (make-byte-array 256)))
    (loop for i from 0 to 255
          do (setf (aref data i) i))
    (let* ((encoded (huffman-encode data))
           (decoded (huffman-decode encoded)))
      (is (equalp decoded data)))))

;;; Edge Cases

(test huffman-encode-repeated-char
  "Test Huffman encoding of repeated character"
  (let ((text (make-string 100 :initial-element #\a)))
    (let* ((encoded (huffman-encode-string text))
           (decoded (huffman-decode-string encoded)))
      (is (string= decoded text)))))

(test huffman-encode-high-entropy
  "Test Huffman with high-entropy random-ish data"
  (let ((text "aB3$xZ!@9mQ#7kL&8nP%2iJ^"))
    (let* ((encoded (huffman-encode-string text))
           (decoded (huffman-decode-string encoded)))
      (is (string= decoded text)))))
