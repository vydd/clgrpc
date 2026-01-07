;;;; hpack-tests.lisp - Tests for HPACK header compression

(in-package #:clgrpc-tests)

(def-suite hpack-tests
  :in http2-tests
  :description "HPACK header compression tests")

(in-suite hpack-tests)

;;; Static Table Tests

(test hpack-static-table-lookup
  "Test HPACK static table lookups"
  ;; Index 1: :authority
  (let ((entry (hpack-lookup-index (make-hpack-context) 1)))
    (is (equal (car entry) :authority))
    (is (string= (cdr entry) "")))

  ;; Index 2: :method GET
  (let ((entry (hpack-lookup-index (make-hpack-context) 2)))
    (is (equal (car entry) :method))
    (is (string= (cdr entry) "GET")))

  ;; Index 4: :path /
  (let ((entry (hpack-lookup-index (make-hpack-context) 4)))
    (is (equal (car entry) :path))
    (is (string= (cdr entry) "/"))))

;;; Integer Encoding/Decoding Tests

(test hpack-integer-encode-decode-5bit
  "Test integer encoding/decoding with 5-bit prefix"
  (loop for value in '(0 1 10 30 31 127 255 1000)
        do (let* ((encoded (hpack-encode-integer value 5))
                 (bytes (make-array (length encoded)
                                   :element-type '(unsigned-byte 8)
                                   :initial-contents encoded)))
            (multiple-value-bind (decoded offset)
                (hpack-decode-integer bytes 0 5)
              (is (= decoded value))
              (is (= offset (length encoded)))))))

(test hpack-integer-encode-decode-7bit
  "Test integer encoding/decoding with 7-bit prefix"
  (loop for value in '(0 1 126 127 128 255 1337)
        do (let* ((encoded (hpack-encode-integer value 7))
                 (bytes (make-array (length encoded)
                                   :element-type '(unsigned-byte 8)
                                   :initial-contents encoded)))
            (multiple-value-bind (decoded offset)
                (hpack-decode-integer bytes 0 7)
              (is (= decoded value))
              (is (= offset (length encoded)))))))

;;; String Encoding/Decoding Tests

(test hpack-string-encode-decode-plain
  "Test HPACK string encoding/decoding without Huffman"
  (let ((text "www.example.com"))
    (let ((encoded (hpack-encode-string text :huffman nil)))
      (multiple-value-bind (decoded offset)
          (hpack-decode-string encoded 0)
        (is (string= decoded text))
        (is (= offset (length encoded)))))))

(test hpack-string-encode-decode-huffman
  "Test HPACK string encoding/decoding with Huffman"
  (let ((text "www.example.com"))
    (let ((encoded (hpack-encode-string text :huffman t)))
      ;; First byte should have Huffman flag set
      (is (not (zerop (logand (aref encoded 0) #x80))))
      (multiple-value-bind (decoded offset)
          (hpack-decode-string encoded 0)
        (is (string= decoded text))
        (is (= offset (length encoded)))))))

;;; Dynamic Table Tests

(test hpack-dynamic-table-add-and-lookup
  "Test adding entries to dynamic table"
  (let ((ctx (make-hpack-context)))
    ;; Add entry
    (hpack-dynamic-table-add ctx "custom-key" "custom-value")

    ;; Lookup by index (62 = first dynamic table entry)
    (let ((entry (hpack-lookup-index ctx 62)))
      (is (string= (car entry) "custom-key"))
      (is (string= (cdr entry) "custom-value")))))

(test hpack-dynamic-table-eviction
  "Test dynamic table eviction when size limit exceeded"
  (let ((ctx (make-hpack-context)))
    ;; Set small max size
    (setf (hpack-context-max-dynamic-table-size ctx) 100)

    ;; Add entries until eviction occurs
    (hpack-dynamic-table-add ctx "key1" "value1")  ; 38 bytes
    (hpack-dynamic-table-add ctx "key2" "value2")  ; 38 bytes
    (hpack-dynamic-table-add ctx "key3" "value3")  ; 38 bytes (total 114 > 100)

    ;; key1 should have been evicted
    (is (<= (hpack-context-dynamic-table-size ctx) 100))

    ;; Most recent entries should still be there
    (let ((entry (hpack-lookup-index ctx 62)))
      (is (string= (car entry) "key3")))))

;;; Header Encoding/Decoding Tests

(test hpack-indexed-header-encoding
  "Test encoding of indexed header field"
  (let ((ctx (make-hpack-context)))
    ;; Encode :method GET (static table index 2)
    (let ((encoded (hpack-encode-indexed 2)))
      ;; Should be single byte: 10000010 = 0x82
      (is (= (length encoded) 1))
      (is (= (aref encoded 0) #x82))

      ;; Decode it back
      (multiple-value-bind (name value offset)
          (hpack-decode-header ctx encoded 0)
        (is (equal name :method))
        (is (string= value "GET"))
        (is (= offset 1))))))

(test hpack-literal-with-indexing
  "Test literal header with incremental indexing"
  (let ((ctx (make-hpack-context)))
    ;; Encode custom header
    (let ((encoded (hpack-encode-literal-with-indexing
                   ctx "custom-key" "custom-value"
                   :huffman nil)))

      ;; Decode it back
      (multiple-value-bind (name value offset)
          (hpack-decode-header ctx encoded 0)
        (is (string= name "custom-key"))
        (is (string= value "custom-value"))
        (is (= offset (length encoded)))

        ;; Should now be in dynamic table
        (let ((entry (hpack-lookup-index ctx 62)))
          (is (string= (car entry) "custom-key"))
          (is (string= (cdr entry) "custom-value")))))))

(test hpack-round-trip-simple
  "Test HPACK encoding/decoding round-trip"
  (let ((ctx-encode (make-hpack-context))
        (ctx-decode (make-hpack-context)))

    (let* ((headers '((:method . "GET")
                     (:path . "/")
                     (:scheme . "https")
                     ("custom-header" . "custom-value")))
           (encoded (hpack-encode-headers ctx-encode headers :huffman nil))
           (decoded (hpack-decode-headers ctx-decode encoded)))

      (is (= (length decoded) (length headers)))
      (loop for original in headers
            for decoded-header in decoded
            do (is (equal (car original) (car decoded-header)))
               (is (string= (cdr original) (cdr decoded-header)))))))

;;; RFC 7541 Examples (Appendix C)

(test hpack-example-c2-1-literal-with-indexing
  "Test RFC 7541 C.2.1 - Literal Header Field with Incremental Indexing"
  (let ((ctx (make-hpack-context)))
    ;; Example: custom-key: custom-header (no Huffman)
    ;; Expected bytes: 40 0a 63757374 6f6d2d6b 6579 0d 63757374 6f6d2d68 65616465 72
    (let* ((expected (bytes #x40  ; Literal with incremental indexing, new name
                           #x0a #x63 #x75 #x73 #x74 #x6f #x6d #x2d #x6b #x65 #x79  ; "custom-key"
                           #x0d #x63 #x75 #x73 #x74 #x6f #x6d #x2d #x68 #x65 #x61 #x64 #x65 #x72))  ; "custom-header"
           (decoded-headers (hpack-decode-headers ctx expected)))

      (is (= (length decoded-headers) 1))
      (is (string= (car (first decoded-headers)) "custom-key"))
      (is (string= (cdr (first decoded-headers)) "custom-header")))))

(test hpack-example-c3-request-no-huffman
  "Test RFC 7541 C.3 - Request Examples without Huffman Coding"
  (let ((ctx (make-hpack-context)))

    ;; First request
    ;; :method GET, :scheme http, :path /, :authority www.example.com
    (let* ((request1-bytes (bytes
                           #x82  ; :method GET (indexed)
                           #x86  ; :scheme http (indexed)
                           #x84  ; :path / (indexed)
                           #x41 #x0f #x77 #x77 #x77 #x2e #x65 #x78 #x61 #x6d #x70 #x6c #x65 #x2e #x63 #x6f #x6d))  ; :authority www.example.com
           (headers (hpack-decode-headers ctx request1-bytes)))

      (is (= (length headers) 4))
      (is (equal (car (nth 0 headers)) :method))
      (is (string= (cdr (nth 0 headers)) "GET"))
      (is (equal (car (nth 3 headers)) :authority))
      (is (string= (cdr (nth 3 headers)) "www.example.com")))))

;;; Compression Efficiency Tests

(test hpack-compression-repeated-headers
  "Test that HPACK achieves good compression on repeated headers"
  (let ((ctx (make-hpack-context)))
    (let* ((headers '((:method . "GET")
                     (:scheme . "https")
                     (:path . "/")
                     (:authority . "example.com")))
           (first-encoded (hpack-encode-headers ctx headers))
           (second-encoded (hpack-encode-headers ctx headers)))

      ;; Second encoding should be much smaller (all indexed)
      (is (< (length second-encoded) (length first-encoded)))

      ;; In fact, should be about 4 bytes (one indexed entry per header)
      (is (< (length second-encoded) 10)))))

(test hpack-find-index-exact-match
  "Test finding exact header match in tables"
  (let ((ctx (make-hpack-context)))
    ;; :method GET is at index 2
    (let ((index (hpack-find-index ctx :method "GET")))
      (is (= index 2)))

    ;; Add custom header
    (hpack-dynamic-table-add ctx "custom" "value")

    ;; Should find it at index 62 (first dynamic table entry)
    (let ((index (hpack-find-index ctx "custom" "value")))
      (is (= index 62)))))

(test hpack-find-name-index
  "Test finding header name in tables"
  (let ((ctx (make-hpack-context)))
    ;; :method is at index 2 (GET) or 3 (POST)
    (let ((index (hpack-find-name-index ctx :method)))
      (is (or (= index 2) (= index 3))))

    ;; Add custom header
    (hpack-dynamic-table-add ctx "x-custom" "value1")

    ;; Should find name at index 62
    (let ((index (hpack-find-name-index ctx "x-custom")))
      (is (= index 62)))))

;;; Edge Cases

(test hpack-empty-header-value
  "Test encoding/decoding header with empty value"
  (let ((ctx-encode (make-hpack-context))
        (ctx-decode (make-hpack-context)))

    (let* ((headers '(("empty-value" . "")))
           (encoded (hpack-encode-headers ctx-encode headers))
           (decoded (hpack-decode-headers ctx-decode encoded)))

      (is (= (length decoded) 1))
      (is (string= (car (first decoded)) "empty-value"))
      (is (string= (cdr (first decoded)) "")))))

(test hpack-large-header-value
  "Test encoding/decoding header with large value"
  (let ((ctx-encode (make-hpack-context))
        (ctx-decode (make-hpack-context)))

    (let* ((large-value (make-string 1000 :initial-element #\x))
           (headers `(("big-header" . ,large-value)))
           (encoded (hpack-encode-headers ctx-encode headers))
           (decoded (hpack-decode-headers ctx-decode encoded)))

      (is (= (length decoded) 1))
      (is (string= (car (first decoded)) "big-header"))
      (is (string= (cdr (first decoded)) large-value)))))

(test hpack-max-table-size-zero
  "Test HPACK with zero max dynamic table size"
  (let ((ctx (make-hpack-context)))
    (setf (hpack-context-max-dynamic-table-size ctx) 0)

    ;; Try to add entry - should not be added
    (hpack-dynamic-table-add ctx "key" "value")

    (is (= (hpack-context-dynamic-table-size ctx) 0))
    (is (= (length (hpack-context-dynamic-table ctx)) 0))))
