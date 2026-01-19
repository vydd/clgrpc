;;;; hpack.lisp - HPACK header compression for HTTP/2 (RFC 7541)

(in-package #:clgrpc.http2)

;;; HPACK Static Table (RFC 7541 Appendix A)

(defparameter *hpack-static-table*
  #(nil  ; Index 0 is not used
    (:authority . "")
    (:method . "GET")
    (:method . "POST")
    (:path . "/")
    (:path . "/index.html")
    (:scheme . "http")
    (:scheme . "https")
    (:status . "200")
    (:status . "204")
    (:status . "206")
    (:status . "304")
    (:status . "400")
    (:status . "404")
    (:status . "500")
    ("accept-charset" . "")
    ("accept-encoding" . "gzip, deflate")
    ("accept-language" . "")
    ("accept-ranges" . "")
    ("accept" . "")
    ("access-control-allow-origin" . "")
    ("age" . "")
    ("allow" . "")
    ("authorization" . "")
    ("cache-control" . "")
    ("content-disposition" . "")
    ("content-encoding" . "")
    ("content-language" . "")
    ("content-length" . "")
    ("content-location" . "")
    ("content-range" . "")
    ("content-type" . "")
    ("cookie" . "")
    ("date" . "")
    ("etag" . "")
    ("expect" . "")
    ("expires" . "")
    ("from" . "")
    ("host" . "")
    ("if-match" . "")
    ("if-modified-since" . "")
    ("if-none-match" . "")
    ("if-range" . "")
    ("if-unmodified-since" . "")
    ("last-modified" . "")
    ("link" . "")
    ("location" . "")
    ("max-forwards" . "")
    ("proxy-authenticate" . "")
    ("proxy-authorization" . "")
    ("range" . "")
    ("referer" . "")
    ("refresh" . "")
    ("retry-after" . "")
    ("server" . "")
    ("set-cookie" . "")
    ("strict-transport-security" . "")
    ("transfer-encoding" . "")
    ("user-agent" . "")
    ("vary" . "")
    ("via" . "")
    ("www-authenticate" . ""))
  "HPACK static table with 61 entries (index 1-61)")

(defconstant +hpack-static-table-size+ 61
  "Number of entries in static table")

;;; Header Field Normalization (needed early for static table indexing)

(defun normalize-header-field (field)
  "Normalize header field (keyword or string) to lowercase string.
   Handles both HTTP/2 pseudo-headers (keywords) and regular headers (strings)."
  (etypecase field
    (string field)
    (keyword (string-downcase (symbol-name field)))
    (symbol (string-downcase (symbol-name field)))))

;;; Static Table Hash Indexes (for O(1) lookup)

(defvar *static-exact-index* (make-hash-table :test 'equal)
  "Hash table mapping (name . value) -> static table index for exact matches")

(defvar *static-name-index* (make-hash-table :test 'equal)
  "Hash table mapping name -> first static table index with that name")

(defun initialize-static-table-indexes ()
  "Build hash indexes for static table lookups. Called once at load time."
  (clrhash *static-exact-index*)
  (clrhash *static-name-index*)
  (loop for i from 1 to +hpack-static-table-size+
        for entry = (aref *hpack-static-table* i)
        when entry
        do (let ((norm-name (normalize-header-field (car entry)))
                 (norm-value (normalize-header-field (cdr entry))))
             ;; Exact match: (name . value) -> index
             (setf (gethash (cons norm-name norm-value) *static-exact-index*) i)
             ;; Name match: name -> first index (only if not already set)
             (unless (gethash norm-name *static-name-index*)
               (setf (gethash norm-name *static-name-index*) i)))))

;; Initialize at load time
(initialize-static-table-indexes)

;;; HPACK Dynamic Table

(defstruct hpack-entry
  "Entry in HPACK dynamic table"
  (name "" :type string)
  (value "" :type string)
  (size 0 :type fixnum))  ; Size in bytes (name-len + value-len + 32)

(defun hpack-entry-bytes (name value)
  "Calculate size of header entry in bytes (RFC 7541 Section 4.1).
   Accepts keywords, symbols, or strings for name/value."
  (let ((norm-name (normalize-header-field name))
        (norm-value (normalize-header-field value)))
    (+ (length norm-name) (length norm-value) 32)))

(defstruct hpack-context
  "HPACK compression context with dynamic table"
  (dynamic-table (make-array 0 :adjustable t :fill-pointer 0)
   :type (vector hpack-entry))
  (dynamic-table-size 0 :type fixnum)
  (max-dynamic-table-size 4096 :type fixnum))  ; Default 4KB

(defun hpack-dynamic-table-evict (context)
  "Evict entries from dynamic table to fit within max size"
  (let ((table (hpack-context-dynamic-table context)))
    (loop while (and (plusp (length table))
                    (> (hpack-context-dynamic-table-size context)
                       (hpack-context-max-dynamic-table-size context)))
          do (let* ((last-index (1- (length table)))
                   (entry (aref table last-index)))
               (decf (hpack-context-dynamic-table-size context)
                     (hpack-entry-size entry))
               (vector-pop table)))))

(defun hpack-dynamic-table-add (context name value)
  "Add entry to dynamic table (at index 0, shifting others).
   Accepts keywords, symbols, or strings for name/value - normalizes to strings."
  (let* ((norm-name (normalize-header-field name))
         (norm-value (normalize-header-field value))
         (size (hpack-entry-bytes norm-name norm-value))
         (entry (make-hpack-entry :name norm-name :value norm-value :size size))
         (table (hpack-context-dynamic-table context)))

    ;; Check if entry fits in table at all
    (when (> size (hpack-context-max-dynamic-table-size context))
      ;; Entry larger than max size - clear table
      (setf (fill-pointer table) 0)
      (setf (hpack-context-dynamic-table-size context) 0)
      (return-from hpack-dynamic-table-add nil))

    ;; Add to front of table
    (vector-push-extend nil table)
    (loop for i from (1- (length table)) downto 1
          do (setf (aref table i) (aref table (1- i))))
    (setf (aref table 0) entry)

    ;; Update size and evict if necessary
    (incf (hpack-context-dynamic-table-size context) size)
    (hpack-dynamic-table-evict context)))

;;; HPACK Table Lookups

(defun denormalize-header-name (name)
  "Convert pseudo-header strings back to keywords (e.g., \"path\" -> :PATH).
   Regular headers remain as strings."
  (if (and (stringp name)
           (member name '("method" "scheme" "path" "authority") :test #'string=))
      ;; Pseudo-header: convert to uppercase keyword (intern in :keyword adds the :)
      (intern (string-upcase name) :keyword)
      ;; Regular header: keep as string
      name))

(defun hpack-lookup-index (context index)
  "Lookup header by index (1-based). Returns (name . value) or nil."
  (cond
    ((zerop index) nil)
    ((<= index +hpack-static-table-size+)
     ;; Static table lookup
     (aref *hpack-static-table* index))
    (t
     ;; Dynamic table lookup
     (let ((dynamic-index (- index +hpack-static-table-size+ 1)))
       (when (< dynamic-index (length (hpack-context-dynamic-table context)))
         (let ((entry (aref (hpack-context-dynamic-table context) dynamic-index)))
           ;; Convert pseudo-headers back to keywords
           (cons (denormalize-header-name (hpack-entry-name entry))
                 (hpack-entry-value entry))))))))

(defun hpack-find-index (context name value)
  "Find index of exact header match. Returns index or nil.
   Uses hash table for O(1) static table lookup."
  (let* ((norm-name (normalize-header-field name))
         (norm-value (normalize-header-field value))
         (key (cons norm-name norm-value)))
    ;; O(1) static table lookup via hash
    (or (gethash key *static-exact-index*)
        ;; Dynamic table: still need linear search (changes frequently)
        ;; but dynamic tables are typically small
        (loop for i from 0 below (length (hpack-context-dynamic-table context))
              for entry = (aref (hpack-context-dynamic-table context) i)
              when (and (string= (hpack-entry-name entry) norm-name)
                        (string= (hpack-entry-value entry) norm-value))
              return (+ i +hpack-static-table-size+ 1)))))

(defun hpack-find-name-index (context name)
  "Find index of header name (ignoring value). Returns index or nil.
   Uses hash table for O(1) static table lookup."
  (let ((norm-name (normalize-header-field name)))
    ;; O(1) static table lookup via hash
    (or (gethash norm-name *static-name-index*)
        ;; Dynamic table: linear search (typically small)
        (loop for i from 0 below (length (hpack-context-dynamic-table context))
              for entry = (aref (hpack-context-dynamic-table context) i)
              when (string= (hpack-entry-name entry) norm-name)
              return (+ i +hpack-static-table-size+ 1)))))

;;; Integer Encoding/Decoding with Prefix (RFC 7541 Section 5.1)

(defun hpack-encode-integer (value prefix-bits)
  "Encode integer with N-bit prefix. Returns list of bytes."
  (declare (type fixnum value prefix-bits))
  (let ((max-prefix (1- (ash 1 prefix-bits))))
    (if (< value max-prefix)
        ;; Fits in prefix
        (list value)
        ;; Multi-byte encoding
        (let ((result (list max-prefix)))
          (decf value max-prefix)
          (loop while (>= value 128)
                do (push (logior 128 (logand value 127)) result)
                   (setf value (ash value -7)))
          (push value result)
          (nreverse result)))))

(defun hpack-decode-integer (bytes offset prefix-bits)
  "Decode integer with N-bit prefix from byte array.
   Returns (values integer new-offset)"
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum offset prefix-bits))
  (let* ((max-prefix (1- (ash 1 prefix-bits)))
         (first-byte (aref bytes offset))
         (prefix-mask (1- (ash 1 prefix-bits)))
         (value (logand first-byte prefix-mask)))
    (incf offset)

    (if (< value max-prefix)
        ;; Value fits in prefix
        (values value offset)
        ;; Multi-byte value
        (let ((m 0))
          (loop
            (when (>= offset (length bytes))
              (error "Unexpected end of integer"))
            (let ((byte (aref bytes offset)))
              (incf offset)
              (incf value (* (logand byte 127) (ash 1 m)))
              (incf m 7)
              (when (zerop (logand byte 128))
                (return))))
          (values value offset)))))

;;; String Encoding/Decoding (RFC 7541 Section 5.2)

(defun hpack-encode-string (string &key huffman)
  "Encode string for HPACK. Returns byte array.
   Accepts strings, keywords, or symbols - all normalized to strings."
  (let* ((str (normalize-header-field string))
         (octets (if huffman
                    (huffman-encode-string str)
                    (babel:string-to-octets str :encoding :utf-8)))
         (length (length octets))
         (length-bytes (hpack-encode-integer length 7))
         (result (make-byte-array (+ (length length-bytes) length))))

    ;; First byte includes Huffman flag
    (setf (aref result 0) (first length-bytes))
    (when huffman
      (setf (aref result 0) (logior (aref result 0) #x80)))

    ;; Remaining length bytes
    (loop for i from 1 below (length length-bytes)
          do (setf (aref result i) (nth i length-bytes)))

    ;; Copy string data
    (copy-bytes octets result :start2 (length length-bytes))
    result))

(defun hpack-decode-string (bytes offset)
  "Decode HPACK string from byte array.
   Returns (values string new-offset)"
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum offset))
  (let* ((first-byte (aref bytes offset))
         (huffman-encoded (not (zerop (logand first-byte #x80)))))

    (multiple-value-bind (length new-offset)
        (hpack-decode-integer bytes offset 7)

      (when (> (+ new-offset length) (length bytes))
        (error "String length exceeds buffer"))

      (let ((string-bytes (subseq bytes new-offset (+ new-offset length))))
        (values
         (if huffman-encoded
             (huffman-decode-string string-bytes)
             (babel:octets-to-string string-bytes :encoding :utf-8))
         (+ new-offset length))))))

;;; Header Encoding

(defun hpack-encode-indexed (index)
  "Encode indexed header field representation. Returns byte array."
  (let* ((int-bytes (hpack-encode-integer index 7))
         (result (make-byte-array (length int-bytes))))
    ;; Set top bit to 1
    (setf (aref result 0) (logior (first int-bytes) #x80))
    (loop for i from 1 below (length int-bytes)
          do (setf (aref result i) (nth i int-bytes)))
    result))

(defun hpack-encode-literal-with-indexing (context name value &key name-index huffman)
  "Encode literal header with incremental indexing. Returns byte array."
  (let ((parts nil))
    ;; Prefix: 01
    (if name-index
        ;; Name in table
        (let ((int-bytes (hpack-encode-integer name-index 6)))
          (push (make-byte-array 1 :initial-element (logior (first int-bytes) #x40)) parts)
          (loop for byte in (cdr int-bytes)
                do (push (make-byte-array 1 :initial-element byte) parts)))
        ;; New name
        (progn
          (push (make-byte-array 1 :initial-element #x40) parts)
          (push (hpack-encode-string name :huffman huffman) parts)))

    ;; Value
    (push (hpack-encode-string value :huffman huffman) parts)

    ;; Add to dynamic table
    (hpack-dynamic-table-add context name value)

    ;; Concatenate parts
    (let* ((total-size (reduce #'+ parts :key #'length))
           (result (make-byte-array total-size))
           (pos 0))
      (dolist (part (nreverse parts))
        (copy-bytes part result :start2 pos)
        (incf pos (length part)))
      result)))

(defun hpack-encode-literal-without-indexing (name value &key name-index huffman)
  "Encode literal header without indexing. Returns byte array."
  (let ((parts nil))
    ;; Prefix: 0000
    (if name-index
        ;; Name in table
        (let ((int-bytes (hpack-encode-integer name-index 4)))
          (push (make-byte-array 1 :initial-element (first int-bytes)) parts)
          (loop for byte in (cdr int-bytes)
                do (push (make-byte-array 1 :initial-element byte) parts)))
        ;; New name
        (progn
          (push (make-byte-array 1 :initial-element #x00) parts)
          (push (hpack-encode-string name :huffman huffman) parts)))

    ;; Value
    (push (hpack-encode-string value :huffman huffman) parts)

    ;; Concatenate parts
    (let* ((total-size (reduce #'+ parts :key #'length))
           (result (make-byte-array total-size))
           (pos 0))
      (dolist (part (nreverse parts))
        (copy-bytes part result :start2 pos)
        (incf pos (length part)))
      result)))

(defun hpack-encode-header (context name value &key (huffman t) (indexing :incremental))
  "Encode a single header field.
   indexing: :incremental (add to table), :none (don't add), :never (sensitive)"
  ;; Try to find exact match
  (let ((exact-index (hpack-find-index context name value)))
    (when exact-index
      (return-from hpack-encode-header (hpack-encode-indexed exact-index))))

  ;; Try to find name match
  (let ((name-index (hpack-find-name-index context name)))
    (ecase indexing
      (:incremental
       (hpack-encode-literal-with-indexing context name value
                                          :name-index name-index
                                          :huffman huffman))
      (:none
       (hpack-encode-literal-without-indexing name value
                                             :name-index name-index
                                             :huffman huffman))
      (:never
       ;; TODO: Implement literal never indexed (prefix: 0001)
       (hpack-encode-literal-without-indexing name value
                                             :name-index name-index
                                             :huffman huffman)))))

(defun hpack-encode-headers (context headers &key (huffman t))
  "Encode list of headers. Headers is list of (name . value) cons cells.
   Returns byte array."
  (let ((parts nil))
    (dolist (header headers)
      (push (hpack-encode-header context (car header) (cdr header)
                                :huffman huffman)
            parts))
    ;; Concatenate all encoded headers
    (let* ((total-size (reduce #'+ parts :key #'length))
           (result (make-byte-array total-size))
           (pos 0))
      (dolist (part (nreverse parts))
        (copy-bytes part result :start2 pos)
        (incf pos (length part)))
      result)))

;;; Header Decoding

(defun hpack-decode-header (context bytes offset)
  "Decode a single header field representation.
   Returns (values name value new-offset)"
  (when (>= offset (length bytes))
    (error "Unexpected end of header block"))

  (let ((first-byte (aref bytes offset)))
    (cond
      ;; Indexed header field (top bit = 1)
      ((not (zerop (logand first-byte #x80)))
       (multiple-value-bind (index new-offset)
           (hpack-decode-integer bytes offset 7)
         (let ((entry (hpack-lookup-index context index)))
           (unless entry
             (error "Invalid header index: ~A" index))
           (values (car entry) (cdr entry) new-offset))))

      ;; Literal with incremental indexing (top 2 bits = 01)
      ((not (zerop (logand first-byte #x40)))
       (multiple-value-bind (name-index new-offset)
           (hpack-decode-integer bytes offset 6)
         (let ((name (if (zerop name-index)
                        ;; New name
                        (multiple-value-bind (decoded-name newer-offset)
                            (hpack-decode-string bytes new-offset)
                          (setf new-offset newer-offset)
                          decoded-name)
                        ;; Name from table
                        (let ((entry (hpack-lookup-index context name-index)))
                          (unless entry
                            (error "Invalid name index: ~A" name-index))
                          (car entry)))))
           ;; Decode value
           (multiple-value-bind (value final-offset)
               (hpack-decode-string bytes new-offset)
             ;; Add to dynamic table
             (hpack-dynamic-table-add context name value)
             (values name value final-offset)))))

      ;; Literal without indexing (top 4 bits = 0000)
      ;; or never indexed (top 4 bits = 0001)
      (t
       (let ((never-indexed (not (zerop (logand first-byte #x10)))))
         (multiple-value-bind (name-index new-offset)
             (hpack-decode-integer bytes offset 4)
           (let ((name (if (zerop name-index)
                          ;; New name
                          (multiple-value-bind (decoded-name newer-offset)
                              (hpack-decode-string bytes new-offset)
                            (setf new-offset newer-offset)
                            decoded-name)
                          ;; Name from table
                          (let ((entry (hpack-lookup-index context name-index)))
                            (unless entry
                              (error "Invalid name index: ~A" name-index))
                            (car entry)))))
             ;; Decode value
             (multiple-value-bind (value final-offset)
                 (hpack-decode-string bytes new-offset)
               (declare (ignore never-indexed))  ; TODO: Mark as sensitive
               (values name value final-offset)))))))))

(defun hpack-decode-headers (context bytes)
  "Decode header block. Returns list of (name . value) cons cells."
  (let ((headers nil)
        (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (name value new-offset)
                 (hpack-decode-header context bytes offset)
               (push (cons name value) headers)
               (setf offset new-offset)))
    (nreverse headers)))
