;;;; huffman.lisp - Huffman encoding/decoding for HPACK (RFC 7541 Appendix B)

(in-package #:clgrpc.http2)

;;; Huffman Code Table
;;; Each entry is (symbol code-bits num-bits)
;;; From RFC 7541 Appendix B

(defparameter *huffman-encode-table*
  #((0 #x1ff8 13) (1 #x7fffd8 23) (2 #xfffffe2 28) (3 #xfffffe3 28)
    (4 #xfffffe4 28) (5 #xfffffe5 28) (6 #xfffffe6 28) (7 #xfffffe7 28)
    (8 #xfffffe8 28) (9 #xffffea 24) (10 #x3ffffffc 30) (11 #xfffffe9 28)
    (12 #xfffffea 28) (13 #x3ffffffd 30) (14 #xfffffeb 28) (15 #xfffffec 28)
    (16 #xfffffed 28) (17 #xfffffee 28) (18 #xfffffef 28) (19 #xffffff0 28)
    (20 #xffffff1 28) (21 #xffffff2 28) (22 #x3ffffffe 30) (23 #xffffff3 28)
    (24 #xffffff4 28) (25 #xffffff5 28) (26 #xffffff6 28) (27 #xffffff7 28)
    (28 #xffffff8 28) (29 #xffffff9 28) (30 #xffffffa 28) (31 #xffffffb 28)
    (32 #x14 6) (33 #x3f8 10) (34 #x3f9 10) (35 #xffa 12) (36 #x1ff9 13)
    (37 #x15 6) (38 #xf8 8) (39 #x7fa 11) (40 #x3fa 10) (41 #x3fb 10)
    (42 #xf9 8) (43 #x7fb 11) (44 #xfa 8) (45 #x16 6) (46 #x17 6)
    (47 #x18 6) (48 #x0 5) (49 #x1 5) (50 #x2 5) (51 #x19 6)
    (52 #x1a 6) (53 #x1b 6) (54 #x1c 6) (55 #x1d 6) (56 #x1e 6)
    (57 #x1f 6) (58 #x5c 7) (59 #xfb 8) (60 #x7ffc 15) (61 #x20 6)
    (62 #xffb 12) (63 #x3fc 10) (64 #x1ffa 13) (65 #x21 6) (66 #x5d 7)
    (67 #x5e 7) (68 #x5f 7) (69 #x60 7) (70 #x61 7) (71 #x62 7)
    (72 #x63 7) (73 #x64 7) (74 #x65 7) (75 #x66 7) (76 #x67 7)
    (77 #x68 7) (78 #x69 7) (79 #x6a 7) (80 #x6b 7) (81 #x6c 7)
    (82 #x6d 7) (83 #x6e 7) (84 #x6f 7) (85 #x70 7) (86 #x71 7)
    (87 #x72 7) (88 #xfc 8) (89 #x73 7) (90 #xfd 8) (91 #x1ffb 13)
    (92 #x7fff0 19) (93 #x1ffc 13) (94 #x3ffc 14) (95 #x22 6) (96 #x7ffd 15)
    (97 #x3 5) (98 #x23 6) (99 #x4 5) (100 #x24 6) (101 #x5 5)
    (102 #x25 6) (103 #x26 6) (104 #x27 6) (105 #x6 5) (106 #x74 7)
    (107 #x75 7) (108 #x28 6) (109 #x29 6) (110 #x2a 6) (111 #x7 5)
    (112 #x2b 6) (113 #x76 7) (114 #x2c 6) (115 #x8 5) (116 #x9 5)
    (117 #x2d 6) (118 #x77 7) (119 #x78 7) (120 #x79 7) (121 #x7a 7)
    (122 #x7b 7) (123 #x7ffe 15) (124 #x7fc 11) (125 #x3ffd 14) (126 #x1ffd 13)
    (127 #xffffffc 28) (128 #xfffe6 20) (129 #x3fffd2 22) (130 #xfffe7 20)
    (131 #xfffe8 20) (132 #x3fffd3 22) (133 #x3fffd4 22) (134 #x3fffd5 22)
    (135 #x7fffd9 23) (136 #x3fffd6 22) (137 #x7fffda 23) (138 #x7fffdb 23)
    (139 #x7fffdc 23) (140 #x7fffdd 23) (141 #x7fffde 23) (142 #xffffeb 24)
    (143 #x7fffdf 23) (144 #xffffec 24) (145 #xffffed 24) (146 #x3fffd7 22)
    (147 #x7fffe0 23) (148 #xffffee 24) (149 #x7fffe1 23) (150 #x7fffe2 23)
    (151 #x7fffe3 23) (152 #x7fffe4 23) (153 #x1fffdc 21) (154 #x3fffd8 22)
    (155 #x7fffe5 23) (156 #x3fffd9 22) (157 #x7fffe6 23) (158 #x7fffe7 23)
    (159 #xffffef 24) (160 #x3fffda 22) (161 #x1fffdd 21) (162 #xfffe9 20)
    (163 #x3fffdb 22) (164 #x3fffdc 22) (165 #x7fffe8 23) (166 #x7fffe9 23)
    (167 #x1fffde 21) (168 #x7fffea 23) (169 #x3fffdd 22) (170 #x3fffde 22)
    (171 #xfffff0 24) (172 #x1fffdf 21) (173 #x3fffdf 22) (174 #x7fffeb 23)
    (175 #x7fffec 23) (176 #x1fffe0 21) (177 #x1fffe1 21) (178 #x3fffe0 22)
    (179 #x1fffe2 21) (180 #x7fffed 23) (181 #x3fffe1 22) (182 #x7fffee 23)
    (183 #x7fffef 23) (184 #xfffea 20) (185 #x3fffe2 22) (186 #x3fffe3 22)
    (187 #x3fffe4 22) (188 #x7ffff0 23) (189 #x3fffe5 22) (190 #x3fffe6 22)
    (191 #x7ffff1 23) (192 #x3ffffe0 26) (193 #x3ffffe1 26) (194 #xfffeb 20)
    (195 #x7fff1 19) (196 #x3fffe7 22) (197 #x7ffff2 23) (198 #x3fffe8 22)
    (199 #x1ffffec 25) (200 #x3ffffe2 26) (201 #x3ffffe3 26) (202 #x3ffffe4 26)
    (203 #x7ffffde 27) (204 #x7ffffdf 27) (205 #x3ffffe5 26) (206 #xfffff1 24)
    (207 #x1ffffed 25) (208 #x7fff2 19) (209 #x1fffe3 21) (210 #x3ffffe6 26)
    (211 #x7ffffe0 27) (212 #x7ffffe1 27) (213 #x3ffffe7 26) (214 #x7ffffe2 27)
    (215 #xfffff2 24) (216 #x1fffe4 21) (217 #x1fffe5 21) (218 #x3ffffe8 26)
    (219 #x3ffffe9 26) (220 #xffffffd 28) (221 #x7ffffe3 27) (222 #x7ffffe4 27)
    (223 #x7ffffe5 27) (224 #xfffec 20) (225 #xfffff3 24) (226 #xfffed 20)
    (227 #x1fffe6 21) (228 #x3fffe9 22) (229 #x1fffe7 21) (230 #x1fffe8 21)
    (231 #x7ffff3 23) (232 #x3fffea 22) (233 #x3fffeb 22) (234 #x1ffffee 25)
    (235 #x1ffffef 25) (236 #xfffff4 24) (237 #xfffff5 24) (238 #x3ffffea 26)
    (239 #x7ffff4 23) (240 #x3ffffeb 26) (241 #x7ffffe6 27) (242 #x3ffffec 26)
    (243 #x3ffffed 26) (244 #x7ffffe7 27) (245 #x7ffffe8 27) (246 #x7ffffe9 27)
    (247 #x7ffffea 27) (248 #x7ffffeb 27) (249 #xffffffe 28) (250 #x7ffffec 27)
    (251 #x7ffffed 27) (252 #x7ffffee 27) (253 #x7ffffef 27) (254 #x7fffff0 27)
    (255 #x3ffffee 26))
  "Huffman encoding table from RFC 7541 Appendix B: indexed by byte value, returns (symbol code num-bits)")

;;; Huffman Decoding Tree
;;; Built lazily on first use

(defvar *huffman-decode-tree* nil
  "Huffman decoding tree for fast decoding")

(defstruct huffman-node
  "Node in Huffman decoding tree"
  (value nil :type (or null (unsigned-byte 8)))  ; Leaf node has byte value
  (left nil :type (or null huffman-node))         ; 0 bit
  (right nil :type (or null huffman-node)))       ; 1 bit

(defun build-huffman-decode-tree ()
  "Build Huffman decoding tree from encoding table"
  (let ((root (make-huffman-node)))
    (loop for symbol from 0 to 255
          do (destructuring-bind (sym code num-bits)
                 (aref *huffman-encode-table* symbol)
               (declare (ignore sym))
               (let ((node root))
                 ;; Walk tree from root, creating nodes as needed
                 (loop for bit-pos from (1- num-bits) downto 0
                       for bit = (ldb (byte 1 bit-pos) code)
                       do (if (zerop bit-pos)
                              ;; Leaf node - store symbol
                              (if (zerop bit)
                                  (setf (huffman-node-left node)
                                        (make-huffman-node :value symbol))
                                  (setf (huffman-node-right node)
                                        (make-huffman-node :value symbol)))
                              ;; Internal node - navigate or create
                              (if (zerop bit)
                                  (progn
                                    (unless (huffman-node-left node)
                                      (setf (huffman-node-left node)
                                            (make-huffman-node)))
                                    (setf node (huffman-node-left node)))
                                  (progn
                                    (unless (huffman-node-right node)
                                      (setf (huffman-node-right node)
                                            (make-huffman-node)))
                                    (setf node (huffman-node-right node)))))))))
    root))

(defun get-huffman-decode-tree ()
  "Get Huffman decode tree, building it if necessary"
  (unless *huffman-decode-tree*
    (setf *huffman-decode-tree* (build-huffman-decode-tree)))
  *huffman-decode-tree*)

;;; Huffman Encoding

(defun huffman-encode (bytes)
  "Encode byte array using Huffman coding. Returns encoded byte array."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))

  ;; First pass: calculate total bits needed
  (let ((total-bits 0))
    (loop for byte across bytes
          do (let ((entry (aref *huffman-encode-table* byte)))
               (incf total-bits (third entry))))

    ;; Allocate output buffer
    (let* ((output-bytes (ceiling total-bits 8))
           (output (make-byte-array output-bytes))
           (bit-pos 0))

      ;; Second pass: write encoded bits
      (loop for byte across bytes
            do (destructuring-bind (sym code num-bits)
                   (aref *huffman-encode-table* byte)
                 (declare (ignore sym))

                 ;; Write bits from most significant to least significant
                 (loop for bit-index from (1- num-bits) downto 0
                       for bit = (ldb (byte 1 bit-index) code)
                       do (let ((byte-index (floor bit-pos 8))
                               (bit-offset (- 7 (mod bit-pos 8))))
                            (when (not (zerop bit))
                              (setf (aref output byte-index)
                                    (logior (aref output byte-index)
                                            (ash 1 bit-offset))))
                            (incf bit-pos)))))

      ;; Pad with 1s to byte boundary (per RFC 7541 Section 5.2)
      (let ((padding-bits (- (* output-bytes 8) bit-pos)))
        (when (plusp padding-bits)
          (let ((byte-index (floor bit-pos 8)))
            (setf (aref output byte-index)
                  (logior (aref output byte-index)
                          (1- (ash 1 (- 8 (mod bit-pos 8)))))))))

      output)))

;;; Huffman Decoding

(defun huffman-decode (bytes)
  "Decode Huffman-encoded byte array. Returns decoded byte array."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))

  (let ((tree (get-huffman-decode-tree))
        (result (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0))
        (node (get-huffman-decode-tree))
        (bits-since-symbol 0)
        (all-ones-padding t))

    ;; Process each bit
    (loop for byte across bytes
          do (loop for bit-offset from 7 downto 0
                   for bit = (ldb (byte 1 bit-offset) byte)
                   do (progn
                        ;; Navigate tree
                        (setf node (if (zerop bit)
                                      (huffman-node-left node)
                                      (huffman-node-right node)))

                        (unless node
                          (error "Huffman decoding error: invalid code"))

                        ;; Track bits since last complete symbol
                        (incf bits-since-symbol)
                        (when (zerop bit)
                          (setf all-ones-padding nil))

                        ;; Check if we reached a leaf
                        (when (huffman-node-value node)
                          (vector-push-extend (huffman-node-value node) result)
                          (setf node tree)
                          (setf bits-since-symbol 0)
                          (setf all-ones-padding t)))))

    ;; RFC 7541 Section 5.2: Validate padding
    ;; Padding must be < 8 bits of all 1s (otherwise it's EOS symbol)
    (when (and (not (eq node tree))
               all-ones-padding
               (>= bits-since-symbol 8))
      (error "Huffman decoding error: invalid padding (>= 8 bits of 1s)"))

    ;; Convert to simple array
    (make-array (length result)
                :element-type '(unsigned-byte 8)
                :initial-contents result)))

;;; String Encoding/Decoding Helpers

(defun huffman-encode-string (string)
  "Encode a string using Huffman coding. Returns byte array."
  (huffman-encode (babel:string-to-octets string :encoding :utf-8)))

(defun huffman-decode-string (bytes)
  "Decode Huffman-encoded bytes to string."
  (babel:octets-to-string (huffman-decode bytes) :encoding :utf-8))
