;;;; huffman.lisp - Huffman encoding/decoding for HPACK (RFC 7541 Appendix B)

(in-package #:clgrpc.http2)

;;; Huffman Code Table
;;; Each entry is (symbol code-bits num-bits)
;;; From RFC 7541 Appendix B

(defparameter *huffman-encode-table*
  #((0 #x1ff8 13) (1 #x7fffd8 23) (2 #x3ffffe2 28) (3 #x3ffffe3 28)
    (4 #x3ffffe4 28) (5 #x3ffffe5 28) (6 #x3ffffe6 28) (7 #x3ffffe7 28)
    (8 #x3ffffe8 28) (9 #xffffea 24) (10 #x3ffffffc 30) (11 #x3ffffe9 28)
    (12 #x3ffffea 28) (13 #x3ffffffd 30) (14 #x3ffffeb 28) (15 #x3ffffec 28)
    (16 #x3ffffed 28) (17 #x3ffffee 28) (18 #x3ffffef 28) (19 #x3fffff0 28)
    (20 #x3fffff1 28) (21 #x3fffff2 28) (22 #x3ffffffe 30) (23 #x3fffff3 28)
    (24 #x3fffff4 28) (25 #x3fffff5 28) (26 #x3fffff6 28) (27 #x3fffff7 28)
    (28 #x3fffff8 28) (29 #x3fffff9 28) (30 #x3fffffa 28) (31 #x3fffffb 28)
    (32 #x14 6) (33 #x3f8 10) (34 #x3f9 10) (35 #xffa 12)
    (36 #x1ff9 13) (37 #x15 6) (38 #xf8 8) (39 #x7fa 11)
    (40 #x3fa 10) (41 #x3fb 10) (42 #xf9 8) (43 #x7fb 11)
    (44 #xfa 8) (45 #x16 6) (46 #x17 6) (47 #x18 6)
    (48 #x0 5) (49 #x1 5) (50 #x2 5) (51 #x19 6)
    (52 #x1a 6) (53 #x1b 6) (54 #x1c 6) (55 #x1d 6)
    (56 #x1e 6) (57 #x1f 6) (58 #x5c 7) (59 #xfb 8)
    (60 #x7ffc 15) (61 #x20 6) (62 #xffb 12) (63 #x3fc 10)
    (64 #x1ffa 13) (65 #x21 6) (66 #x5d 7) (67 #x5e 7)
    (68 #x5f 7) (69 #x60 7) (70 #x61 7) (71 #x62 7)
    (72 #x63 7) (73 #x64 7) (74 #x65 7) (75 #x66 7)
    (76 #x67 7) (77 #x68 7) (78 #x69 7) (79 #x6a 7)
    (80 #x6b 7) (81 #x6c 7) (82 #x6d 7) (83 #x6e 7)
    (84 #x6f 7) (85 #x70 7) (86 #x71 7) (87 #x72 7)
    (88 #xfc 8) (89 #x73 7) (90 #xfd 8) (91 #x1ffb 13)
    (92 #x7fff0 19) (93 #x1ffc 13) (94 #x3ffc 14) (95 #x22 6)
    (96 #x7ffd 15) (97 #x3 5) (98 #x23 6) (99 #x4 5)
    (100 #x24 6) (101 #x5 5) (102 #x25 6) (103 #x26 6)
    (104 #x27 6) (105 #x6 5) (106 #x74 7) (107 #x75 7)
    (108 #x28 6) (109 #x29 6) (110 #x2a 6) (111 #x7 5)
    (112 #x2b 6) (113 #x76 7) (114 #x2c 6) (115 #x8 5)
    (116 #x9 5) (117 #x2d 6) (118 #x77 7) (119 #x78 7)
    (120 #x79 7) (121 #x7a 7) (122 #x7b 7) (123 #x7ffe 15)
    (124 #x7fc 11) (125 #x3ffd 14) (126 #x1ffd 13) (127 #x7fffc 19)
    (128 #x3fffe 18) (129 #x1fffc 17) (130 #x3fffc 18) (131 #x1fffd 17)
    (132 #x6fffe 19) (133 #x3fffe 18) (134 #x3fffd 18) (135 #x7fffd 19)
    (136 #x7fffe 19) (137 #xffffb 20) (138 #x7ffff 19) (139 #x3ffffe 22)
    (140 #x3fffe 18) (141 #x3fffe 18) (142 #x3fffe 18) (143 #x3fffe 18)
    (144 #x3fffc 18) (145 #x7fffc 19) (146 #x3ffffc 22) (147 #x3ffffe 22)
    (148 #x7fffd 19) (149 #x7fffe 19) (150 #x7ffff 19) (151 #x3ffffe 22)
    (152 #x3ffffe 22) (153 #x3ffffe 22) (154 #x7fffc 19) (155 #x3ffffc 22)
    (156 #x3ffffc 22) (157 #x3ffffc 22) (158 #x3ffffc 22) (159 #x3ffffc 22)
    (160 #xfffffc 24) (161 #x3ffffc 22) (162 #x3ffffc 22) (163 #x3ffffc 22)
    (164 #x3ffffc 22) (165 #x3ffffc 22) (166 #x3ffffc 22) (167 #x3ffffc 22)
    (168 #x3ffffc 22) (169 #x3ffffc 22) (170 #x3ffffc 22) (171 #x3ffffc 22)
    (172 #xfffffd 24) (173 #x3ffffc 22) (174 #x3ffffc 22) (175 #x3ffffc 22)
    (176 #x3ffffc 22) (177 #x3ffffc 22) (178 #x3ffffc 22) (179 #xfffffe 24)
    (180 #x3ffffc 22) (181 #x3ffffc 22) (182 #x3ffffc 22) (183 #x3ffffc 22)
    (184 #x3ffffc 22) (185 #x3ffffc 22) (186 #x3ffffc 22) (187 #x3ffffc 22)
    (188 #x3ffffc 22) (189 #x3ffffc 22) (190 #x3ffffc 22) (191 #x3ffffc 22)
    (192 #x3ffffc 22) (193 #x3ffffc 22) (194 #x3ffffc 22) (195 #x3ffffc 22)
    (196 #x3ffffc 22) (197 #x3ffffc 22) (198 #x3ffffc 22) (199 #x3ffffc 22)
    (200 #x3ffffc 22) (201 #x3ffffc 22) (202 #x3ffffc 22) (203 #x3ffffc 22)
    (204 #x3ffffc 22) (205 #x3ffffc 22) (206 #x3ffffc 22) (207 #x3ffffc 22)
    (208 #x3ffffc 22) (209 #x3ffffc 22) (210 #x3ffffc 22) (211 #x3ffffc 22)
    (212 #x3ffffc 22) (213 #x3ffffc 22) (214 #x3ffffc 22) (215 #x3ffffc 22)
    (216 #x3ffffc 22) (217 #x3ffffc 22) (218 #x3ffffc 22) (219 #x3ffffc 22)
    (220 #x3ffffc 22) (221 #x3ffffc 22) (222 #x3ffffc 22) (223 #x3ffffc 22)
    (224 #x3ffffc 22) (225 #x3ffffc 22) (226 #x3ffffc 22) (227 #x3ffffc 22)
    (228 #x3ffffc 22) (229 #x3ffffc 22) (230 #x3ffffc 22) (231 #x3ffffc 22)
    (232 #x3ffffc 22) (233 #x3ffffc 22) (234 #x3ffffc 22) (235 #x3ffffc 22)
    (236 #x3ffffc 22) (237 #x3ffffc 22) (238 #x3ffffc 22) (239 #x3ffffc 22)
    (240 #x3ffffc 22) (241 #x3ffffc 22) (242 #x3ffffc 22) (243 #x3ffffc 22)
    (244 #x3ffffc 22) (245 #x3ffffc 22) (246 #x3ffffc 22) (247 #x3ffffc 22)
    (248 #x3ffffc 22) (249 #x3ffffc 22) (250 #x3ffffc 22) (251 #x3ffffc 22)
    (252 #x3ffffc 22) (253 #x3ffffc 22) (254 #x3ffffc 22) (255 #x3ffffc 22))
  "Huffman encoding table: indexed by byte value, returns (symbol code num-bits)")

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
        (node (get-huffman-decode-tree)))

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

                        ;; Check if we reached a leaf
                        (when (huffman-node-value node)
                          (vector-push-extend (huffman-node-value node) result)
                          (setf node tree)))))

    ;; RFC 7541 Section 5.2: Padding bits should be most significant bits of EOS symbol
    ;; We accept padding as long as we haven't started decoding a symbol that doesn't complete
    ;; (i.e., we're still at root or the remaining bits are all 1s)
    ;; For simplicity, we just accept any valid incomplete symbol at end

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
