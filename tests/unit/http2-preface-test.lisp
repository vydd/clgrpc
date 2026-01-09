;;;; Unit test: HTTP/2 preface bytes

(require :asdf)
(push #P"/home/vydd/Code/clgrpc/" asdf:*central-registry*)
(asdf:load-system :clgrpc :verbose nil)

(format t "~%Testing HTTP/2 preface generation...~%")

;; Test 1: Preface length
(let ((preface (clgrpc.http2:http2-client-preface-bytes)))
  (format t "1. Preface length: ~D bytes (expected 24)~%" (length preface))
  (assert (= (length preface) 24) () "Preface must be 24 bytes"))

;; Test 2: Exact byte values (RFC 9113)
(let ((preface (clgrpc.http2:http2-client-preface-bytes))
      (expected #(#x50 #x52 #x49 #x20 #x2A #x20 #x48 #x54 #x54 #x50 #x2F #x32
                  #x2E #x30 #x0D #x0A #x0D #x0A #x53 #x4D #x0D #x0A #x0D #x0A)))
  (format t "2. Byte values: ~{~2,'0X ~}~%" (coerce preface 'list))
  (assert (equalp preface expected) () "Preface bytes don't match RFC 9113")
  (format t "   ✓ Matches RFC 9113 spec~%"))

(format t "~%✓ All preface tests passed~%")
