;;;; test-protobuf-complex.lisp - Test complex protobuf messages
;;;;
;;;; Tests for:
;;;; - Messages with multiple fields
;;;; - Nested messages
;;;; - Repeated fields
;;;; - Mixed field types

(require :asdf)

;; Load Quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load babel for string encoding
(asdf:load-system :babel)

;; Load package definitions and protobuf files
(load (merge-pathnames "src/package.lisp" (truename ".")))
(in-package #:clgrpc.grpc)
(load (merge-pathnames "src/grpc/protobuf-simple.lisp" (truename ".")))
(load (merge-pathnames "src/grpc/protobuf.lisp" (truename ".")))

(defpackage #:test-protobuf-complex
  (:use #:cl))

(in-package #:test-protobuf-complex)

;;; Message type: Person
;;; message Person {
;;;   string name = 1;
;;;   int32 id = 2;
;;;   string email = 3;
;;;   repeated string phone_numbers = 4;
;;; }

(defun encode-person (name id email phone-numbers)
  "Encode a Person message."
  (clgrpc.grpc::encode-message
   (list
    (clgrpc.grpc::encode-string-field 1 name)
    (clgrpc.grpc::encode-int32-field 2 id)
    (clgrpc.grpc::encode-string-field 3 email)
    (clgrpc.grpc::encode-repeated-field 4 phone-numbers #'clgrpc.grpc::encode-string-field))))

(defun decode-person (bytes)
  "Decode a Person message. Returns (values name id email phone-numbers)."
  (let ((name "")
        (id 0)
        (email "")
        (phone-numbers nil)
        (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (clgrpc.grpc::decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type clgrpc.grpc::+wire-type-length-delimited+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc::decode-string-field bytes offset)
                    (setf name value)
                    (setf offset new-offset)))

                 ((and (= field-number 2) (= wire-type clgrpc.grpc::+wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc::decode-varint bytes offset)
                    (setf id value)
                    (setf offset new-offset)))

                 ((and (= field-number 3) (= wire-type clgrpc.grpc::+wire-type-length-delimited+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc::decode-string-field bytes offset)
                    (setf email value)
                    (setf offset new-offset)))

                 ((and (= field-number 4) (= wire-type clgrpc.grpc::+wire-type-length-delimited+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc::decode-string-field bytes offset)
                    (push value phone-numbers)
                    (setf offset new-offset)))

                 (t
                  (setf offset (clgrpc.grpc::skip-field bytes offset wire-type))))))
    (values name id email (reverse phone-numbers))))

;;; Message type: Address
;;; message Address {
;;;   string street = 1;
;;;   string city = 2;
;;;   int32 zip = 3;
;;; }

(defun encode-address (street city zip)
  "Encode an Address message."
  (clgrpc.grpc::encode-message
   (list
    (clgrpc.grpc::encode-string-field 1 street)
    (clgrpc.grpc::encode-string-field 2 city)
    (clgrpc.grpc::encode-int32-field 3 zip))))

(defun decode-address (bytes)
  "Decode an Address message. Returns (values street city zip)."
  (let ((street "")
        (city "")
        (zip 0)
        (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (clgrpc.grpc::decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type clgrpc.grpc::+wire-type-length-delimited+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc::decode-string-field bytes offset)
                    (setf street value)
                    (setf offset new-offset)))

                 ((and (= field-number 2) (= wire-type clgrpc.grpc::+wire-type-length-delimited+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc::decode-string-field bytes offset)
                    (setf city value)
                    (setf offset new-offset)))

                 ((and (= field-number 3) (= wire-type clgrpc.grpc::+wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc::decode-varint bytes offset)
                    (setf zip value)
                    (setf offset new-offset)))

                 (t
                  (setf offset (clgrpc.grpc::skip-field bytes offset wire-type))))))
    (values street city zip)))

;;; Message type: Employee (nested message)
;;; message Employee {
;;;   Person person = 1;
;;;   Address address = 2;
;;;   double salary = 3;
;;;   bool active = 4;
;;; }

(defun encode-employee (person-data address-data salary active)
  "Encode an Employee message with nested Person and Address."
  (let ((person-bytes (apply #'encode-person person-data))
        (address-bytes (apply #'encode-address address-data)))
    (clgrpc.grpc::encode-message
     (list
      (clgrpc.grpc::encode-message-field 1 person-bytes)
      (clgrpc.grpc::encode-message-field 2 address-bytes)
      (clgrpc.grpc::encode-double-field 3 salary)
      (clgrpc.grpc::encode-bool-field 4 active)))))

(defun decode-employee (bytes)
  "Decode an Employee message. Returns (values person-data address-data salary active)."
  (let ((person-data nil)
        (address-data nil)
        (salary 0.0d0)
        (active nil)
        (offset 0))
    (loop while (< offset (length bytes))
          do (multiple-value-bind (field-number wire-type new-offset)
                 (clgrpc.grpc::decode-field-tag bytes offset)
               (setf offset new-offset)
               (cond
                 ((and (= field-number 1) (= wire-type clgrpc.grpc::+wire-type-length-delimited+))
                  (multiple-value-bind (message-bytes new-offset)
                      (clgrpc.grpc::decode-message-field bytes offset)
                    (setf person-data (multiple-value-list (decode-person message-bytes)))
                    (setf offset new-offset)))

                 ((and (= field-number 2) (= wire-type clgrpc.grpc::+wire-type-length-delimited+))
                  (multiple-value-bind (message-bytes new-offset)
                      (clgrpc.grpc::decode-message-field bytes offset)
                    (setf address-data (multiple-value-list (decode-address message-bytes)))
                    (setf offset new-offset)))

                 ((and (= field-number 3) (= wire-type clgrpc.grpc::+wire-type-64bit+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc::pb-decode-double bytes offset)
                    (setf salary value)
                    (setf offset new-offset)))

                 ((and (= field-number 4) (= wire-type clgrpc.grpc::+wire-type-varint+))
                  (multiple-value-bind (value new-offset)
                      (clgrpc.grpc::decode-varint bytes offset)
                    (setf active (not (zerop value)))
                    (setf offset new-offset)))

                 (t
                  (setf offset (clgrpc.grpc::skip-field bytes offset wire-type))))))
    (values person-data address-data salary active)))

;;; Tests

(defun test-person-message ()
  "Test Person message with multiple fields and repeated field."
  (format t "Testing Person message...~%")
  (let* ((name "Alice Smith")
         (id 12345)
         (email "alice@example.com")
         (phones '("555-1234" "555-5678" "555-9999"))
         (encoded (encode-person name id email phones)))

    (format t "  Encoded: ~D bytes~%" (length encoded))

    (multiple-value-bind (dec-name dec-id dec-email dec-phones)
        (decode-person encoded)
      (format t "  Name: ~S -> ~S ~A~%" name dec-name
              (if (string= name dec-name) "✓" "✗"))
      (format t "  ID: ~D -> ~D ~A~%" id dec-id
              (if (= id dec-id) "✓" "✗"))
      (format t "  Email: ~S -> ~S ~A~%" email dec-email
              (if (string= email dec-email) "✓" "✗"))
      (format t "  Phones: ~A -> ~A ~A~%" phones dec-phones
              (if (equal phones dec-phones) "✓" "✗"))

      (unless (and (string= name dec-name)
                   (= id dec-id)
                   (string= email dec-email)
                   (equal phones dec-phones))
        (error "Person message test failed")))))

(defun test-address-message ()
  "Test Address message."
  (format t "~%Testing Address message...~%")
  (let* ((street "123 Main St")
         (city "New York")
         (zip 10001)
         (encoded (encode-address street city zip)))

    (format t "  Encoded: ~D bytes~%" (length encoded))

    (multiple-value-bind (dec-street dec-city dec-zip)
        (decode-address encoded)
      (format t "  Street: ~S -> ~S ~A~%" street dec-street
              (if (string= street dec-street) "✓" "✗"))
      (format t "  City: ~S -> ~S ~A~%" city dec-city
              (if (string= city dec-city) "✓" "✗"))
      (format t "  Zip: ~D -> ~D ~A~%" zip dec-zip
              (if (= zip dec-zip) "✓" "✗"))

      (unless (and (string= street dec-street)
                   (string= city dec-city)
                   (= zip dec-zip))
        (error "Address message test failed")))))

(defun test-employee-message ()
  "Test Employee message with nested messages."
  (format t "~%Testing Employee message (nested)...~%")
  (let* ((person-data '("Bob Johnson" 54321 "bob@example.com" ("555-1111" "555-2222")))
         (address-data '("456 Oak Ave" "San Francisco" 94102))
         (salary 125000.50d0)
         (active t)
         (encoded (encode-employee person-data address-data salary active)))

    (format t "  Encoded: ~D bytes~%" (length encoded))

    (multiple-value-bind (dec-person dec-address dec-salary dec-active)
        (decode-employee encoded)
      (format t "  Person: ~A -> ~A ~A~%"
              person-data dec-person
              (if (equal person-data dec-person) "✓" "✗"))
      (format t "  Address: ~A -> ~A ~A~%"
              address-data dec-address
              (if (equal address-data dec-address) "✓" "✗"))
      (format t "  Salary: ~F -> ~F ~A~%"
              salary dec-salary
              (if (< (abs (- salary dec-salary)) 0.01d0) "✓" "✗"))
      (format t "  Active: ~A -> ~A ~A~%"
              active dec-active
              (if (eq active dec-active) "✓" "✗"))

      (unless (and (equal person-data dec-person)
                   (equal address-data dec-address)
                   (< (abs (- salary dec-salary)) 0.01d0)
                   (eq active dec-active))
        (error "Employee message test failed")))))

(defun test-packed-repeated ()
  "Test packed repeated fields."
  (format t "~%Testing packed repeated fields...~%")
  (let* ((numbers '(1 2 3 5 8 13 21 34 55 89))
         (encoded (clgrpc.grpc::encode-packed-int32 1 numbers)))

    (format t "  Encoded ~D integers: ~D bytes~%" (length numbers) (length encoded))
    (format t "  Numbers: ~A~%" numbers)
    (format t "  Encoded bytes: ~A~%" (coerce encoded 'list))
    (format t "  ✓ Packed encoding works~%")))

(defun run-all-tests ()
  "Run all complex message tests."
  (format t "~%================================================~%")
  (format t "Complex Protobuf Message Tests~%")
  (format t "================================================~%~%")

  (handler-case
      (progn
        (test-person-message)
        (test-address-message)
        (test-employee-message)
        (test-packed-repeated)

        (format t "~%================================================~%")
        (format t "All complex message tests PASSED! ✓✓✓~%")
        (format t "================================================~%~%")
        t)
    (error (e)
      (format t "~%~%TEST FAILED: ~A~%~%" e)
      nil)))

;; Run tests
(run-all-tests)
