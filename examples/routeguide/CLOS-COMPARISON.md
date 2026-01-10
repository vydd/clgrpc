# RouteGuide CLOS API Comparison

This document compares the old handler-based API with the new CLOS-based API for the RouteGuide service.

## Service Definition

### Old API (server.lisp)

```lisp
;;; Define a handler class
(defclass route-guide-handler ()
  ()
  (:documentation "Handler for RouteGuide service"))

;;; Implement methods with manual string matching
(defmethod handle-unary ((handler route-guide-handler)
                         service-name method-name
                         request-bytes context)
  (declare (ignore service-name context))
  (cond
    ((string= method-name "GetFeature")  ; <-- Manual string matching!
     (let ((point (proto-deserialize 'routeguide:point request-bytes)))  ; <-- Manual deserialization
       (format *error-output* "GetFeature: lat=~D lon=~D~%"
               (routeguide:point-latitude point)
               (routeguide:point-longitude point))

       (let ((feature (find-if (lambda (f)
                                (point-equal point
                                            (routeguide:feature-location f)))
                              *route-features*)))
         (if feature
             (values (proto-serialize feature)  ; <-- Manual serialization
                     +grpc-status-ok+ nil nil)
             (values (proto-serialize
                      (routeguide:make-feature :name "" :location point))
                     +grpc-status-ok+ nil nil)))))

    (t  ; <-- Must handle unknown methods
     (values nil +grpc-status-unimplemented+
             (format nil "Unknown method: ~A" method-name)
             nil))))

;;; Manual registration for EACH method
(let ((handler (make-instance 'route-guide-handler)))
  (register-handler router "routeguide.RouteGuide" "GetFeature"
                    handler :rpc-type :unary)
  (register-handler router "routeguide.RouteGuide" "ListFeatures"
                    handler :rpc-type :server-streaming)
  (register-handler router "routeguide.RouteGuide" "RecordRoute"
                    handler :rpc-type :client-streaming)
  (register-handler router "routeguide.RouteGuide" "RouteChat"
                    handler :rpc-type :bidirectional))
```

**Problems:**
- Manual string matching error-prone
- Manual serialization/deserialization boilerplate
- No type safety
- Each method needs explicit registration
- Verbose and repetitive

---

### New CLOS API (server-clos.lisp)

```lisp
;;; Define service class with metaclass
(defclass route-guide-service (grpc-service)
  ()
  (:metaclass grpc-service-metaclass)
  (:service-name "routeguide.RouteGuide")
  (:package "routeguide")
  (:documentation "RouteGuide service definition"))

;;; Define method - automatic name conversion and defaults!
(defgrpc-method get-feature ((service route-guide-service)
                             (request routeguide:point)
                             context)
  (:documentation "Get the feature at the given point")
  ;; :method-name defaults to "GetFeature" (auto CamelCase)
  ;; :rpc-type defaults to :unary

  (declare (ignore service context))

  (format *error-output* "GetFeature: lat=~D lon=~D~%"
          (routeguide:point-latitude request)  ; <-- request is already deserialized!
          (routeguide:point-longitude request))

  (let ((feature (find-if (lambda (f)
                           (point-equal request
                                       (routeguide:feature-location f)))
                         *route-features*)))
    (if feature
        feature  ; <-- Just return the object, auto-serialized!
        (routeguide:make-feature :name "" :location request))))

;;; Automatic registration of ALL methods!
(register-service router (make-instance 'route-guide-service))
```

**Benefits:**
- ✅ No string matching - type-safe dispatch
- ✅ Automatic serialization/deserialization
- ✅ Smart defaults (kebab-case → CamelCase, :unary)
- ✅ Single registration call for entire service
- ✅ Clean, minimal syntax
- ✅ Matches patterns from Go/Python gRPC

---

## Streaming Methods

### Old API - Server Streaming

```lisp
(defmethod handle-server-streaming ((handler route-guide-handler)
                                     service-name method-name
                                     request-bytes stream context)
  (declare (ignore service-name context))
  (cond
    ((string= method-name "ListFeatures")  ; <-- String matching again
     (let ((rect (proto-deserialize 'routeguide:rectangle request-bytes)))  ; <-- Manual deserialize
       (format *error-output* "ListFeatures: rectangle~%")

       (dolist (feature *route-features*)
         (when (in-range (routeguide:feature-location feature) rect)
           (format *error-output* "  Sending: ~A~%"
                   (routeguide:feature-name feature))
           (server-stream-send stream
                              (proto-serialize feature))))  ; <-- Manual serialize

       (values +grpc-status-ok+ nil nil)))

    (t
     (values +grpc-status-unimplemented+
             (format nil "Unknown method: ~A" method-name)
             nil))))
```

### New CLOS API - Server Streaming

```lisp
(defgrpc-method list-features ((service route-guide-service)
                               (request routeguide:rectangle)
                               context)
  (:rpc-type :server-streaming)
  (:documentation "List all features in the given rectangle")

  (declare (ignore service))

  (format *error-output* "ListFeatures: rectangle~%")

  (let ((stream (get-stream context)))  ; <-- Get stream from context
    (dolist (feature *route-features*)
      (when (in-range (routeguide:feature-location feature) request)
        (format *error-output* "  Sending: ~A~%"
                (routeguide:feature-name feature))
        (server-stream-send stream
                           (proto-serialize feature))))  ; Still need proto-serialize for streaming

    (values +grpc-status-ok+ nil nil)))
```

**Note:** For streaming methods, you still manually serialize individual messages when sending/receiving,
but the request is auto-deserialized and you get clean access to the stream via `(get-stream context)`.

---

## Line Count Comparison

### Unary Method (GetFeature)

- **Old API**: 28 lines (includes string matching, error handling)
- **New CLOS API**: 15 lines (just business logic)
- **Reduction**: 46% fewer lines

### Service Registration

- **Old API**: 9 lines (manual registration × 4 methods)
- **New CLOS API**: 1 line (auto-registers all methods)
- **Reduction**: 89% fewer lines

### Total Server Code

- **server.lisp** (old): 314 lines
- **server-clos.lisp** (new): 287 lines
- **Reduction**: 9% fewer lines overall

*Note: Line count similar because helper functions are the same. The key improvement
is code quality, type safety, and maintainability.*

---

## Summary of Improvements

| Feature | Old API | New CLOS API |
|---------|---------|--------------|
| **String matching** | Manual `(string= method-name ...)` | None - type-safe dispatch |
| **Serialization** | Manual serialize/deserialize | Automatic for request/response |
| **Method names** | Manual strings `"GetFeature"` | Auto-converted from `get-feature` |
| **RPC type** | Must specify for each | Defaults to `:unary` |
| **Registration** | One call per method | One call per service |
| **Type safety** | None | Full CLOS type dispatch |
| **Boilerplate** | High | Minimal |

---

## Generated vs Handwritten

The CLOS API works identically whether:
1. **Handwritten** in Lisp (as shown here)
2. **Generated** from .proto files

This means you can:
- Define services in Lisp and export to .proto
- Import .proto and generate CLOS service definitions
- Mix and match as needed

The generated code from .proto will look exactly like the handwritten CLOS code!
