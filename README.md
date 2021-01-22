# spec.course

Make use of spec even for functions you didn't write specs for.

```
(require '[colinkahn.spec.course :as sc])

; use track on functions that don't have specs
(sc/track called-by-a)

(s/fdef a :args ... :ret ...)

; run check on our fully spec'd function to collect samples for our tracked fn
(st/check `a)

(sc/samples `called-by-a :args) ; => #{...}

; once we have samples they'll be used during generation
(s/exercise-fn `called-by-a)

; we can run check to get samples for :ret and :fn
(st/check `called-by-a)

(sc/samples `called-by-a :ret) ; => #{...}
(sc/samples `called-by-a :fn)  ; => #{...}

; we can check conformance of our samples
(sc/explain-sampled-data `called-by-a :ret (s/spec ...)))

; or add a constraint before checking
(sc/add-contraint `called-by-a :ret (s/spec ...))

; then remove it later
(sc/remove-constraint `called-by-a :ret)
```

## Additional sample utilities

```
(sc/vary-samples `called-by-a :ret (constantly #{...}))

(sc/reset-samples `called-by-a :args)
```
