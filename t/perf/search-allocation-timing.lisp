(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry
   (:directory ,(uiop:pathname-directory-pathname (uiop:getcwd)))
   :inherit-configuration))
(let ((host-cache-directory (uiop:getenv "AITOOLS_HOST_CACHE_DIR"))
      (xdg-cache-home (uiop:getenv "XDG_CACHE_HOME"))
      (cache-state (uiop:getenv "AITOOLS_ALLOCATION_CACHE_STATE")))
  (unless (and host-cache-directory xdg-cache-home)
    (error "AITOOLS_HOST_CACHE_DIR and XDG_CACHE_HOME are required."))
  (unless (member cache-state '("cold" "warm") :test #'string=)
    (error "AITOOLS_ALLOCATION_CACHE_STATE must be cold or warm."))
  (asdf:initialize-output-translations
   `(:output-translations
     (t (,(uiop:ensure-directory-pathname host-cache-directory) :**/ :*.*.*))
     :ignore-inherited-configuration))
  (asdf:load-system "aitools/test")
  (let ((workspace (truename (uiop:pathname-directory-pathname (uiop:getcwd))))
        (source (truename (asdf:system-source-directory "aitools")))
        (test-source (truename (asdf:system-source-directory "aitools/test"))))
    (unless (and (equal workspace source) (equal workspace test-source))
      (error "The allocation tests did not load the current workspace source.")))
  (load "t/perf/search-allocation-timing-runner.lisp"))