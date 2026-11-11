(:context :workspace :state :measured :source :linux-ci
 :expression-covered 3759 :expression-total 3963
 :branch-covered 500 :branch-total 578
 :coverage-exclusions
 ((:kind :branch
   :package "AITOOLS.WORKSPACE.INFRASTRUCTURE"
   :symbol "PROCESSOR-COUNT"
   :reason "This measured Linux CI arm requires sysconf(_SC_NPROCESSORS_ONLN) to fail. The call does not fail on supported Darwin/Linux hosts, and no spec can force this libc failure without patching libc.")))

