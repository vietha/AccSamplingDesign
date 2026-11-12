# AccSamplingDesign 0.1.3 validation

The v0.1.3 search uses the Delta-MLE plan only as the initial sample size and
acceptance-constant bracket for `saddlepoint_mle`. The comparison search uses
the v0.1.2 Normal initializer. Both searches otherwise use the same
saddlepoint probabilities, tolerances, feasibility checks, and integer-plan
selection.

## Equivalence across 44 configurations

- Both searches returned plans in 43 configurations.
- All 43 commonly supported plans had the same delivered integer sample size.
- All 43 met the working producer- and consumer-risk constraints.
- The maximum absolute acceptance-constant difference was 2.46328e-9.
- The maximum absolute analytical-risk difference was 5.904105e-9.
- No new failure was introduced.
- `high_1e-05_1e+05` remained unsupported by both searches because the
  saddlepoint quadrature did not converge.
- `holdout_1_1` used the Normal fallback because Delta MLE did not return an
  analytically feasible initializer; the final saddlepoint plan was unchanged.

## Timing

Five sequential cold-process trials were run for six representative settings.
Process startup was excluded and Delta initialization time was included. Median
paired speedup across the 30 comparisons was 1.249. Median elapsed times in
seconds (Normal start, Delta start) were: ordinary (6.784, 5.338), original_1
(5.746, 5.538), original_7 (4.491, 2.830), high_1e-04_1e+06 (4.292, 3.857),
holdout_5_1 (6.519, 5.326), and holdout_12_1 (6.063, 4.173).

Ten of the 44 single coverage trials were slower with Delta initialization, so
the change is not described as a universal speed improvement. Complete results
are retained in `all-trials.csv`, `comparison.csv`, and
`repeated-timings.csv`.

## Package tests

The development source passed 435 testthat expectations with no failures,
warnings, or skips before package build and check.
