# AccSamplingDesign 0.0.9.9000

## Bug fixes

- Known-theta Beta variables plans now meet both risk constraints at the
  delivered (rounded-up) sample size. Previously, the soft-penalty
  constrained search could return a plan that already violated
  `PR <= alpha` or `CR <= beta`, and rounding the sample size up could not
  repair it. The search is now refined by solving the two binding risk
  equations (Nelder-Mead in log coordinates), the sample size is rounded
  up from the binding solution, and both risks are verified at the
  delivered size. Known-theta sample sizes are now the minimal feasible
  integers, e.g. the reported moisture case (PRQ = 0.005, CRQ = 0.01,
  USL = 0.05, theta = 500) changes from n = 112 (violating both risks) to
  n = 117. If the refinement does not converge, the constrained-search
  solution is delivered after verification, with a warning.
- For known-theta Beta plans, the reported `PR` and `CR`, the OC curve and
  the plots now describe the delivered integer plan, and `n` equals
  `sample_size`. Unknown-theta plans keep the previous reporting semantics.
- `accProb()` no longer fails for a Beta plan with acceptability constant
  k = 0 (floating-point cancellation made the closed-form discriminant
  marginally negative).

# AccSamplingDesign 0.0.9

## New features

- Added analytical Delta--MLE and Delta--MoM methods for Beta variables
  acceptance sampling when the precision parameter, theta, is unknown.
- Delta--MLE is now the default unknown-theta method. The sample-size
  adjustment used in version 0.0.8 remains available as `"gk_adjustment"`.

## Documentation

- Added the citation for the AccSamplingDesign article published in *The R
  Journal*, volume 18, issue 1, pages 368--381
  ([doi:10.32614/RJ-2026-007](https://doi.org/10.32614/RJ-2026-007)).
