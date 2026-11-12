#!/usr/bin/env Rscript

args <- commandArgs(TRUE)
stopifnot(length(args) == 5L)
source_dir <- args[1L]
scenario_file <- args[2L]
scenario_id <- args[3L]
strategy <- args[4L]
output_file <- args[5L]

suppressPackageStartupMessages(devtools::load_all(source_dir, quiet = TRUE))
scenario <- read.csv(scenario_file, stringsAsFactors = FALSE)
scenario <- scenario[scenario$id == scenario_id, , drop = FALSE]
stopifnot(nrow(scenario) == 1L, strategy %in% c("normal", "delta"))

ns <- asNamespace("AccSamplingDesign")
cache <- get(".beta_sp_cache", ns)
rm(list = ls(cache, all.names = TRUE), envir = cache)

arguments <- list(
  PRQ = scenario$PRQ, CRQ = scenario$CRQ, alpha = 0.05, beta = 0.10,
  USL = if (scenario$direction == 1) scenario$limit else NULL,
  LSL = if (scenario$direction == -1) scenario$limit else NULL,
  theta = scenario$theta, sigma_type = "unknown", sigma = NULL
)

run <- function() {
  if (strategy == "delta") {
    return(do.call(get(".beta_saddlepoint_plan", ns), arguments))
  }
  evaluation_env <- new.env(parent = ns)
  evaluation_env$.beta_saddlepoint_initial <- function(PRQ, CRQ, alpha, beta,
                                                        USL, LSL, theta, ...) {
    get(".beta_saddlepoint_normal_initial", ns)(PRQ, CRQ, alpha, beta)
  }
  plan_function <- get(".beta_saddlepoint_plan", ns)
  environment(plan_function) <- evaluation_env
  do.call(plan_function, arguments)
}

warnings <- character()
elapsed <- system.time(result <- tryCatch(
  withCallingHandlers(run(), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }),
  error = identity
))[["elapsed"]]

failed <- inherits(result, "error")
row <- data.frame(
  scenario = scenario_id,
  family = scenario$family,
  strategy = strategy,
  status = if (failed) "failed" else "returned",
  message = if (failed) conditionMessage(result) else "",
  warnings = paste(warnings, collapse = " | "),
  elapsed_seconds = elapsed,
  n = if (failed) NA_real_ else result$n,
  k = if (failed) NA_real_ else result$k,
  PR = if (failed) NA_real_ else result$PR,
  CR = if (failed) NA_real_ else result$CR,
  initializer = if (failed) NA_character_ else result$integration$search_initializer,
  fallback_reason = if (failed || is.null(result$integration$initializer_fallback_reason)) "" else
    result$integration$initializer_fallback_reason,
  evaluated_sizes = if (failed) NA_integer_ else result$integration$evaluated_sample_sizes,
  distribution_constructions = if (failed) NA_integer_ else
    result$integration$distribution_constructions,
  stringsAsFactors = FALSE
)
write.csv(row, output_file, row.names = FALSE)

