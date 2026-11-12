#!/usr/bin/env Rscript

args <- commandArgs(TRUE)
source_dir <- normalizePath(if (length(args)) args[1L] else ".")
scenario_file <- normalizePath(if (length(args) >= 2L) args[2L] else
  "../Technometrics-Journal-Submission-MS3/results/v0.1.9/stress44/scenarios.csv")
output_dir <- normalizePath(if (length(args) >= 3L) args[3L] else
  file.path(source_dir, "validation", "v0.1.3"), mustWork = FALSE)
dir.create(file.path(output_dir, "cases"), recursive = TRUE, showWarnings = FALSE)

scenarios <- read.csv(scenario_file, stringsAsFactors = FALSE)
stopifnot(nrow(scenarios) == 44L)
repeated <- c("ordinary", "original_1", "original_7", "high_1e-04_1e+06",
              "holdout_5_1", "holdout_12_1")
worker <- file.path(source_dir, "dev", "validate-v0.1.3-worker.R")

jobs <- list()
for (replicate in seq_len(5L)) {
  selected <- if (replicate == 1L) scenarios$id else repeated
  for (i in seq_along(selected)) {
    order <- if ((i + replicate) %% 2L) c("normal", "delta") else c("delta", "normal")
    for (strategy in order) jobs[[length(jobs) + 1L]] <-
      list(id = selected[i], strategy = strategy, replicate = replicate)
  }
}

for (i in seq_along(jobs)) {
  job <- jobs[[i]]
  destination <- file.path(output_dir, "cases", sprintf("%s__%s__%d.csv",
    job$id, job$strategy, job$replicate))
  if (!file.exists(destination)) {
    message(sprintf("%d/%d %s %s repeat %d", i, length(jobs), job$id,
      job$strategy, job$replicate))
    status <- system2("Rscript", c(worker, source_dir, scenario_file, job$id,
      job$strategy, destination))
    if (status != 0L) stop("Validation worker failed: ", destination)
  }
}

files <- list.files(file.path(output_dir, "cases"), pattern = "[.]csv$", full.names = TRUE)
results <- do.call(rbind, lapply(files, read.csv, stringsAsFactors = FALSE))
parts <- strsplit(sub("[.]csv$", "", basename(files)), "__", fixed = TRUE)
results$replicate <- as.integer(vapply(parts, `[`, "", 3L))
write.csv(results, file.path(output_dir, "all-trials.csv"), row.names = FALSE)

normal <- results[results$strategy == "normal", ]
delta <- results[results$strategy == "delta", ]
comparison <- merge(normal, delta, by = c("scenario", "replicate"),
  suffixes = c("_normal", "_delta"))
comparison$both_returned <- comparison$status_normal == "returned" &
  comparison$status_delta == "returned"
comparison$same_size <- comparison$n_normal == comparison$n_delta
comparison$k_difference <- abs(comparison$k_normal - comparison$k_delta)
comparison$max_risk_difference <- pmax(abs(comparison$PR_normal-comparison$PR_delta),
  abs(comparison$CR_normal-comparison$CR_delta))
comparison$working_feasible <- comparison$PR_delta <= 0.05 + 1e-7 &
  comparison$CR_delta <= 0.10 + 1e-7
comparison$equivalent <- comparison$both_returned & comparison$same_size &
  comparison$k_difference <= 1e-6 & comparison$max_risk_difference <= 1e-7 &
  comparison$working_feasible
comparison$speedup <- comparison$elapsed_seconds_normal/comparison$elapsed_seconds_delta
write.csv(comparison, file.path(output_dir, "comparison.csv"), row.names = FALSE)

coverage <- comparison[comparison$replicate == 1L, ]
timing <- do.call(rbind, lapply(repeated, function(id) {
  x <- comparison[comparison$scenario == id, ]
  data.frame(
    scenario = id,
    normal_median_seconds = median(x$elapsed_seconds_normal),
    delta_median_seconds = median(x$elapsed_seconds_delta),
    median_paired_speedup = median(x$speedup),
    equivalent_trials = sum(x$equivalent),
    trials = nrow(x)
  )
}))
write.csv(timing, file.path(output_dir, "repeated-timings.csv"), row.names = FALSE)

summary <- data.frame(
  measure = c("configurations", "both_returned", "equivalent_plans", "new_failures",
    "fallbacks", "slower_delta_trials", "median_repeated_speedup"),
  value = c(nrow(coverage), sum(coverage$both_returned), sum(coverage$equivalent),
    sum(coverage$status_normal == "returned" & coverage$status_delta != "returned"),
    sum(coverage$initializer_delta == "normal", na.rm = TRUE),
    sum(coverage$speedup < 1, na.rm = TRUE), median(comparison$speedup[
      comparison$scenario %in% repeated], na.rm = TRUE))
)
write.csv(summary, file.path(output_dir, "summary.csv"), row.names = FALSE)

stopifnot(nrow(coverage) == 44L)
stopifnot(!any(coverage$status_normal == "returned" & coverage$status_delta != "returned"))
stopifnot(all(coverage$equivalent[coverage$both_returned]))
stopifnot(median(comparison$speedup[comparison$scenario %in% repeated], na.rm = TRUE) > 1)

capture.output(sessionInfo(), file = file.path(output_dir, "session-info.txt"))
print(summary)
print(timing)
