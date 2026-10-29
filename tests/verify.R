# Run from project root: Rscript tests/verify.R
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]])
  Sys.setlocale("LC_CTYPE", ".UTF-8")
if (dir.exists(".Rlibrary")) .libPaths(c(normalizePath(".Rlibrary"), .libPaths()))
library(shiny)
library(PowerTOST)
invisible(parse("app.R", encoding = "UTF-8"))
source("R/calculations.R", encoding = "UTF-8")
checks <- 0L
check <- function(ok, label) {
  if (!isTRUE(ok)) stop(paste("FAIL:", label))
  checks <<- checks + 1L
  cat("PASS:", label, "\n")
}
rejects <- function(expr) inherits(tryCatch(force(expr), error = identity), "error")
make_spec <- function(scenario = "standard", design = "2x2", target = 0.8,
                      dropout = 10, opts = list(dosing = "single", hvd = FALSE,
                      cmax_critical = FALSE, partial_auc = FALSE)) {
  s <- SCENARIOS[[scenario]]
  ps <- resolve_params(s, opts)
  ps <- ps[vapply(ps, function(p) isTRUE(p$on), logical(1))]
  ps <- lapply(ps, function(p) { p$cv <- p$cv / 100; p })
  list(design = design, params = ps, target = target, dropout = dropout,
       scenario_label = s$label, excluded = character(), mode_label = NULL)
}
check(adjust_n(28L, 10, 2L) == 32L && adjust_n(39L, 20, 3L) == 51L,
      "dropout inflation and balanced sequences")
check(apply_rules(8L, "2x2", list(p_abe("x", "x", 0.25)))$n == 12L,
      "minimum analysis N")
check(apply_rules(14L, "2x2x3", list(p_abel("x", "x", 0.4)))$n == 24L,
      "RTR minimum for ABEL")
check(DESIGN_ORDER[1] == "2x2", "crossover default")

configs <- 0L
for (scenario in names(SCENARIOS)) {
  s <- SCENARIOS[[scenario]]
  for (dosing in c("single", "multiple")) for (hvd in c(FALSE, TRUE))
    for (critical in c(FALSE, TRUE)) for (partial in c(FALSE, TRUE)) {
      opts <- list(dosing = dosing, hvd = hvd && "hvd" %in% s$toggles,
                   cmax_critical = critical && "cmax_critical" %in% s$toggles,
                   partial_auc = partial && "partial_auc" %in% s$toggles)
      for (d in s$designs(opts)) {
        validate_spec(make_spec(scenario, d, opts = opts))
        configs <- configs + 1L
      }
    }
}
check(configs == 408L, paste("valid scenario/design configurations:", configs))

# Compare application wrappers with independent direct calls to the reference library.
for (d in ALL_DESIGNS) {
  p <- p_abe("Cmax", "Cmax", 0.25)
  ours <- sample_param(p, d, 0.8)
  reference <- PowerTOST::sampleN.TOST(CV = 0.25, theta0 = 0.95,
    theta1 = 0.8, theta2 = 1.25, alpha = 0.05, targetpower = 0.8,
    design = d, print = FALSE, details = FALSE)
  check(ours$n == reference[["Sample size"]], paste("ABE matches PowerTOST:", d))
}
for (d in REPLICATE_DESIGNS) {
  p <- p_abel("Cmax", "Cmax", 0.4)
  p$cvwt <- 0.3
  ours <- sample_param(p, d, 0.8)
  reference <- PowerTOST::sampleN.scABEL(CV = c(0.3, 0.4), theta0 = 0.9,
    theta1 = 0.8, theta2 = 1.25, alpha = 0.05, targetpower = 0.8,
    design = d, regulator = "EMA", setseed = TRUE, print = FALSE, details = FALSE)
  check(ours$n == reference[["Sample size"]] && ours$power == reference[["Achieved power"]],
        paste("ABEL and unequal CVwT/CVwR match PowerTOST:", d))
}

baseline <- compute_all(make_spec())
check(is.null(baseline$error) && baseline$N == 28L && baseline$n_adj == 32L,
      "complete standard calculation: N=28, recruitment N=32")
check(all(baseline$pow_N >= baseline$target), "power of every selected endpoint at final N")
check(nrow(baseline$curve) <= 24L && baseline$N %in% baseline$curve$n,
      "bounded chart includes final N")
check(grepl(format(baseline$calculated_at, "%d.%m.%Y", tz = app_timezone()),
            build_summary(baseline), fixed = TRUE), "summary uses calculation timestamp")

for (scenario in c("nti", "biotech", "mr")) {
  design <- if (scenario == "biotech") "parallel" else "2x2"
  ans <- compute_all(make_spec(scenario, design))
  check(is.null(ans$error) && ans$N %% n_seq(design) == 0 && all(ans$pow_N >= ans$target),
        paste("complete calculation:", scenario))
}
abel_spec <- make_spec("hvd", "2x2x3")
abel_spec$params[[1]]$cvwt <- 0.3
abel <- compute_all(abel_spec)
abel_again <- compute_all(abel_spec)
check(is.null(abel$error) && abel$N >= 24L && abel$n_adj %% 2L == 0L,
      "complete ABEL calculation")
check(identical(abel$n_raw, abel_again$n_raw) && identical(abel$curve, abel_again$curve),
      "repeatable seeded ABEL results")
check(grepl("CVwT 30%", build_summary(abel), fixed = TRUE), "summary includes unequal CVwT")

bad <- make_spec(); bad$target <- 0.99
check(!is.null(compute_all(bad)$error), "reject unsupported target")
bad <- make_spec(); bad$dropout <- 91
check(!is.null(compute_all(bad)$error), "reject excessive dropout")
bad <- make_spec(); bad$params[[1]]$cv <- NA_real_
check(!is.null(compute_all(bad)$error), "reject nonfinite CV")
bad <- make_spec("hvd", "parallel")
check(!is.null(compute_all(bad)$error), "reject ABEL in parallel design")
check(rejects(sample_param(p_abe("x", "x", 3, theta0 = 0.975, limits = LIM_NTI),
                           "parallel", 0.95)), "reject analysis N above public-service limit")

# A real background worker must remain separate even when workers=1.
future::plan(future::multisession, workers = I(1))
main_pid <- Sys.getpid()
background_spec <- make_spec()
task <- future::future(list(pid = Sys.getpid(), result = compute_all(background_spec)), seed = TRUE)
value <- future::value(task)
future::plan(future::sequential)
check(value$pid != main_pid && value$result$N == 28L,
      "separate worker executes complete calculation")
cat(sprintf("\nAll %d checks passed. R %s; PowerTOST %s.\n", checks,
            getRversion(), packageVersion("PowerTOST")))
