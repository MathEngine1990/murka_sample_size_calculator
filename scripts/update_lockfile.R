# Only run intentionally after installing/upgrading dependencies and passing tests.
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]])
  Sys.setlocale("LC_CTYPE", ".UTF-8")
stopifnot(file.exists("app.R"), dir.exists(".Rlibrary"))
.libPaths(c(normalizePath(".Rlibrary"), .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"))
Sys.setenv(RENV_PATHS_CACHE = file.path(getwd(), ".renv-cache"))
renv::snapshot(project = getwd(), library = .libPaths()[1],
  packages = c("shiny", "PowerTOST", "ggplot2", "future", "promises", "renv"),
  prompt = FALSE)
