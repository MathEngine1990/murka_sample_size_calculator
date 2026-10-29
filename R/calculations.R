# Calculation configuration, scenario definitions and shared helpers.

FIXED_ALPHA <- 0.05
MIN_N       <- 12L          # минимум субъектов в анализируемой популяции (Правила ЕАЭС)
MIN_N_RTR   <- 24L          # 2×2×3 + ABEL: не менее 12 субъектов в последовательности RTR
MAX_ANALYSIS_N <- 10000L    # public service resource limit, not a methodological limit
CALC_TIMEOUT <- 120         # seconds; R time limit, checked at interruptible operations
CHART_NSIMS <- 10000L       # симуляций на точку графика для ABEL

LIM_STD <- c(0.80, 1.25)
LIM_NTI <- c(0.90, 1.1111)

GRID_STD  <- c(0.90, 0.925, 0.95)   # чувствительность к GMR, ABE со стандартными границами
GRID_ABEL <- c(0.85, 0.90, 0.95)    # ABEL
GRID_NTI  <- c(0.95, 0.975, 1.00)   # узкий терапевтический индекс

DESIGN_LABELS <- c(
  "2x2"      = "Перекрёстный 2×2 (TR/RT)",
  "parallel" = "Параллельный",
  "2x2x4"    = "Полный репликативный 2×2×4 (TRTR/RTRT)",
  "2x2x3"    = "Частичный репликативный 2×2×3 (TRT/RTR)",
  "2x3x3"    = "Частичный репликативный 2×3×3 (TRR/RTR/RRT)"
)
REPLICATE_DESIGNS <- c("2x2x3", "2x3x3", "2x2x4")
# Перекрёстный дизайн по умолчанию; параллельный выбирается явно, если допустим.
DESIGN_ORDER <- c("2x2", "parallel", "2x2x3", "2x3x3", "2x2x4")
ALL_DESIGNS  <- DESIGN_ORDER
# Минимальное выбывание по умолчанию, обусловленное числом периодов
DESIGN_DROPOUT <- c("2x2" = 0, "parallel" = 0, "2x2x4" = 20, "2x2x3" = 15, "2x3x3" = 15)

n_seq <- function(design) if (identical(design, "2x3x3")) 3L else 2L

# Конструкторы ФК-параметров
# on = FALSE — дополнительная точка: в списке, но по умолчанию не отмечена
p_abe <- function(key, label, cv, theta0 = 0.95, limits = LIM_STD, grid = GRID_STD,
                  abel_ok = FALSE, cv_abel = 40, on = TRUE) {
  list(key = key, label = label, method = "ABE", cv = cv, theta0 = theta0,
       limits = limits, grid = grid, abel_ok = abel_ok, cv_abel = cv_abel, on = on)
}
p_abel <- function(key, label, cv, on = TRUE) {
  list(key = key, label = label, method = "ABEL", cv = cv, theta0 = 0.90,
       limits = LIM_STD, grid = GRID_ABEL, abel_ok = TRUE, cv_abel = cv, on = on)
}

NOTE_ABEL <- paste(
  "ABEL (average bioequivalence with expanding limits — ABE с расширяемыми границами) применяется",
  "только к Cmax (при многократном приёме — к Cmax,ss; для ЛП с модифицированным высвобождением также",
  "к Cτ,ss) при CVwR > 30%, обоснованном клинически и не связанном с выбросами. Границы exp(±0,760·sWR),",
  "потолок 69,84–143,19% при CVwR = 50%; точечная оценка GMR должна лежать в 80,00–125,00%.",
  "AUC всегда оценивается в стандартных границах. Только репликативный дизайн.",
  "Применимость расширения к конкретному параметру проверяйте по руководствам."
)

HVD_LABEL <- "Высоковариабельный ЛП (CVwR > 30%): ABEL, репликативный дизайн"

DOSING_DEFAULT <- c("Однократный приём" = "single",
                    "Многократный приём (до равновесного состояния)" = "multiple")
DOSING_NOTE <- paste("Многократный приём: основные параметры — в равновесном состоянии.",
                     "Параметры после 1-го введения по умолчанию не отмечены: включите их,",
                     "если этого требуют протокол или продукт-специфическое руководство.")

# Параметры многократного приёма для малых молекул: ss — основные, 1-е введение — дополнительные.
# mk() создаёт параметр нужного метода (ABE / ABEL) с заданными CV и границами.
multiple_params <- function(cmax_cv, auc_cv, cmax_abel = FALSE, theta0 = 0.95,
                            lim_auc = LIM_STD, lim_cmax = LIM_STD, grid = GRID_STD, ctau = NULL,
                            theta0_cmax = theta0, grid_cmax = grid) {
  cmax <- function(key, label, on) {
    if (cmax_abel) p_abel(key, label, 40, on = on)
    else p_abe(key, label, cmax_cv, theta0_cmax, lim_cmax, grid_cmax, abel_ok = TRUE, on = on)
  }
  ps <- list(cmax("Cmax1", "Cmax (1-е введение)", FALSE),
             p_abe("AUCtau1", "AUC0–τ (1-е введение)", auc_cv, theta0, lim_auc, grid, on = FALSE),
             cmax("Cmaxss", "Cmax,ss", TRUE))
  if (!is.null(ctau)) ps <- c(ps, list(p_abe("Ctauss", "Cτ,ss", ctau, theta0, lim_cmax, grid, abel_ok = TRUE)))
  c(ps, list(p_abe("AUCtauss", "AUCτ,ss", auc_cv, theta0, lim_auc, grid)))
}

SCENARIOS <- list(
  standard = list(
    label   = "Стандартный ЛП с немедленным высвобождением",
    toggles = "dosing",
    designs = function(o) ALL_DESIGNS,
    params  = function(o) {
      if (identical(o$dosing, "multiple")) return(multiple_params(25, 20))
      list(p_abe("Cmax", "Cmax", 25),
           p_abe("AUC", "AUC0–t", 20),
           p_abe("AUCinf", "AUC0–∞", 20, on = FALSE))
    },
    dropout = function(o) if (identical(o$dosing, "multiple")) 20 else 10,
    dosing_note = paste("Исследование в равновесном состоянии — исключение: например, у пациентов, которым",
                        "однократное введение невозможно, или при недостаточной чувствительности аналитического метода.",
                        DOSING_NOTE),
    note    = "Границы 80,00–125,00% для Cmax и AUC. Параллельный дизайн — только если перекрёстный нецелесообразен."
  ),
  hvd = list(
    label   = "Высоковариабельный ЛП (CVwR Cmax > 30%)",
    toggles = character(0),
    designs = function(o) REPLICATE_DESIGNS,
    params  = function(o) list(p_abel("Cmax", "Cmax", 40),
                               p_abe("AUC", "AUC0–t", 25),
                               p_abe("AUCinf", "AUC0–∞", 25, on = FALSE)),
    dropout = function(o) 10,
    note    = NOTE_ABEL
  ),
  nti = list(
    label   = "ЛП с узким терапевтическим индексом (NTI)",
    toggles = c("dosing", "cmax_critical"),
    designs = function(o) ALL_DESIGNS,
    params  = function(o) {
      # Cmax: GMR 0,975 только при узких границах; при 80,00–125,00% — 0,95, как в стандартном сценарии
      narrow <- isTRUE(o$cmax_critical)
      lim_cmax <- if (narrow) LIM_NTI else LIM_STD
      th_cmax  <- if (narrow) 0.975 else 0.95
      gr_cmax  <- if (narrow) GRID_NTI else GRID_STD
      if (identical(o$dosing, "multiple"))
        return(multiple_params(20, 15, theta0 = 0.975, lim_auc = LIM_NTI, lim_cmax = lim_cmax, grid = GRID_NTI,
                               theta0_cmax = th_cmax, grid_cmax = gr_cmax))
      list(p_abe("AUC", "AUC0–t", 15, theta0 = 0.975, limits = LIM_NTI, grid = GRID_NTI),
           p_abe("AUCinf", "AUC0–∞", 15, theta0 = 0.975, limits = LIM_NTI, grid = GRID_NTI, on = FALSE),
           p_abe("Cmax", "Cmax", 20, theta0 = th_cmax, limits = lim_cmax, grid = gr_cmax))
    },
    dropout = function(o) if (identical(o$dosing, "multiple")) 20 else 10,
    note    = paste0(
      "<b>AUC:</b> границы 90,00–111,11%.<br>",
      "<b>Cmax:</b> по умолчанию 80,00–125,00%. Если Cmax особенно важен для безопасности, ",
      "эффективности или мониторинга концентраций, границы для Cmax сужаются до 90,00–111,11%: ",
      "отметьте галочку ниже.<br>",
      "Необходимость узких границ для Cmax проверяйте по Правилам ЕАЭС и продукт-специфическим ",
      "руководствам для конкретного действующего вещества.<br>",
      "Масштабирование границ (ABEL) для ЛП с узким терапевтическим индексом не применяется.")
  ),
  long_half = list(
    label   = "ЛП с длительным периодом полувыведения",
    toggles = character(0),
    designs = function(o) ALL_DESIGNS,
    params  = function(o) list(p_abe("Cmax", "Cmax", 30), p_abe("AUC72", "AUC0–72", 25)),
    dropout = function(o) 15,
    note    = paste("Допускается усечённая AUC0–72. Для параллельного дизайна вводится межиндивидуальный CV.",
                    "Перекрёстный дизайн — если отмывочный период выполним.")
  ),
  mr = list(
    label   = "ЛП с модифицированным высвобождением",
    toggles = c("dosing", "hvd", "partial_auc"),
    designs = function(o) if (isTRUE(o$hvd)) REPLICATE_DESIGNS else ALL_DESIGNS,
    params  = function(o) {
      if (identical(o$dosing, "multiple")) return(multiple_params(25, 20, ctau = 30))
      ps <- list(p_abe("Cmax", "Cmax", 25, abel_ok = TRUE),
                 p_abe("AUC", "AUC0–t", 20),
                 p_abe("AUCinf", "AUC0–∞", 20, on = FALSE))
      if (isTRUE(o$partial_auc)) ps <- c(ps, list(p_abe("pAUC", "Частичные AUC", 30)))
      ps
    },
    dropout = function(o) if (identical(o$dosing, "multiple")) 20 else 10,
    note    = paste("Все параметры — 80,00–125,00%. При высокой вариабельности расширение границ (ABEL)",
                    "возможно для Cmax, Cmax,ss и Cτ,ss только в репликативном дизайне.",
                    "Исследования однократного и многократного приёма рассчитываются отдельно.")
  ),
  endogenous = list(
    label   = "Эндогенное вещество (коррекция на базовый уровень)",
    toggles = "hvd",
    designs = function(o) if (isTRUE(o$hvd)) REPLICATE_DESIGNS else c("2x2", "parallel"),
    params  = function(o) list(p_abe("Cmax", "Cmax (после коррекции)", 30, abel_ok = TRUE),
                               p_abe("AUC", "AUC0–t (после коррекции)", 25)),
    dropout = function(o) 10,
    note    = paste("Границы 80,00–125,00%. Используйте CV данных после коррекции на базовый уровень —",
                    "обычно он выше, чем у нескорректированных концентраций. Для высоковариабельных",
                    "эндогенных веществ (например, прогестерон) возможен ABEL для Cmax в репликативном дизайне.")
  ),
  biotech = list(
    label   = "Биотехнологический ЛП (сравнение ФК)",
    toggles = "dosing",
    dosing_choices = c("Однократное введение (добровольцы)" = "single",
                       "Многократное введение (пациенты)" = "multiple"),
    dosing_note = paste("Многократное введение: основные параметры — AUC0–τ после 1-го введения и AUCτ,ss.",
                        "Cmax после 1-го введения и Cmax,ss по умолчанию не отмечены (обычно вторичные)."),
    designs = function(o) "parallel",
    params  = function(o) {
      if (identical(o$dosing, "multiple")) {
        list(p_abe("AUCtau1", "AUC0–τ (1-е введение)", 35),
             p_abe("AUCtauss", "AUCτ,ss", 35),
             p_abe("Cmax1", "Cmax (1-е введение)", 35, on = FALSE),
             p_abe("Cmaxss", "Cmax,ss", 35, on = FALSE))
      } else {
        list(p_abe("AUCinf", "AUC0–∞", 30),
             p_abe("AUC", "AUC0–t", 30),
             p_abe("Cmax", "Cmax", 30))
      }
    },
    dropout = function(o) if (identical(o$dosing, "multiple")) 20 else 10,
    note    = paste("ФК-исследования биотехнологических ЛП и биоаналогов регулируются отдельными",
                    "правилами ЕАЭС, а не Правилами БЭ № 85.")
  )
)

# ---------------------------------------------------------------------------
# Вспомогательные функции
# ---------------------------------------------------------------------------

round_up <- function(n, k) as.integer(k * ceiling(n / k))

adjust_n <- function(n, dropout_pct, nseq) {
  if (is.na(n)) return(NA_integer_)
  round_up(n / (1 - dropout_pct / 100), nseq)
}

fmt_num <- function(x, d) formatC(x, format = "f", digits = d, decimal.mark = ",")
fmt_gmr <- function(g) fmt_num(g, if (abs(g * 100 - round(g * 100)) > 1e-9) 3 else 2)
fmt_lim <- function(l) paste0(fmt_num(l[1] * 100, 2), "–", fmt_num(l[2] * 100, 2), "%")
fmt_cv <- function(cv) fmt_num(cv, if (abs(cv - round(cv)) > 1e-9) 1 else 0)
fmt_method <- function(p) {
  if (p$method == "ABEL") "ABEL (EMA/ЕАЭС), до 69,84–143,19%" else paste("ABE", fmt_lim(p$limits))
}
# Короткая форма для таблицы конечных точек
fmt_method_short <- function(p) {
  if (p$method == "ABEL") return("ABEL")
  if (isTRUE(all.equal(p$limits, LIM_NTI))) "ABE 90–111%" else "ABE 80–125%"
}
# Нижние индексы в обозначениях ФК-параметров для отображения в интерфейсе (Cmax → C<sub>max</sub> и т. п.).
# В копируемой сводке и программном выводе обозначения остаются обычным текстом.
sub_html <- function(x) {
  x <- gsub("AUC(0–t|0–∞|0–72|0–τ|τ,ss)", "AUC<sub>\\1</sub>", x, perl = TRUE)
  x <- gsub("C(max,ss|max|τ,ss|trough,ss)", "C<sub>\\1</sub>", x, perl = TRUE)
  x <- gsub("CV(wR|wT|w)(?![A-Za-zА-Яа-я])", "CV<sub>\\1</sub>", x, perl = TRUE)
  x <- gsub("sWR", "s<sub>WR</sub>", x, fixed = TRUE)
  HTML(x)
}
per_seq <- function(n, nseq) paste(rep(n %/% nseq, nseq), collapse = " / ")

cv_label <- function(p, design) {
  if (p$method == "ABEL") return(paste0("CVwR ", p$label, " (%)"))
  if (identical(design, "parallel")) paste0("CV ", p$label, ", межиндивидуальный (%)")
  else paste0("CVw ", p$label, ", внутрииндивидуальный (%)")
}

# Параметры сценария с учётом переключателя HVD
resolve_params <- function(scn, o) {
  lapply(scn$params(o), function(p) {
    if (isTRUE(p$abel_ok) && isTRUE(o$hvd) && p$method == "ABE") p_abel(p$key, p$label, p$cv_abel, on = p$on) else p
  })
}

# Вызов функции PowerTOST с перехватом предупреждений и сообщений
run_ptost <- function(fn, args) {
  warns <- character(0)
  value <- withCallingHandlers(
    do.call(fn, args),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") },
    message = function(m) { warns <<- c(warns, trimws(conditionMessage(m))); invokeRestart("muffleMessage") }
  )
  list(value = value, warnings = warns)
}

param_args <- function(p, design, theta0 = p$theta0, cv = p$cv, cvwt = p$cvwt) {
  args <- list(alpha = FIXED_ALPHA, theta0 = theta0,
               theta1 = p$limits[1], theta2 = p$limits[2], design = design)
  if (p$method == "ABEL") {
    args$CV <- if (!is.null(cvwt)) c(cvwt, cv) else cv
    args$regulator <- "EMA"
    args$setseed <- TRUE
  } else {
    args$CV <- cv
  }
  args
}

# Размер выборки для одного параметра (без правил минимума)
sample_param <- function(p, design, target, keep_output = FALSE, ...) {
  fn <- if (p$method == "ABEL") PowerTOST::sampleN.scABEL else PowerTOST::sampleN.TOST
  args <- c(param_args(p, design, ...),
            list(targetpower = target, print = keep_output, details = FALSE))
  raw <- NULL
  res <- NULL
  if (keep_output) {
    raw <- utils::capture.output(res <- run_ptost(fn, args), type = "output")
  } else {
    res <- run_ptost(fn, args)
  }
  ans <- res$value
  n <- ans[["Sample size"]][1]
  achieved <- ans[["Achieved power"]][1]
  if (length(n) != 1L || !is.finite(n) || n < 2 || n > MAX_ANALYSIS_N)
    stop(sprintf("Расчёт выходит за пределы публичного калькулятора (N не более %s). Проверьте вводные или выполните расчёт с биостатистиком.", MAX_ANALYSIS_N))
  if (length(achieved) != 1L || !is.finite(achieved) || achieved < target)
    stop("PowerTOST не подтвердил достижение целевой мощности. Проверьте вводные.")
  list(n = as.integer(ans[["Sample size"]][1]),
       power = as.numeric(ans[["Achieved power"]][1]),
       raw = paste(raw, collapse = "\n"),
       warnings = res$warnings)
}

power_param <- function(p, design, n, nsims = NULL) {
  if (p$method == "ABEL") {
    args <- c(param_args(p, design), list(n = n))
    if (!is.null(nsims)) args$nsims <- nsims
    as.numeric(suppressMessages(suppressWarnings(do.call(PowerTOST::power.scABEL, args)))[1])
  } else {
    args <- c(param_args(p, design), list(n = n))
    as.numeric(suppressMessages(suppressWarnings(do.call(PowerTOST::power.TOST, args)))[1])
  }
}

# Правила ЕАЭС поверх результата PowerTOST: минимум 12, требование RTR для 2×2×3, баланс последовательностей
apply_rules <- function(n_max, design, params) {
  nseq <- n_seq(design)
  n <- max(n_max, MIN_N)
  flag_min <- n_max < MIN_N
  needs_rtr <- identical(design, "2x2x3") &&
    any(vapply(params, function(p) p$method == "ABEL" && p$cv > 0.30, logical(1)))
  flag_rtr <- needs_rtr && n < MIN_N_RTR
  if (needs_rtr) n <- max(n, MIN_N_RTR)
  list(n = round_up(n, nseq), flag_min = flag_min, flag_rtr = flag_rtr)
}

# Полный расчёт. Чистая функция: выполняется в фоновом процессе (future).
compute_all <- function(spec) {
  setTimeLimit(cpu = Inf, elapsed = CALC_TIMEOUT, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
  tryCatch({
    validate_spec(spec)
    design <- spec$design
    params <- spec$params
    nseq   <- n_seq(design)
    target <- spec$target

    per <- lapply(params, function(p) sample_param(p, design, target, keep_output = TRUE))
    n_raw <- vapply(per, function(r) r$n, integer(1))
    calculated_at <- Sys.time()
    if (any(is.na(n_raw)) || any(n_raw < 2L)) stop("PowerTOST не вернул допустимый размер выборки.")
    det <- which.max(n_raw)
    rules <- apply_rules(max(n_raw), design, params)
    N <- rules$n
    pow_N <- vapply(params, function(p) power_param(p, design, N), numeric(1))
    n_adj <- adjust_n(N, spec$dropout, nseq)

    # Итоговый N при изменении одного (определяющего) параметра; остальные фиксированы
    overall_n <- function(p_mod) {
      n_det <- tryCatch(sample_param(p_mod, design, target)$n, error = optional_failure)
      if (is.na(n_det)) return(NA_integer_)
      ps <- params; ps[[det]] <- p_mod
      apply_rules(max(c(n_det, n_raw[-det])), design, ps)$n
    }

    pd <- params[[det]]
    cv_base <- pd$cv * 100
    cvs <- cv_base + c(-10, -5, 0, 5, 10)
    cvs <- cvs[cvs > 0]
    sens_cv <- do.call(rbind, lapply(cvs, function(cv) {
      p_mod <- pd; p_mod$cv <- cv / 100
      if (!is.null(p_mod$cvwt)) p_mod$cvwt <- p_mod$cvwt * (cv / 100) / pd$cv  # сохраняем отношение CVwT/CVwR
      v <- overall_n(p_mod)
      data.frame(value = fmt_cv(cv), n = v, n_adj = adjust_n(v, spec$dropout, nseq),
                 base = isTRUE(all.equal(cv, cv_base)))
    }))

    sens_gmr <- do.call(rbind, lapply(sort(unique(c(pd$grid, pd$theta0))), function(g) {
      p_mod <- pd; p_mod$theta0 <- g
      v <- overall_n(p_mod)
      data.frame(value = fmt_gmr(g), n = v, n_adj = adjust_n(v, spec$dropout, nseq),
                 base = isTRUE(all.equal(g, pd$theta0)))
    }))

    # A bounded grid independent of dropout; always include the selected analysis N.
    points <- if (pd$method == "ABEL") 15L else 23L
    upper <- min(nseq * floor(MAX_ANALYSIS_N / nseq), round_up(max(24, N * 1.7), nseq))
    ns <- sort(unique(c(round_up(seq(2L * nseq, upper, length.out = points), nseq), N)))
    nsims <- if (pd$method == "ABEL") CHART_NSIMS else NULL
    powers <- vapply(ns, function(k) tryCatch(power_param(pd, design, k, nsims),
                                              error = optional_failure), numeric(1))

    header <- c(
      sprintf("Дата расчёта: %s", format(calculated_at, "%d.%m.%Y %H:%M:%S %Z", tz = app_timezone())),
      sprintf("R %s; PowerTOST %s", paste(R.version$major, R.version$minor, sep = "."),
              utils::packageDescription("PowerTOST")$Version),
      "Симуляции ABEL: фиксированный seed PowerTOST (setseed = TRUE)", "")
    raw <- paste(c(header, unlist(lapply(seq_along(params), function(i) {
      c(sprintf("=== %s — %s ===", params[[i]]$label, fmt_method(params[[i]])), per[[i]]$raw, "")
    }))), collapse = "\n")

    warns <- unique(unlist(lapply(seq_along(params), function(i) {
      w <- per[[i]]$warnings
      if (length(w)) paste0(params[[i]]$label, ": ", w) else NULL
    })))

    list(error = NULL, calculated_at = calculated_at, params = params, per = per, n_raw = n_raw, det = det,
         N = N, pow_N = pow_N, n_adj = n_adj, nseq = nseq, rules = rules,
         sens_cv = sens_cv, sens_gmr = sens_gmr,
         curve = data.frame(n = ns, power = powers),
         raw = raw, warnings = warns,
         design = design, target = target, dropout = spec$dropout,
         scenario_label = spec$scenario_label, excluded = spec$excluded,
         mode_label = spec$mode_label)
  }, error = function(e) list(error = if (grepl("time limit|Zeitlimit|лимит времени", conditionMessage(e), ignore.case = TRUE))
    "Расчёт превысил лимит времени. Упростите сценарий или выполните расчёт локально с биостатистиком."
    else conditionMessage(e)))
}

# Единый формат таблиц чувствительности; строка с введённым значением — жирным
sens_table <- function(df, first_col) {
  tags$table(class = "result-table sens-table",
    tags$thead(tags$tr(tags$th(first_col), tags$th("N (анализ)"), tags$th("N (с выбыванием)"))),
    tags$tbody(lapply(seq_len(nrow(df)), function(i) {
      tags$tr(class = if (isTRUE(df$base[i])) "base" else NULL,
              tags$td(df$value[i]),
              tags$td(if (is.na(df$n[i])) "—" else df$n[i]),
              tags$td(if (is.na(df$n_adj[i])) "—" else df$n_adj[i]))
    })))
}

build_summary <- function(x) {
  lines <- c(
    paste("Сценарий:", x$scenario_label),
    if (length(x$mode_label)) paste("Режим дозирования:", x$mode_label) else NULL,
    paste("Дизайн:", DESIGN_LABELS[[x$design]]),
    sprintf("Целевая мощность: %d%%; α = 0,05 (двусторонний 90%% ДИ)", round(x$target * 100)),
    "Конечные точки в расчёте:",
    vapply(seq_along(x$params), function(i) {
      p <- x$params[[i]]
      sprintf("  - %s: %s; %s %s%%; GMR %s -> N = %d", p$label, fmt_method(p),
              if (p$method == "ABEL") "CVwR" else if (x$design == "parallel") "CV" else "CVw",
              fmt_cv(p$cv * 100), fmt_gmr(p$theta0), x$n_raw[i]) |>
        paste0(if (p$method == "ABEL" && !is.null(p$cvwt)) paste0("; CVwT ", fmt_cv(p$cvwt * 100), "%") else "")
    }, character(1)),
    if (length(x$excluded)) paste("Не включены в расчёт:", paste(x$excluded, collapse = ", ")) else NULL,
    sprintf("Итоговый N (анализ): %d (%s); определяющий параметр: %s; мощность %s%%",
            x$N, per_seq(x$N, x$nseq), x$params[[x$det]]$label, fmt_num(x$pow_N[x$det] * 100, 1)),
    if (x$rules$flag_min) "Применён минимум 12 субъектов в анализируемой популяции (Правила ЕАЭС)." else NULL,
    if (x$rules$flag_rtr) "Применено требование: не менее 12 субъектов в последовательности RTR (2×2×3, ABEL)." else NULL,
    sprintf("Выбывание: %s%% -> N для включения: %d (%s)", fmt_num(x$dropout, 0), x$n_adj, per_seq(x$n_adj, x$nseq)),
    sprintf("PowerTOST %s, дата расчёта %s.", utils::packageDescription("PowerTOST")$Version,
            format(x$calculated_at, "%d.%m.%Y %H:%M:%S %Z", tz = app_timezone())),
    "Расчёт для бюджета КИ; не заменяет обоснование размера выборки в протоколе."
  )
  paste(lines, collapse = "\n")
}


app_timezone <- function() {
  tz <- Sys.getenv("BE_TIMEZONE", "Europe/Moscow")
  if (!(tz %in% OlsonNames())) "UTC" else tz
}

optional_failure <- function(e) {
  if (grepl("time limit|Zeitlimit|лимит времени", conditionMessage(e), ignore.case = TRUE)) stop(e)
  NA_integer_
}

validate_spec <- function(spec) {
  scalar <- function(x, lower, upper) is.numeric(x) && length(x) == 1L && is.finite(x) && x >= lower && x <= upper
  if (!is.list(spec) || !(length(spec$design) == 1L && spec$design %in% ALL_DESIGNS)) stop("Недопустимый дизайн.")
  if (!scalar(spec$target, 0.8, 0.95) || !(spec$target %in% c(0.8, 0.85, 0.9, 0.95))) stop("Недопустимая целевая мощность.")
  if (!scalar(spec$dropout, 0, 90)) stop("Выбывание должно находиться в диапазоне от 0 до 90%.")
  if (!is.list(spec$params) || !length(spec$params) || length(spec$params) > 10L) stop("Выберите конечные точки.")
  for (p in spec$params) {
    if (length(p$method) != 1L || !(p$method %in% c("ABE", "ABEL"))) stop("Недопустимый метод.")
    if (!scalar(p$cv, 0.001, 3)) stop("CV должен находиться в диапазоне от 0,1 до 300%.")
    if (!scalar(p$theta0, 0.01, 10) || length(p$limits) != 2L || any(!is.finite(p$limits)) ||
        p$limits[1] <= 0 || p$limits[1] >= p$limits[2] || p$theta0 <= p$limits[1] || p$theta0 >= p$limits[2])
      stop("Недопустимые GMR или границы эквивалентности.")
    if (p$method == "ABEL" && !(spec$design %in% REPLICATE_DESIGNS)) stop("ABEL требует репликативного дизайна.")
    if (!is.null(p$cvwt) && !scalar(p$cvwt, 0.001, 3)) stop("CVwT должен находиться в диапазоне от 0,1 до 300%.")
  }
  invisible(TRUE)
}
