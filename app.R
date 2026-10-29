# Run this directory with shiny::runApp(). See README.md and DEPLOYMENT.md.
# Local dependencies are isolated; Cloud uses the packages from renv.lock.
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]])
  Sys.setlocale("LC_CTYPE", ".UTF-8")
if (dir.exists(".Rlibrary")) .libPaths(c(normalizePath(".Rlibrary"), .libPaths()))
library(shiny)
library(PowerTOST)
library(ggplot2)
library(future)
library(promises)
if (packageVersion("shiny") < "1.8.1") stop("Требуется shiny >= 1.8.1.")
source(file.path("R", "calculations.R"), local = TRUE, encoding = "UTF-8")

# Exactly one separate worker, including on a one-core Cloud instance.
# Queued requests share this worker; increasing workers also increases memory use.
future::plan(future::multisession, workers = I(1))
shiny::onStop(function() future::plan(future::sequential))
options(shiny.sanitize.errors = TRUE)

logo_tag <- if (file.exists(file.path("www", "farm_logo.png")))
  tags$img(src = "farm_logo.png", alt = "Логотип ФАРМ") else NULL

ui <- fluidPage(
  title = "Калькулятор выборки БЭ",
  lang = "ru",
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$style(HTML("
      body {background: #f7faf8;color: #24322b;font-family: Arial, sans-serif;font-size: 13px;}
      .header-row {display:flex; justify-content:space-between; align-items:center;
                   gap:20px; padding:15px 0 18px; margin-bottom:15px;
                   border-bottom:2px solid #008d40;}
      .header-row h2 {font-weight:700; font-size:25px; margin:0;}
      .header-row img {width:86px; max-height:82px; object-fit:contain; flex-shrink:0;}
      .well {background:white; border:1px solid #e1e8e3; border-radius:9px;}
      .btn-primary {background-color:#008c43; border-color:#008c43;}
      .btn-primary:hover, .btn-primary:focus {background-color:#006d34; border-color:#006d34;}
      .copy-button {background:#008c43 !important; border-color:#008c43 !important;
              color:white !important; margin:8px 8px 18px 0;}
      .btn-primary, .copy-button { font-family: Arial, sans-serif !important;font-size: 16px !important;font-weight: 500 !important;}
      .copy-button:hover {background:#006d34 !important;}
      input:disabled, select:disabled {background:#f1f3f1 !important; color:#525f58 !important; opacity:1 !important;}
      .result-table {width:100%; border-collapse:collapse; margin-bottom:12px;}
      .result-table th, .result-table td {padding:4px 10px; border-bottom:1px solid #e3e9e5;}
      .result-table th {background:#edf6f0; text-align:left;}
      .result-table td:last-child, .result-table th:last-child {text-align:right; white-space:nowrap;}
      .result-table tr.important td {font-style:italic; font-size:13px; font-weight:600;}
      .result-table tr.det td {background:#f3faf5; font-weight:600;}
      .raw-output {white-space:pre-wrap; font-size:12px; background:#f6f8f6;
                   border:1px solid #e4eae5; border-radius:7px; padding:14px;}
      .method-label {font-weight:600;font-size:16px; color:#007d39; margin-bottom:9px;}
      .help-block {font-size:12px;}
      .param-box {border-left:3px solid #cfe3d6; padding-left:10px; margin-bottom:6px;}
      .param-use .checkbox {margin:4px 0 6px;}
      .param-use label {font-weight:600;}
      .param-meta {font-size:12px; color:#4c5f55; margin:-8px 0 10px;}
      .flag {background:#fff6e5; border:1px solid #f0d9a8; border-radius:6px; padding:6px 10px;
             margin:6px 0; font-size:12px;}
      .warn-box {background:#fdf1ec; border:1px solid #efc9b8; border-radius:6px; padding:8px 12px;
                 font-size:12px; white-space:pre-wrap;}
      .scenario-note {background:#eef6f1; border-radius:6px; padding:8px 10px; font-size:12px;
                      margin:-6px 0 14px; color:#2d4a3a;}
      .status-running {color:#007d39; font-weight:600; margin-top:8px;}
      details summary {cursor:pointer; font-weight:600; margin:10px 0 6px;}
      .app-grid {display:grid; grid-template-columns:4fr 5fr 3fr; gap:20px; align-items:start;}
      .app-grid > div {min-width:0;}
      .table-scroll {overflow-x:auto;}
      .col-input {padding:19px;}
      .sens-table td:first-child, .sens-table th:first-child {text-align:left;}
      .sens-table td, .sens-table th {text-align:right;}
      .result-table tr.base td {font-weight:700;}
      .param-table td, .param-table th {white-space:nowrap;}
      .param-table td:first-child {white-space:normal;}
      .param-table td:nth-child(n+3), .param-table th:nth-child(n+3) {text-align:right;}
      .param-table td[title] {cursor:help; text-decoration:underline dotted #9fb5a8;}
      @media(max-width:1199px) {
        .app-grid {grid-template-columns:4fr 8fr;}
        .col-input {grid-row:span 2;}
        .col-side {grid-column:2;}
      }
      @media(max-width:767px) {
        .app-grid {grid-template-columns:1fr;}
        .col-input {grid-row:auto;}
        .col-side {grid-column:1;}
        .header-row h2 {font-size:19px;} .header-row img {width:65px;}
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('copyBeText', function(msg) {
        var content = msg.text || '';
        function send(ok) {
          Shiny.setInputValue('clipboard_result', {ok:ok, what:msg.what, timestamp:Date.now()}, {priority:'event'});
        }
        function oldCopy() {
          var textarea = document.createElement('textarea');
          textarea.value = content;
          textarea.style.position = 'fixed'; textarea.style.opacity = '0';
          document.body.appendChild(textarea); textarea.focus(); textarea.select();
          var ok = false;
          try {ok = document.execCommand('copy');} catch(err) {}
          document.body.removeChild(textarea);
          send(ok);
        }
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(content).then(function() {send(true);}, oldCopy);
        } else {oldCopy();}
      });
      Shiny.addCustomMessageHandler('setBusy', function(msg) {
        var b = document.getElementById('calculate');
        if (b) { b.disabled = !!msg.busy; b.innerHTML = msg.busy ? 'Расчёт выполняется…' : 'Рассчитать'; }
      });
    "))),
  div(class = "header-row",
      div(style = "flex: 1; min-width: 0;",
          h2(style = "color:#007d39;", "Калькулятор размера выборки для исследований БЭ и эквивалентности ФК"),
          p(style = "margin: 8px 0 0; font-size: 14px;",
            "Калькулятор предназначен для формирования бюджета КИ. При необходимости обоснования размера выборки в Протоколе КИ обратитесь к биостатистику"),
          p(style = "margin: 4px 0 0; font-size: 11px; font-style: italic; color:#4c5f55;",
            sprintf("Расчёты выполняются в R с помощью пакета PowerTOST (версия %s).",
                    utils::packageDescription("PowerTOST")$Version))),
      logo_tag),
  div(class = "app-grid",
    div(class = "col-input well",
      selectizeInput("scenario", "Тип ЛП / сценарий",
                     choices = setNames(names(SCENARIOS),
                                        vapply(SCENARIOS, function(x) as.character(sub_html(x$label)), "")),
                     options = list(render = I("{
                       option: function(item, escape) { return '<div>' + item.label + '</div>'; },
                       item:   function(item, escape) { return '<div>' + item.label + '</div>'; }
                     }"))),
      uiOutput("scenario_note"),
      uiOutput("toggles_ui"),
      uiOutput("design_ui"),
      uiOutput("params_ui"),
      selectInput("target", "Целевая мощность (%)", choices = c(
        "80%" = "80", "85%" = "85", "90%" = "90", "95%" = "95"
      ), selected = "80"),
      div(class = "form-group",
          tags$label(`for` = "fixed_alpha",
                     "Уровень α = 0,05 (соответствует двустороннему 90% ДИ)"),
          tags$input(id = "fixed_alpha", type = "text", class = "form-control",
                     value = "0,05", disabled = "disabled")),
      numericInput("dropout", "Ожидаемое выбывание (%)", value = 10,
                   min = 0, max = 90, step = 1),
      uiOutput("dropout_hint"),
      actionButton("calculate", "Рассчитать", class = "btn-primary", width = "100%"),
      uiOutput("status")
    ),
    div(class = "col-main",
      uiOutput("error"),
      uiOutput("stale_result"),
      uiOutput("result_context"),
      div(class = "method-label", "Результат"),
      div(class = "table-scroll", uiOutput("param_table")),
      uiOutput("result_table"),
      uiOutput("flags"),
      helpText("N — общий размер анализируемой выборки: максимум по отмеченным конечным точкам, не менее 12. Выбывание учтено как N/(1 − доля выбывания) с округлением вверх до кратного числу последовательностей. Целевая мощность относится к отдельным конечным точкам; совместная мощность не рассчитывается."),
      actionButton("copy_summary", "Копировать сводку", icon = icon("copy"), class = "copy-button"),
      div(class = "method-label", "Программный вывод PowerTOST"),
      tags$pre(id = "raw_text", class = "raw-output", textOutput("raw", inline = TRUE), .noWS = "inside"),
      uiOutput("warnings_ui"),
      actionButton("copy_raw", "Копировать вывод", icon = icon("copy"), class = "copy-button")
    ),
    div(class = "col-side",
      uiOutput("sens_cv_title"),
      uiOutput("sens_cv"),
      uiOutput("sens_gmr_title"),
      uiOutput("sens_gmr"),
      helpText("Чувствительность рассчитана для определяющего параметра; остальные параметры фиксированы. Жирным — введённое значение."),
      uiOutput("plot_title"),
      plotOutput("powerplot", height = "280px"),
      uiOutput("graph_note")
    )
  )
)

# ---------------------------------------------------------------------------
# Сервер
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  scn <- reactive({ req(input$scenario); SCENARIOS[[input$scenario]] })

  opts <- reactive({
    list(hvd = isTRUE(input$hvd) && "hvd" %in% scn()$toggles,
         dosing = if ("dosing" %in% scn()$toggles && !is.null(input$dosing)) input$dosing else "single",
         partial_auc = isTRUE(input$partial_auc) && "partial_auc" %in% scn()$toggles,
         cmax_critical = isTRUE(input$cmax_critical) && "cmax_critical" %in% scn()$toggles)
  })

  mode_label <- reactive({
    if (!("dosing" %in% scn()$toggles)) return(NULL)
    ch <- if (is.null(scn()$dosing_choices)) DOSING_DEFAULT else scn()$dosing_choices
    names(ch)[ch == opts()$dosing]
  })

  allowed_designs <- reactive({
    d <- scn()$designs(opts())
    d[order(match(d, DESIGN_ORDER))]
  })
  params <- reactive(resolve_params(scn(), opts()))

  output$scenario_note <- renderUI(div(class = "scenario-note", sub_html(scn()$note)))

  output$toggles_ui <- renderUI({
    tg <- scn()$toggles
    keep <- function(id, default) { v <- isolate(input[[id]]); if (is.null(v)) default else v }
    tagList(
      if ("dosing" %in% tg) tagList(
        radioButtons("dosing", "Режим дозирования",
                     choices = if (is.null(scn()$dosing_choices)) DOSING_DEFAULT else scn()$dosing_choices,
                     selected = keep("dosing", "single")),
        conditionalPanel("input.dosing == 'multiple'",
          helpText(sub_html(if (is.null(scn()$dosing_note)) DOSING_NOTE else scn()$dosing_note)))),
      if ("partial_auc" %in% tg) conditionalPanel("input.dosing == 'single'",
        checkboxInput("partial_auc", "Требуются частичные AUC", keep("partial_auc", FALSE))),
      if ("hvd" %in% tg) checkboxInput("hvd", sub_html(HVD_LABEL), keep("hvd", FALSE)),
      if ("cmax_critical" %in% tg) checkboxInput("cmax_critical",
        sub_html("Применить узкие границы 90,00–111,11% и к Cmax"), keep("cmax_critical", FALSE))
    )
  })

  output$design_ui <- renderUI({
    ch <- allowed_designs()
    # При смене сценария выбирается первый допустимый дизайн; 2×2 предпочтителен.
    selectInput("design", "Дизайн исследования", choices = setNames(ch, DESIGN_LABELS[ch]), selected = ch[1])
  })

  cur_design <- reactive({
    d <- input$design
    if (is.null(d) || !(d %in% allowed_designs())) allowed_designs()[1] else d
  })

  cv_id <- function(p, prefix = "cv_") paste0(prefix, input$scenario, "_", p$key, "_", p$method,
    if (identical(cur_design(), "parallel")) "_between" else "_within")

  output$params_ui <- renderUI({
    ps <- params(); design <- cur_design()
    has_abel <- any(vapply(ps, function(p) p$method == "ABEL", logical(1)))
    boxes <- lapply(ps, function(p) {
      id <- cv_id(p); old <- isolate(input[[id]])
      idt <- cv_id(p, "cvwt_"); oldt <- isolate(input[[idt]])
      idu <- cv_id(p, "use_"); oldu <- isolate(input[[idu]])
      div(class = "param-box",
          div(class = "param-use",
              checkboxInput(idu, sub_html(paste0(p$label, if (!isTRUE(p$on)) " (доп.)" else "")),
                            value = if (is.null(oldu)) isTRUE(p$on) else oldu)),
          conditionalPanel(sprintf("input['%s']", idu),
            numericInput(id, sub_html(cv_label(p, design)), value = if (is.null(old)) p$cv else old,
                         min = 0.1, max = 300, step = 1),
            if (p$method == "ABEL") conditionalPanel("input.cvwt_diff",
              numericInput(idt, sub_html(paste0("CVwT ", p$label, " (%)")),
                           value = if (is.null(oldt)) p$cv else oldt, min = 0.1, max = 300, step = 1)),
            div(class = "param-meta", paste0(fmt_method(p), " · GMR ", fmt_gmr(p$theta0)))))
    })
    tagList(
      div(class = "method-label", style = "font-size:14px;", "Конечные точки"),
      boxes,
      helpText("Расчёт ведётся только по отмеченным параметрам; итоговое N — по наиболее требовательному из них."),
      if (has_abel) tags$details(
        tags$summary("Дополнительно"),
        checkboxInput("cvwt_diff", sub_html("CVwT отличается от CVwR"), isTRUE(isolate(input$cvwt_diff))),
        helpText("По умолчанию предполагается CVwT = CVwR.")),
      helpText("GMR задан сценарием и не редактируется; влияние GMR показано в таблице чувствительности.")
    )
  })

  # Выбывание по умолчанию: обновляется, пока пользователь не изменил его вручную
  default_dropout <- reactive({
    max(scn()$dropout(opts()), DESIGN_DROPOUT[[cur_design()]])
  })
  rv <- reactiveValues(last_auto = 10)
  observeEvent(default_dropout(), {
    d <- default_dropout(); cur <- isolate(input$dropout)
    if (is.null(cur) || is.na(cur) || isTRUE(all.equal(cur, rv$last_auto))) {
      updateNumericInput(session, "dropout", value = d)
      rv$last_auto <- d
    }
  })
  output$dropout_hint <- renderUI(
    helpText(sprintf("По умолчанию для сценария и дизайна: %d%%.", as.integer(default_dropout()))))

  # Snapshot contains every value used by the calculation and stale-result check.
  requested_spec <- reactive({
    ps_all <- params(); design <- cur_design()
    used <- vapply(ps_all, function(p) {
      v <- input[[cv_id(p, "use_")]]
      if (is.null(v)) isTRUE(p$on) else isTRUE(v)
    }, logical(1))
    if (!any(used)) {
      return(list(error = "Отметьте хотя бы одну конечную точку для расчёта."))
    }
    ps <- ps_all[used]
    excluded <- vapply(ps_all[!used], function(p) p$label, character(1))
    errs <- character(0)
    ps <- lapply(ps, function(p) {
      v <- input[[cv_id(p)]]
      if (is.null(v) || !is.finite(v) || v < 0.1 || v > 300)
        errs <<- c(errs, paste0(cv_label(p, design), ": значение должно быть в диапазоне [0,1; 300]."))
      else p$cv <- v / 100
      if (p$method == "ABEL" && isTRUE(input$cvwt_diff)) {
        vt <- input[[cv_id(p, "cvwt_")]]
        if (is.null(vt) || !is.finite(vt) || vt < 0.1 || vt > 300)
          errs <<- c(errs, paste0("CVwT ", p$label, ": значение должно быть в диапазоне [0,1; 300]."))
        else p$cvwt <- vt / 100
      }
      p
    })
    d <- input$dropout
    if (is.null(d) || !is.finite(d) || d < 0 || d > 90)
      errs <- c(errs, "Выбывание должно находиться в диапазоне от 0 до 90%.")
    target <- suppressWarnings(as.numeric(input$target) / 100)
    if (length(target) != 1L || !is.finite(target) || !(target %in% c(0.8, 0.85, 0.9, 0.95)))
      errs <- c(errs, "Выберите целевую мощность из списка.")
    if (length(errs)) return(list(error = errs))
    list(error = NULL, spec = list(design = design, params = ps, target = target,
      dropout = d, scenario_label = scn()$label, excluded = excluded, mode_label = mode_label()))
  })

  # Keep only the last successful calculation in this visitor's session.
  # Repeating the same request does not spend another round of simulations.
  last_calculation <- NULL
  calc_task <- ExtendedTask$new(function(spec) {
    if (!is.null(last_calculation) && identical(last_calculation$spec, spec))
      return(promises::promise_resolve(last_calculation$result))
    promises::then(promises::future_promise(compute_all(spec), seed = TRUE),
      onFulfilled = function(value) {
        if (is.null(value$error)) last_calculation <<- list(spec = spec, result = value)
        value
      })
  })
  input_error <- reactiveVal(NULL)
  submitted_spec <- reactiveVal(NULL)

  observeEvent(input$calculate, {
    if (calc_task$status() == "running") return()
    request <- requested_spec()
    input_error(request$error)
    if (!is.null(request$error)) return()
    submitted_spec(request$spec)
    calc_task$invoke(request$spec)
  })

  output$stale_result <- renderUI({
    req(submitted_spec())
    request <- requested_spec()
    if (!is.null(request$error) || !identical(request$spec, submitted_spec()))
      div(class = "flag", role = "status", "Параметры изменены. Показанный результат относится к предыдущим вводным. Нажмите «Рассчитать».")
  })

  observe({
    session$sendCustomMessage("setBusy", list(busy = calc_task$status() == "running"))
  })

  output$status <- renderUI({
    if (calc_task$status() == "running")
      div(class = "status-running", "Расчёт выполняется или ожидает очереди. Для ABEL это может занять несколько минут…")
  })

  res <- reactive({
    req(calc_task$status() == "success")
    x <- calc_task$result()
    req(is.null(x$error))
    x
  })

  output$error <- renderUI({
    e <- input_error()
    if (!is.null(e)) return(div(class = "warn-box", paste("Ошибка ввода:", paste(e, collapse = "\n"))))
    st <- calc_task$status()
    if (st == "error") return(div(class = "warn-box", paste("Ошибка:", tryCatch(
      { calc_task$result(); "" }, error = function(err) "Не удалось выполнить фоновый расчёт. Повторите попытку; если ошибка сохраняется, сообщите администратору."))))
    if (st == "success") {
      x <- calc_task$result()
      if (!is.null(x$error)) return(div(class = "warn-box", paste("Ошибка:", x$error)))
    }
    NULL
  })

  output$result_context <- renderUI({
    x <- res()
    div(class = "scenario-note", paste(x$scenario_label, DESIGN_LABELS[[x$design]],
      paste0("Мощность: ", round(x$target * 100), "%"),
      paste0("Выбывание: ", fmt_num(x$dropout, 1), "%"),
      format(x$calculated_at, "%d.%m.%Y %H:%M:%S %Z", tz = app_timezone()), sep = " · "))
  })

  output$param_table <- renderUI({
    x <- res()
    rows <- lapply(seq_along(x$params), function(i) {
      p <- x$params[[i]]
      tags$tr(class = if (i == x$det) "det" else "",
              tags$td(sub_html(p$label)), tags$td(title = fmt_method(p), fmt_method_short(p)),
              tags$td(fmt_gmr(p$theta0)), tags$td(if (p$method == "ABEL" && !is.null(p$cvwt))
                paste0("R: ", fmt_cv(p$cv * 100), "; T: ", fmt_cv(p$cvwt * 100)) else fmt_cv(p$cv * 100)), tags$td(x$n_raw[i]),
              tags$td(paste0(fmt_num(x$pow_N[i] * 100, 1), "%")))
    })
    tags$table(class = "result-table param-table",
      tags$thead(tags$tr(tags$th("Параметр"), tags$th("Метод"), tags$th("GMR"),
                         tags$th("CV, %"), tags$th("N"), tags$th("Мощн. при N"))),
      tags$tbody(rows))
  })

  output$result_table <- renderUI({
    x <- res()
    grp <- "На группу"
    lines <- list(
      list(sub_html(sprintf("Размер выборки (анализ), определяющий параметр — %s", x$params[[x$det]]$label)),
           format(x$N, big.mark = " "), TRUE),
      list(grp, per_seq(x$N, x$nseq), FALSE),
      list("Достигнутая мощность (определяющий параметр)",
           sprintf("%s%%", fmt_num(x$pow_N[x$det] * 100, 2)), FALSE),
      list("Размер выборки с учётом выбывания", format(x$n_adj, big.mark = " "), TRUE),
      list(grp, per_seq(x$n_adj, x$nseq), FALSE)
    )
    tags$table(class = "result-table",
      tags$tbody(lapply(lines, function(row) {
        tags$tr(class = if (isTRUE(row[[3]])) "important" else "",
                tags$td(row[[1]]), tags$td(row[[2]]))
      })))
  })

  output$flags <- renderUI({
    x <- res()
    tagList(
      if (length(x$excluded)) div(class = "flag",
        sub_html(paste("Не включены в расчёт:", paste(x$excluded, collapse = ", ")))),
      if (x$rules$flag_min) div(class = "flag",
        "Применён минимум 12 субъектов в анализируемой популяции (Правила ЕАЭС); PowerTOST рассчитал меньше."),
      if (x$rules$flag_rtr) div(class = "flag",
        sub_html("Применено требование для 2×2×3 с ABEL: не менее 12 субъектов в последовательности RTR для оценки CVwR.")))
  })

  output$sens_cv_title <- renderUI({
    x <- res()
    div(class = "method-label", sub_html(paste0("Чувствительность к CV (", x$params[[x$det]]$label, ")")))
  })
  output$sens_cv <- renderUI(sens_table(res()$sens_cv, "CV, %"))

  output$sens_gmr_title <- renderUI({
    x <- res()
    div(class = "method-label", sub_html(paste0("Чувствительность к GMR (", x$params[[x$det]]$label, ")")))
  })
  output$sens_gmr <- renderUI(sens_table(res()$sens_gmr, "GMR"))

  output$raw <- renderText(res()$raw)

  output$warnings_ui <- renderUI({
    x <- res()
    if (length(x$warnings))
      div(class = "warn-box", paste0("Предупреждения PowerTOST:\n", paste(x$warnings, collapse = "\n")))
  })

  observeEvent(input$copy_raw, {
    x <- res()
    session$sendCustomMessage("copyBeText", list(text = x$raw, what = "raw"))
  })
  observeEvent(input$copy_summary, {
    x <- res()
    session$sendCustomMessage("copyBeText", list(text = build_summary(x), what = "summary"))
  })

  observeEvent(input$clipboard_result, {
    what <- if (identical(input$clipboard_result$what, "summary")) "Сводка скопирована" else "Вывод PowerTOST скопирован"
    if (isTRUE(input$clipboard_result$ok)) {
      showNotification(paste(what, "в буфер обмена."), type = "message", duration = 3)
    } else {
      showNotification("Не удалось скопировать. Выделите текст вручную.", type = "error", duration = 5)
    }
  })

  output$plot_title <- renderUI({
    x <- res()
    div(class = "method-label", sub_html(paste0("Мощность в зависимости от N (", x$params[[x$det]]$label, ")")))
  })

  output$powerplot <- renderPlot({
    x <- res()
    ggplot(x$curve, aes(x = n, y = power * 100)) +
      geom_line(linewidth = 0.9, colour = "#008b42", na.rm = TRUE) +
      geom_point(size = 1.7, colour = "#008b42", na.rm = TRUE) +
      geom_hline(yintercept = x$target * 100, linetype = 2, colour = "#ba5c38") +
      geom_vline(xintercept = x$N, linetype = 3, colour = "#546a60") +
      labs(x = "Общий размер выборки N", y = "Мощность (%)") +
      coord_cartesian(ylim = c(0, 100)) +
      theme_minimal(base_size = 12, base_family = "sans") +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 5))
  })

  output$graph_note <- renderUI({
    x <- res()
    if (x$params[[x$det]]$method == "ABEL") {
      helpText("Для графика ABEL использовано 10 000 симуляций на точку; результаты графика приблизительные. Основной расчёт размера выборки выполнен с настройками PowerTOST по умолчанию (100 000 симуляций).")
    }
  })
}

shinyApp(ui = ui, server = server)
