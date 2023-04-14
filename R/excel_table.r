#' Excel output
#' @noRd
excel_out3 <-
  function(filename="", reportTables) {

    todaydate<- as.character(Sys.Date())
    fakewords = "Input Text"

    fit.dt    <- reportTables$fit.dt %>% mutate_if(is.numeric, ~ round(.x, 3))
    est.dt    <- reportTables$est.dt %>% mutate_if(is.numeric, ~ round(.x, 3)) %>%
      dplyr::select(.data$lhs, .data$op, .data$rhs, .data$est,.data$se, .data$pvalue)
    grmest.dt <- reportTables$grmest.dt %>% mutate_if(is.numeric, ~ round(.x, 3)) %>%
      mutate(Variable = rownames(reportTables$grmest.dt), .before = .data$a1)

    temp_wb <- createWorkbook("temp_wb")

    # Title page
    addWorksheet(temp_wb, "Results")

    col_p <- 1
    writeData(
      temp_wb,
      "Results",
      x = "MODEL FIT",
      startCol = col_p,
      startRow = 1
    )

    writeDataTable(
      temp_wb,
      "Results",
      x = fit.dt,
      startCol = col_p,
      startRow = 2
    )

    col_p <- col_p + ncol(fit.dt) + 1
    writeData(
      temp_wb,
      "Results",
      x = "MODEL ESTIMATES",
      startCol = col_p,
      startRow = 1
    )

    writeDataTable(
      temp_wb,
      "Results",
      x = est.dt,
      startCol = col_p,
      startRow = 2
    )

    col_p <- col_p + ncol(est.dt) + 1
    writeData(
      temp_wb,
      "Results",
      x = "GRM ESTIMATES",
      startCol = col_p,
      startRow = 1
    )

    col_p <- col_p + 1
    writeDataTable(
      temp_wb,
      "Results",
      x = grmest.dt,
      startCol = col_p,
      startRow = 2
    )

    return(temp_wb)
  }
