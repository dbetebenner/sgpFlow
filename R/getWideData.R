getWideData <-
    function(
        long_data,
        sgpFlow.config,
        cohort_end_year = NULL
    ) {

    ### Parameters
    if (is.null(cohort_end_year)) cohort_end_year <- long_data[, max(YEAR)]
    year.progression <-
        if (length(sgpFlow.config[['year_lags.progression']]) > 0) {
            yearIncrement(cohort_end_year, sgpFlow.config[['year_lags.progression']])
         } else cohort_end_year

    cohort_lookup <-
        SJ( VALID_CASE = "VALID_CASE", 
            YEAR = year.progression, 
            GRADE = sgpFlow.config[['grade.progression']], 
            CONTENT_AREA = sgpFlow.config[['content_area.progression']])
    new.names <- c("ID", paste0("SS", sgpFlow.config[['grade.progression']]))
    vars.to.keep <- c("ID", names(cohort_lookup), "SCALE_SCORE")

    return(
        pivot(
            how = "wider",
            data =
              join(
                x = fselect(long_data, vars.to.keep), y = cohort_lookup,
                how = "inner", on = names(cohort_lookup), verbose = FALSE, overid = 2),
            ids = "ID", names = c("YEAR", "CONTENT_AREA"),
            values = "SCALE_SCORE"
        ) |> 
          setnames(new.names) |> na_omit(cols = tail(new.names, 1)) |> setkeyv("ID")
    )
} ## END getWideData
