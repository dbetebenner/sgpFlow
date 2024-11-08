`getPanelData` <-
function(
	long_data,
	grade.progression,
	content_area.progression,
	year_lags.progression,
	cohort_end_year=NULL
	) {

	### Parameters
	if (is.null(cohort_end_year)) cohort_end_year <- long_data[,max(YEAR)]
	year.progression <- fifelse(length(year_lags.progression) > 0, yearIncrement(cohort_end_year, year_lags.progression), cohort_end_year)

	### 
	tmp.lookup <- SJ("VALID_CASE", content_area.progression, year.progression, grade.progression)
	setkey(tmp.lookup, V3)
	setkey(tmp.lookup, NULL)

	return(ddcast(long_data[tmp.lookup][, 'tmp.timevar':=paste(YEAR, CONTENT_AREA, sep=".")], ID ~ tmp.timevar, value.var=c("GRADE", "SCALE_SCORE"), sep="."))
} ## END getPanelData
