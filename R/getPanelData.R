`getPanelData` <-
function(
	long_data,
	sgpFlow.config,
	cohort_end_year=NULL) {

	### Parameters
	if (is.null(cohort_end_year)) cohort_end_year <- long_data[,max(YEAR)]
	year.progression <- if (length(sgpFlow.config[['year_lags.progression']]) > 0) yearIncrement(cohort_end_year, sgpFlow.config[['year_lags.progression']]) else cohort_end_year

	tmp.lookup <- SJ("VALID_CASE", sgpFlow.config[['content_area.progression']], year.progression, sgpFlow.config[['grade.progression']])
	tmp.dt <- ddcast(long_data[tmp.lookup, on = .(YEAR = V3, CONTENT_AREA = V2, GRADE = V4)], ID ~ YEAR + CONTENT_AREA, value.var="SCALE_SCORE", sep=".")
	tmp.dt <- na.omit(tmp.dt, cols=ncol(tmp.dt))
	setnames(tmp.dt, 2:dim(tmp.dt)[2], paste0("SS", sgpFlow.config[['grade.progression']]))
} ## END getPanelData
