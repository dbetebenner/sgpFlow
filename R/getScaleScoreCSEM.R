`getScaleScoreCSEM` <- 
function(
    scale_score,
    grade,
    content_area,
    state
) {
    tmp.approx.fun.data <- SGPstateData[[state]][['Assessment_Program_Information']][['CSEM']][CONTENT_AREA==content_area & GRADE==grade]
    tmp.approx.fun <- approxfun(tmp.approx.fun.data[['SCALE_SCORE']], tmp.approx.fun.data[['SCALE_SCORE_CSEM']])
    return(tmp.approx.fun(scale_score))
} ### END getScaleScoreCSEM
