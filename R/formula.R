#' Retrieve response data from a formula and a data.frame.
#'
#' @param x A formula object with a non-empty left-hand side.
#' @param data A data.frame to retrieve the response data from.
#'
#' @return A vector containing the response variable, with a `name' attribute
#'  containing the name of the response variable.
.response_from_formula<- function(x,data) {
  the_terms<- terms(x,specials=c("mean","time"))
  if( attr(the_terms,"response") == 0 ) {
    stop("Response variable must be specified.")
  } else {}
  idx<- attr(the_terms,"response")
  var_call<- attr(the_terms,"variables")[[idx+1]] # +1 because [[1]] is just `list`
  response<- with(data,eval(var_call))
  attr(response,"name")<- deparse(var_call)
  return(response)
}

#' Retrieve observation mean covariate data from a formula and a data.frame.
#'
#' @param x A formula object with terms grouped in a \code{mean(...)} function.
#' @param data A data.frame containing the covariate data.
#'
#' @return The design matrix for the covariates.
.mean_design_from_formula<- function(x,data) {
  the_terms<- terms(x,specials=c("mean","time"))
  term.labels<- attr(the_terms,"term.labels")
  the_call<- grep("^mean",term.labels,value=T)
  if( length(the_call) == 0 ) {
    the_df<- matrix(0,nrow=nrow(data),ncol=0)
    return(the_df)
  } else {}
  new_formula<- sub("mean\\(","~",the_call)
  new_formula<- sub("\\)$","",new_formula)
  new_terms<- terms(formula(new_formula))
  attr(new_terms,"intercept")<- 0
  the_df<- model.matrix(new_terms,data=data)
  attr(the_df,"assign")<- NULL
  rownames(the_df)<- NULL
  return(the_df)
}

#' Retrieve a time index from a formula and a data.frame.
#'
#' @param x A formula object with a time(...,type) function containing one entry,
#'  and a `type' argument which can be one of "ar1", "rw", or "independent".
#' @param data A data.frame containing the time information.
#'
#' @return A vector containing the index, with a `name' attribute
#'  containing the name of the time index and a `type' attribute containing the
#'  type of time process (ar1, random walk, or independent).
.time_from_formula<- function(x,data) {
  time<- function(x,type="ar1") {
    type<- pmatch(type[[1]],c("ar1","rw","independent"))
    type<- c("ar1","rw","independent")[type]
    attr(x,"type")<- type
    return(x)
  }
  the_terms<- terms(x,specials=c("mean","time"))
  term.labels<- attr(the_terms,"term.labels")
  the_call<- grep("time",term.labels,value=T)
  if( length(the_call) == 0 ) {
    x<- rep(0,nrow(data))
    attr(x,"name")<- "Time"
    attr(x,"type")<- "independent"
    return(x)
  } else {}
  new_formula<- sub("time","~",the_call)
  new_formula<- sub(",.*","",new_formula)
  new_formula<- sub("\\(","",new_formula)
  new_formula<- sub("\\)$","",new_formula)
  new_terms<- terms(formula(new_formula))
  if( length(attr(new_terms,"term.labels")) > 1 ) {
    stop("Only one variable is allowed for time.")
  } else {
    the_name<- attr(new_terms,"term.labels")[[1]]
  }
  new_formula<- sub("time","~time",the_call)
  new_terms<- terms(formula(new_formula),specials="time")
  attr(new_terms,"intercept")<- 0
  x<- model.frame(new_terms,data=data)[[1]]
  attr(x,"name")<- the_name

  return(x)
}

#' Check which covariates are used in a staRVe formula
#'
#' @param x A formula object with terms grouped in a \code{mean(...)} function.
#'
#' @return A character vector giving the names of covariates used in a formula.
.names_from_formula<- function(x) {
  # Remove response and time from formula
  the_terms<- terms(x,specials=c("mean","time"))
  term.labels<- attr(the_terms,"term.labels")
  the_call<- grep("time",term.labels,value=T,invert=T) # Get rid of time(...)
  if( length(the_call) == 0 ) {
    return(character(0))
  } else {}
  new_formula<- paste(the_call,collapse=" + ")
  new_formula<- paste("~",new_formula)
  var_names<- all.vars(formula(new_formula))
  return(var_names)
}
