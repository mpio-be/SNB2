% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/A_EVENTS_elements_v1.R
\name{assign_direction_v1}
\alias{assign_direction_v1}
\title{Assign direction to different pieces of activity}
\usage{
assign_direction_v1(x)
}
\arguments{
\item{x}{A data.table as supplied by \link{fetch_ins_outs_v1}.}
}
\description{
Assign direction to different pieces of activity, e.g. "IN" and "OUT"
}
\details{
See \link{events_v1} for further information. Note that after some minor alterations, the output of this function is returned, if in function \link{events_v1} groups_ins_and_outs = FALSE.
}
\examples{
#Not run
#see help of function events_v1
?events_v1()
}
