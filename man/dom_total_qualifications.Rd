% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dom-total-qualifications.R
\name{dom_total_qualifications}
\alias{dom_total_qualifications}
\title{Build a query for Total Qualifications as calculated in the DOM}
\usage{
dom_total_qualifications(
  FR_ID,
  Unit,
  Plan_Type = "0",
  BeginDate = "2021-07-01",
  EndDate = "2022-06-30"
)
}
\arguments{
\item{FR_ID}{a fundraiser's entity_id}

\item{Unit}{a unit code}

\item{Plan_Type}{plan types to include, leave '0' for all plan types}

\item{BeginDate}{a date, as a string in YYYY-MM-DD format. Qualification date should between BeginDate and EndDat}

\item{EndDate}{a date, as a string in YYYY-MM-DD format}
}
\value{
a string that can be used as an SQL query
}
\description{
Build a query for Total Qualifications as calculated in the DOM
}
