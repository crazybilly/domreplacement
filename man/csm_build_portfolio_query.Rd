% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csm_portfolio.R
\name{csm_build_portfolio_query}
\alias{csm_build_portfolio_query}
\title{Build a SQL Query like the CSM for the base portfolio}
\usage{
csm_build_portfolio_query(
  office,
  fundraiser_id,
  role,
  role_fy = 2022,
  plan_type
)
}
\arguments{
\item{office}{an office code (from assignments)}

\item{fundraiser_id}{fundraiser ID (numeric)}

\item{role}{role (from goals)}

\item{role_fy}{4 digit fiscal year for roles}

\item{plan_type}{plan type. Leave blank for all}
}
\value{
a sql query (ie. text)
}
\description{
Build a SQL Query like the CSM for the base portfolio
}
