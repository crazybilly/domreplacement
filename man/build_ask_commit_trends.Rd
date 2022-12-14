% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_ask_commit_trends.R
\name{build_ask_commit_trends}
\alias{build_ask_commit_trends}
\title{Build Ask and Commit Trend Data}
\usage{
build_ask_commit_trends(
  proposal_data,
  metric = "asks",
  value = "#",
  first_fy = 2015
)
}
\arguments{
\item{proposal_data}{a data frame of proposal data, built by get_proposal_data(). Filter by date, fundraiser and unit, if desired, BEFORE using this function.}

\item{metric}{a character strings, the type of thing to count, should be 'asks' or 'commits'}

\item{value}{a character to determine if we're counting proposals or totalling the value of those proposals. Should be '#' or '$'}

\item{first_fy}{the first fiscal year to look at. Helps to filter data down, just in case you didn't already filter by date.}
}
\value{
a dataframe to test against
}
\description{
build a data frame to test USM/CSM/DOM Tab #2 against.
}
