% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{merge_data}
\alias{merge_data}
\title{Merge the data from the list returned from `scrape_data()`}
\usage{
merge_data(output_folder)
}
\arguments{
\item{output_folder}{T}
}
\value{
out_list A list with the same length as the number number of rows in `storify_metadata`, with each list item a data frame with data for all of the tweets scraped.
}
\description{
Merge the data from the list returned from `scrape_data()`
}

