# scrapestorify

An R package to scrape data from Storify using the Storify API.

The package currently has three functions:

1. `collect_metadata()`

This function takes a vector of Storify URLs, and outputs a data frame with each URL and associated data. 

2. `scrape_data()`

This function takes the output of collect_metadata(), and outputs a list of data frames with data from each Storify. 

3. `merge_data()`

This function merges the list output from scrape_data() into one data frame.

Use `?` before each function to view the documentation.
