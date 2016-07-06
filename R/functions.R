# functions.R

#' Collect meta data for each storify from a vector of Storify URLs.
#'
#' @param vector_of_urls A vector of Storify URLs.
#' @return out_df A data frame with the number of rows equal to the length of `vector_of_urls` with metadata for each Storify.

collect_metadata <- function(vector_of_urls){
    out_df <- data.frame(url = vector_of_urls)
    for (i in 1:nrow(out_df)){
        split_url <- httr::parse_url(out_df$url[i])
        out_df$url[i] <- paste0(as.character(split_url$scheme), "://", as.character(split_url$hostname), "/", as.character(split_url$path)) # not saving some urls (but still works for api_url)
        out_df$api_url[i] <- paste0("https://api.storify.com/v1/stories/", as.character(split_url$path)) # makes a url
        temp_dat <- jsonlite::fromJSON(out_df$api_url[i], simplifyDataFrame = T)
        out_df$num_tweets[i] <- (temp_dat$content$stats$elements$text + temp_dat$content$stats$elements$quote + temp_dat$content$stats$elements$image + temp_dat$content$stats$elements$video + temp_dat$content$stats$elements$link + temp_dat$content$stats$elements$other) # number of tweets in story
        out_df$n_pages[i] <- ceiling(out_df$num_tweets[i] / 50) # number of pages, to be used later
        out_df$date[i] <- stringr::str_split(temp_dat$content$date$created, "T")[[1]][1]
        print(paste0("Processed Storify ", i, " from ", out_df$date[i], " with ", out_df$num_tweets[i], " tweets"))
    }
    return(out_df)
}

#' Scrape data for every tweet in every Storify for which metadata was collected
#'
#' @param storify_metadata A data frame with a URL for each Storify and associated metadata as output from `collect_metadata()`.
#' @param output_folder Folder for the data frames for each Storify written as a CSV.
#' @return out_list A list with the same length as the number number of rows in `storify_metadata`, with each list item a data frame with data for all of the tweets scraped. In addition, each data frame is written to a CSV file in the `output_folder` argument.

scrape_data <- function(storify_metadata, output_folder){

    out_list <- list()

    for (i in (1:nrow(storify_metadata))){ # this iterates through the storifies

        temp_page_num <- 1:storify_metadata$n_pages[i] # temp_page_num
        temp_urls_to_process <- paste0(storify_metadata$api_url[i], "?page=", temp_page_num, "&per_page=50") # temp_urls_to_process

        out_text <- list() # for output for text
        out_time <- list() # for output for time
        out_username <- list() # for output for username

        for (j in 1:length(temp_urls_to_process)){ # this selects one page of one storify
            temp_dat <- jsonlite::fromJSON(temp_urls_to_process[j])

            temp_link <- ifelse(grepl("http:", temp_dat$content$elements$permalink) | grepl("https:", temp_dat$content$elements$permalink),
                                temp_dat$content$elements$permalink, paste0("http:", temp_dat$content$elements$permalink)) # gets links and corrects broken links

            temp_link_time <- temp_dat$content$elements$posted_at

            my_output_vec <- vector() # makes vector for text output for page
            my_output_vec_time <- vector() # makes vector for text output for page

            for (k in 1:length(temp_link)){ # this processes one page of one storify
                my_output_vec[k] <- missing_func(temp_link[k], i = i, j = j, k = k)

                parsed_time <- temp_link_time[k] # timestamp

                # these are all temps
                date <- stringr::str_split(parsed_time, "T") # date is t1[[1]][1]
                time <- stringr::str_split(date[[1]][2], "\\.") # time is time[[1]][1]
                date_time <- paste0(date[[1]][1], " ", time[[1]][1])
                my_output_vec_time[k] <- lubridate::parse_date_time(date_time, "ymd hms", tz = "UST")
            }

            out_time[[j]] <- my_output_vec_time
            out_username[[j]] <- temp_dat$content$elements$source$username # username
            out_text[[j]] <- my_output_vec # text
        }

        out_data <- data.frame(time = .POSIXct(unlist(out_time)), username = unlist(out_username), text = unlist(out_text))
        out_list[[i]] <- out_data
        write.csv(out_data, paste0(output_folder, i, ".csv"), row.names = F) # change to local dir
    }

    return(out_list)

}

#' Merge the data from the list returned from `scrape_data()`
#'
#' @param output_folder T
#' @return out_list A list with the same length as the number number of rows in `storify_metadata`, with each list item a data frame with data for all of the tweets scraped.

merge_data <- function(output_folder){
    file_names <- dir(output_folder, pattern =".csv") #
    out_ls <- list()
    for (i in 1:length(file_names)){
        out_ls[[i]] <- read.csv(paste0(output_folder, file_names[i]), header = T, stringsAsFactors = F)
    }

    dat <- plyr::ldply(out_ls) # need to have plyr installed
}
