# helpers.R

missing_func <- function(temp_link, i, j, k){
    out <- tryCatch({
        temp_vec <- xml2::read_html(temp_link)
        temp_vec <- rvest::html_nodes(temp_vec, ".tweet-text") # extracts tweet
        out <- rvest::html_text(temp_vec) # saves text to vector for page
    },
    error = function(cond){
        message(cond)
        out <- NA
    },
    finally = print(paste0("Parsing HTML for Storify ", i, " page ", j, ", tweet ", k))
    )
    return(out)
}

