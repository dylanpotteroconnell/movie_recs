library(tidyverse)
library(rvest)

base_url <- "https://letterboxd.com"

usernames <- read_csv("data/username_set.csv") %>%
  pull(username)


GrabHtml <- function(url) {
  print(paste("Grabbing from:", url))
  html_output <- tryCatch(read_html(url), 
                          error = function(e) { print(paste0("Error grabbing: ", 
                                                             url, 
                                                             ". Error was: ", 
                                                             e))
                            return(e)})
  if ("error" %in% class(html_output)) {
    if (str_detect(html_output$message, "503")) {
      Sys.sleep(10)
      html_output <- GrabHtml(url)
    }
  }
  return(html_output)
}

GrabAllUserMovies <- function(username, start_page = 1, max_num_movies = 2000) {
  # These movies are sorted by their most recent release date. (Popularity would be 
  # convenient, but robots.txt requests that sorting not be used by a scraper).
  url <- paste0(base_url,
                username,
                "films/page/",
                start_page,
                "/")
  user_movies <- NULL
  # We set the next_url_slug to a dummy value, it will be set to NA
  # when we should not proceed.
  next_url_slug <- "PROCEED"
  while (! is.na(next_url_slug)) {
    Sys.sleep(runif(1, 1, 2))
    user_html <- GrabHtml(url)
    # Return NULL if the page is missing
    if ("error" %in% class(user_html)) {
      next_url_slug <- NA
    } else {
      user_movies <- bind_rows(user_movies, GrabUserMoviePage(user_html))
      next_url_slug <- user_html %>%
        html_nodes(".next") %>%
        html_attr("href")
      # If we have grabbed too many movies, we set next_url_slug to be NA to stop
      # us proceeding any further.
      num_movies <- ifelse(is.null(user_movies), 0, nrow(user_movies))
      if (num_movies > max_num_movies) {
        next_url_slug <- NA
      } else {
        url <- paste0(base_url, next_url_slug)
      }
    }
  }
  
  # Add the username label, and return the data frame
  user_movies %>%
    mutate(username = username) %>%
    slice(1:max_num_movies) %>%
    return()
}

# Input:
#    user_html: from rvest, the html object for a single user review page
# Output:
#    Data frame with the relevant entries
GrabUserMoviePage <- function(user_html) {
  movie_posters <- user_html %>% 
    html_nodes(".poster-list")
  num_posters <- movie_posters %>% 
    html_children() %>%
    length()
  if (num_posters == 0) {
    print("No movies on page")
    return(NULL)
  } else {
    user_df <- movie_posters %>%
      html_children() %>%
      map_df(function(movie) 
      {
        movie_sub <- movie %>% html_children()
        tibble(user_rating = movie %>% html_attr("data-owner-rating"),
               target_link = movie_sub %>% html_attr("data-target-link"),
               film_id = movie_sub %>% html_attr("data-film-id"),
               film_slug = movie_sub %>% html_attr("data-film-slug"))
      })
    return(user_df)
  }
}


CompileManyUsers <- function(usernames) {
# If the file "all_user_reviews.csv" exists, we start by appending to it
  if("all_user_reviews.csv" %in% list.files("data/")) {
    all_user_reviews <- read_csv("data/all_user_reviews.csv") %>%
      mutate(user_rating = as.character(user_rating),
               film_id = as.character(film_id))
# We also remove all elements from usernames that exist in our data
    usernames <- setdiff(usernames,
            all_user_reviews %>% 
              pull(username) %>%
              unique())
    if (length(usernames) == 0) {
      return(all_user_reviews)
    }
  } else {
    all_user_reviews <- NULL
  }
  
  for(i in 1:length(usernames)) {
# We save our progress every 5 users
    if(i %% 5 == 0) {
      print(paste0("On user: ", i))
      all_user_reviews %>% write_csv("data/all_user_reviews.csv")
    }
    all_user_reviews <- all_user_reviews %>%
      bind_rows(usernames[i] %>% GrabAllUserMovies(max_num_movies = 100))
  }
  return(all_user_reviews)
}

all_movies <- CompileManyUsers(usernames[1:9])
all_movies %>% write_csv("data/all_user_reviews.csv")

temp <- GrabHtml("https://letterboxd.com/bratpitt/films/page/abc")
