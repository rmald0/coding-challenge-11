rm(list=ls())
# Wikipedia explorer
library(tidyverse)
library(rvest)
library(polite)

rand_page <- function(){
  random_url <- "https://en.wikipedia.org/wiki/Special:Random"
  read_html(random_url)
  #politely(read_html)(random_url) # not going to work for random url.
}

# Get the title from a given page
title <- function(page){
  page %>% html_element("title") %>% html_text %>%
    str_remove(" - Wikipedia")
}

# Get all unique relative links to other wikipedia pages
links <- function(page){
  rel_links <- page %>%
    html_elements("#content") %>%
    html_elements("p") %>%
    html_elements("a") %>%
    html_attr("href") %>%
    unique()
  rel_links[str_detect(rel_links, '/wiki/')]
}

links_and_names <- function(page){
  rel_links <- page %>%
    html_elements("#content") %>%
    html_elements("p") %>%
    html_elements("a") %>%
    html_attr("href")
  links <- rel_links[str_detect(rel_links, '/wiki/')]
  names <- page %>%
    html_elements("#content") %>%
    html_elements("p") %>%
    html_elements("a") %>%
    html_text()
  names <- names[str_detect(rel_links, "/wiki/")]
  out <- data.frame(name = names, url = links)
  unique(out)
}

# Get a page from a relative link
rel_get <- function(rel_link){
  url <- paste0("https://en.wikipedia.org", rel_link, collapse="")
  politely(read_html)(url)
}
# Given a relative link, get it's title, link count, links, and the titles of those links

get_links <- function(rel_link){
  page <- rel_get(rel_link)
  page_title <- title(page)
  links_names <- links_and_names(page)
  data.frame(title = page_title, links = links_names$url, link_name = links_names$name, link_count = nrow(links_names))
}

recursive_get <- function(rel_link = "/wiki/Moth"){
  out <- get_links(rel_link)
  links_to_try <- out$links
  for(l in links_to_try){
    print(l)
    for(l in out$links){
      out2 <- get_links(l, depth + 1, max_depth, stop_recursion)
      out <- rbind(out, out2)
    rm(out2)
  }
  out
}

example_page <- recursive_get()

write_csv(example_page)
