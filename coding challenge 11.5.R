rm(list=ls())
# Wikipedia explorer
library(tidyverse)
library(rvest)
library(polite)

# Download a random page
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
# Get a page from a relative link
rel_get <- function(rel_link){
  url <- paste0("https://en.wikipedia.org", rel_link, collapse="")
  politely(read_html)(url)
}
# Given a relative link, get it's title, link count, 
# and a nested tibble column of relative links.
nest_get <- function(rel_link){
  page <- rel_get(rel_link)
  tibble(title=title(page),
         links=links(page),
         n_links = length(links(page))) %>%
    nest(links=links)
}
# Given a relative link, get link information and do 
# the same for those links

recursive_get <- function(link = "/wiki/Moth", levels = 0){
  # if(link == "" | link == "rand"){
  #   page <- rand_page()
  #   link <- sample(links(page),1)
  # }
  out <- nest_get(link)
  tried_links <- c(link)
  while(levels >= 1){
    L <- out %>% unnest(links) %>% pull(links) %>% setdiff(tried_links)
    for(l in L){
      print(l)
      this_page <- nest_get(l)
      out <- full_join(out, this_page)
      rm(this_page)
    }
    tried_links <- c(tried_links, L)
    levels <- levels - 1
  }
  out
}

result0 <- recursive_get("/wiki/John_Candy", levels = 0)
result1 <- recursive_get("/wiki/John_Candy", levels = 1)
result2 <- recursive_get("/wiki/John_Candy", levels = 2)
result3 <- recursive_get("/wiki/John_Candy", levels = 3)

result1 <- recursive_get("/wiki/Farmingdale_State_College", levels=1)