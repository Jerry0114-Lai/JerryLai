
#task1
get_wiki_covid19_page <- function() {
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  query_wiki <- list(title = "Template:COVID-19_testing_by_country")
  response <- httr::GET(url = wiki_base_url, query = query_wiki)
  return(response)
}

wiki_covid19_response <- get_wiki_covid19_page()

content(wiki_covid19_response,"text")

#task2
root_html<-read_html(wiki_covid19_response)

table_nodes<-html_nodes(root_html,"table")

table_node<-table_nodes[[2]]

wiki_data_frame<-html_table(table_node, fill = TRUE)

print(wiki_data_frame)

#task3
summary(wiki_data_frame)

preprocess_covid_data_frame <- function(data_frame) {
  
  shape <- dim(data_frame)
  
 
  data_frame<-data_frame[!(data_frame$`Country or region`=="World"),]

  data_frame <- data_frame[1:172, ]
  
  
  data_frame["Ref."] <- NULL
  data_frame["Units[b]."] <- NULL
  

  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  
 
  data_frame$country <- as.factor(data_frame$country)
  data_frame$date <- as.factor(data_frame$date)
  data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
  data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
  data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
  
  return(data_frame)
}

new_wiki_data_frame<-preprocess_covid_data_frame(wiki_data_frame)
print(new_wiki_data_frame)
