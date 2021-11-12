rm(list = ls())

library(polite)
library(rvest)
library(purrr)

session <- bow("https://ingatlan.com")

raw_list <- purrr::map(
  paste0('lista/kiado+lakas?page=', 701:723), 
  ~ { nod(session, .x) %>%
      scrape()%>%
      html_nodes(".listing.js-listing")%>%
      html_text(trim=TRUE)
  }
)



raw_df<-as.data.frame(unlist(raw_list))

out <- strsplit(as.character(raw_df$`unlist(raw_list)`),'    ') 
separated_df<-as.data.frame(do.call(rbind, out))


separated_df <- data.frame(lapply(separated_df, trimws), stringsAsFactors = FALSE)

separated_df[t(apply(separated_df,1, function(x){duplicated(x)}))]<-NA 

separated_df <- as.data.frame(t(apply(separated_df[c(2:9)],1, function(x) { return(c(x[is.na(x)],x[!is.na(x)]) )} )))

func<-function(input_df,col_num, separator){
  
  Running_var<-strsplit(as.character(input_df[,col_num]),separator)
  Running_var<-do.call(rbind, Running_var)
  Running_var[t(apply(Running_var,1, function(x){duplicated(x)}))]<-NA 
  return(Running_var)
  
}

extrak<-func(separated_df,1,'  ')
arak<-func(separated_df,4,'  ')
cimek<-func(separated_df,5,',')
attributomok<-func(separated_df,6,'  ')

fin_df<-as.data.frame(cbind(extrak,arak,cimek,attributomok))

write.csv(fin_df, paste("data", format(Sys.time(),"%d.%m.%Y"),".csv"),row.names=T)
