
SFF <- function(df){
 some <- matrix(nrow = dim(df)[1],ncol = 3)

 for (i in 1:dim(df)[1]){
  ts <- unlist(strsplit(as.character(df$Correspondence.Address)[i],",|;|)"))
  if(nchar(as.character(df$Correspondence.Address[i]))==0){
    some[i,1] <- 0
    some[i,2] <- 0
    some[i,3] <- 0
  }else if( str_detect((rev(ts)[1]),"email:")==T ){
    some[i,1] <- 1
    some[i,2] <- rev(ts)[2]
    some[i,3] <- gsub(" ","",some[i,2],fixed=T)
  }else if(str_detect((rev(ts)[1]),"email:")==F){
    if(  nchar(rev(ts)[1])<15  ){
      some[i,1] <- 1
      some[i,2] <- rev(ts)[1]
      some[i,3] <- gsub(" ","",some[i,2],fixed=T)
          }else{
          some[i,1] <- 0
          some[i,2] <- as.character(df$Correspondence.Address)[i]
          some[i,3] <- 0
          }
  }
 }
 some
}

  SFF(df_2_CM) %>% .[,3] %>% table %>% .[(order(.,decreasing=T))] %>%head(10)
  
  #plot
  dl <- SFF(df_3_CM) %>% .[,3] %>% table %>% .[(order(.,decreasing=T))] %>% as.data.frame %>% 
      `colnames<-`(c("country","n")) %>%
    mutate(., c2 = ifelse(country=="UnitedStates", "USA",
                                      ifelse(country=="UnitedKingdom", "UK",
                                             as.character(country))))
                                      
  #map_data('world') %>% group_by(region) %>% summarise() %>% print(n = Inf)
  
  map.world <- map_data('world')
  map.world$V4 <- gsub(" ","",map.world$region,fixed=T)
  
  df <- left_join(map.world, dl, by = c('V4' = 'c2'))
  
  pg <- ggplot(data = df, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = n)) + scale_fill_distiller(palette = rev("OrRd"),direction = 1)
  pg