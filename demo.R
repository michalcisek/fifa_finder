library(tidyverse)
library(magrittr)

df <- read_csv("CompleteDataset.csv")

#remove Goalkeepers and GK attribute
df %<>% 
  filter(!(`Preferred Positions` %in% 'GK')) %>% 
  select(-starts_with('GK'))

#select attributes
df %<>% 
  select(Name, Nationality, Club, Overall, Acceleration:Volleys)

#mode - update/attribute
parse_attribute <- function(attribute, mode = "update"){
  if (mode == "update"){
    attribute <- unname(sapply(attribute, function(x) eval(parse(text = x))))
  } else if (mode == "baseline"){
    attribute <- unlist(strsplit(attribute, "[+-]"))[1]
    attribute <- as.numeric(attribute)
  }
  
  return(attribute)
}

#parse players' attributes
df[, 5:ncol(df)] <- sapply(df[, 5:ncol(df)], parse_attribute, "update")

#calculate attributes as deviation from overall performance
df[, 5:ncol(df)] <- sapply(df[, 5:ncol(df)], function(x) x/df$Overall)

#remove duplicates rows
df <- df[!duplicated(df), ]

d <- dist(df[, 5:ncol(df)])
d <- as.matrix(d)

#convert euclidean distance to similarity score
d <- 1/(d+1)

#remove similarity between the same players
d <- d - diag(1, nrow(d))

d <- readRDS("similarity_matrix_relative.rds")

playersList <- df %>% select(Name:Club)

#I assume that user can select either club or nationality, not both
get_similar_players <- function(playersList, simMatrix, player, club, 
                                nationality, n = 5){
  #limit similarity matrix if given nationality or club
  if (missing(club) & !missing(nationality)){
    sel <- which(playersList$Nationality == nationality)
  } else if (!missing(club) & missing(nationality)){
    sel <- which(playersList$Club == club)
  } else if (missing(club) & missing(nationality)){
    sel <- 1:nrow(playersList)
  }
  
  player_pos <- which(playersList$Name == player)
  
  #handle cases where there are players with the same name (e.g L. Suárez)
  if(length(player_pos) > 1) player_pos <- player_pos[1]
  
  simMatrix[player_pos, sel] %>% 
    sort(decreasing = TRUE) %>% 
    head(n) -> sim_scores
  
  sim_scores %>% 
    names %>%
    as.numeric %>% 
    {playersList$Name[.]} -> players
  
  names(sim_scores) <- players
  
  return(sim_scores)
}

lapply(df$Name[1:10], function(x) get_similar_players(playersList, d, x, n = 5))


lapply(df$Name[1:10], function(x) get_similar_players(playersList, d, x, 
                                                      nationality = "Poland", 
                                                      n = 4))

lapply(df$Name[1:10], function(x) get_similar_players(playersList, d, x, 
                                                      club = "Crystal Palace", 
                                                      n = 4))

med_sim <- apply(d, 1, median)
hist(med_sim, breaks = 500)

q75 <- apply(d, 1, quantile, .75)
hist(q75, breaks = 500)

q90 <- apply(d, 1, quantile, .90)
hist(q90, breaks = 500)


qntls <- c(.5, .6, .7, .8, .9, .95, .99)
qntl_df <- sapply(qntls, function(x) apply(d, 1, quantile, x))
qntl_df <- as.data.frame(qntl_df)
colnames(qntl_df) <- c("q50", "q60", "q70", "q80", "q90", "q95", "q99")

qntl_df <- gather(qntl_df)

ggplot(qntl_df, aes(value))+ 
  geom_density()+
  facet_grid(key~.)
