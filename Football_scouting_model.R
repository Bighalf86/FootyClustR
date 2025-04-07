### Football scouting with k-means clustering ###


library(cluster)
library(dplyr)
library(readxl)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(worldfootballR)
library(tidyverse)

#########################################################################################
# Importing the file, which contains player possession KPIs data extracted from fbref.com 
#########################################################################################

file_path <- "C:/Users/*****/Desktop/Sport Analytics/Roma mock scouting project/Roma Transfermarket Scouting Database.xlsx"
sheet_name <- "All leagues combined"

Players_data <- read_excel(file_path, sheet = sheet_name)

#########################
# Filtering out NA values
#########################

Players_data <- na.omit(Players_data)

#######################################################################
# Selecting KPIs of interest for wings - Following Hughes et al., 2012.
# For this scouting analysis, I've only focused on possession KPIs data
#######################################################################

wings_KPIs <- Players_data %>%
  select(`Att 3rd`, Att, `Succ%`, PrgC, `45352`, PrgDist, PrgR)

#################################
# Scaling the data for clustering
#################################

wings_KPIs_scaled <- scale(wings_KPIs)

#####################
# For reproducibility
#####################

set.seed(123)     

######################################################################################################
# The number of clusters (I chose 7 based on players' positions from Hughes et al., 2012 - see figure)
######################################################################################################

k <- 7     

#######################
# Performing clustering
#######################

kmeans_result <- kmeans(wings_KPIs_scaled, centers = k, nstart = 25)     

############################################
# Adding cluster values to the original file
############################################

Players_data$Cluster <- kmeans_result$cluster    

player_names <- gsub("[^[:alnum:] ]", "", Players_data$Player)

########################################################
# I visualized the clusters using the factoextra package
########################################################

fviz_cluster(kmeans_result, data = wings_KPIs_scaled, geom = "point",
             ellipse.type = "norm", main = "K-Means Clustering of Players - Wings KPIs") +
  geom_text_repel(aes(label = ifelse(Players_data$Player == "Jeremie Frimpong", Players_data$Player, "")), size = 3, vjust = 1.5, hjust = 0.5, max.overlaps = 2000)

##########################################################################
# Following the predefined business requirements, I have shortlisted
# players belonging to the same cluster as Frimpong's, who were born after 
# the year 2000, and had played at least ten 90 minutes
##########################################################################

Clustered_Wings <- data.frame(filter(Players_data, Cluster == "1" & Born >= 2000 & `90s` > 10))

###########################################################################################
# To extract players' transfer market values, I used the worldfootballR package by JaseZiv.
# This line of code may take up to 5-10 minutes to run
###########################################################################################

All_players_value <- data.frame(tm_player_market_values(country_name = c("Italy", "Germany", "Spain", 
                                                        "France", "England", "Belgium", "Mexico", 
                                                        "United States", "Netherlands", "Portugal", "Argentina", 
                                                        "Brazil"), 
                        start_year = 2023))

#################################################################################
# After extracting and creating the players' transfer market values data frame, 
# I have merged that into the "Clustered wings" one. I have matched the records 
# by the player names. Discrepancies in name spellings created na values, 
# so I adjusted the names for consistency, and performed the merge a second time.
#################################################################################

Clustered_Wings_with_market_value <- merge(Clustered_Wings, 
                                           All_players_value[, c("player_name", "player_market_value_euro")], 
                                           by.x = "Player", 
                                           by.y = "player_name", 
                                           all.x = TRUE)

na_matches <- Clustered_Wings_with_market_value[is.na(Clustered_Wings_with_market_value$player_market_value_euro), ]

Adjusted_names_for_TM_matching <- c("Ângelo Borges" = "Ângelo", 
                                    "Gabriel Chaves" = "Gabriel Pec", 
                                    "Jamie Bynoe-Gittens" = "Jamie Gittens", 
                                    "João Mário Lopes" = "João Mário", 
                                    "Lee Kang-in" = "Kang-in Lee", 
                                    "Matìas Soulé" = "Matías Soulé", 
                                    "Maximiliano Araújo" = "Maxi Araújo", 
                                    "Mohamed El Amine Amoura" = "Mohamed Amoura", 
                                    "Mohammed Daramy" = "Mohamed Daramy", 
                                    "Mykhailo Mudryk" = "Mykhaylo Mudryk", 
                                    "Roberto Navarro" = "Robert Navarro", 
                                    "Tidjany Chabrol" = "Tidjany Touré", 
                                    "Vinicius Júnior" = "Vinicius Junior") 

Clustered_Wings$Player <- ifelse(Clustered_Wings$Player %in% names(Adjusted_names_for_TM_matching), 
                                 Adjusted_names_for_TM_matching[Clustered_Wings$Player], 
                                 Clustered_Wings$Player)

Clustered_Wings_with_market_value <- merge(Clustered_Wings, 
                                           All_players_value[, c("player_name", "player_market_value_euro")], 
                                           by.x = "Player", 
                                           by.y = "player_name", 
                                           all.x = TRUE)

############################################################################################################################
# One player still didn't show his market value after these steps, so I added it manually based on transfermarkt.com records.
############################################################################################################################

Clustered_Wings_with_market_value$player_market_value_euro[Clustered_Wings_with_market_value$Player == "Carlos Gómez"] <- "3500000"

#########################################################################################################
# Some players had duplicate rows as their market values varied throughout the selected year.
# I decided to group players based on their names and keep the record with the highest value in the year.
#########################################################################################################

Clustered_Wings_with_market_value <- Clustered_Wings_with_market_value %>% 
  distinct() %>% 
  group_by(Player) %>% 
  slice_max(player_market_value_euro, with_ties = FALSE) %>% 
  ungroup()

Clustered_Wings_with_market_value$player_market_value_euro <- as.integer(Clustered_Wings_with_market_value$player_market_value_euro)

#####################################################################################
# Following the initial business requirements, I filtered out players valued over 8M.
# I kept Frimpong as he is the reference player I want to assess the similarity with
#####################################################################################

Clustered_Wings_with_market_value_below_8M <- Clustered_Wings_with_market_value %>% 
  filter(player_market_value_euro <= 8000000 | Player == "Jeremie Frimpong")

############################################################################################################################################
# When reviewing the results, I noticed a few players whose market value was well above 8M were kept in the list, so I removed them manually.
############################################################################################################################################

Clustered_Wings_with_market_value_below_8M <- Clustered_Wings_with_market_value_below_8M %>%
  filter(!Player %in% c("Antony", 	
                        "João Mário", 
                        "Sávio"))

##########################################################################
# Performing and visualising the second clustering required in my project.
##########################################################################

Shortlisted_Wings_KPIs_only <- Clustered_Wings_with_market_value_below_8M %>%
  select(`Att.3rd`, Att, `Succ.`, PrgC, `X45352`, PrgDist, PrgR)

Shortlisted_wings_KPIs <- Clustered_Wings_with_market_value_below_8M %>%
  select(Player, player_market_value_euro, Squad, Born, `Att.3rd`, Att, `Succ.`, PrgC, `X45352`, PrgDist, PrgR)

Shortlisted_wings_KPIs <- Shortlisted_wings_KPIs %>%
  rowwise() %>%
  mutate(Aggregate_Score = mean(c_across(`Att.3rd`:PrgR), na.rm = TRUE))

print(Shortlisted_wings_KPIs)

################################################################################################
# Before plotting the final clustering, I wanted to add context and visualize the top 10 players
# according to the aggregate score of the selected possession KPIs
################################################################################################


top_10_players <- Shortlisted_wings_KPIs %>%
  arrange(desc(Aggregate_Score)) %>%
  head(10)

top_10_players <- top_10_players %>%
  mutate(highlight = case_when(
    Player == "Jeremie Frimpong" ~ "highlight",
    Player == "Brian Rodríguez" ~ "highlight",
    Player == "Jarne Steuckers" ~ "highlight",
    TRUE ~ "normal"
  ))

###################################################
# I chose a bar chart to visualize players' ranking
###################################################

ggplot(top_10_players, aes(x = reorder(Player, Aggregate_Score), y = Aggregate_Score, fill = highlight)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend
  coord_flip() +  
  labs(title = "Top 10 Shortlisted Players by Aggregate Score of selected possession KPIs",
       x = "Player",
       y = "Aggregate Score") +
  scale_fill_manual(values = c("highlight" = "orange", "normal" = "steelblue")) +  # Assign colors
  theme_minimal()

###########################
# Resuming final clustering
###########################

Shortlisted_wings_KPIs_scaled <- scale(Shortlisted_Wings_KPIs_only)

set.seed(123)

k <- 3

Second_kmeans_result <- kmeans(Shortlisted_wings_KPIs_scaled, centers = k, nstart = 25)

Shortlisted_wings_KPIs$Cluster <- Second_kmeans_result$cluster

Shortlisted_player_names <- Shortlisted_wings_KPIs$Player

fviz_cluster(Second_kmeans_result, data = Shortlisted_wings_KPIs_scaled, geom = "point",
             ellipse.type = "norm", main = "K-Means Clustering of players most similar to Frimpong", 
             repel = TRUE) + 
  geom_text(aes(label = Shortlisted_player_names), size = 3, vjust = 1.5, hjust = 0.5)

#################################################################################################
# The final shortlist of players similar to Frimpong according to possession KPIs only, 
# who were born after the year 2000, have played more than ten 90 minutes in the 2023/2024 season, 
# and at the time of this project had a market value of €8M or below, includes
# Brian Rodriguez from América, Gabriel Pec from LA Galaxy, and Tidjany Touré from Gil Vicente FC.
# Brian Rodriguez was also the player with the highest aggregate score of selected wings KPIs,
# followed by Jarne Steuckers from Sint-Truiden.
#################################################################################################

Frimpong_cluster_players <- Shortlisted_wings_KPIs %>%
  filter(Cluster == 1)

print(Frimpong_cluster_players)

#################################################################################################################
# To complete this project, I wanted to compare the selected players with Frimpong, according to possession KPIs.
# I chose radar charts. The values of the radar charts would be the player percentiles for each KPI, taking into 
# consideration all players shortlisted after the first k-means clustering (532 players). I wanted to introduce 
# players' headshots for greater engagement.
#################################################################################################################

library(fmsb)
library(png)
library(grid)
library(httr)

kpi_labels <- c(
  "Att 3rd" = "Touches in Attacking 1/3",
  "Att" = "Take-ons \n Attempted",
  "Succ%" = "Success.\nTake-ons\n%",
  "PrgC" = "Progressive Carries",
  "45352" = "Carries into Final Third",
  "PrgDist" = "Progress.\nCarrying\nDistance",
  "PrgR" = "Progressive \n Passes Received"
)

Players_data <- Players_data %>%
  mutate(Player = case_when(
    Player == "Gabriel Chaves" ~ "Gabriel Pec",
    Player == "Tidjany Chabrol" ~ "Tidjany Touré",
    TRUE ~ Player
  ))

download_image <- function(url, filename) {
  temp_file <- tempfile(fileext = ".png")  # Create a temporary file
  GET(url, write_disk(temp_file, overwrite = TRUE))  # Download the image
  img <- tryCatch(readPNG(temp_file), error = function(e) NULL)  # Read PNG
  return(img)
}

player_images <- list(
  "Jeremie Frimpong" = "https://images.fotmob.com/image_resources/playerimages/966018.png",
  "Brian Rodríguez" = "https://images.fotmob.com/image_resources/playerimages/941660.png",
  "Jarne Steuckers" = "https://images.fotmob.com/image_resources/playerimages/1224465.png",
  "Tidjany Touré" = "https://images.fotmob.com/image_resources/playerimages/1436985.png",
  "Gabriel Pec" = "https://images.fotmob.com/image_resources/playerimages/1090238.png"
)

plot_radar <- function(player_name) {
  player_data <- selected_players_percentiles %>%
    filter(Player %in% c("Jeremie Frimpong", player_name))
  
  radar_data <- rbind(
    rep(100, length(kpi_cols)),  # Max values
    rep(0, length(kpi_cols)),    # Min values
    player_data %>% select(-Player)  # Player data
  )
  
  radar_data <- as.data.frame(radar_data)
  rownames(radar_data) <- c("Max", "Min", "Jeremie Frimpong", player_name)
  
  par(mar = c(6, 6, 6, 6))
  
##########################
# Plotting the radar chart
##########################
  
  radarchart(
    radar_data,
    axistype = 1,
    pcol = c("red", "blue"),  # Colors for each player
    pfcol = c(rgb(1, 0, 0, 0.3), rgb(0, 0, 1, 0.3)),  # Transparent fill colors
    plwd = 2,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey",
    vlcex = 0.6,
    title = paste("2023/24 Possession KPIs comparison \n Jeremie Frimpong vs", player_name),
    vlabels = kpi_labels
  )
  
#######################  
# Loading player images
#######################
  
  frimpong_img <- download_image(player_images[["Jeremie Frimpong"]], "frimpong.png")
  player_img <- download_image(player_images[[player_name]], paste0(player_name, ".png"))
  
  if (!is.null(frimpong_img) & !is.null(player_img)) {
    grid.raster(frimpong_img, x = 0.2, y = 0.15, width = 0.1, height = 0.1, just = "center")
    grid.raster(player_img, x = 0.8, y = 0.15, width = 0.1, height = 0.1, just = "center")
    
    grid.text("Jeremie Frimpong", x = 0.2, y = 0.07, gp = gpar(col = "red", fontsize = 11, fontface = "bold"))
    grid.text(player_name, x = 0.8, y = 0.07, gp = gpar(col = "blue", fontsize = 11, fontface = "bold"))
  }
}

comparison_players <- setdiff(selected_players, "Jeremie Frimpong")

for (player in comparison_players) {
  dev.new()  # Opens a new plotting window for each comparison
  plot_radar(player)
}
