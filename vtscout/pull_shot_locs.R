
library(plyr)
library(png)
source('plotcourt.R', local=TRUE)
library(cowplot)
library(magick)
library(grid)
library(ggplot2)
load("ncaa_colors.rda")   

library(aws.s3)

#access S3 bucket with hist. data in it
s3BucketName <- "cbb-data"
Sys.setenv("AWS_ACCESS_KEY_ID" = "KEY",
           "AWS_SECRET_ACCESS_KEY" = "KEY",
           "AWS_DEFAULT_REGION" = "us-east-2")

#get_bucket(bucket = s3BucketName, check_region = FALSE, url_style = 'virtual')

#pulls s3 file into df
#test2 = s3read_using(read.csv, object ='2018-19/pbp_logs/2018-11-28/401083088.csv', bucket = s3BucketName)

#testtt = head_object('2018-19/pbp_logs/2018-11-28/40108308.csv', bucket = s3BucketName)


#s3read_using()
pull_shot_locs <- function(year,player,team) {

  
#non-s3 implementation ------  
#initialize df list
data_list <- list()
m <- nrow(pbp_loc)
for(i in 1:m) {
    game_loc = paste0(year,'/pbp_logs/', pbp_loc$date[i], '/', pbp_loc$game_id[i],'.csv')
    if (file.exists(game_loc)) {
      game_df <- read.csv(game_loc)
      shot_data_test <- filter(game_df, shooter == player)
      #standardize column selection, some games have added features like n.w.p causing rbind error
      shot_data_test <- subset(shot_data_test, select=game_id:possession_after)
      
      if (sum(is.na(shot_data_test$shot_x)) < 1){
        data_list[[i]] <- shot_data_test
        #gam <<- data_list
      }
  
    }
}

#s3 implementation ------  
data_list <- list()
m <- nrow(pbp_loc)
for(i in 1:m) {
  game_loc = paste0(year,'/pbp_logs/', pbp_loc$date[i], '/', pbp_loc$game_id[i],'.csv')
  if (head_object(game_loc, bucket = s3BucketName)) {
    game_df <- s3read_using(read.csv, object =game_loc, bucket = s3BucketName)
    shot_data_test <- filter(game_df, shooter == player)
    #standardize column selection, some games have added features like n.w.p causing rbind error
    shot_data_test <- subset(shot_data_test, select=game_id:possession_after)
    
    if (sum(is.na(shot_data_test$shot_x)) < 1){
      data_list[[i]] <- shot_data_test
      #gam <<- data_list
    }
    
  }
}

#remove nulls from game list
filtered_data_list <- Filter(Negate(is.null), data_list)
#remove empty / NaN length games from list
keepers <- filtered_data_list[sapply(filtered_data_list, function(x) dim(x)[1]) > 0]

#merge all game_df into one df
big_data <<- do.call(rbind, keepers)

#error message for missing shot data on query
if (length(big_data) == 0) {
  showModal(modalDialog(title="No valid shot information for selected games. Please refresh the page and try again.","Check the ESPN game page to ensure shot locations are availible. The selected player could have possibly not attempted any shots, resulting in this error.", easyClose = TRUE, fade=TRUE))
}

shot_data <- filter(big_data, shooter == player)

#shot_data <<- shot_data %>% rename(x = shot_x) 
#shot_data <<- shot_data %>% rename(y = shot_y) 
#shot_data <<- rename(shot_data, c("shot_x"="x", "shot_y"="y"))

#test = nrow(game_df[rowSums(is.na(game_df))==0,] )

#convert to one sided data
shot_data[shot_data$shot_y > 47, "shot_x"] <- 50 - shot_data[shot_data$shot_y > 47, "shot_x"]
shot_data[shot_data$shot_y > 47, "shot_y"] <- 94 - shot_data[shot_data$shot_y > 47, "shot_y"]

#calculate additional information from raw shot data. this includes: 
# estimated distance in feet, general distance category, shot zone (location) and shot zone (specific)
shots <<- mutate(shot_data,
                 shot_distance = sqrt((shot_data$shot_x - 25) ^ 2 + ((shot_data$shot_y- 4.177778) ^ 2)),
                 shot_angle = acos((shot_data$shot_x -25)/ shot_distance) * 180 / pi,
                 shot_zone_area = case_when(
                   grepl('Free', shot_data$description, fixed=TRUE) == TRUE ~ "Free Throw",
                   shot_data$shot_x>=44  ~ "Right Side(R)",
                   shot_data$shot_x>=31 & shot_data$shot_x<44  ~ "Right Side Center(RC)",
                   shot_data$shot_x>6 & shot_data$shot_x < 19 ~ "Left Side Center(LC)",
                   shot_data$shot_x< 6 ~ "Left Side(L)",
                   TRUE ~"Center(C)",
                 ),
                 shot_zone_range = case_when(
                   shot_distance < 8 ~ "Less Than 8 ft.",
                   shot_distance < 16 ~ "8-16 ft.",
                   shot_distance < 24 ~ "16-24 ft.",
                   TRUE ~ "24+ ft."
                 ),
                 shot_zone_basic = case_when(
                   grepl('Free', shot_data$description, fixed=TRUE) == TRUE ~ "Free Throw",
                   shot_distance > 40 ~ "Backcourt",
                   shot_distance < 4 ~ "Restricted Area",
                   shot_data$shot_x > 19 & shot_data$shot_x < 31 & shot_data$shot_y < 19 ~ "In The Paint (Non-RA)",
                   shot_data$three_pt == TRUE & shot_data$shot_x>=44 & shot_data$shot_y<=9.20~ "Right Corner 3",
                   shot_data$three_pt == TRUE & shot_data$shot_x<=6 & shot_data$shot_y<=9.20 ~ "Left Corner 3",
                   shot_data$three_pt == TRUE ~ "Above the Break 3",
                   TRUE ~ "Mid-Range"
                 )
)



made <<- filter(shots, shot_outcome == 'made')
missed <<- filter(shots, shot_outcome == 'missed')



made_dist_1 =  as.data.frame(table(made$shot_zone_basic))
missed_dist_1 = as.data.frame(table(missed$shot_zone_basic))

made_dist_loc_1 =  as.data.frame(table(made$shot_zone_area))
missed_dist_loc_1 =  as.data.frame(table(missed$shot_zone_area))

made_ft <<- nrow(subset(shots, shot_outcome == 'made' & shot_zone_basic == 'Free Throw'))
missed_ft = nrow(subset(shots, shot_outcome == 'missed' & shot_zone_basic == 'Free Throw'))
total_ft <<- made_ft + missed_ft
ft_shooting_percentage <<- round(made_ft/total_ft,2)


made_3pt_ab = nrow(subset(shots, shot_outcome == 'made' &  shot_zone_basic == 'Above the Break 3'))
missed_3pt_ab = nrow(subset(shots, shot_outcome == 'missed' & shot_zone_basic == 'Above the Break 3'))

made_3pt_rc = nrow(subset(shots, shot_outcome == 'made' &  shot_zone_basic == 'Right Corner 3'))
missed_3pt_rc = nrow(subset(shots, shot_outcome == 'missed' & shot_zone_basic == 'Right Corner 3'))

made_3pt_lc = nrow(subset(shots, shot_outcome == 'made' &  shot_zone_basic == 'Left Corner 3'))
missed_3pt_lc = nrow(subset(shots, shot_outcome == 'missed' & shot_zone_basic == 'Left Corner 3'))

total_made_3pt <<- made_3pt_ab + made_3pt_rc + made_3pt_lc
total_3pt <<- made_3pt_ab + made_3pt_rc + made_3pt_lc + missed_3pt_ab + missed_3pt_rc + missed_3pt_lc
threept_shooting_percentage <<- round((made_3pt_ab + made_3pt_rc + made_3pt_lc)/total_3pt,2)

made_count = nrow(made)
missed_count = nrow(missed)

made_fg <<- made_count - made_ft
missed_fg <<- missed_count - missed_ft

total_shots <<- made_fg + missed_fg
shooting_percentage <<- round(made_fg/total_shots,2)

ts_percentage <<- round((((made_fg-total_made_3pt)*2) + (total_made_3pt*3) + (made_ft)) / (2* ((total_shots) + (0.44*total_ft))), 2)

#summary by location
shot_area_summary <<- merge(made_dist_loc_1,missed_dist_loc_1, by = "Var1", all=TRUE)
shot_area_summary[is.na(shot_area_summary)] <<- 0
shot_area_summary["Shooting Percentage"] <<- round(shot_area_summary$Freq.x   / (shot_area_summary$Freq.x + shot_area_summary$Freq.y), 2)
shot_area_summary["Frequency"] <<- round((shot_area_summary$Freq.x + shot_area_summary$Freq.y) / (sum(shot_area_summary$Freq.x) + sum(shot_area_summary$Freq.y)),2)

colnames(shot_area_summary) <<- c("Location", "Made", "Missed", "Shooting Percentage", "Frequency")

#summary by zone
shot_zone_summary <<- merge(made_dist_1,missed_dist_1, by = "Var1", all=TRUE)
shot_zone_summary[is.na(shot_zone_summary)] <<- 0
shot_zone_summary["Shooting Percentage"] <<- round(shot_zone_summary$Freq.x / (shot_zone_summary$Freq.x + shot_zone_summary$Freq.y),2)
shot_zone_summary["Frequency"] <<- round((shot_zone_summary$Freq.x + shot_zone_summary$Freq.y) / (sum(shot_zone_summary$Freq.x) + sum(shot_zone_summary$Freq.y)),2)

#analysis NEW
#renamed column with underscored to be able to reference
#--------
store_max <<- shot_area_summary[which.max(shot_area_summary$Frequency),][1]

if (apply(store_max, 1, function(x) any(x %in% "Center(C)"))) {
  shot_location_analysis <<- "Center"
} 

else if (apply(store_max, 1, function(x) any(x %in% "Right Side Center(RC)"))) {
  shot_location_analysis <<- "Right Side Center"
} 

else if (apply(store_max, 1, function(x) any(x %in% "Left Side Center(LC)"))) {
  shot_location_analysis <<- "Left Side Center"
} 

else if (apply(store_max, 1, function(x) any(x %in% "Left Side(L)"))) {
  shot_location_analysis <<- "Left Side"
} 

else if (apply(store_max, 1, function(x) any(x %in% "Right Side(R)"))) {
  shot_location_analysis <<- "Right Side"
}


store_max_2 <<- shot_zone_summary[which.max(shot_zone_summary$Frequency),][1]

if (apply(store_max_2, 1, function(x) any(x %in% "Backcourt"))) {
  shot_location_analysis_2 <<- "Backcourt"
} 

else if (apply(store_max_2, 1, function(x) any(x %in% "Restricted Area"))) {
  shot_location_analysis_2 <<- "Restricted Area"
} 

else if (apply(store_max_2, 1, function(x) any(x %in% "In The Paint (Non-RA)"))) {
  shot_location_analysis_2 <<- "In The Paint (Non-RA)"
} 

else if (apply(store_max_2, 1, function(x) any(x %in% "Right Corner 3"))) {
  shot_location_analysis_2 <<- "Right Corner 3"
} 

else if (apply(store_max_2, 1, function(x) any(x %in% "Left Corner 3"))) {
  shot_location_analysis_2 <<- "Left Corner 3"
}

else if (apply(store_max_2, 1, function(x) any(x %in% "Above the Break 3"))) {
  shot_location_analysis_2 <<- "Above the Break 3"
}

else if (apply(store_max_2, 1, function(x) any(x %in% "Mid-Range"))) {
  shot_location_analysis_2 <<- "Mid-Range"
}

colnames(shot_zone_summary) <<- c("Zone", "Made", "Missed", "Shooting Percentage", "Frequency")


#overall_summary <<- cbind.data.frame(made_fg, missed_fg, total_shots, shooting_percentage)
#colnames(overall_summary) <<- c("Made", "Missed", "Total Shots", "Shooting Percentage")

#games = lapply(shots, function(x) length(table(x)))

#test = table(tomas$three_pt)
#test

#total = test[1] + test[2]
#total

#sum(shot_zone_summary$Freq.y) 
#three_pt_perc <- count(tomas,)

color_shot<- c("green","red")

#team specific formatting
team_color <- (subset(ncaa_colors, espn_name == team))$primary_color
team_logo_link = (subset(ncaa_colors, espn_name == team))$logo_url
team_logo_file <- download.file(team_logo_link, destfile = 'team_logo.png', mode = 'wb')
team_logo <- readPNG('team_logo.png')


#working dark
#end_plot <<- plot_court() + draw_image(team_logo, x = 45, y=42, scale=5) +
#geom_point(data = tomas, aes(x = x, y=y, colour = outcome), alpha = 0.8) + scale_colour_manual(values=color_shot) + ggtitle(paste(specify_player,' (',specify_year,') ', "tracked shots", ' (', total_shots, ')', sep="")) + theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold', color = team_color), plot.background = element_rect(fill = "#151515"), legend.key = element_rect(fill = "black"))

#working normal
end_plot <<- plot_court() + draw_image(team_logo, x = 45, y=42, scale=5) +
  geom_point(data = shots, aes(x = shot_x, y=shot_y, colour = shot_outcome), alpha = 0.8) + scale_colour_manual(values=color_shot) + ggtitle(paste(player,' (',year,') ', "tracked shots", ' (', total_shots, ')', sep="")) + theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold', color = team_color))


return(list(shot_zone_summary,shot_area_summary,end_plot, shooting_percentage))
}




