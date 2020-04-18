bball <<- function (specify_team, specify_player, specify_year){
  source('plotcourt.R')
  source('get_shot_locs_edited.R')
  
  
  
  library(ncaahoopR)
  vector = get_game_ids(team = specify_team, season = specify_year)
  team_shots = get_shot_locs_edited(vector)
  attach(team_shots)
  
  opponents = filter(unique(team_shots[,cbind(1,4)]), team_name != specify_team )
  
  shot_data = team_shots
  
  
  #convert to one sided data
  shot_data[shot_data$y > 47, "x"] <- 50 - shot_data[shot_data$y > 47, "x"]
  shot_data[shot_data$y > 47, "y"] <- 94 - shot_data[shot_data$y > 47, "y"]
  
  #calculate additional information from raw shot data. this includes: 
  # estimated distance in feet, general distance category, shot zone (location) and shot zone (specific)
  shots = mutate(shot_data,
                 shot_distance = sqrt((shot_data$x - 25) ^ 2 + ((shot_data$y- 4.177778) ^ 2)),
                 shot_angle = acos((shot_data$x -25)/ shot_distance) * 180 / pi,
                 shot_zone_area = case_when(
                   shot_data$x>=44  ~ "Right Side(R)",
                   shot_data$x>=31 & shot_data$x<44  ~ "Right Side Center(RC)",
                   shot_data$x>6 & shot_data$x < 19 ~ "Left Side Center(LC)",
                   shot_data$x<= 6 ~ "Left Side(L)",
                   TRUE ~"Center(C)",
                 ),
                 shot_zone_range = case_when(
                   shot_distance < 8 ~ "Less Than 8 ft.",
                   shot_distance < 16 ~ "8-16 ft.",
                   shot_distance < 24 ~ "16-24 ft.",
                   TRUE ~ "24+ ft."
                 ),
                 shot_zone_basic = case_when(
                   shot_distance > 40 ~ "Backcourt",
                   shot_distance < 4 ~ "Restricted Area",
                   shot_data$x > 19 & shot_data$x < 31 & shot_data$y < 19 ~ "In The Paint (Non-RA)",
                   shot_data$three_pt == TRUE & shot_data$x>=44 & shot_data$y<=9.20~ "Right Corner 3",
                   shot_data$three_pt == TRUE & shot_data$x<=6 & shot_data$y<=9.20 ~ "Left Corner 3",
                   shot_data$three_pt == TRUE ~ "Above the Break 3",
                   TRUE ~ "Mid-Range"
                 )
                 
                 
                 
                 
                 
  )
  
  
  
  #tomas = filter(shots, shooter == 'Tomas Woldetensae', three_pt == TRUE)
  tomas = filter(shots, shooter == specify_player)
  
  
  made = filter(tomas, outcome == 'made')
  missed = filter(tomas, outcome == 'missed')
  
  made_dist =  as.data.frame(table(made$shot_zone_basic))
  missed_dist = as.data.frame(table(missed$shot_zone_basic))
  
  made_dist_loc =  as.data.frame(table(made$shot_zone_area))
  missed_dist_loc =  as.data.frame(table(missed$shot_zone_area))
  
  
  total_shots = nrow(made) + nrow(missed)
  shooting_percentage = round(nrow(made) / total_shots,2)
  
  #summary by location
  shot_area_summary <<- merge(made_dist_loc,missed_dist_loc, by = "Var1")
  shot_area_summary["Shooting Percentage"] <<- round(shot_area_summary$Freq.x / (shot_area_summary$Freq.x + shot_area_summary$Freq.y), 2)
  shot_area_summary["Percentage of Shots"] <<- round((shot_area_summary$Freq.x + shot_area_summary$Freq.y) / (sum(shot_area_summary$Freq.x) + sum(shot_area_summary$Freq.y)),2)
  
  colnames(shot_area_summary) <<- c("Location", "Made", "Missed", "Shooting Percentage", "Percentage of Shots")
  
  
  shot_zone_summary <<- merge(made_dist,missed_dist, by = "Var1")
  shot_zone_summary["Shooting Percentage"] <<- round(shot_zone_summary$Freq.x / (shot_zone_summary$Freq.x + shot_zone_summary$Freq.y),2)
  shot_zone_summary["Percentage of Shots"] <<- round((shot_zone_summary$Freq.x + shot_zone_summary$Freq.y) / (sum(shot_zone_summary$Freq.x) + sum(shot_zone_summary$Freq.y)),2)
  colnames(shot_zone_summary) <<- c("Zone", "Made", "Missed", "Shooting Percentage", "Percentage of Shots")
  
  overall_summary <<- cbind.data.frame(nrow(missed), nrow(made), total_shots, shooting_percentage)
  
  games = lapply(tomas, function(x) length(table(x)))
  
  test = table(tomas$three_pt)
  test
  
  total = test[1] + test[2]
  total
  
  sum(shot_zone_summary$Freq.y) 
  three_pt_perc <- count(tomas,)
  
  color_shot<- c("green","red")
  
  
  end_plot <<- plot_court() + geom_point(data = tomas, aes(x = x, y=y, colour = outcome), alpha = 0.8) + scale_colour_manual(values=color_shot) + ggtitle(paste(specify_player,' (',specify_year,') ', "tracked shots", ' (', total_shots, ')', sep="")) + theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'))
  
  #colnames(overall_summary) <-c("Missed","Made", "Percentage")
  #colnames(shot_area_summary) <- c("Location", "Made", "Missed", "Shooting Percentage", "Percentage of Shots")
  #colnames(shot_zone_summary) <- c("Zone", "Made", "Missed", "Shooting Percentage", "Percentage of Shots")
  
  
  
  return(list(overall_summary,shot_zone_summary,shot_area_summary,end_plot))
}
