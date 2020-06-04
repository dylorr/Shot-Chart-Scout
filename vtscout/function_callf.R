bball <<- function (specify_team, specify_player, specify_year, specify_game_ids){
  source('plotcourt.R', local=TRUE)
  source('get_shot_locs_edited.R', local = TRUE)
  
  #source('ncaa_colors.rda', local = TRUE)
  library(ncaahoopR)
  library(png)
  library(cowplot)
  library(magick)
  library(grid)
  library(ggplot2)
  load("ncaa_colors.rda")    
  
  #vector = get_game_ids(team = specify_team, season = specify_year)
  
  
  team_shots <- get_shot_locs_edited(vector)
  attach(team_shots)

  #if (is.na(team_shots)){
   # showModal(modalDialog(title="ERROR: No shot data found for selected game(s). Please refresh and try another query.", easyClose = TRUE, fade=TRUE))
 # }
  opponents = filter(unique(team_shots[,cbind(1,4)]), team_name != specify_team )
  
  shot_data = team_shots
  
  
  #convert to one sided data
  shot_data[shot_data$y > 47, "x"] <- 50 - shot_data[shot_data$y > 47, "x"]
  shot_data[shot_data$y > 47, "y"] <- 94 - shot_data[shot_data$y > 47, "y"]
  
  #calculate additional information from raw shot data. this includes: 
  # estimated distance in feet, general distance category, shot zone (location) and shot zone (specific)
  shots <<- mutate(shot_data,
                 shot_distance = sqrt((shot_data$x - 25) ^ 2 + ((shot_data$y- 4.177778) ^ 2)),
                 shot_angle = acos((shot_data$x -25)/ shot_distance) * 180 / pi,
                 shot_zone_area = case_when(
                   grepl('Free', shot_data$shot_text, fixed=TRUE) == TRUE ~ "Free Throw",
                   shot_data$x>=44  ~ "Right Side(R)",
                   shot_data$x>=31 & shot_data$x<44  ~ "Right Side Center(RC)",
                   shot_data$x>6 & shot_data$x < 19 ~ "Left Side Center(LC)",
                   shot_data$x< 6 ~ "Left Side(L)",
                   TRUE ~"Center(C)",
                 ),
                 shot_zone_range = case_when(
                   shot_distance < 8 ~ "Less Than 8 ft.",
                   shot_distance < 16 ~ "8-16 ft.",
                   shot_distance < 24 ~ "16-24 ft.",
                   TRUE ~ "24+ ft."
                 ),
                 shot_zone_basic = case_when(
                   grepl('Free', shot_data$shot_text, fixed=TRUE) == TRUE ~ "Free Throw",
                   shot_distance > 40 ~ "Backcourt",
                   shot_distance < 4 ~ "Restricted Area",
                   shot_data$x > 19 & shot_data$x < 31 & shot_data$y < 19 ~ "In The Paint (Non-RA)",
                   shot_data$three_pt == TRUE & shot_data$x>=44 & shot_data$y<=9.20~ "Right Corner 3",
                   shot_data$three_pt == TRUE & shot_data$x<=6 & shot_data$y<=9.20 ~ "Left Corner 3",
                   shot_data$three_pt == TRUE ~ "Above the Break 3",
                   TRUE ~ "Mid-Range"
                 )
  )
  
  
  
  tomas <- filter(shots, shooter == specify_player)
  
  #wtf <- filter(shots, outcome != 'made')
  
  tomas_1 <<- filter(shots, shooter == specify_player)
  tomas_2 <<- filter(shots, shooter == specify_player)
  
  made <<- filter(tomas_1, outcome == 'made')
  missed <<- filter(tomas_2, outcome == 'missed')
  

  
  made_dist_1 =  as.data.frame(table(made$shot_zone_basic))
  missed_dist_1 = as.data.frame(table(missed$shot_zone_basic))
  
  made_dist_loc_1 =  as.data.frame(table(made$shot_zone_area))
  missed_dist_loc_1 =  as.data.frame(table(missed$shot_zone_area))
  
  made_ft <<- nrow(subset(tomas, outcome == 'made' & shot_zone_basic == 'Free Throw'))
  missed_ft = nrow(subset(tomas, outcome == 'missed' & shot_zone_basic == 'Free Throw'))
  total_ft <<- made_ft + missed_ft
  ft_shooting_percentage <<- round(made_ft/total_ft,2)
  
  
  made_3pt_ab = nrow(subset(tomas, outcome == 'made' &  shot_zone_basic == 'Above the Break 3'))
  missed_3pt_ab = nrow(subset(tomas, outcome == 'missed' & shot_zone_basic == 'Above the Break 3'))
  
  made_3pt_rc = nrow(subset(tomas, outcome == 'made' &  shot_zone_basic == 'Right Corner 3'))
  missed_3pt_rc = nrow(subset(tomas, outcome == 'missed' & shot_zone_basic == 'Right Corner 3'))
  
  made_3pt_lc = nrow(subset(tomas, outcome == 'made' &  shot_zone_basic == 'Left Corner 3'))
  missed_3pt_lc = nrow(subset(tomas, outcome == 'missed' & shot_zone_basic == 'Left Corner 3'))
  
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
  
  games = lapply(tomas, function(x) length(table(x)))
  
  test = table(tomas$three_pt)
  test
  
  total = test[1] + test[2]
  total
  
  sum(shot_zone_summary$Freq.y) 
  three_pt_perc <- count(tomas,)
  
  color_shot<- c("green","red")
  
  #team specific formatting
  team_color <- (subset(ncaa_colors, espn_name == specify_team))$primary_color
  team_logo_link = (subset(ncaa_colors, espn_name == specify_team))$logo_url
  team_logo_file <- download.file(team_logo_link, destfile = 'team_logo.png', mode = 'wb')
  team_logo <- readPNG('team_logo.png')
  
  
  #working dark
  #end_plot <<- plot_court() + draw_image(team_logo, x = 45, y=42, scale=5) +
    #geom_point(data = tomas, aes(x = x, y=y, colour = outcome), alpha = 0.8) + scale_colour_manual(values=color_shot) + ggtitle(paste(specify_player,' (',specify_year,') ', "tracked shots", ' (', total_shots, ')', sep="")) + theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold', color = team_color), plot.background = element_rect(fill = "#151515"), legend.key = element_rect(fill = "black"))
  
  #working normal
  end_plot <<- plot_court() + draw_image(team_logo, x = 45, y=42, scale=5) +
    geom_point(data = tomas, aes(x = x, y=y, colour = outcome), alpha = 0.8) + scale_colour_manual(values=color_shot) + ggtitle(paste(specify_player,' (',specify_year,') ', "tracked shots", ' (', total_shots, ')', sep="")) + theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold', color = team_color))
  
  
  return(list(shot_zone_summary,shot_area_summary,end_plot, shooting_percentage))
}

