# handling old years

split_team_name = strsplit('New Jersey Tech', " ")
if (length(split_team_name[[1]]) > 1){
  words = (split_team_name[[1]])

  build_name = ''
  for (i in 1:(length(words))){
    build_name = paste0(build_name,words[i], '_')
  }
  build_name = substr(build_name,1,nchar(build_name)-1)
}

  else {
  build_name = 
  }

testtt = strsplit('Virginia', " ")

testtt[[1]]


 >1 
range(length(words))

words[1]
 = 

wtf[1][1]









#if (specify_year() == "2018-19"){
specify_year = "2018-19"

if (specify_year == "2018-19"){
    
  
  #initial_name = specify_team()
  initial_name = 'Virginia Tech'
  
  
  split_team_name = strsplit(initial_name, " ")
  if (length(split_team_name[[1]]) > 1){
    words = (split_team_name[[1]])
    
    build_name = ''
    for (i in 1:(length(words))){
      build_name = paste0(build_name,words[i], '_')
    }
    build_name = substr(build_name,1,nchar(build_name)-1)
  }
  
  else {
    build_name = split_team_name[[1]]
  }
  
  #sched_loc = paste0('2018-19/schedules/', build_name, '_schedule.csv')
  sched_loc = paste0('2018-19/schedules/', build_name, '_schedule.csv')
  
  #sched <- read.csv(paste0('2018-19/schedules/', build_name, '_schedule.csv'))
  sched <<- read.csv(paste0(sched_loc))
  items <- as.character(paste(sched$opponent, sched$date))
  updateSelectInput(session, "dropdown", choices = c("",items), selected = if(input$all) c("",items))
}

dates1 <- as.Date(unname(sapply(opps(),get_dates)))
game_ids <-sched$game_id[sched$date %in% dates1]

else {
  
  sched<<-get_schedule(specify_team(),specify_year())
  items <- as.character(paste(sched$opponent, sched$date))
  updateSelectInput(session, "dropdown", choices = c("",items), selected = if(input$all) c("",items))
}
