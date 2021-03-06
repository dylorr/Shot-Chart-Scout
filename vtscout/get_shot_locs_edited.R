get_shot_locs_edited <- function(game_ids) {
  source("helpers.R", local = TRUE)
  if(any(is.na(game_ids))) {
    error("game_ids missing with no default")
  }
  
  
  #adding-------
  
  get_dates = function(x){
    substring(x,nchar(x)-10+1)
  }
  
  #sched$date <- as.Date(
   # paste0(as.character(sched$date)), 
    #format = "%Y-%m-%d")
  
  dates1 <- as.Date(unname(sapply(opps(),get_dates)))
  game_ids <-sched$game_id[sched$date %in% dates1]
  
  #-------
  
  n <- length(game_ids)
  showNotification("Calculation in progress. Please allow for up to 60 seconds. Don't click run again", duration = 10, closeButton = TRUE)
  
  #add new condition-----

  #add new condition-----
  
  for(i in 1:n) {
    message(paste("Getting Shots for Game", i, "of", n))
    withProgress(message = 'Please wait...', detail = paste('Getting shots for game',i,'of',n), value = 0, {
    incProgress(i/n)  
    
    url = paste0('http://www.espn.com/mens-college-basketball/playbyplay?gameId=', game_ids[i])
    date <- get_date(game_ids[i])
    
    away_team_name <-
      stringr::str_replace_all(xml2::read_html(url) %>%
                                 rvest::html_nodes(".away h3") %>%
                                 rvest::html_text(), "[\r\n\t]" , "")
    
    ## if not equal to 1, then print this
    if(length(away_team_name) == 0){
      message("No shot location data available for this game.")
    }else{
      away_shot_text <- xml2::read_html(url) %>%
        rvest::html_nodes(".away-team li") %>%
        rvest::html_text()
      
      ## Style, get shot location data from here
      away_shot_style <- xml2::read_html(url) %>%
        rvest::html_nodes(".away-team li") %>%
        xml2::xml_attr("style")
      away_color  <- gsub("^.*border-color:\\s*|\\s*;.*$", "", away_shot_style[1])
      
      ### home text
      home_team_name <- stringr::str_replace_all(xml2::read_html(url) %>% rvest::html_nodes(".home h3") %>% rvest::html_text(), "[\r\n\t]" , "")
      home_shot_text <- xml2::read_html(url) %>% rvest::html_nodes(".home-team li") %>% rvest::html_text()
      
      ## Style, get shot location data from here
      home_shot_style <- xml2::read_html(url) %>% rvest::html_nodes(".home-team li") %>% xml2::xml_attr("style")
      home_color <- gsub("^.*border-color:\\s*|\\s*;.*$", "", home_shot_style[1])
      
      
      away_df <- data.frame(
        team_name = away_team_name,
        shot_text = away_shot_text,
        shot_style = away_shot_style,
        color = away_color,
        stringsAsFactors = F
      )
      
      home_df <- data.frame(
        team_name = home_team_name,
        shot_text = home_shot_text,
        shot_style = home_shot_style,
        color = home_color,
        stringsAsFactors = F
      )
      
      total_df = rbind(away_df, home_df)
      
      total_df <-total_df %>%
        mutate(
          "date" = date,
          "outcome" = ifelse(grepl("made", shot_text), "made", "missed"),
          "shooter" = stripwhite(gsub("made.*", "", shot_text)),
          "shooter" = stripwhite(gsub("missed.*", "", shooter)),
          "assisted" = stripwhite(gsub(".{1}$", "", gsub(".*Assisted by", "", shot_text))),
          "assisted" = stripwhite(ifelse(grepl("made", assisted) |
                                           grepl("missed", assisted), NA, assisted)),
          "three_pt" = grepl("Three Point", shot_text),
          "x" = as.numeric(gsub('^.*top:\\s*|\\s*%;.*$', '', total_df$shot_style)) * 0.5,
          "y" = as.numeric(gsub('^.*left:\\s*|\\s*%;top.*$', '', total_df$shot_style)) * .94
        ) %>% select(-shot_style)
      
      total_df$game_id <- game_ids[i]
      
      if(!exists("total_df_all")) {
        total_df_all <- total_df
      }else{
        total_df_all <- rbind(total_df_all, total_df)
      }
    }
    }
    )}
  
  if(!exists("total_df_all")) {
    return(NULL)
  }
  return(total_df_all)
  }
