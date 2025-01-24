# Kill Shot Example -----------------------------------------------------------

schedule = bigballR::get_team_schedule(team.name="Purdue",season="2024-25")
pbp = bigballR::get_play_by_play(schedule$Game_ID)

  # Filter for only made baskets
  
  pbp = pbp %>% 
    dplyr::filter(Shot_Value %in% c(1,2,3) & Event_Result=="made")

  # Cumsum for Scoring Runs
  
  pbp = pbp %>% 
    dplyr::group_by(ID) %>% 
    dplyr::arrange(Game_Seconds) %>% 
    dplyr::mutate(run = cumsum(Event_Team != lag(Event_Team, default = first(Event_Team))) + 1) %>% 
      dplyr::ungroup()
  
  # Calculate scoring runs
  
  killshots = pbp %>% 
    dplyr::group_by(ID,run) %>% 
    dplyr::summarise(run_team = first(Event_Team),
              def_team = if_else(run_team==first(Home),first(Away),first(Home)),
              run_length = sum(Shot_Value),
              start_score = if_else(run_team==first(Home),paste0(first(Home_Score)-first(Shot_Value),"-",first(Away_Score)),
                                                                 paste0(first(Away_Score)-first(Shot_Value),"-",first(Home_Score))),
              end_score = if_else(run_team==first(Home),paste0(last(Home_Score),"-",last(Away_Score)),
                                  paste0(last(Away_Score),"-",last(Home_Score))),
              run_start = paste0(first(Half_Status),"H-",first(Time)),
              run_end = paste0(last(Half_Status),"H-",last(Time))) %>% 
    dplyr::arrange(desc(run_length)) %>% 
    dplyr::select(-run) %>% 
    dplyr::ungroup()
  
  # Results will give you all runs of any length - filter for run length as needed (such as run_length>=10)
