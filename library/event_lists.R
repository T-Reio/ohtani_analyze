ev <- c(
  "batter_interference",          "catcher_interf",              
  "caught_stealing_2b",           "caught_stealing_3b",          
  "caught_stealing_home",         "double",                      
  "double_play",                  "field_error",                 
  "field_out",                    "fielders_choice",             
  "fielders_choice_out",          "force_out",                   
  "grounded_into_double_play",    "hit_by_pitch",                
  "home_run",                     "other_out",                   
  "pickoff_1b",                   "pickoff_2b",                  
  "pickoff_3b",                   "pickoff_caught_stealing_2b",  
  "pickoff_caught_stealing_3b",   "pickoff_caught_stealing_home",
  "run",                          "sac_bunt",                    
  "sac_bunt_double_play",         "sac_fly",                     
  "sac_fly_double_play",          "single",                      
  "strikeout",                    "strikeout_double_play",       
  "triple",                       "triple_play",                 
  "walk",                         'interf_def', #defence-interference 
  NA 
)

PAresult <- ev[c(1, 2, 6:16, 24:34)]

AtBat <- PAresult[c(3:10, 12, 13, 18:22)]

BABIP_den <- PAresult[c(3:10, 13, 18, 21, 22)]

baseHit <- AtBat[c(1, 9, 11, 14)]

des <- c(
  "ball",                    "blocked_ball",            "bunt_foul_tip",          
  "called_strike",           "foul",                    "foul_bunt",              
  "foul_pitchout",           "foul_tip",                "hit_by_pitch",           
  "hit_into_play",           "hit_into_play_no_out",    "hit_into_play_score",    
  "missed_bunt",             "pitchout",                "swinging_pitchout",      
  "swinging_strike",         "swinging_strike_blocked"
)

strike <- des[c(3:8, 10:13, 15:17)]
calledstr <- des[c(4)]
swing <- des[c(3, 5:8, 10:13, 15:17)]
swst <- swing[c(8:11)]
foul <- swing[c(1, 2, 3)]
contact <- swing[c(1:8)]