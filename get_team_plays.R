get_team_plays <- function(team){
      P <- getWPBL("play", 2013, team)
      get_plays <- function(j) data.frame(Game=P[[j]]$id[1], P[[j]]$play)
      do.call("rbind", lapply(1:length(P), get_plays))
    }