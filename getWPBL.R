getWPBL <- function(type, team, schedSplit = NULL, stringsAsFactors = FALSE){
    type <- match.arg(type, c("game", "play", "roster", "schedule"))

    if(type == "play" && missing(team)) {
        stop("argument 'team' must be supplied when 'type = \"play\"")
    }
path <- switch(type,
                   "schedule" = "./schedules/2026schedule.csv",
                   "game" = "./gamelogs/gl2026.txt",
                   "roster" = sprintf("./rosters/%s2026.ROS",team))

if (type == "schedule") {

        zcon <- path
        unparsed_file <- readLines(zcon)

        original_cols <- unlist(strsplit(unparsed_file[[1]], ","))

        # Read the unparsed CSV and use the correct column names
        out <- read.csv(
            text = paste(unparsed_file, collapse = "\n"),
            header = FALSE, skip = 1L,
            col.names = get_column_names(original_cols, type = "schedule"),
            stringsAsFactors = stringsAsFactors
        )

        out <- out[!is.na(out$GameNo), ] # Filter out the last line, which is often parsed incorrectly
        if(is.character(schedSplit)) {
            schedSplit <- match.arg(schedSplit, c("Date", "HmTeam", "TimeOfDay"))
            out <- split(out, out[[schedSplit]])
        }
        return(out)
    }

    if (type == "game"){
        out <- path
        return(out)
    }
    if (type == "roster"){
        out <- read.csv(file = path, header=FALSE,col.names=retrosheetFields$roster)
        return(out)
    }
}