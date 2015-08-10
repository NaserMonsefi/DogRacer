dogfetcher <- function(startdate, enddate,  raceclass = "A", tracks = c("SPK", "HRX")) {
  dogs.records <- list()
  for (track in tracks){
    datetrack = "00"
    # going through dates in reverse order
    for (racedate in format(seq(as.Date(enddate, format="%d-%b-%Y"),as.Date(startdate, format="%d-%b-%Y"), by= "-1 day"), "%d-%b-%Y")) {
      # fetch results webpage
      page <- htmlParse(getURL(paste0('http://www.igb.ie/results/view-results/?track=',track,'&date=',racedate)), asText = TRUE)
      racecheck <- xpathSApply(page, "//div[@class='col-6']", xmlValue)
      #check for results availibility
      if (substr(racecheck[1],19,35) != "There are no race") {
        # print the month being fetched
        #if (format(as.Date(racedate,format="%d-%b-%Y"), "%m") != datetrack){
          print(paste0(track," in ",format(as.Date(racedate,format="%d-%b-%Y"), "%d-%b-%Y")))
        #  datetrack <- format(as.Date(racedate,format="%d-%b-%Y"), "%m")
        #}
        races.names <- xpathSApply(page, "//div[@class='col-11']", xmlValue)
        races.results <- readHTMLTable(page)
        races.traps <- xpathSApply(page, "//table//img//@alt")
        races.traps <- races.traps[(length(grep("Grade", races.names))+1):length(races.traps)]
        dogtrapidx = 0
        # going through races that have grade associated
        for (r in grep("Grade", races.names)){ 
          # extract race grade and lenght
          racedet <- strsplit(races.names[r],"Grade : ")
          racedet <- racedet[[1]][2]
          racedet <- strsplit(racedet,")")
          racegrade <- racedet[[1]][1] 
          # select only desired race class
          if (substr(racegrade,1,3) == raceclass) { 
            racelength <- as.numeric(substr(racedet[[1]][2],nchar(racedet[[1]][2])-5,nchar(racedet[[1]][2])))
            racewintime <- as.numeric(as.character(races.results[[r+7]]$`Win Time`[1]))
            racetimedis <- strsplit(as.character(races.results[[r+7]]$Going[1]),' ')
            racetimedis <- as.numeric(racetimedis[[1]][1])
            # correct for track rating 
            if (!is.na(racetimedis)) { 
              racewintime <- round(racewintime + racetimedis,2)
            }
            # going through dogs in each selected race
            for (dn in 1:(nrow(races.results[[r+7]])-1)){
              if (!is.na(as.numeric(substr(as.character(races.results[[r+7]]$EstTime[dn]),1,5)))){ # check if dog have been eliminated
                # extract dog name and record
                dogname <- strsplit(as.character(races.results[[r+7]]$Greyhound[dn]),'\n')
                dogname <- dogname[[1]][1]
                dogid <- match(dogname,names(dogs.records))
                dogrecord <- data.frame(row.names = racedate)
                dogrecord$Track <- track
                dogrecord$Trap <- as.numeric(substr(races.traps[dogtrapidx + dn],6,6))
                dogrecord$RaceLength <- racelength
                dogrecord$RaceGrade <- racegrade
                dogrecord$DogGrade <- as.character(races.results[[r+7]]$Grade[dn])
                dogrecord$Time <- as.numeric(substr(as.character(races.results[[r+7]]$EstTime[dn]),1,5))
                dogrecord$FastestTime <- dogrecord$Time - racewintime
                dogrecord$Speed <- racelength / dogrecord$Time
                dogrecord$Pos <- as.numeric(races.results[[r+7]]$Pos.[dn])
                
                # check if dog been recoreded before
                if (is.na(dogid)) { 
                  dogs.records[[length(dogs.records)+1]] <- dogrecord
                  names(dogs.records)[[length(dogs.records)]] <- dogname
                } else {
                  dogs.records[[dogid]]<-rbind(dogs.records[[dogid]],dogrecord)
                }
              }
            }
          }
          dogtrapidx = dogtrapidx + nrow(races.results[[r+7]])-1
        }
      }
    }
  }
  return(dogs.records)
}
