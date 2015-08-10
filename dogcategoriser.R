dogcategoriser <- function(dogs.records, minraceave = 7, includelast = 0) {
  dogs.features <- data.frame()
  fr <- 2 - includelast
  tr <- minraceave - includelast + 1
  for (j in 1:length(dogs.records)) {
    
    dogfeatures <- data.frame(row.names = names(dogs.records[j]))
    dogfeatures$TrapAve <- mean(dogs.records[[j]]$Trap[fr:tr], na.rm = TRUE)
    dogfeatures$FastestTime <- mean(dogs.records[[j]]$FastestTime[fr:tr], na.rm = TRUE)
    dogfeatures$WinPercent <- mean(dogs.records[[j]]$Pos[fr:tr] == 1, na.rm = TRUE) * 100
    dogfeatures$PlacePercent <- dogfeatures$WinPercent + (mean(dogs.records[[j]]$Pos[fr:tr] == 2, na.rm = TRUE) * 100)
    dogfeatures$ShowPercent <- dogfeatures$PlacePercent + (mean(dogs.records[[j]]$Pos[fr:tr] == 3, na.rm = TRUE) * 100)
    dogfeatures$PosAve <- mean(dogs.records[[j]]$Pos[fr:tr], na.rm = TRUE)
    dogfeatures$SpeedAveMin <- mean(dogs.records[[j]]$Speed[fr:tr], na.rm = TRUE)
    dogfeatures$SpeedAve3 <- mean(dogs.records[[j]]$Speed[fr:(3-includelast + 1)], na.rm = TRUE)
    # switch from character grade to numeric grade
    dogfeatures$DogGradeAve <- mean(grader(dogs.records[[j]]$DogGrade[fr:tr]), na.rm = TRUE)
    # calculating unpgrade
    last4 <- grader(dogs.records[[j]]$RaceGrade[1:4])
    last4[is.na(last4[1:4])] <- 12
    dogfeatures$Upgrade <- (3 * ((last4[1] - last4[2]) > 0)) + (2 * ((last4[2] - last4[3]) > 0)) + ((last4[3] - last4[4]) > 0)
    dogfeatures$Trap <- dogs.records[[j]]$Trap[1]
    dogfeatures$RaceGrade <- grader(dogs.records[[j]]$RaceGrade[1])
    dogfeatures$Pos <- dogs.records[[j]]$Pos[1]
    dogfeatures$NumRaces <- nrow(dogs.records[[j]])
    
    dogs.features <- rbind(dogs.features,dogfeatures)
    
  }
  return(dogs.features)
}
