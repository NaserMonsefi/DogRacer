grader <- function(gradeinchr, gradeclass ="A"){
  # switch from character grade to numeric grade
  grade <- gradeinchr[!is.na(gradeinchr)]
  grade[grade == paste0(gradeclass,gradeclass,"0")] <- "-1" # make AA0 top grade
  grade[grade == paste0(gradeclass,gradeclass,"0/0")] <- "-0.5" # fix for AA0/0 grade
  grade[grade == paste0(gradeclass,"10")] <- "10"
  for ( mix in grep("/", grade)) {
    
    grade[mix] <- mean(as.numeric(strsplit(substr(grade[mix],2,nchar(grade[mix])) ,"/")[[1]]))
  }
  grade[substr(grade,1,1) == gradeclass] <- substr(grade[substr(grade,1,1) == gradeclass],2,2)
  grade <- as.numeric(grade) + 2
  return(grade)
}
