library(shiny)
library(DT)
library(shinyjs)
library(shinythemes)

#### COMPUTATIONAL CODE ####
cleanData = function(dirty){
  # convert empties to NAs dates
  dirty$First.Seen..TJ[dirty$First.Seen..TJ == ''] = NA
  dirty$First.Seen..TRAC[dirty$First.Seen..TRAC == ''] = NA
  dirty$Last.Seen..TJ[dirty$Last.Seen..TJ == ''] = NA
  dirty$Last.Seen..TRAC[dirty$First.Seen..TRAC == ''] = NA
  # convert empties to NAs hits
  dirty$Hit.Count..TJ[dirty$Hit.Count..TJ == ''] = NA
  dirty$Use.Count..TRAC[dirty$Use.Count..TRAC == ''] = NA
  # clean NAs for dates
  dirty = dirty[!is.na(dirty$First.Seen..TJ),]
  dirty = dirty[!is.na(dirty$First.Seen..TRAC),]
  dirty = dirty[!is.na(dirty$Last.Seen..TJ),]
  dirty = dirty[!is.na(dirty$Last.Seen..TRAC),]
  # clean NAs for hits
  dirty = dirty[!is.na(dirty$Use.Count..TRAC),]
  dirty = dirty[!is.na(dirty$Hit.Count..TJ),]
  # convert to characters
  dirty$First.Seen..TJ = as.character(dirty$First.Seen..TJ)
  dirty$First.Seen..TRAC = as.character(dirty$First.Seen..TRAC)
  dirty$Last.Seen..TJ = as.character(dirty$Last.Seen..TJ)
  dirty$Last.Seen..TRAC = as.character(dirty$Last.Seen..TRAC)
  # split the date times
  dateTimeFTJ = strsplit(dirty$First.Seen..TJ, ' ')
  dateTimeLTJ = strsplit(dirty$Last.Seen..TJ, ' ')
  dateTimeFTrac = strsplit(dirty$First.Seen..TRAC, ' ')
  dateTimeLTrac = strsplit(dirty$Last.Seen..TRAC, ' ')
  # get all the individual dates from the split
  dirty$First.Seen.Date.TJ = rep('', dim(dirty)[1])
  dirty$First.Seen.Date.Trac = rep('', dim(dirty)[1])
  dirty$Last.Seen.Date.TJ = rep('', dim(dirty)[1])
  dirty$Last.Seen.Date.Trac = rep('', dim(dirty)[1])
  for (x in 1:dim(dirty)[1]){
    dirty$First.Seen.Date.TJ[x] = as.character(dateTimeFTJ[[x]][1])
    dirty$First.Seen.Date.Trac[x] = as.character(dateTimeFTrac[[x]][1])
    dirty$Last.Seen.Date.TJ[x] = as.character(dateTimeLTJ[[x]][1])
    dirty$Last.Seen.Date.Trac[x] = as.character(dateTimeLTrac[[x]][1])
  }
  dirty$First.Seen.Date.TJ = as.Date(dirty$First.Seen.Date.TJ)
  dirty$First.Seen.Date.Trac = as.Date(dirty$First.Seen.Date.Trac)
  dirty$Last.Seen.Date.TJ = as.Date(dirty$Last.Seen.Date.TJ)
  dirty$Last.Seen.Date.Trac = as.Date(dirty$Last.Seen.Date.Trac)
  
  return(dirty)
}

computeMatches = function(phoneData, tjHits, tracHits, weeks){
  # verify hits
  tjMatch = phoneData$Hit.Count..TJ >= tjHits
  tracMatch = phoneData$Use.Count..TRAC >= tracHits
  hitMatches = tjMatch & tracMatch
  
  # verify date overlapping
  firstOverlapTrac = phoneData$First.Seen.Date.Trac >= phoneData$First.Seen.Date.TJ &
    phoneData$First.Seen.Date.Trac <= phoneData$Last.Seen.Date.TJ
  secondOverlapTrac = phoneData$Last.Seen.Date.Trac <= phoneData$Last.Seen.Date.TJ &
    phoneData$Last.Seen.Date.Trac >= phoneData$First.Seen.Date.TJ
  firstOverlapTJ = phoneData$First.Seen.Date.TJ >= phoneData$First.Seen.Date.Trac &
    phoneData$First.Seen.Date.TJ <= phoneData$Last.Seen.Date.Trac
  secondOverlapTJ = phoneData$Last.Seen.Date.TJ <= phoneData$Last.Seen.Date.Trac &
    phoneData$Last.Seen.Date.TJ >= phoneData$First.Seen.Date.Trac
  # check the weeks difference
  firstCheck = floor(abs((phoneData$Last.Seen.Date.Trac - phoneData$First.Seen.Date.TJ)/7)) <= weeks
  secondCheck = floor(abs((phoneData$Last.Seen.Date.TJ - phoneData$First.Seen.Date.Trac)/7)) <= weeks
  # get the final boolean
  finalDatesBool = firstOverlapTJ | secondOverlapTJ | firstOverlapTrac | secondOverlapTrac |
    firstCheck | secondCheck
  
  # filter the data
  Matches = phoneData[hitMatches & finalDatesBool, 1:(dim(phoneData)[2]-4)]
  
  return(Matches)
}