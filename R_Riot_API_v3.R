#gittest
rm(list=ls())     #czyści workspace
wd<-"C:/JPiszcz/Jedrzej/R_test" #ustawić working directory
if(getwd()!=wd){setwd(wd)}
env_ZIPCMD<-"C:/Rtools/bin/zip"                           #nalezy ustawic sciezke do pliku zip w pakiecie rtools
if(Sys.getenv("R_ZIPCMD")!=env_ZIPCMD){Sys.setenv(R_ZIPCMD = env_ZIPCMD)}      

library(data.table)
library(RCurl)
library(tidyjson)
library(dplyr)
library(mailR)
library(anytime)
library(reshape2)

API_key<-"RGAPI-7112da49-03a6-4753-b688-827a7d014b1d"

serverName<-"eun1"

summonerName<-"Divinar"

NoOfGames<-1327

getSummonerData<-function(summonerName, serverName = "eun1", API_key){
  
  fileURL <- paste0("https://", serverName, ".api.riotgames.com/lol/summoner/v3/summoners/by-name/", summonerName, "?api_key=", API_key)
  
  json_file <- getURL(fileURL)
  
  json_items <-json_file %>%
    
    spread_values(id = jstring("id"),
                  accountId = jstring("accountId"),
                  name = jstring("name"),
                  profileIconId = jstring("profileIconId"),
                  revisionDate = jstring("revisionDate"),
                  summonerLevel = jstring("summonerLevel")
    ) %>%
    
    select(id, accountId, name, profileIconId, revisionDate, summonerLevel)
  
  return(json_items)
}
getSummonerMatches<-function(accountId, serverName = "eun1", API_key, beginIndex, endIndex){
  
  fileURL <- paste0("https://", serverName, ".api.riotgames.com/lol/match/v3/matchlists/by-account/", accountId, "?endIndex=", endIndex, "&beginIndex=", beginIndex ,"&api_key=", API_key)
  
  json_file <- getURL(fileURL)
  
  json_items <-json_file %>%
    
    enter_object("matches") %>% gather_array %>%  
    
    spread_values(platformId = jstring("platformId"),
                  gameId  = jstring("gameId"),
                  champion  = jstring("champion"),
                  queue  = jstring("queue"),
                  season  = jstring("season"),
                  timestamp  = jstring("timestamp"),
                  role  = jstring("role"),
                  lane  = jstring("lane")
                  
    ) %>%  
    
    select(platformId, gameId, champion, queue, season, timestamp, role, lane)
  
  return(json_items)
}
getGameData<-function(gameId, serverName = "eun1", API_key, champions, items){
  
  fileURL <- paste0("https://", serverName, ".api.riotgames.com/lol/match/v3/matches/", gameId,"?api_key=", API_key)
  json_file <- getURL(fileURL)
  
  getTeamData<-function(json_file){
    
    team_data <-json_file %>%
      
      spread_values( gameId = jstring("gameId"),
                     platformId = jstring("platformId"),
                     gameCreation = jstring("gameCreation"),
                     gameDuration = jstring("gameDuration"),
                     queueId = jstring("queueId"),
                     mapId = jstring("mapId"),
                     seasonId = jstring("seasonId"),
                     gameVersion = jstring("gameVersion"),
                     gameMode = jstring("gameMode"),
                     gameType = jstring("gameType")) %>%
      
      enter_object("teams") %>%
      gather_array() %>%
      
      spread_values(teamId = jstring("teamId"),
                    win = jstring("win"),
                    firstBlood = jstring("firstBlood"),
                    firstTower = jstring("firstTower"),
                    firstInhibitor = jstring("firstInhibitor"),
                    firstBaron = jstring("firstBaron"),
                    firstDragon = jstring("firstDragon"),
                    firstRiftHerald = jstring("firstRiftHerald"),
                    towerKills = jstring("towerKills"),
                    inhibitorKills = jstring("inhibitorKills"),
                    baronKills = jstring("baronKills"),
                    dragonKills = jstring("dragonKills"),
                    vilemawKills = jstring("vilemawKills"),
                    riftHeraldKills = jstring("riftHeraldKills"),
                    dominionVictoryScore = jstring("dominionVictoryScore"))
    
    return(team_data)
  }
  getBansData<-function(json_file){
    
    bans_data <-json_file %>%
      enter_object("teams") %>%
      gather_array() %>%
      spread_values(teamId = jstring("teamId")) %>%
      enter_object("bans") %>%
      gather_array() %>%
      spread_values(championId = jstring("championId"),
                    pickTurn = jstring("pickTurn"))%>%
      select(teamId, pickTurn, championId)
    
    casted_bans_data<-dcast(bans_data, teamId ~ pickTurn)
    colnames(casted_bans_data) <- paste0("Ban_", colnames(casted_bans_data))
    colnames(casted_bans_data)[1] <- "teamId"
    
    return(casted_bans_data)
  } #bans might be empty, catch that stuff
  
  getParticipantIdentity<-function(json_file){
    
    participant_identity_data <- json_file %>%
      enter_object("participantIdentities") %>%
      gather_array() %>%
      spread_values(participantId = jstring("participantId")) %>%
      enter_object("player") %>%
      spread_values(platformId = jstring("platformId"),
                    accountId = jstring("accountId"),
                    summonerName = jstring("summonerName"),
                    summonerId = jstring("summonerId"),
                    currentPlatformId = jstring("currentPlatformId"),
                    currentAccountId = jstring("currentAccountId"),
                    matchHistoryUri = jstring("matchHistoryUri"),
                    profileIcon = jstring("profileIcon"))
    
    return(participant_identity_data)
    
  }
  getParticipantStats<-function(json_file, champions, items){
    
    idToName<-function(Table, mergeField, NameTable){
      
      NameTable<-NameTable[,c("id", "name")]
      result<-merge(Table , NameTable , by.x = paste0(mergeField), by.y="id" ,all = FALSE, all.x = TRUE)
      result<-result[, !(names(result) %in% paste0(mergeField))]
      names(result)[names(result) == "name"] <- paste0(mergeField)
      
      
      return(result)
    }
    
    participant_stats_data<-json_file %>%
      enter_object("participants") %>%
      gather_array()%>%
      spread_values(participantId = jstring("participantId"),
                    teamId = jstring("teamId"),
                    championId = jstring("championId"),
                    spell1Id = jstring("spell1Id"),
                    spell2Id = jstring("spell2Id"),
                    highestAchievedSeasonTier = jstring("highestAchievedSeasonTier")) %>%
      enter_object("stats") %>%
      spread_values(
        participantId = jstring("participantId"),
        win = jstring("win"),
        item0 = jstring("item0"),
        item1 = jstring("item1"),
        item2 = jstring("item2"),
        item3 = jstring("item3"),
        item4 = jstring("item4"),
        item5 = jstring("item5"),
        item6 = jstring("item6"),
        kills = jstring("kills"),
        deaths = jstring("deaths"),
        assists = jstring("assists"),
        largestKillingSpree = jstring("largestKillingSpree"),
        largestMultiKill = jstring("largestMultiKill"),
        killingSprees = jstring("killingSprees"),
        longestTimeSpentLiving = jstring("longestTimeSpentLiving"),
        doubleKills = jstring("doubleKills"),
        tripleKills = jstring("tripleKills"),
        quadraKills = jstring("quadraKills"),
        pentaKills = jstring("pentaKills"),
        unrealKills = jstring("unrealKills"),
        totalDamageDealt = jstring("totalDamageDealt"),
        magicDamageDealt = jstring("magicDamageDealt"),
        physicalDamageDealt = jstring("physicalDamageDealt"),
        trueDamageDealt = jstring("trueDamageDealt"),
        largestCriticalStrike = jstring("largestCriticalStrike"),
        totalDamageDealtToChampions = jstring("totalDamageDealtToChampions"),
        magicDamageDealtToChampions = jstring("magicDamageDealtToChampions"),
        physicalDamageDealtToChampions = jstring("physicalDamageDealtToChampions"),
        trueDamageDealtToChampions = jstring("trueDamageDealtToChampions"),
        totalHeal = jstring("totalHeal"),
        totalUnitsHealed = jstring("totalUnitsHealed"),
        damageSelfMitigated = jstring("damageSelfMitigated"),
        damageDealtToObjectives = jstring("damageDealtToObjectives"),
        damageDealtToTurrets = jstring("damageDealtToTurrets"),
        visionScore = jstring("visionScore"),
        timeCCingOthers = jstring("timeCCingOthers"),
        totalDamageTaken = jstring("totalDamageTaken"),
        magicalDamageTaken = jstring("magicalDamageTaken"),
        physicalDamageTaken = jstring("physicalDamageTaken"),
        trueDamageTaken = jstring("trueDamageTaken"),
        goldEarned = jstring("goldEarned"),
        goldSpent = jstring("goldSpent"),
        turretKills = jstring("turretKills"),
        inhibitorKills = jstring("inhibitorKills"),
        totalMinionsKilled = jstring("totalMinionsKilled"),
        neutralMinionsKilled = jstring("neutralMinionsKilled"),
        neutralMinionsKilledTeamJungle = jstring("neutralMinionsKilledTeamJungle"),
        neutralMinionsKilledEnemyJungle = jstring("neutralMinionsKilledEnemyJungle"),
        totalTimeCrowdControlDealt = jstring("totalTimeCrowdControlDealt"),
        champLevel = jstring("champLevel"),
        visionWardsBoughtInGame = jstring("visionWardsBoughtInGame"),
        sightWardsBoughtInGame = jstring("sightWardsBoughtInGame"),
        wardsPlaced = jstring("wardsPlaced"),
        wardsKilled = jstring("wardsKilled"),
        firstBloodKill = jstring("firstBloodKill"),
        firstBloodAssist = jstring("firstBloodAssist"),
        firstTowerKill = jstring("firstTowerKill"),
        firstTowerAssist = jstring("firstTowerAssist"),
        firstInhibitorKill = jstring("firstInhibitorKill"),
        firstInhibitorAssist = jstring("firstInhibitorAssist"))
    
    participant_stats_data<-idToName(participant_stats_data, "championId", champions)
    participant_stats_data<-idToName(participant_stats_data, "item0" , items)
    participant_stats_data<-idToName(participant_stats_data, "item1" , items)
    participant_stats_data<-idToName(participant_stats_data, "item2" , items)
    participant_stats_data<-idToName(participant_stats_data, "item3" , items)
    participant_stats_data<-idToName(participant_stats_data, "item4" , items)
    participant_stats_data<-idToName(participant_stats_data, "item5" , items)
    participant_stats_data<-idToName(participant_stats_data, "item6" , items)
    
    
    return(participant_stats_data)
  } #also holds general participant data such as teamId or summoner spells
  getParticipantTimeline<-function(json_file){
    
    participant_timeline_data<-json_file %>%
      enter_object("participants") %>%
      gather_array()%>%
      enter_object("timeline") %>%
      spread_values(participantId =jstring("participantId"),
                    role = jstring("role"),
                    lane = jstring("lane"))%>%
      select(participantId, role, lane)
    
    
    getTimelineDetails<-function(json_file, detail){
      
      participant_timeline_details<-json_file %>%
        enter_object("participants") %>%
        gather_array()%>%
        enter_object("timeline") %>%
        enter_object(detail)%>%
        spread_values(Value0to10 = jstring("0-10"),
                      Value10to20 = jstring("10-20"),
                      Value20to30 = jstring("20-30"),
                      Value30to40 = jstring("30-40"),
                      Value40to50 = jstring("40-50"),
                      Value50to60 = jstring("50-60")) %>%
        select(array.index, Value0to10, Value10to20, Value20to30, Value30to40, Value40to50, Value50to60)
      
      colnames(participant_timeline_details) <- c("Id", paste0(detail, "_Value0to10"), paste0(detail, "_Value10-20"), paste0(detail, "_Value20-30"))
      
      return(participant_timeline_details)
    }
    
    detailsList<- json_file %>% enter_object("participants") %>% gather_array() %>% enter_object("timeline") %>% gather_keys() %>% append_values_string() %>% select (key, string)
    toExclude<-c("participantId", "role", "lane")
    detailsList<- detailsList[!grepl(paste(toExclude, collapse="|"), detailsList$key),] %>% select(key) %>% unique() %>% as.data.frame()
    
    objectsList<-list()
    
    if(nrow(detailsList)>0){
      for(i in 1:nrow(detailsList)){
        objectsList[[i]]<-as.data.frame(getTimelineDetails(json_file, detailsList[i,1]))
      }
      objectsList<-Reduce(function(x, y) merge(x, y, by.x="Id", by.y="Id", all=TRUE), objectsList)
      result<-merge(participant_timeline_data, objectsList, by.x="participantId", by.y = "Id")
    }else{
      result<-participant_timeline_data
    }
    
    #result<-result[, -grep("Id.", colnames(result))]
    
    return(result)
  }
  getParticipantRunes<-function(json_file){
    
    participant_runes_data<-json_file %>%
      enter_object("participants") %>%
      gather_array()%>%
      spread_values(participantId = jstring("participantId")) %>%
      enter_object("runes") %>%
      gather_array() %>%
      spread_values(runeId = jstring("runeId"),
                    rank = jstring("rank"))
    
    casted_participant_runes_data<-dcast(participant_runes_data, participantId ~ runeId)
    
    return(casted_participant_runes_data)
  }
  getParticipantMasteries<-function(json_file){
    
    participant_masteries_data<-json_file %>%
      enter_object("participants") %>%
      gather_array()%>%
      spread_values(participantId = jstring("participantId")) %>%
      enter_object("masteries") %>%
      gather_array() %>%
      spread_values(masteryId = jstring("masteryId"),
                    rank = jstring("rank"))
    
    casted_participant_masteries_data<-dcast(participant_masteries_data, participantId ~ masteryId)
    # casted table of masteries, might be easier for further analysis
    
    
    return(casted_participant_masteries_data)
  } #not yet decided on proper output format
  
  
  
  
  TeamData<-getTeamData(json_file)
  BansData<-getBansData(json_file)
  
  ParticipantIdentity<-getParticipantIdentity(json_file)
  ParticipantStats<-getParticipantStats(json_file, champions, items)
  ParticipantTimeline<-getParticipantTimeline(json_file)
  ParticipantRunes<-getParticipantRunes(json_file)
  ParticipantMasteries<-getParticipantMasteries(json_file)
  
  GeneralTeamData<-merge(TeamData, BansData, by="teamId")
  
  GeneralParticipantData<-Reduce(function(x, y) merge(x, y, all=TRUE), list(ParticipantIdentity,
                                                                            ParticipantStats,
                                                                            ParticipantTimeline,
                                                                            ParticipantRunes,
                                                                            ParticipantMasteries))
  
  FullGameDataByParticipant<-merge(GeneralParticipantData, GeneralTeamData, by = "teamId")
  
  return(FullGameDataByParticipant)
}

getStaticChampions<-function(serverName = "eun1", API_key){
  
  fileURL <- paste0("https://", serverName, ".api.riotgames.com/lol/static-data/v3/champions?api_key=", API_key)
  
  json_file <- getURL(fileURL)
  
  json_items <-json_file %>%
    enter_object("data") %>%
    gather_keys() %>%
    
    spread_values(id = jstring("id"),
                  key  = jstring("key"),
                  name  = jstring("name"),
                  title  = jstring("title")
                  
    ) %>%  
    
    select(id, key, name, title)
  
  return(json_items)
}
getStaticRunes<-function(serverName = "eun1", API_key){
  
  fileURL <- paste0("https://", serverName, ".api.riotgames.com/lol/static-data/v3/runes?api_key=", API_key)
  
  json_file <- getURL(fileURL)
  
  json_items <-json_file %>%
    enter_object("data") %>%
    gather_keys() %>%
    spread_values(id = jstring("id"),
                  name  = jstring("name"),
                  description  = jstring("description")
                  
    ) %>%  
    enter_object("rune") %>%
    spread_values(isRune = jstring("isRune"),
                  tierRune = jstring("tier"),
                  typeRune = jstring("type")) %>%
    select(id, name, description, isRune, tierRune, typeRune)
  
  return(json_items)
  
}
getStaticMasteries<-function(serverName = "eun1", API_key){
  
  fileURL <- paste0("https://", serverName, ".api.riotgames.com/lol/static-data/v3/masteries?api_key=", API_key)
  json_file <- getURL(fileURL)
  
  json_items <-json_file %>%
    enter_object("data") %>%
    gather_keys() %>%
    spread_values(id = jstring("id"),
                  name  = jstring("name")) %>%
    select(id, name)
  
  return(json_items)
}
getStaticItems<-function(serverName = "eun1", API_key){
  
  fileURL <- paste0("https://", serverName, ".api.riotgames.com/lol/static-data/v3/items?api_key=", API_key)
  json_file <- getURL(fileURL)
  
  
  json_items <-json_file %>%
    enter_object("data") %>%
    gather_keys() %>%
    spread_values(id = jstring("id"),
                  name  = jstring("name")) %>%
    select(id, name)
  
  return(json_items)
}

summoner<-getSummonerData(summonerName, API_key = API_key)

matches<-NULL
for(i in 1:(NoOfGames%%50)){
  
  beginIndex<-((i-1)*50)+1
  endIndex<-i*50
  
  matchesPartial<-getSummonerMatches(summoner$accountId, API_key = API_key, beginIndex = beginIndex, endIndex = endIndex)
  matches<-bind_rows(list(matches,matchesPartial))
  
  Sys.sleep(1.5)
}
matches<-as.data.frame(matches)



matches$timestamp<-anytime(as.numeric(matches$timestamp)/1000)  #because timestamp is in miliseconds

champions<-getStaticChampions(API_key = API_key)
items<-getStaticItems(API_key = API_key)
runes<-getStaticRunes(API_key = API_key)
masteries<-getStaticMasteries(API_key = API_key)

#GameData<-getGameData(matches$gameId[1], API_key = API_key)

Result<-NULL
for(
  i in 1:nrow(matches)
){
  #tryCatch({  
  
  print(matches$gameId[i])
  GameData<-getGameData(matches$gameId[i], API_key = API_key, champions = champions, items = items)
  GameData<-cbind(GameData[which(GameData$summonerName=="Divinar"),], matches[1, c("gameId", "season", "timestamp")])
  Result<-bind_rows(list(Result,GameData))
  Sys.sleep(1.25)
  # },
  #
  # error=function(e) {
  #  
  #   warning(paste("Error in da loop"))
  #  
  #   return(NA)
  # }
  # )
  #
}
