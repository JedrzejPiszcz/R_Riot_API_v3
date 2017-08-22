
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

API_key<-"RGAPI-3e957a38-2c28-49fe-9790-af8bc2d7505c"

serverName<-"eun1"

summonerName<-"Divinar"

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

getSummonerMatches<-function(accountId, serverName = "eun1", API_key){
  
  fileURL <- paste0("https://", serverName, ".api.riotgames.com//lol/match/v3/matchlists/by-account/", accountId, "?api_key=", API_key)
  
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

getGameData<-function(gameId, serverName = "eun1", API_key){
  
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
                    pickTurn = jstring("pickTurn"))
    
    return(bans_data)
  }
  getParticipantStats<-function(json_file){
    
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
        DamageDealt = jstring("DamageDealt"),
        largestCriticalStrike = jstring("largestCriticalStrike"),
        totalDamageDealtToChampions = jstring("totalDamageDealtToChampions"),
        magicDamageDealtToChampions = jstring("magicDamageDealtToChampions"),
        physicalDamageDealtToChampions = jstring("physicalDamageDealtToChampions"),
        DamageDealtToChampions = jstring("DamageDealtToChampions"),
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
        DamageTaken = jstring("DamageTaken"),
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
    
    
    
    return(participant_stats_data)
  } #also holds general participant data such as teamId or summoner spells
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
  getParticipantRunes<-function(json_file){
    
    participant_runes_data<-json_file %>%
      enter_object("participants") %>%
      gather_array()%>%
      spread_values(participantId = jstring("participantId")) %>%
      enter_object("runes") %>%
      gather_array() %>%
      spread_values(runeId = jstring("runeId"),
                    rank = jstring("rank"))
    
    casted_participant_runes_data<-dcast(participant_runes_data, participantId ~ masteryId)
    
    return(casted_participant_runes_data)
  }
  getParticipantTimeline<-function(json_file){
    
    participant_timeline_data<-json_file
  }
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
  
  
  
  
  
  
  
  
  
}

summoner<-getSummonerData(summonerName, API_key = API_key)

matches<-getSummonerMatches(summoner$accountId, API_key = API_key)

matches$timestamp<-anytime(as.numeric(matches$timestamp)/1000)  #because timestamp is in miliseconds

champions<-getStaticChampions(API_key = API_key)