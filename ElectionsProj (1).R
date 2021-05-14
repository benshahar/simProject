
##----------------------------------------- 1.  all functions ------------------------------------------------
set.seed(456)

addService<- function  (trajectory,sname,timeDist){
  updatedPath <- seize(trajectory, resource = sname, amount = 1)%>%
    timeout(timeDist) %>%
    release(resource = sname, amount = 1)
  
  return(updatedPath)
}

addServiceVote<- function  (trajectory,sname){
  idTime <-  function() rexp(1,3/2) 
  votingTime <-  function() rnorm(1,1,0.1*0.1)
  updatedPath <- seize(trajectory, resource = sname, amount = 1)%>%
    timeout(1) %>% #idenitification + voting time  
    log_("Voting!")%>%
    release(resource = sname, amount = 1)
  
  return(updatedPath)
}

addServiceFavorite<- function  (path,sname){
  
  updatedPath <- seize(path, sname)%>%
    timeout(function() runif(1,2,6)) %>%
    log_("visiting my favorite party")%>%
    release(sname)
  
  return(updatedPath)
}

getRadicality <- function(){
  u <- runif (1,0,1)
  if (u < 0.5){
    return ((u/8)^(1/3))
  }
  else{
    return (u/2)
  }
}




getFirstOpinion <- function(){
  voterOpinion <- get_attribute(elections, keys="voterOpinion")
  #print(voterOpinion)
  z <- get_attribute(elections, keys="radicality")
  print(z)
  if (voterOpinion=='1'){ # 1-NITZAN
    print("ok1")
    leftProb= (0.25+z)
    centerProb =(0.5-(z/2))
    rightProb = 0.25-(z/2)
  }
  
  if (voterOpinion=='2'){ # 2-GANTZ
    print("ok2")
    leftProb= 0.25-(z/2)
    centerProb =(0.5+z)
    rightProb =  0.25-(z/2)
  }
  
  else {# 3- BENET
    print("ok3")
    leftProb= (0.25-(z/2))
    centerProb =(0.5-(z/2))
    rightProb = (0.25+z)
  }
  print(c(leftProb,centerProb,rightProb))
  return (c(leftProb,centerProb,rightProb))
}

##----------------------------------------- 2.  all simulation parameters ------------------------------------------------

simulationTime <- 30

scheduleSouthBig <- schedule(timetable = c(0,15*60), values = c(4, 0), period = Inf)
scheduleSouthSmall<- schedule(timetable = c(1*60, 13*60,24*60), values = c(0, 2, 0), period = Inf)

scheduleCenterBig <- schedule(timetable = c(15*60,24*60), values = c(6, 0), period = Inf)
scheduleCenterSmall<- schedule(timetable = c(1*60, 13*60,24*60), values = c(0, 3, 0), period = Inf)

scheduleNorthBig <- schedule(timetable = c(15*60,24*60), values = c(2, 0), period = Inf)
scheduleNorthSmall<- schedule(timetable = c(1*60, 13*60,24*60), values = c(0, 1, 0), period = Inf)


##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

myElection<- simmer("myElection") %>%
  #South area
  add_resource("leftSouthStand",capacity=3,queue_size=0)%>%
  add_resource("centerSouthStand",capacity=5,queue_size=0)%>%
  add_resource("rightSouthStand",capacity=5,queue_size=0)%>%
  add_resource("SouthBigBallot",capacity=0,queue_size=Inf)%>%
  add_resource("SouthSmallBallot",capacity=scheduleSouthSmall,queue_size=Inf)%>%

  #Center area
  add_resource("leftCenterStand",capacity=10,queue_size=0)%>%
  add_resource("centerCenterStand",capacity=10,queue_size=0)%>%
  add_resource("rightCenterStand",capacity=10,queue_size=0)%>%
  add_resource("CenterBigBallot",capacity=scheduleCenterBig,queue_size=Inf)%>%
  add_resource("CenterSmallBallot",capacity=scheduleCenterSmall,queue_size=Inf)%>%
    
  #North area
  add_resource("leftNorthStand",capacity=5,queue_size=0)%>%
  add_resource("centerNorthStand",capacity=5,queue_size=0)%>%
  add_resource("rightNorthStand",capacity=4,queue_size=0)%>%
  add_resource("NorthBigBallot",capacity=scheduleNorthBig,queue_size=Inf)%>%
  add_resource("NorthSmallBallot",capacity=scheduleNorthSmall,queue_size=Inf)


##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------

centerBigBallotTraj <- trajectory("centerBigBallotTraj") %>%
  addServiceVote("CenterBigBallot")

centerSmallBallotTraj <- trajectory("centerSmallBallotTraj") %>%
  log_("im at centerSmallBallotTraj")%>%
  addServiceVote("CenterSmallBallot")

northBigBallotTraj <- trajectory("northBigBallotTraj") %>%
  addServiceVote("NorthBigBallot")

northSmallBallotTraj <- trajectory("northSmallBallotTraj") %>%
#  log_("im at northSmallBallotTraj")%>%
  addServiceVote("NorthSmallBallot")

southBigBallotTraj <- trajectory("southBigBallotTraj") %>%
  addServiceVote("SouthBigBallot")

southSmallBallotTraj <- trajectory("southSmallBallotTraj") %>%
  #  log_("im at southSmallBallotTraj")%>%
  addServiceVote("SouthSmallBallot")

  NorthernPrefersLeft <- trajectory("NorthernPrefersLeft") %>%
    log_("im at NorthernPrefersLeft")%>%
    addService("rightNorthStand", function() rexp (1,1/4))%>% #NEED TO ADD THIS
    addService("centerNorthStand", function() rexp (1,1/4))%>%
    addServiceFavorite("leftNorthStand")%>%
    branch(option = function() get_attribute(myElection, "voterType"), continue = c(FALSE,FALSE,FALSE),northBigBallotTraj,northSmallBallotTraj,northBigBallotTraj)
  

    NorthernPrefersCenter <- trajectory("NorthernPrefersCenter") %>% #left->right->center
    log_("im at  NorthernPrefersCenter")%>%
    addService("leftNorthStand", function() rexp (1,1/4))%>% #NEED TO ADD THIS
    addService("rightNorthStand", function() rexp (1,1/4))%>%
    addServiceFavorite("centerNorthStand")%>%
    branch(option = function() get_attribute(myElection, "voterType"), continue = c(FALSE,FALSE,FALSE),northBigBallotTraj,northSmallBallotTraj,northBigBallotTraj)
  
  NorthernPrefersRight <- trajectory("NorthernPrefersRight") %>%
    log_("im at  NorthernPrefersRight")%>%
    addService("centerNorthStand", function() rexp (1,1/4))%>% #NEED TO ADD THIS
    addService("leftNorthStand", function() rexp (1,1/4))%>%
    addServiceFavorite("rightNorthStand")%>%
    branch(option = function() get_attribute(myElection, "voterType"), continue = c(FALSE,FALSE,FALSE),northBigBallotTraj,northSmallBallotTraj,northBigBallotTraj)
  
  
  CenterPrefersLeft <- trajectory("CenterPrefersLeft") %>%
    log_("im at CenterPrefersLeft")%>%
    addService("rightCenterStand", function() rexp (1,1/4))%>% #NEED TO ADD THIS
    addService("centerCenterStand", function() rexp (1,1/4))%>%
    addServiceFavorite("leftCenterStand")%>%
    branch(option = function() get_attribute(myElection, "voterType"), continue = c(FALSE,FALSE,FALSE),northBigBallotTraj,northSmallBallotTraj,northBigBallotTraj)

  
  CenterPrefersCenter <- trajectory("CenterPrefersCenter") %>%
    log_("im at  CenterPrefersCenter")%>%
    addService("leftCenterStand", function() rexp (1,1/4))%>% #NEED TO ADD THIS
    addService("rightCenterStand", function() rexp (1,1/4))%>%
    addServiceFavorite("centerCenterStand")%>%
    branch(option = function() get_attribute(myElection, "voterType"), continue = c(FALSE,FALSE,FALSE),northBigBallotTraj,northSmallBallotTraj,northBigBallotTraj)
  
  
  CenterPrefersRight <- trajectory("CenterPrefersRight") %>%
    log_("im at  CenterPrefersRight")%>%
    addService("rightCenterStand", function() rexp (1,1/4))%>% #NEED TO ADD THIS
    addService("centerCenterStand", function() rexp (1,1/4))%>%
    addServiceFavorite("leftCenterStand")%>%
    branch(option = function() get_attribute(myElection, "voterType"), continue = c(FALSE,FALSE,FALSE),centerBigBallotTraj,centerSmallBallotTraj,centerBigBallotTraj)
  
  
  SouthernPrefersLeft <- trajectory("SouthernPrefersLeft") %>%
    log_("im at SouthernPrefersLeft")%>%
    addService("rightSouthStand", function() rexp (1,1/4))%>% #NEED TO ADD THIS
    addService("centerSouthStand", function() rexp (1,1/4))%>%
    addServiceFavorite("leftSouthStand")%>%
    branch(option = function() get_attribute(myElection, "voterType"), continue = c(FALSE,FALSE,FALSE),southBigBallotTraj,southSmallBallotTraj,southBigBallotTraj)
  
  
  
  SouthernPrefersCenter <- trajectory("SouthernPrefersCenter") %>%
    log_("im at  SouthernPrefersCenter")%>%
    addService("leftSouthStand", function() rexp (1,1/4))%>% #NEED TO ADD THIS
    addService("rightSouthStand", function() rexp (1,1/4))%>%
    addServiceFavorite("centerSouthStand")%>%
    branch(option = function() get_attribute(myElection, "voterType"), continue = c(FALSE,FALSE,FALSE),southBigBallotTraj,southSmallBallotTraj,southBigBallotTraj)
  
  
  SouthernPrefersRight <- trajectory("SouthernPrefersRight") %>%
    log_("im at  SouthernPrefersRight")%>%
    addService("centerSouthStand", function() rexp (1,1/4))%>% #NEED TO ADD THIS
    addService("leftSouthStand", function() rexp (1,1/4))%>%
    addServiceFavorite("rightSouthStand")%>%
    branch(option = function() get_attribute(myElection, "voterType"), continue = c(FALSE,FALSE,FALSE),southBigBallotTraj,southSmallBallotTraj,southBigBallotTraj)
  
northTraj <- trajectory("northTraj") %>%
  log_("im at NorthTrajectory")%>%
  branch(option = function() get_attribute(myElection, "voterOpinion"), continue = c(FALSE,FALSE,FALSE),NorthernPrefersLeft,NorthernPrefersCenter,NorthernPrefersRight)


centerTraj <- trajectory("centerTraj") %>%
  log_("im at  centerTraj")%>%
  branch(option = function() get_attribute(myElection, "voterOpinion"), continue = c(FALSE,FALSE,FALSE),CenterPrefersLeft,CenterPrefersCenter,CenterPrefersRight)


southTraj <- trajectory("southTraj") %>%
  log_("im at  SouthTraj")%>%
  branch(option = function() get_attribute(myElection, "voterOpinion"), continue = c(FALSE,FALSE,FALSE),SouthernPrefersLeft,SouthernPrefersCenter,SouthernPrefersRight)

#shutting down all ballots when the supervisor comes
supervisingTraj <- trajectory("supervisingTraj") %>%
  log_("im at supervisingTraj")%>%
  set_capacity("NorthBigBallot",0)%>%
  set_capacity("NorthSmallBallot",0)%>%
  set_capacity("CenterBigBallot",0)%>%
  set_capacity("CenterSmallBallot",0)%>%
  set_capacity("SouthBigBallot",0)%>%
  set_capacity("SouthSmallBallot",0)%>%
  timeout(function() rtriangle(1,3,5,4))%>% #need to change the c here, it's supposed to be 9 )%>%
  set_capacity("NorthBigBallot",2)%>%
  set_capacity("NorthSmallBallot",1)%>%
  set_capacity("CenterBigBallot",6)%>%
  set_capacity("CenterSmallBallot",3)%>%
  set_capacity("SouthBigBallot",4)%>%
  set_capacity("SouthSmallBallot",2)

 
  
  startTraj <- trajectory("startTraj") %>%
    log_("im at start trajectory")%>%
    set_attribute(keys="location", values = function() rdiscrete(1, c(0.3,0.4,0.3),c(1,2,3)))%>% #1-north, 2-center, 3-south
    set_attribute(keys="voterOpinion", values = function() rdiscrete(1, c(0.4,0.1,0.5),c(1,2,3)))%>% #1-left, 2-center, 3-right
    set_attribute(keys="radicality", values = function() getRadicality())%>% 
    set_attribute(keys=c("leftProb","centerProb","rightProb"), values = function() getFirstOpinion)%>% 
    branch(option = function() get_attribute(myElection, "location"), continue = c(FALSE,FALSE,FALSE),northTraj,centerTraj,southTraj) 
  
  InitBigVoterTraj <- trajectory("InitBigVoterTraj") %>%
    log_("im at start InitBigVoterTraj")%>%
    set_attribute(keys="voterType", values = 1)%>% #1-Big, 2-Small, 3-Disable
      join(startTraj)
  # branch(option = function() rdiscrete (1,1,1), continue = c(FALSE),startTraj) 
  
  
  InitSmallVoterTraj <- trajectory("InitSmallVoterTraj") %>%
    log_("im at start InitSmallVoterTraj")%>%
    set_attribute(keys="voterType", values = 2)%>% #1-Big, 2-Small, 3-Disable
      join(startTraj)
  # branch(option = function() rdiscrete (1,1,1), continue = c(FALSE),startTraj) 
  
  InitDisableVoterTraj <- trajectory("InitDisableVoterTraj") %>%
    log_("im at start InitDisableVoterTraj")%>%
    set_attribute(keys="voterType", values = 3)%>% #1-Big, 2-Small, 3-Disable
    join(startTraj)
  # branch(option = function() rdiscrete (1,1,1), continue = c(FALSE),startTraj)

##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------

  myElection%>%
    add_generator("voterBig", InitBigVoterTraj, distribution = function () rexp(1,3), mon=2)%>% #excel
    add_generator("voterDisable", InitDisableVoterTraj, distribution = function () rexp(1,2), mon=2)%>%##excel
    add_generator("voterSmall", InitSmallVoterTraj, distribution = function () rexp(1,0.901), mon=2)%>%
    add_generator("supevisor", supervisingTraj,at(100), mon=2) #triggers the arrival of a group of families at 14:00
  
  
  ##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
  
  reset(myElection)%>%run(until=simulationTime)
  
  plot(startTraj)
  
  arrivalData <- get_mon_arrivals(myElection,per_resource=T)
  resourceData <- get_mon_resources(myElection)
  attributeData <- get_mon_attributes(myElection)