################################################################################
## Pump Hydro Example - J. Birge, 29 March 2024
################################################################################
#
################################################################################
# read in the load data 
################################################################################
# comment this out after loading into your environment
################################################################################
ddata<- read.csv("load.csv")
demand<- ddata$Load

# Define the variables used in the solution
stages <- 168
states <- 390
action <- 12
# here, the initial state is a full reservoir - coal is setup 1hr and gas is idle
initial <- 12 * 30 + 3 * 0 + 1 * 1
infty <- 90000000
################################################################################
# You might need other additional parameters for your problem
#  stages - number of stages
#  states - number of states
#  action - number of actions
#  initial - beginning state
#  infty - a big number
#  newcst - used in recursion for trial cost of new action
#  curstate - current state - used in recovering solution
#   dpcost(i,j) - opt. cost from stage i, state j to end
#   dpact(i,j) - opt. action from stage i, state j to end
#   stcost - function to find state costs
#   accost - function to find action costs
#   nxtst - function to find next state
################################################################################
# States: 30*(hydstate-1) + 3*(coalstate-1) + gasstate
#    where hydstate=1....13 for water level 0....12
#          coalstate=1...10 where 1,2 correspond to setting up Hr 1, Hr 2
#                           (so it can then generate the next hour)
#                    3, 4, correspond to idle 1 hour , 2+ Hrs (and can then 
#                             be started the next hour)
#                    5, 6, .. , 10 corresponds to generating 1... 6+ hours
#                            (and can then be idled the next hour)
#          gasstate=1 for setup for 1 hour (and can generate the next hour)
#                   2 for idle for 1 hour (and can then start the next hour)
#                   3 generating for 1+ hour (and can idle the next hour)
################################################################################

################################################################################
# actions: 4*(hydroact-1)+2*(coalact-1)+gasact-1
#         where hydroact=1 means pumping
#                       =2 means generating
#                       =3 means idle
#               coalact =1 means "on"  (turn on or keep generating)
#                      plot =2 means "off"  (stay or turn off)
#               gasact  =1 means "on" (turn on or keep generating)
#                       =2 means "off" (stay or turn off)
# In other words,
# actions 1-4: pump, 5-8: generate, 9-12: no hydro
# 1,3,5,7,9,11: on coal, 2,4,6,8,10,12:  off coal
# 1-2,5-6,9-10: on gas, 3-4,7-8,11-12: off gas
# Examples: 1 means pump hydro, turn or keep on the coal plant, and 
#          turn or keep on the gas plant; 12 means no hydro, coal off,
#          and gas off 
################################################################################
# so at that 1-4: pump, 5-8: generate, 9-12: no hydro
# 1,3,5,7,9,11: turn on coal, 2,4,6,8,10,12: turn off coal
# 1-2,5-6,9-10: turn on gas, 3-4,7-8,11-12: turn off gas
# Example: 1 means pump hydro, turn on the coal plant, and 
#          turn on the gas plant; 12 mean no hydro, coal off,
#          and gas off 
################################################################################
# set minimum and maximum output levels, generation costs, startup costs
#  shutdown costs, and pumping and hydro generation values
################################################################################
minnuc <- 2000 
maxnuc <- 7500
mincoal <- 2300
maxcoal <- 3000
mingas <- 700
maxgas <- 1500
gennuc <- 5
gencoal <- 15
gengas <- 40
startcoal <- 2000
startgas <- 800
shutcoal <- 500
shutgas <- 100
genpump <- 900
#genpump <- 0
fillpump <- 1200
################################################################################
#  Next are the functions:
#      stcost to recover the cost of generation in a 
#             state for one hour (or infinite if not feasible); 
#      accost for the cost of taking an action (startup or 
#              shutdown); 
#      nxtst to find the next state given current state and action
################################################################################
stcost <- function(coalstate, gasstate, effdem) {
  minnuc <- 2000
  maxnuc <- 7500
  mincoal <- 2300
  maxcoal <- 3000
  mingas <- 700
  maxgas <- 1500
  gennuc <- 5
  gencoal <- 15
  gengas <- 40
  
  x <- gennuc * minnuc
  effdem <- effdem - minnuc
  
  if (coalstate >= 5) {
    effdem <- effdem - mincoal
    x <- x + gencoal * mincoal
  }
  
  if (gasstate == 3) {
    effdem <- effdem - mingas
    x <- x + gengas * mingas
  }
  
  if (effdem < 0) {
    x <- 9000000000
  } else {
    nucgen <- min(effdem, maxnuc - minnuc)
    x <- x + gennuc * nucgen
    effdem <- effdem - nucgen
    
    if (coalstate >= 5) {
      coalgen <- min(effdem, maxcoal - mincoal)
      x <- x + gencoal * coalgen
      effdem <- effdem - coalgen
      
      if (effdem > 0) {
        if (gasstate == 3) {
          gasgen <- min(effdem, maxgas - mingas)
          x <- x + gengas * gasgen
          effdem <- effdem - gasgen
          
          if (effdem > 0) {
            x <- 9000000000
          }
        } else {
          x <- 90000000
        }
      }
    } else {
      if (effdem > 0) {
        x <- 90000000
      }
    }
  }
  
  return(x)
}

accost <- function(hydstate, coalstate, gasstate, hydact, gasact, coalact) {
  # this function defines the action costs 
  startcoal <- 2000
  startgas <- 800
  shutcoal <- 500
  shutgas <- 100
  infty <- 90000000
  x <- 0
  
  if (gasstate == 1 && gasact == 1) {
    x <- x + startgas
  } else if (gasstate == 2) {
    if (gasact == 1) {
      x <- x + 0
    } else if (gasact == 2) {
      x <- x + shutgas
    }
  }
  
  if (coalstate == 1 && coalact == 1) {
    x <- x + startcoal
  } else if (coalstate == 2 && coalact == 1) {
    x <- x + 0
  } else if (coalstate >= 3) {
    if (coalstate < 10 && coalact == 2) {
      x <- infty
    } else if (coalact == 2) {
      x <- x + shutcoal
    }
  }
  
  if (hydstate == 1 && hydact == 2) {
    x <- x + infty
  }
  
  return(x)
}

nxtst <- function(hydstate, coalstate, gasstate, hydact, gasact, coalact) {
  # Initialize state variables
  hyds <- hydstate
  coals <- coalstate
  gass <- gasstate
  
  # Update hydstate based on hydact
  if (hydact == 1) {
    hyds <- min(13, hyds + 1)
  } else if (hydact == 2) {
    hyds <- max(1, hyds - 1)
  }
  
  # Update gass based on gasstate and gasact
  if (gasstate == 3) {
    if (gasact == 2) {
      gass <- 1
    }
  } else if (gasstate == 2) {
    if (gasact == 2) {
      gass <- 1
    } else {
      gass <- 3
    }
  } else if (gasstate == 1) {
    if (gasact == 1) {
      gass <- 2
    }
  }
  
  # Update coals based on coalstate and coalact
  if (coalstate < 3) {
    if (coalact == 1) {
      coals <- 3
    } else {
      coals <- max(coals - 1, 1)
    }
  } else if (coalstate >= 3) {
    if (coalact == 1) {
      coals <- min(coals + 1, 10)
    } else {
      coals <- 2
    }
  }
  
  # Calculate next state cost
  n <- 30 * (hyds - 1) + 3 * (coals - 1) + gass
  return(n)
}
################################################################################
#  Here are the Dynamic Program Steps
################################################################################
# Initialize dpact and dpcost matrices
dpact <- matrix(1, nrow = 169, ncol = 390)
dpcost <- matrix(infty, nrow = 169, ncol = 390)

# Set the cost for the final stage (169)
dpcost[169, initial] <- 0

# Dynamic programming loop
# The cost-to-go is in dpcost
for (i in stages:1) {
  for (j in states:1) {
    hydstate <- floor((j - 1) / 30) + 1
    jstat <- j - 1 - 30 * (hydstate - 1)
    coalstate <- floor(jstat / 3) + 1
    gasstate <- jstat - 3 * (coalstate - 1) + 1
    
    for (k in action:1) {
      hydact <- floor((k - 1) / 4) + 1
      kact <- k - 1 - 4 * (hydact - 1)
      gasact <- floor(kact / 2) + 1
      coalact <- kact - 2 * (gasact - 1) + 1
      
      nx <- nxtst(hydstate, coalstate, gasstate, hydact, gasact, coalact)
      
      # Calculate new cost by first including hydro charge or discharge 
      #  as effective demand (effdem)
      if (hydact == 1) {
        effdem <- demand[i] + fillpump
      } else if (hydact == 2) {
        effdem <- demand[i] - genpump
      } else {
        effdem <- demand[i]
      }
      
      # For the new cost-to-go Q(s,a)=r(s,a)+V(s'(s,a))
      #   r(s,a) is composed as a state part "stcost" 
      #          and an action part "accost"
      newcst <- stcost(coalstate, gasstate, effdem) +
        accost(hydstate, coalstate, gasstate, hydact, gasact, coalact) +
        dpcost[i + 1, nx]
      
      # Update dpact and dpcost if new cost is smaller
      if (newcst < dpcost[i, j]) {
        dpcost[i, j] <- newcst
        dpact[i, j] <- k
      }
    }
  }
}
################################################################################
# Next: recover the optimal solution path beginning in the initial state
# optcost is the optimal cost-to-go at each stage
# optstate is the optimal state for each stage
# opthydro is the optimal level of the pump storage at each stage
# optact is the optimal action at each stage 
#  # NOTE: if you have random transitions, then you will just return a sample
#  #       solution path with a random choice of the next state (here, nxtst) 
################################################################################

optcost <- numeric(stages+1)
optstate <- numeric(stages+1)
opthydro <-numeric(stages+1)
optact <-numeric(stages+1)
curstate <- initial
optstate[1] <- initial
optcost[1] <- dpcost[1,initial]
opthydro[1] <- floor((optstate[1]-1)/30)
optact[1] <- dpact[1,initial]
hydstate <- floor((optstate[1] - 1) / 30) + 1
jstat <- optstate[1] - 1 - 30 * (hydstate - 1)
coalstate <- floor(jstat / 3) + 1
gasstate <- jstat - 3 * (coalstate - 1) + 1
hydact <- floor(optact[1]/ 4) + 1
kact <- optact[1] - 1 - 4 * (hydact - 1)
gasact <- floor(kact / 2) + 1
coalact <- kact - 2 * (gasact - 1) + 1
curstate <- nxtst(hydstate, coalstate, gasstate, hydact, gasact, coalact)
for (i in 2:(stages+1)) {
  # record the optimal state, cost, hydro level, and actions at each state
  optstate[i] <- curstate
  optcost[i] <- dpcost[i,curstate]
  opthydro[i] <- floor((optstate[i] - 1) / 30) 
  optact[i] <- dpact[i, curstate]
  # decompose the action to hydro, gas, and coal
  hydact <- floor((optact[i]-1) / 4) + 1
  kact <- optact[i] -1 - 4 * (hydact - 1)
  gasact <- floor(kact / 2) + 1
  coalact <- kact - 2 * (gasact - 1) + 1
  # decompose the state for hydro, gas, and
  hydstate <- floor((optstate[i] - 1) / 30) + 1
  jstat <- optstate[i] - 1 - 30 * (hydstate - 1)
  coalstate <- floor(jstat / 3) + 1
  gasstate <- jstat - 3 * (coalstate - 1) + 1
  # find the next state 
  curstate <- nxtst(hydstate, coalstate, gasstate, hydact, gasact, coalact)
}
# 
# plot(opthydro, c(1:169))
# plot(c(1:169), opthydro)
# plot(c(1:169), optcost)
# plot(c(1:169), optstate)
# plot(c(1:169), shutcoal)

