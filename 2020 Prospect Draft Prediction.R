# 2020 Draft Analysis
# Data Import, Cleaning, Formatting


# --- Loading Packages --- 


library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library("ggpubr")
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)
library(RColorBrewer)
library(ggrepel)
library(tidyverse)
library(e1071)


# ---- Importing data ----

setwd("C:/Users/User/Documents/Data Science Projects/2020 Draft Analysis")

mydf <- read.csv("2020 Mock Draft Results.csv")
mydf <- data.frame(mydf)



# ---- Viewing the data ----

str(mydf)
typeof(mydf)



# ---- Basic data formatting ----

mydf <- mydf[-1,]

mydf <- mydf %>%
  rename(
    Mock1 = X
    )

# Convert to factors

for (i in 1:ncol(mydf)) {
  mydf[,i] <- factor(mydf[,i])
}




# Gather data into 3 columns (1: Draft Slot, repeating 1-64 for every mock, 2: Mock Date, 3: Player)

mydf3 <- gather(mydf,
                key = "draft_slot1",
                value = "player",
                -Mock1
                )

mydf3 <- mydf3 %>% 
  rename(
    draft_slot = Mock1,
    mock_date = draft_slot1
  )


# More formatting. Check the type of data in each string, then make sure no misspellings, manual fixes to data.

for (i in 1:ncol(mydf3)) {
  print(typeof(mydf3[,i]))
}

mydf3$draft_slot  <- as.numeric(mydf3$draft_slot)
mydf3$mock_date   <- factor(mydf3$mock_date)
mydf3$player      <- factor(mydf3$player)

summary(mydf3$mock_date)
data.frame(table(mydf3$player))

# Manual fixes - need to fix:
    # A.J. Epenesa/Terrell  -> AJ
    # Antoine Winfield JR   -> no JR
    # Joshua Jones          -> Josh
    # K.J. Hamler           -> KJ
    # Marlon Davidson       -> no space at the end
    # Michael Pittman JR    -> no JR


mydf3$player[mydf3$player == "A.J. Epenesa"]        <-  "AJ Epenesa"
mydf3$player[mydf3$player == "A.J. Terrell"]        <-  "AJ Terrell"
mydf3$player[mydf3$player == "Antoine Winfield JR"] <-  "Antoine Winfield"
mydf3$player[mydf3$player == "Joshua Jones"]        <-  "Josh Jones"
mydf3$player[mydf3$player == "K.J. Hamler"]         <-  "KJ Hamler"
mydf3$player[mydf3$player == "Marlon Davidson "]    <-  "Marlon Davidson"
mydf3$player[mydf3$player == "Michael Pittman JR"]  <-  "Michael Pittman"
mydf3$player[mydf3$player == "Micahel Pittman"]     <-  "Michael Pittman"
mydf3$player[mydf3$player == "Lloyd Cushenberry "]  <-  "Lloyd Cushenberry"
mydf3$player[mydf3$player == "Laviska Shenault "]   <-  "Laviska Shenault"
mydf3$player[mydf3$player == "Joe Burrow "]         <-  "Joe Burrow"
mydf3$player[mydf3$player == "Antoine Winfield "]   <-  "Antoine Winfield"
mydf3$player[mydf3$player == "Ceedee Lamb"]         <-  "CeeDee Lamb"
mydf3$player[mydf3$player == "Robert Hunt "]        <-  "Robert Hunt"

mydf3$player <- droplevels(mydf3$player)

counts <- data.frame(table(mydf3$player))
counts[order(-counts$Freq),]



# Exploring how to determine players' average draft position, distribution of draft positions, etc.

# Would like to write functions that will output:
    # vector of draft slots
    # histogram with vector of draft slots
    # mean, standard deviation, skewness and kurtosis of a given player's draft slots
    # Monte Carlo visualization of a player's possible draft outcomes


mydf3$draft_slot[mydf3$player == "Tua Tagovailoa"]




# ---- Prospect Info Function ----

# Function to output draft slots for each level in mydf3$player (each player)
    # This iteration, if including the lines commented out, returns the last line of output twice    
    # (the draft slots of the final player)


draftslots <- function(x, name = TRUE, stats = TRUE, viz = TRUE, mcstats = FALSE, raw = FALSE) {
  
    for (i in x) {
      
    if (name == TRUE)   { print(i) }
      
    if (stats == TRUE)  { 
      noquote(print(paste0("Average pick: ", mean(mydf3$draft_slot[mydf3$player == i]))))
      noquote(print(paste0("SD: ", sd(mydf3$draft_slot[mydf3$player == i]))))
      noquote(print(paste0("Top 64 %: ", (length(mydf3$draft_slot[mydf3$player == i])/
                                            (length(mydf3$draft_slot)/64)))))
      noquote(print(paste0("Highest pick: ", min(mydf3$draft_slot[mydf3$player == i]))))
      noquote(print(paste0("Lowest pick: ", max(mydf3$draft_slot[mydf3$player == i]))))
                        }
      
    if (mcstats == TRUE){
      noquote(print(paste0(mean(mydf3$draft_slot[mydf3$player == i]))))
      noquote(print(paste0(sd(mydf3$draft_slot[mydf3$player == i]))))
      # noquote(print(paste0(skewness(mydf3$draft_slot[mydf3$player == i]))))
      # noquote(print(paste0(kurtosis(mydf3$draft_slot[mydf3$player == i]))))
                        }
      
    if (raw == TRUE)    {
    print(mydf3$draft_slot[mydf3$player == i])  # need to figure out to get output to be an object that I
                                                # can do something with
                        }
      
    if (viz == TRUE)    {
      
      print(ggplot(subset(mydf3, mydf3$player == i),
             aes(draft_slot))     + geom_dotplot(binwidth = 1, fill = "blue1") +
        scale_y_continuous(NULL, breaks = NULL) +
        ggtitle(as.character(i), subtitle = "Mock Draft Selections") +
        theme_bw() +
        geom_vline(xintercept = 32.5, color = "green", size = 2) +
        scale_x_continuous("Draft Slot", breaks = c(0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60), 
                           limits = c(0,64)))
    }


    # my_list <- print(mydf3$draft_slot[mydf3$player == i]) # May be used to return a usable object, idk??
  } 
  
    # return(my_list) # This command is what returns the slots for the final prospects again - need to figure out how to 
                    # get a usable object that's in the format of NOT having that second slots for the final prospect
  
}





# -------------------------------------------



# Some demonstrations..
draftslots("K'Lavon Chaisson")
CBs <- c("Jeff Okudah", "CJ Henderson", "Kristian Fulton", "Trevon Diggs", "AJ Terrell", "Jeff Gladney",
         "Jaylon Johnson", "Bryce Hall", "Amik Robertson")
draftslots(CBs, stats = FALSE)
# draftslots("Denzel Mims", name = FALSE)
prospects <- levels(mydf3$player)
# draftslots(prospects)



#   Monte carlo doable from this function?



# ---- Visualizations comparing players to each other ----



# Multiple players on same chart


# ggarrange/multiple charts 





# ---- Monte Carlo Function ----


# Practice - find a prospect with approximately normal distribution

# JaJohnson

draftslots("Jaylon Johnson", viz = FALSE)

# Mean 39.1
# SD = 11.8

runs <- 100000
sims <- rnorm(runs, mean = 39.1, sd = 11.85)
firstround <- sum(sims < 33)/runs
firstround

simsdf <- data.frame(sims)

ggplot(simsdf, aes(x = sims)) + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 32.5, color = "red")



# ---

# Chaisson

# Mean 19.8, sd = 5.3

chaissonmean <- 20.4

chaissonsims <- rnorm(runs, mean = 20.4, sd = 5.43)
firstroundchaisson <- sum(chaissonsims <= 32)/runs
firstroundchaisson

chaissonsimsdf <- data.frame(chaissonsims)

ggplot(chaissonsimsdf, aes(x = chaissonsims)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 32.5, color = "red") +
  geom_vline(xintercept = chaissonmean, color = "purple")




# Approach

# Build a dataset with the following variables
  # player name (player)
  # mean draft slot (mean)
  # sd of draft slots (sd)


# Only include players who are in 50% or more of mocks

# Write function to output player name, mean draft slot, sd of draft slot
# Turn this list into a dataframe
# Write a monte carlo function to perform monte carlo simulation on any players



draftmcstats <- function(x, name = TRUE, mcstats = TRUE) {
  
  my.df <- data.frame(NA, ncol = 4, nrow = length(prospects))
  
  for (i in x) {
    
    if (name == TRUE)   { 
      my.df[i,1] <- as.character(i)
      }
   
    if (mcstats == TRUE){
     
      my.df[i,2] <- mean(mydf3$draft_slot[mydf3$player == i])
      my.df[i,3] <- sd(mydf3$draft_slot[mydf3$player == i])
      
      a <- table(mydf3$player)
      
      my.df[i,4] <- a[i]
    }
    
  }
  
  return(my.df)
}

draft.mc.df <- draftmcstats(prospects)  
    
    


# Build montecarlo function , finally :)


# ---- Monte Carlo Dataset Cleaning ----

  # Thresholds for eligibility
    # in over 40% of mocks (pickcount(i)/max(pickcount) > 0.4)
    
  # Steps
    # 1. Remove top line
    # 2. Rename columns
    # 3. Remove all players below 40%
    # 4. Add positions manually
    # 5. Cut down to top 64
    # 6. Add tier column ( if mean between 0-12, 1, 12-24, 2, 24-36 3, > 36 4)


# Remove first row

drops <- 1
draft.mc.df <- draft.mc.df[-drops,]


# Rename columns

draft.mc.df <- draft.mc.df %>% 
  rename(
    playername = NA.,
    avgpick = ncol,
    sdpick = nrow,
    pickcount = V4
  )


# Eliminate all players not picked in at least 30% of mocks

draft.mc.df <- draft.mc.df[(draft.mc.df$pickcount/max(draft.mc.df$pickcount)) > 0.3,]


# Add positions

draft.mc.df$pos <- NA

pos <- c("EDGE", "CB", "OT", "S", "OT", "WR", "CB", "RB", "WR", "IOL", 
         "EDGE", "CB", "RB", "TE", "EDGE", "RB", "CB", "WR", "IDL", "OT",
         "S", "WR", "LB", "OT", "WR", "IDL", "CB", "OT", "CB", "CB",
         "S", "WR", "RB", "QB", "RB", "IDL", "QB", "OT", "EDGE", "EDGE",
         "QB", "WR", "IDK", "EDGE", "LB", "WR", "CB", "S", "WR", "IOL",
         "LB", "IDL", "OT", "WR", "IDL", "CB", "LB", "OT", "IOL", "IDL",
         "WR", "EDGE", "CB", "OT", "QB", "LB", "S", "EDGE", "EDGE")

draft.mc.df$pos <- pos
draft.mc.df$pos <- factor(draft.mc.df$pos)


# Cut down to top 64, remove (n - 64) lowest avgpicks

draft.mc.df <- draft.mc.df[order(draft.mc.df$avgpick, decreasing = FALSE),]

draft.mc.df <- draft.mc.df[(1:64),]




# ---- Monte Carlo Function ----

  # Write a function that allows you put in a player name and specific pick
    # and it returns a monte carlo visualization of outcomes with a labeled
    # vline at the mean, vline at 32.5, vline at specified pick, and also
    # returns "There is a ___ % chance this player will be available at 
    # (i) pick."



  # May also write a second monte carlo function in which you can input 
  # X number of players and it tells you "there is a ___ % chance that 
  # one of these players will be available at (i) pick" and also shows a 
  # histogram or area plot of all the players overlaid, with the same extra
  # aesthetics as the prior function.


# Single player MC function


mc1 <- function(x, y, name = TRUE, avail = TRUE, text = TRUE, viz = TRUE) {

    runs <- 50000
    
  for (i in x) {
    
    if (name == TRUE) { 
      print(paste0((i)))
    }
    
    # Monte Carlo inputs
    
    playermean  <-      draft.mc.df$avgpick[draft.mc.df$player == i]
    playersd    <-      draft.mc.df$sdpick[draft.mc.df$player == i]
    
    playersims  <- rnorm(runs, mean = playermean, sd = playersd)
    availprob   <- sum(playersims >= y)/runs
    goneprob    <- sum(playersims <= y)/runs
    
    
    if (avail == TRUE) {
      
    a <- availprob
    b <- "available"
    print(paste0(availprob))
    }
    
   # if (gone == TRUE) 
      else { 
      
      a <- goneprob
      b <- "gone"
      print(paste0(goneprob))
      }
    
    if (text == TRUE) {
      print(paste0("This player has a ", as.character(a), 
                   " probability of being ", b, " at pick ", y))
    }
    
    if (viz == TRUE) {
      # print(ggplot... etc)
    }
  }
}




# Player draft means viz

ggplot(draft.mc.df, aes(x = draft.mc.df$avgpick, y = draft.mc.df$pos)) + 
  geom_jitter() + 
  ggtitle("Aggregate Mock Draft Results") + xlab("Draft Slot") + ylab("Position") +
  geom_text(aes(label=draft.mc.df$playername), angle = 90, size = 3) +
  geom_vline(xintercept = 12, color = "green", size = 2) +
  geom_vline(xintercept = 31, color = "green", size = 2) +
  geom_vline(xintercept = 36, color = "green", size = 2) 




# ---- Monte Carlo Function with Multiple inputs x is players, y is pick, z is number of players ---- 


mc2 <- function(x, y, z, name = TRUE, avail = TRUE, text = TRUE) {
  
  runs <- 50000
  
  for (i in x) {
    
    if (name == TRUE) { 
      print(paste0((i)))
    }
    
    # Monte Carlo inputs
    
    
    # availprob    <- vector("numeric", length = z)
    # goneprob     <- vector("numeric", length = z)
    # my.df <- data.frame(availprob = numeric(),
                       # goneprob = numeric())
    
    my.df <- data.frame(matrix(NA, nrow = z, ncol = 2))
    
    
    
    playermean      <-      draft.mc.df$avgpick[draft.mc.df$player == i]
    playersd        <-      draft.mc.df$sdpick[draft.mc.df$player == i]

    
    playersims   <- rnorm(runs, mean = playermean, sd = playersd)
    
    availprob    <- sum(playersims >= y)/runs
    goneprob     <- sum(playersims <= y)/runs
    
    my.df[i,1] <- availprob
    my.df[i,2] <- goneprob
    
    print(my.df)
    
    
    if (avail == TRUE) {
      
      a <- availprob
      b <- "available"
      # print(paste0(availprob))
    }
    
    # if (gone == TRUE) 
    else { 
      
      a <- goneprob
      b <- "gone"
      # print(paste0(goneprob))
    }
    
    if (text == TRUE) {
      print(paste0("This player has a ", as.character(a), 
                   " probability of being ", b, " at pick ", y))
    }
  }
  return(my.df)
}

#########################################################################################

# Currently trying to figure out how to append to DF that gets returned, 
# rather than overwriting. Trying different ways of creating empty datasets, 
# different placement of df assignment (in and out of "if true" clauses)