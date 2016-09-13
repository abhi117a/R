# run the following line once to install the package random
# install.packages("random", dependencies = TRUE)
require(random)
craps <- function() {
  field <- c(2,3,12)
  wins <- c(7,11)
  loseCount <- 0
  startAmt <- 100
  bank <- 900
  gameCount <- 11
  while(gameCount > 0){
  initialRoll <- as.integer(colSums(randomNumbers(2, 1, 6, 1)))
  if (initialRoll %in% field){
    loseCount <- loseCount + 1
    if(loseCount>0){
      bank <- bank-(startAmt)
      startAmt <- startAmt*2
    }
    loseCount <- loseCount + 1
    gameCount <- gameCount -1
    bank
    out <- 0
  }
  else if (initialRoll %in% wins){
    startAmt <- startAmt*2
    bank <- bank + startAmt
    startAmt <- 100
    gameCount <- gameCount - 1 
    loseCount <- 0
    bank
    out <- 1
  }
  else {
    point <- initialRoll
    # now run the game until you get 7 or point again
    roll <- 0
    while(roll!= point && roll!=7) {
      roll <- as.integer(colSums(randomNumbers(2, 1, 6, 1)))
    }
    if (roll == point){
      startAmt <- startAmt*2
      bank <- bank + startAmt
      startAmt <- 100
      gameCount <- gameCount - 1 
      loseCount <- 0
      bank
      out <- 1
    }
    else if (roll == 7){
      loseCount <- loseCount + 1
      if(loseCount>0){
        bank <- bank-(startAmt)
        startAmt <- startAmt*2
      }
      loseCount <- loseCount + 1
      gameCount <- gameCount -1
      bank
      out <- 0
    }
    bank <- bank
    #print(bank)
    #out
  }
  }
  print(bank)
}
craps()
