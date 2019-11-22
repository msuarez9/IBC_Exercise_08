#Exercise 8 Assignment
#Jacob Fry and Mariana Suarez

#PART 1: Score Progression for UW vs. MSU

#Load in original dataframe
df=read.table("UWvMSU_1-22-13.txt", sep =  , 
              header = TRUE, stringsAsFactors = FALSE)

#Create new dataframe with three columns: time, msu cumulative score, uw cumulative score
result_df <- data.frame(matrix(ncol = 3, nrow = 51)) 
x <- c("time", "uwcum", "msucum")
colnames(result_df) <- x

#First row corresponds to time 0 where each time has score 0
result_df$time[1] <- 0
result_df$uwcum[1] <- 0
result_df$msucum[1] <- 0

#Fill in cumulative score columns based on data in original df
for (i in 1:nrow(df)){
  result_df$time[i+1] <- df$time[i]
  if (df$team[i] == "UW") {
    result_df$uwcum[i+1] <- df$score[i] + result_df$uwcum[i]
    result_df$msucum[i+1] <- result_df$msucum[i]
  }
  else if (df$team[i] == "MSU"){
    result_df$uwcum[i+1] <- result_df$uwcum[i]
    result_df$msucum[i+1] <- df$score[i] + result_df$msucum[i]
  }
}

#Plot
y1 <- result_df$uwcum
y2 <- result_df$msucum
x <- result_df$time

plot(main = "Score Progression for UW vs. MSU", x = x, y = y1, col = "blue", xlab = "Time (min)", ylab = "Points", type ="l" )
lines(x = x, y = y2, col = "red")
grid()




#PART 2: Guess my number

#Generate random number between 1 and 100
random.num <- sample(1:100,1)

#User input guess
print("I'm thinking of a number 1-100...")
user.num <- readline(prompt="Guess: ")
user.num <- as.integer(user.num)

#Counter to count number of user-input guesses
counter <- 1

#Keep asking the user for guess until he/she guesses correctly or after guessing 10 times
while (user.num!=random.num && counter < 10){
  if (user.num < random.num){
    print("Higher")
    user.num <- readline(prompt="Guess: ")
    user.num <- as.integer(user.num)
    counter <- counter + 1
  }
  else if (user.num > random.num){
    print("Lower")
    user.num <- readline(prompt="Guess: ")
    user.num <- as.integer(user.num)
    counter <- counter + 1
  }
}

#Display if correct
if (user.num == random.num){
  print("Correct!")
}
#Display if user hit 10 guesses without being successful
if (counter >= 10){
  print("Too many tries. You failed")
}


