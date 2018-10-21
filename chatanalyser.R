## This program analyses WhatsApp chat data.
## Chat data formats: "dd/mm/yy, HH:MM PM - Name: Messages"
## Note that "dd/mm/yy" can be "d/mm/yy", "dd/m/yy" or "d/m/yy"
## and that "HH:MM PM" can be "H:MM PM"
## Media data format: "<Media omitted>"
## Generally works for group chats, but wordcloud generation does not work
## for group chat currently.
##
## Copyright (C) 2018 Cheong Jian Wei
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.


# Import library
library(ggplot2)   # For plotting
library(quanteda)  # For words processing


# Function to process contents of text file into a data frame and save it
# Generate data frame with columns: date, time, name, text, day, media, charcounts
# day converts the date to day in a week, media is a flag to tell if line
# is media or message, charcounts gives the character counts of the line
process_file <- function(filePath) {

    # Open file as "read"
    con <- file(filePath, "r")
    count <- 0

    # Loop to read line by line (will auto end when file end)
    while (TRUE) {

        # Print progress
        count <- count + 1
        if (count %% 1000 == 0) {
            print(paste("Processing line number:", count))
        }

        # Read line n=1 at a time
        line <- readLines(con, n = 1, encoding="UTF-8")

        # Ignore blank lines
        if (length(line) == 0) {
            break
        }

        # Find indices of colons, hypens and commas for formatting
        colonIndex <- which(strsplit(line, "")[[1]] == ":")
        hyphenIndex <- which(strsplit(line, "")[[1]] == "-")
        commaIndex <- which(strsplit(line, "")[[1]] == ",")

        # Set NA values to 0 to prevent error in the following if condition statement
        # NA values appear when no colon or hyphen is detected
        colonIndex[1] <- ifelse(is.na(colonIndex[1]), 0, colonIndex[1])
        colonIndex[2] <- ifelse(is.na(colonIndex[2]), 0, colonIndex[2])
        hyphenIndex <- ifelse(is.na(hyphenIndex[1]), 0, hyphenIndex)

        # Create new message row by checking format
        # Can check for more formatting to be safer
        if (hyphenIndex[1] - 4 == colonIndex[1] && colonIndex[1] != 0) {

            # Store date, time, name, and text data based on format
            date <- as.Date(substr(line, 0, commaIndex[1] - 1), "%d/%m/%y")
            time <- strptime(substr(line, commaIndex[1] + 2, hyphenIndex[1] - 2), "%H%M")
            time <- format(time, format="%H:%M")
            name <- substr(line, hyphenIndex[1] + 2, colonIndex[2] - 1)
            text <- substring(line, colonIndex[2] + 2)

            # Create data frame if first iteration
            # stringsAsFactors=FALSE is needed for some reason
            if (count == 1) {
                chatData <- data.frame(date, time, name, text, stringsAsFactors=FALSE)
            }
            # Else append to existing data frame
            else {
                chat <- data.frame(date, time, name, text)
                chatData <- rbind(chatData, chat)
            }
        }
        # Append message when not new message
        else {
            chatData$text[length(chatData$text)] <- paste(chatData$text[length(chatData$text)], line, sep=" ")
        }
    }

    # Close file
    close(con)

    # Create new columns for extra data
    chatData$day <- weekdays(chatData$date)
    chatData$media <- ifelse(chatData$text == "<Media omitted>", "Media", "Messages")
    chatData$charcounts <- nchar(chatData$text)

    # Order the days from Sun to Sat (ordering the factors?)
    chatData$day <- factor(chatData$day, weekdays(as.Date('1970-01-03') + 1:7))

    # Save data
    saveRDS(chatData, file="chatData.rds")

    # Check for errors in data processing. Set to FALSE to comment out
    # Will write a csv file with rows containing NA values
    # Not really sure if working
    if (FALSE) {
        errorData <- chatData[rowSums(is.na(chatData)) > 0]
        write.csv(errorData, file = "errorData.csv")
    }

}


# Function to analyse data in data frame
# Plot messages against date, day, time and name
# Will analyse text and generate wordcloud if wordcloud == TRUE
analyse_chat <- function(chatData, wordcloud) {

    print("Analysing data...")

    # Data cleaning for text analysis (omit media, urls, emojis)
    chatData_msg <- subset(chatData, chatData$media == "Messages")
    chatData_msg$text <- gsub("http\\S+\\s*", "", chatData_msg$text)
    chatData_msg$text <- gsub("[^[:ascii:]]", "", chatData_msg$text, perl=T)

    # Text analysis with library(quanteda) and wordcloud generation of 2 person
    if (wordcloud == TRUE) {
        # Split data based on name
        chatData_msgSplit <- split(chatData_msg, chatData_msg$name)

        # Tokenization of texts (break into sentences)(the removes doesnt seems to work)
        # what = 'sentence' or 'word'
        # Which person is which requires checking (depends on how split() work)
        tokensA <- tokens(chatData_msgSplit[[1]]$text, what = "sentence",
                        remove_numbers = TRUE, remove_punct = TRUE,
                        remove_symbols = TRUE, remove_hyphens = TRUE,
                        remove_url = TRUE)
        tokensB <- tokens(chatData_msgSplit[[2]]$text, what = "sentence",
                        remove_numbers = TRUE, remove_punct = TRUE,
                        remove_symbols = TRUE, remove_hyphens = TRUE,
                        remove_url = TRUE)

        # Change all to lower case and remove stopwords
        tokensA <- tokens_tolower(tokensA)
        tokensA <- tokens_select(tokensA, stopwords(), selection="remove")
        tokensB <- tokens_tolower(tokensB)
        tokensB <- tokens_select(tokensB, stopwords(), selection="remove")

        # Stem the words (ate, eats, eaten, eating -> eat) (Only works for words)
        #tokensA <- tokens_wordstem(tokensA, language="english")
        #tokensB <- tokens_wordstem(tokensB, language="english")

        # Creating DFM from tokens data and convert to matrix for manipulation
        tokensA_DFM <- dfm(tokensA, tolower = FALSE)
        tokensB_DFM <- dfm(tokensB, tolower = FALSE)
        tokensA_DFMMatrix <- as.matrix(tokensA_DFM)
        tokensB_DFMMatrix <- as.matrix(tokensB_DFM)

        # Sort indices of tokens frequency in decending order
        idA <- order(colSums(tokensA_DFMMatrix), decreasing = TRUE)
        idB <- order(colSums(tokensB_DFMMatrix), decreasing = TRUE)

        # Printing 100 most frequent words and phrases for checking for error
        tokensAName <- colnames(tokensA_DFMMatrix)[idA]
        print(tokensAName[1:100])
        tokensBName <- colnames(tokensB_DFMMatrix)[idB]
        print(tokensBName[1:100])

        # Generate wordcloud
        # A small min_count is better for sentences, while a large one is better for words
        # PDF file gives a clearer image
        pdf(file="Awordcloud.pdf")
        wordcloud <- textplot_wordcloud(tokensA_DFM, min_size=0.5,
        max_size=4, min_count=2, max_words=100, color="red",
        rotation=0.1)

        pdf(file="Bwordcloud.pdf")
        wordcloud <- textplot_wordcloud(tokensB_DFM, min_size=0.5,
        max_size=4, min_count=2, max_words=100, color="blue",
        rotation=0.1)
    }

    # Bar plot of messages per day
    daybar <- ggplot(chatData, aes(day, fill=name)) +
    geom_bar(position = "dodge") +
    ylab("Messages")
    ggsave(filename="daybar.png", plot=daybar)

    # Bar plot of total messages
    totalbar <- ggplot(chatData, aes(name, fill=media)) +
    geom_bar(position = "dodge") +
    geom_text(stat='count', aes(label=..count..), vjust=-1, position=position_dodge(1)) +
    ylab("Counts")
    ggsave(filename="totalbar.png", plot=totalbar)

    # Bar plot of messages against date
    datebar <- ggplot(chatData, aes(date, fill=name)) +
    geom_bar() +
    ylab("Counts")
    ggsave(filename="datebar.png", plot=datebar)

    # Clock plot of messages against time
    # Cleaning time data to hourly frequency
    time <- table(substr(chatData$time, 1, regexpr(':', chatData$time)-1))
    hourData <- data.frame(time)

    # Function to plot a clock (copied online)
    clock.plot <- function (x, col = rainbow(n), ...) {
        if( min(x)<0 ) x <- x - min(x)
        if( max(x)>1 ) x <- x/max(x)
        n <- length(x)
        if(is.null(names(x))) names(x) <- 0:(n-1)
        m <- 1.05
        plot(0, type = 'n', xlim = c(-m,m), ylim = c(-m,m), axes = F, xlab = '', ylab = '', ...)
        a <- pi/2 - 2*pi/200*0:200
        polygon( cos(a), sin(a) )
        v <- .02
        a <- pi/2 - 2*pi/n*0:n
        segments( (1+v)*cos(a), (1+v)*sin(a), (1-v)*cos(a), (1-v)*sin(a) )
        segments( cos(a), sin(a),0, 0, col = 'light grey', lty = 3) 
        ca <- -2*pi/n*(0:50)/50
        for (i in 1:n) {
            a <- pi/2 - 2*pi/n*(i-1)
            b <- pi/2 - 2*pi/n*i
            polygon( c(0, x[i]*cos(a+ca), 0), c(0, x[i]*sin(a+ca), 0), col=col[i] )
            v <- .1
            text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
        }
    }

    # Calling the clock plot function
    # PDF file gives a clearer image
    pdf(file="clockplot.pdf")
    clock.plot(hourData$Freq)

}


# Set path of file
filePath <- "C:\\Users\\solar\\Desktop\\Jianwei.txt"

# Call process file function to save data frame
# Can comment out after processing file as data frame is saved
process_file(filePath)

# Load data frame and analyse
# wordcloud = TRUE will only generate wordclouds for 2 person
chatData <- readRDS(file="E:\\MyDocuments\\Projects\\R\\chatData.rds")
analyse_chat(chatData, wordcloud = TRUE)
