# WhatsApp-Chat-Analyser

Program is written in R.

## About
This program analyses WhatsApp chat data and plot statistics such as:
+ Barplot of number of messages against dates
+ Barplot of number of messages and media from different person in the chat
+ A normalised clock plot showing the time with the most messages
+ Barplot of messages against days
+ Word clouds for phrases and words

Input file is a .txt file of the conversations. The conversation .txt file can be obtained by tapping the three dots at the top right of a chat followed by "More" and "Export chat". Choose "Without Media".

Chat data formats: "dd/mm/yy, HH:MM PM - Name: Messages".

e.g. "13/9/15, 8:33 PM - Bob: Hello world!"

Note that "dd/mm/yy" can be "d/mm/yy", "dd/m/yy" or "d/m/yy" and that "HH:MM PM" can be "H:MM PM".

Media data format: "\<Media omitted\>".
  
Generally works for group chats, but wordcloud generation does not work for group chat currently.

## Requirements
Libraries:
+ [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html)
+ [quanteda](https://cran.r-project.org/web/packages/quanteda/index.html)

Packages can be installed in the R shell with:
```
install.packages("ggplot2")
install.packages("quanteda")
```
ggplot2 is used for plotting of graphs, and quanteda is used to tokenise and analyse the chat data.

## Examples

![screenshot](https://github.com/SataJW/WhatsApp-Chat-Analyser/blob/master/images/dayplot.PNG)

![screenshot](https://github.com/SataJW/WhatsApp-Chat-Analyser/blob/master/images/clockplot.PNG)

## License
See the [LICENSE](https://github.com/SataJW/WhatsApp-Chat-Analyser/blob/master/LICENSE.md) file for license rights and limitations.
