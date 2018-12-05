#'@title Emoji-to-Text Helper
#'
#'
#'@description
#'\code{emojitotext_for_one} replaces emojis and accents with corresponding english characters/phrases.
#'
#'
#'@details
#'This function identifies emojis and accents within a text string and replaces them
#'with their corresponding english characters/phrases using \code{emoji_df} and \code{accent_df} reference dataframes.
#'
#'
#'@param text a character string.
#'@param accents a boolean which is TRUE if user wants to replace accents, and FALSE otherwise.
#'@param delete a boolean which is TRUE if user wants to delete bytes that are not matched to an emoji or accent.
#'
#'
#'@return a character string.
#'@import stringr
#'
#'@author Kelly Jamrog <kjamrog@@wesleyan.edu>
#'@author Jacob Kuruvilla <jkuruvilla@@wesleyan.edu>
#'@author Kim Pham <spham@@wesleyan.edu>
#'
#'
#'@examples
#'test <- iconv("Hi Röb, whats up 😬", from = "UTF8", to = "ascii", sub = "byte")
#'emojitotext_for_one(test, TRUE, FALSE)


emojitotext_for_one <- function(text, accents, delete){
  for (i in 1:length(emoji_df$byte)) {
    if (str_detect(text, emoji_df$byte[i])) {
      text <- str_replace_all(text, emoji_df$byte[i], emoji_df$words[i])
    }
    next
  }

  if(accents == TRUE) {
    for (i in 1:length(accent_df$byte)) {
      if (str_detect(text, accent_df$byte[i])) {
        text <- str_replace_all(text, accent_df$byte[i], accent_df$words[i])
      }
      next
    }
  }

  if(delete == TRUE) {
    text <- gsub("<[A-Za-z0-9]{2}>", "", text)
  }

  return(text)
}
