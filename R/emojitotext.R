#'@title Emoji-to-Text
#'
#'
#'@description
#'\code{emojitotext} applies \code{emojitotext_for_one} over a vector to replace
#'emojis and accents with corresponding english characters/phrases.
#'
#'
#'@details
#'This function identifies emojis and accents within a text string and replaces them
#'with their corresponding english characters/phrases using \code{emoji_df} and \code{accent_df} reference dataframes.
#'
#'
#'@param vector a vector of character strings.
#'@param accents a boolean which is TRUE if user wants to replace accents, and FALSE otherwise.
#'@param delete a boolean which is TRUE if user wants to delete bytes that are not matched to an emoji or accent.
#'
#'@import dplyr
#'@return a vector of character strings.
#'
#'@author Kelly Jamrog <kjamrog@@wesleyan.edu>
#'@author Jacob Kuruvilla <jkuruvilla@@wesleyan.edu>
#'@author Kim Pham <spham@@wesleyan.edu>
#'
#'@export
#'@examples
#'test <- c("Hi Röb, whats up 😬")
#'emojitotext(test, TRUE, FALSE)


emojitotext <- function(vector, accents = FALSE, delete = FALSE) {
  # add catch for if do not enter a string
  if(!inherits(vector, "character")) {stop("Vector must be type char")}

  unlist(lapply(vector, function(x){
    x <- iconv(x, from = "UTF8", to = "ascii", sub = "byte")
    emojitotext_for_one(x, accents, delete)
  }))
}
