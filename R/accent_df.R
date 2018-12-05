#'@title Accent Reference Dataframe
#'
#'@description
#'A dataframe containing accent characters and symbols, their unicodes, and corresponding ASCII characters.
#'If a character is a symbol, it gets replaced by an empty string.
#'
#'@docType data
#'@keywords datasets
#'@name accent_df
#'@usage accent_df
#'
#'@format A data frame with 222 rows and five variables
#'The variables are as follows:
#'\describe{
#'    \item{unicode}{unicode for accent/symbol as string}
#'    \item{words}{ASCII character representation for accent/symbol}
#'    \item{character}{accent/symbol}
#'    \item{byte}{byte sequence for accent/symbol}
#'    \item{nchar}{length of byte sequence}
#'}
#'
#'@source The data was sourced from \href{https://en.wikipedia.org/wiki/List_of_Unicode_characters}.
#'
#'
#'@examples
#'head(accent_df)


NULL
