# indian name canonicalization
library (plyr)

#' @param x a string
#' @param y a string
#' @return compatibility value of x and y, returns 0 if not-compatible, > 0 if compatible. Higher number indicates higher strength
#' @description 
#' current rules for compatibility: 
#' 1. the 2 strings have at least two tokens (each with more than 1 letter) in common
#' 2. all of the tokens in x can be "mapped" to some token in y
#' by "mapping", we mean:
#' initial expansion if a single letter. e.g. A -> AHMAD is a valid mapping
#' suffix or prefix containment if more than a single letter, e.g. OM -> OMPRAKASH, PRAKASH -> OMPRAKASH, SH -> OMPRAKASH
#' (suffix match is not allowed if only a single letter e.g. A cannot map to SANIA, but IA can)
#' note: multiple tokens in x can map to the same token in y
#' e.g. OM and PRAKASH will both map to OMPRAKASH
compatibility <- function (x, y) {
  if (is.na(x) || is.na(y))
    return (0)
  
  xtokens <- strsplit(x, ' ')[[1]]
  ytokens <- strsplit(y, ' ')[[1]]
  
  # if more than 2 multi-letter tokens are common, return true straightaway
  # this sometimes causes noise when there are 2 common tokens in x and y, e.g. MULAYAM SINGH YADAV and RAGHUVIR SINGH YADAV
  # but it is rare enough that we keep this rule
  commonMultiLetterTokens <- intersect(xtokens [nchar(xtokens) > 1], ytokens [nchar(ytokens) > 1])
  if (length (commonMultiLetterTokens) >= 2)
    return (length (commonMultiLetterTokens))
  
  # returns whether each xtoken can be mapped to a ytoken? (i.e. is a suffix or prefix or initial of)
  # this also takes care of cases like OMPRAKASH being split OM and PRAKASH
  canTokensMap <- function(xtokens, ytokens) {
    # check that each token in x maps to a token in y
    for (i in 1:length(xtokens)) {
      xt = xtokens[i]  
      matchFound = FALSE
      for (j in 1:length(ytokens)) {
        yt = ytokens[j]
        if (startsWith (yt, xt)) {
          matchFound = TRUE
          break
        } 
        
        # allow yt to end with xt, but only if xt is more than 1 char!
        if (nchar(xt) > 0 && endsWith (yt, xt)) {
          matchFound = TRUE
          break
        }
      }
      if (!matchFound)
        return (FALSE)
    }
    return (TRUE)
  }
  
  # we're not sure about the order, so try mapping both x tokens to y tokens and vice versa
  if (canTokensMap(xtokens, ytokens) || canTokensMap(ytokens, xtokens))
    return (1)
  else
    return (0)
}

# tests
compatibility ('JUGAL KISOR', 'JUGAL KISOR SARMA') > 0
compatibility ('IERAM REDI SUBA WENKATA', 'I REDI SUBA W') > 0
compatibility ('ADWANI LAL KRISNA', 'L K ADWANI') > 0
compatibility ('AJMAL SIRAJUDIN', 'AJMAL SIRAJ UDIN') > 0
compatibility ('AJMAL SIRAJUDIN', 'AJMAL SERAJ UDIN') == 0
compatibility ('GAJENDRA SEKAWAT SING', 'GAJENDRASING SEKAWAT') > 0

