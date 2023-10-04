# Authors: Yinjia Chen (s2520995@ed.ac.uk), Yiwen Xing (s2530703@ed.ac.uk), Chaoyue Yan (s2572326@ed.ac.uk)
# Contributions:
#   Yinjia Chen:
#   Yiwen Xing:
#   Chaoyue Yan:

# Q.3
# setwd("txtfiles") ## comment out of submitted
a <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73)  ## read the text into a
a <- gsub("_(", "", a, fixed = TRUE)  ## remove "_("

# Q.4
split_punc <- function(words, punc) {  ## declaration of function "split_punc"
  punc_idx <- grep(punc, words, fixed = TRUE)  ## find the index of words in "words" containing "punc"
  new_words <- rep("", length(words) + length(punc_idx))  ## create a all-blank-strings vector, with size of the original a plus count of targeted punctuations
  words[punc_idx] <- gsub(punc, "", words[punc_idx], fixed = TRUE)  ## remove "punc" in their original appearing location.
  ## Note that fied=TRUE ensures punc to not be recognized as regular expressions

  new_punc_idx <- punc_idx + 1:length(punc_idx)  ## specify the new single punc position in the new vector
  new_words[new_punc_idx] <- punc  ## insert the punc in the right position
  new_words[-new_punc_idx] <- words  ## insert the words in the corresponding else where
  return(new_words)  ## return the new word vector
}

# Q.5
a <- split_punc(a, ",")  ## pull the "," out with defined function split_punc
a <- split_punc(a, ".")
a <- split_punc(a, ";")
a <- split_punc(a, "!")
a <- split_punc(a, ":")
a <- split_punc(a, "?")  ## obtain the final a: the word vector the article splited into, with punctuations pulled out

# Q.6
a_lower <- tolower(a)  ## a with all words in lower case
dict <- unique(a_lower)  ## dictionary of the article, i.e. each unique word only occurs once here
a_idx <- match(a_lower, dict)  ## a_lower with each word represented as its word order in dict
freq <- tabulate(a_idx)  ## the occuring times of each word in the a_lower, in the word order of dict

thr <- 0
repeat {  ## searching process of the threshold of sequence of "common words"
  ## seach starts from 0
  common_word_counts <- length(which(freq > thr))  ## obtain the the count of common words with current threshold
  if (common_word_counts < 1000) {  ## if the common word count first exceed 1000, judge:
    if (1000 - common_word_counts > length(which(freq > thr - 1)) - 1000) {  
      ## thr & thr - 1 are two thresholds getting common word count around 1000 most closely, compare which one is closer
      thr <- thr - 1  ## if thr - 1 is closer, set thr - 1
    }
    break
  }
  thr <- thr + 1  ## iterate thr
}
b <- dict[which(freq > thr)]  ## common word dictionary with threshold thr (different from "dict")

# Q.7
common_words_idx <- match(a_lower, b)  ## a_lower with each word represented as its word order in b (display NA for words not in b)

col1 <- common_words_idx  ## first column in T
col2 <- c(common_words_idx[-1], NA)  ## second column in T (shift up one step)
col3 <- c(common_words_idx[-c(1, 2)], c(NA, NA))  ## third column in T (shift up teo steps)
ma_t <- cbind(col1, col2, col3)  ## combine three columns into one matrix (each row: three adjacent words in article, i.e. a word triplet in article)
## cbind returns a matrix, not df
ma_t <- ma_t[which(!is.na(rowSums(ma_t))), ]  # delete rows containing NA

ma_p <- cbind(col1, col2)  ## combine two columns into one matrix (each row: two adjacent words in article, i.e. a word pair in article)
ma_p <- ma_p[which(!is.na(rowSums(ma_p))), ]  ## delete rows containing NA

# Q.8

generate <- function(n) {  ## function to generate an n-words article based on the given texts under standard criteria
  last2 <- NA  ## the second previous word
  last1 <- NA  ## the previous word
  article <- rep(NA, n)  ## n-legth vector for storing the newly generated article
  for (i in 1:n) {
    tri_gram_candidates <-
      matrix(ma_t[which(ma_t[, 1] == last2 & ma_t[, 2] == last1), ], ncol = 3)  ## subtract the triplet submatrix with the words in first two columns equal to the given previous two words
    bi_gram_candidates <-
      matrix(ma_p[which(ma_p[, 1] == last1), ], ncol = 2)  ## subtract the pair submatrix with the words in first two columns equal to the given previous two words
    # matrix(): demanding bi & tri to be 2-d matricies even if having no rows
    if (nrow(tri_gram_candidates) != 0) {
      candidates <- tri_gram_candidates[, 3]  ## if rows exist in triplet submatrix, subtract candidate words in the third column
    } else if (nrow(bi_gram_candidates) != 0) {
      candidates <- bi_gram_candidates[, 2]  ## else if rows exist in pair submatrix, subtract candidate words in the third column
    } else {
      candidates <- common_words_idx[which(!is.na(common_words_idx))]  ## else candidate words are all non-NA words
    }
    cur_word <- sample(x = candidates, size = 1, replace = TRUE)
    ## sample a word in candidates with each of them in identical probability, thus the prob of picking a specific word is exactly the occuring times of that word in candidates
    ## Note that replce=True means the chosen elsement can be chosen again next time
    article[i] <- b[cur_word]  ## concatenate the selected word in the new article
    last2 <- last1
    last1 <- cur_word  ## renew the previous words for looping
  }
  paste(article, collapse = " ")  ## return the new article in a single string by joining each ward with a space
}

generate(50)  ## generate a 50-word article and print it, under the standard criteria

generate_only_from_freq <- function(n) {  ## function to generate an n-words article based on the given texts solely under word frequencies
  article <- rep(NA, n)
  for (i in 1:n) {
    candidates <- common_words_idx[which(!is.na(common_words_idx))]
    cur_word <- sample(x = candidates, size = 1, replace = TRUE)
    article[i] <- (b[cur_word])
  }
  paste(article, collapse = " ")
}

generate_only_from_freq(50)  ## generate a 50-word article and print it, only based on word frequencies

# Q.10
common_words_cap_freq <- tabulate(match(a_lower[a != a_lower], dict))  ## the occuring times of each capitalized word in the article, in the word order of dict
common_words_freq <- tabulate(match(a_lower, dict))  ## the occuring times of each word in the a_lower, in the word order of dict

common_words_cap_prob <- common_words_cap_freq / common_words_freq  ## the capitalizing prob of each word in the article, in the word order of dict
cap_word_set <- dict[which(common_words_cap_prob > 0.5)]  ## the words with capitalizing prob greater than 0.5

generate_cap <- function(n) {
  last2 <- NA
  last1 <- NA
  article <- rep(NA, n)
  for (i in 1:n) {
    tri_gram_candidates <-
      matrix(ma_t[which(ma_t[, 1] == last2 & ma_t[, 2] == last1), ], ncol = 3)
    bi_gram_candidates <-
      matrix(ma_p[which(ma_p[, 1] == last1), ], ncol = 2)
    if (nrow(tri_gram_candidates) != 0) {
      candidates <- tri_gram_candidates[, 3]
    } else if (nrow(bi_gram_candidates) != 0) {
      candidates <- bi_gram_candidates[, 2]
    } else {
      candidates <- common_words_idx[which(!is.na(common_words_idx))]
    }
    cur_word <- b[sample(x = candidates, size = 1, replace = TRUE)]
    if (cur_word %in% cap_word_set) {  ## capitalize the word if it is in cap_word_set
      cur_word <- paste(toupper(substr(cur_word, 1, 1)), substr(cur_word, 2, nchar(cur_word)), sep = "")
    }
    article[i] <- cur_word
    last2 <- last1
    last1 <- cur_word
  }
  paste(article, collapse = " ")
}

generate_cap(50)  ## generate a 50-word article and print it, under the standard criteria with capitalization
