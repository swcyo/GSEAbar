##' wraping long string to multiple lines
##'
##'
##' @title str_wrap
##' @param string input string
##' @param width the maximum number of characters before wrapping to a new line
##' @return update strings with new line character inserted
##' @export
##' @author Guangchuang Yu and Erqiang Hu
str_wrap <- function(string, width = getOption("width")) {
  # x <- gregexpr(' ', string)
  # vapply(seq_along(x),
  #        FUN = function(i) {
  #            y <- x[[i]]
  #            n <- nchar(string[i])
  #            len <- (c(y,n) - c(0, y)) ## length + 1
  #            idx <- len > width
  #            j <- which(!idx)
  #            if (length(j) && max(j) == length(len)) {
  #                j <- j[-length(j)]
  #            }
  #            if (length(j)) {
  #                idx[j] <- len[j] + len[j+1] > width
  #            }
  #            idx <- idx[-length(idx)] ## length - 1
  #            start <- c(1, y[idx] + 1)
  #            end <- c(y[idx] - 1, n)
  #            words <- substring(string[i], start, end)
  #            paste0(words, collapse="\n")
  #        },
  #        FUN.VALUE = character(1)
  # )
  result <- vapply(string,
                   FUN = function(st) {
                     words <- list()
                     i <- 1
                     while(nchar(st) > width) {
                       if (length(grep(" ", st)) == 0) break
                       y <- gregexpr(' ', st)[[1]]
                       n <- nchar(st)
                       y <- c(y,n)
                       idx <- which(y < width)
                       # When the length of first word > width
                       if (length(idx) == 0) idx <- 1
                       # Split the string into two pieces
                       # The length of first piece is small than width
                       words[[i]] <- substring(st, 1, y[idx[length(idx)]] - 1)
                       st <- substring(st, y[idx[length(idx)]] + 1, n)
                       i <- i + 1
                     }
                     words[[i]] <- st
                     paste0(unlist(words), collapse="\n")
                   },
                   FUN.VALUE = character(1)
  )
  names(result) <- NULL
  result
}

##' Detect the presence or absence of a pattern at the beginning or end of a string or string vector.
##'
##'
##' @title str_starts
##' @rdname str-starts-ends
##' @param string input string
##' @param pattern pattern with which the string starts or ends
##' @param negate if TRUE, return non-matching elements
##' @return a logical vector
##' @export
##' @author Guangchuang Yu
str_starts <- function(string, pattern, negate=FALSE) {
  pattern <- paste0('^', pattern)
  str_detect(string, pattern, negate)
}

##' @rdname str-starts-ends
##' @export
str_ends <- function(string, pattern, negate=FALSE) {
  pattern <- paste0(pattern, '$')
  str_detect(string, pattern, negate)
}

##' @importFrom stats setNames
str_detect <- function(string, pattern, negate) {
  res <- setNames(
    vapply(string, grepl, pattern=pattern,
           FUN.VALUE=logical(1)),
    NULL)
  if (negate) res <- !res
  return(res)
}



##' Get the distance of the label
##'
##' @param dimension one of 1 and 2
##' @param label_location label_location
##' @noRd
get_label_diss <- function(dimension, label_location) {
  nn <- nrow(label_location)
  label_dis <- matrix(NA, nrow = nn, ncol = nn)
  colnames(label_dis) <- rownames(label_dis) <- label_location$label
  for (i in seq_len(nn - 1)) {
    for (j in (i + 1):nn) {
      label_dis[i ,j] <- label_location[i, dimension] -  label_location[j, dimension]
    }
  }
  label_diss <- reshape2::melt(label_dis)
  label_diss <- label_diss[label_diss[,1] != label_diss[,2], ]
  label_diss <- label_diss[!is.na(label_diss[,3]), ]
  label_diss[, 1] <- as.character(label_diss[, 1])
  label_diss[, 2] <- as.character(label_diss[, 2])
  return(label_diss)
}



# adjust_location <- function(label_location, x_adjust, y_adjust) {
# label_diss_x <- get_label_diss(1, label_location)
# label_diss_y <- get_label_diss(2, label_location)

# label_diss_large <- which(abs(label_diss_y[, 3]) < y_adjust) %>%
# intersect(which(label_diss_y[, 3] > 0)) %>%
# intersect(which(abs(label_diss_x[, 3]) < x_adjust))

# label_diss_small <- which(abs(label_diss_y[, 3]) < y_adjust) %>%
# intersect(which(label_diss_y[, 3] < 0)) %>%
# intersect(which(abs(label_diss_x[, 3]) < x_adjust))

# label_location[label_diss_y[label_diss_large, 1], 2] <- label_location[label_diss_y[label_diss_large, 2], 2] + y_adjust
# label_location[label_diss_y[label_diss_small, 1], 2] <- label_location[label_diss_y[label_diss_small, 2], 2] - y_adjust
# return(label_location)
# }



#' default_labeller
#'
#' default labeling function that uses the
#' internal string wrapping function `GSEAbar::str_wrap`
#' @noRd
default_labeller <- function(n) {
  function(str){
    str <- gsub("_", " ", str)
    str_wrap(str, n)
  }
}


