#' @title Watch R Session to Detect If Idle Sessions
#' @param delay Delay between session polling
idle_update <- function(delay = 15 * 60) {
  last_history <- .repromonkey$history_hash
  if (is.null(last_history)) last_history <- ""
  .repromonkey$history_hash <- hash_history()
  .repromonkey$idle <- identical(last_history, .repromonkey$history_hash)
  if (!isTRUE(.repromonkey$banished)) {
    later::later(function() idle_update(delay), delay)
  }
}

#' @title Is the R Session Idle?
#' @keywords internal
is_session_idle <- function() {
  isTRUE(.repromonkey$idle)
}

hash_history <- function() {
  file1 <- tempfile("Rrawhist")
  on.exit(unlink(file1))
  utils::savehistory(file1)
  digest::digest(file1, file = TRUE)
}
