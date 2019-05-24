#' Set up an experiment
#' 
#' @export
#' @param name The name of the experiment
#' @param use,try the use and try code blocks
#' @return an object of class `Experiment`
#'
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{use()}}{
#'       use code block 
#'     }
#'     \item{\code{try()}}{
#'       try code block 
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples
#' library(scientist)
#' res <- Experiment$new(name = "jane")
#' res$use({
#'   x = 5
#'   x^2
#' })
#' res$try({
#'   y = 5
#'   y^2
#' })
#' res
#' res$run()
#' res$use_result
#' res$try_result
#' res$result()
Experiment <- R6::R6Class(
  "Experiment",
  public = list(
    name = NA,
    use_block = NULL,
    try_block = NULL,
    use_result = NULL,
    try_result = NULL,
    times = list(),

    initialize = function(name) {
      self$name <- name
    },

    print = function() {
      cat(paste0("<Experiment> ", self$name), sep = "\n")
      # cat(paste0("  use: ", self$use), sep = "\n")
      invisible(self)
    },

    call_block = function(...) {
      urt <- system.time(self$use_result <- private$r(self$use_block))
      trt <- system.time(self$try_result <- private$r(self$try_block))
      self$times <- list(
        use_time = urt[["elapsed"]], try_time = trt[["elapsed"]])
    },

    use = function(...) self$use_block <- lazyeval::lazy_dots(...),
    try = function(...) self$try_block <- lazyeval::lazy_dots(...),
    run = function() {
      self$call_block()
    },
    result = function() {
      list(
        use = list(
          result = self$use_result,
          time = self$times$use_time
        ),
        try = list(
          result = self$try_result,
          time = self$times$try_time 
        )
      )
    }

  ),

  private = list(
    r = function(m) {
      callr::r(function(w) lazyeval::lazy_eval(w), args = list(m))
    }
  )
)
