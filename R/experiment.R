#' Set up an experiment
#'
#' @export
#' @param name The name of the experiment
#' @param error_on_mismatch (logical) whether to error on mismatch of
#' results. dafault: `FALSE`
#' @param progress (logical) whether to turn on progress information or not,
#' default: `TRUE`
#' @return an object of class `Experiment`
#'
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{control(...)}}{
#'       control code block
#'     }
#'     \item{\code{candidate(...)}}{
#'       candidate code block
#'     }
#'     \item{\code{run()}}{
#'       execute code, runs `call_block()`, then `do_comparison()`
#'     }
#'     \item{\code{call_block()}}{
#'       execute code, and collecting timing data
#'     }
#'     \item{\code{do_comparison()}}{
#'       mostly an internal fxn, run after `call_block`
#'     }
#'     \item{\code{result()}}{
#'       fetch the result (named list)
#'     }
#'     \item{\code{publish(browse = TRUE)}}{
#'       publish results. creates an html file. if `browse=TRUE`
#'       the file opens in your default browser. if `browse=FALSE`
#'       you get a file path.
#'     }
#'     \item{\code{diff()}}{
#'       compare results by "diffing" in the R console. used inside of publish()
#'       to prepare diffs. different diff representations for different object
#'       types:
#'       \itemize{
#'        \item numeric/integer: take difference between 2, or matrix comparing
#'         > 2 results
#'        \item character/factor: highlight differences in strings, like git diffs
#'        \item data.frame/matrix: use \pkg{visdat}
#'        \item images: use \pkg{vdiffr}
#'        \item list: not sure how to do this
#'        \item environments: not sure how to do this
#'       }
#'     }
#'     \item{\code{compare(x = NULL)}}{
#'       set a custom comparison function, must result in a single
#'       boolean
#'       fun: a function
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @section Terminology:
#' The `control` block is the control, the current state of the code.
#' The `candidate` block is the candidate, or the new version of the code
#' you want to compare to the control.
#' @examples
#' library(scientist)
#'
#' # basic eg
#' res <- Experiment$new(name = "jane")
#' res$control({
#'   x = 5
#'   x^2
#' })
#' res$candidate({
#'   y = 5
#'   y^3
#' })
#' res
#' res$run()
#' res$control_result
#' res$candidate_results
#' res$result()
#' # publish results
#' res$publish()
#' 
#' # many candidates
#' res <- Experiment$new(name = "doe")
#' res$control(stuff = {
#'   Sys.sleep(5)
#'   x = 5
#'   x^2
#' })
#' res$candidate(foo = {
#'   Sys.sleep(5)
#'   y = 5
#'   y^3
#' }, bar = {
#'   Sys.sleep(5)
#'   w = 1000
#'   (w - 20) / 34
#' })
#' res
#' res$run()
#' res$control_result
#' res$candidate_results
#' res$result()
#' # publish results
#' res$publish()
#'
#' # raise errors when the control and experiment do no match
#' res <- Experiment$new(name = "treetest", error_on_mismatch = TRUE)
#' res$control({
#'   x = 5
#'   x^2
#' })
#' res$candidate({
#'   y = 5
#'   y^3
#' })
#' res
#' \dontrun{res$run()}
#' res$control_result
#' res$candidate_results
#' res$result()
#' res$publish()
#'
#' # set explicit comparison
#' # FIXME: not working yet
#' # res <- Experiment$new(name = "jane")
#' # res$control({
#' #   x = 5
#' #   x^2
#' # })
#' # res$candidate({
#' #   y = 5
#' #   y^2
#' # })
#' # res$compare(function(control, candidate) {
#' #   control/2 == candidate/1
#' # })
#' # res$run()
#' # res$result()
Experiment <- R6::R6Class(
  "Experiment",
  public = list(
    name = NA,
    control_block = NULL,
    candidate_blocks = NULL,
    control_result = NULL,
    candidate_results = NULL,
    times = list(),
    compare_fun = NULL,
    error_on_mismatch = FALSE,
    progress = FALSE,
    comparison_result = NULL,

    initialize = function(name, error_on_mismatch = FALSE, progress = TRUE) {
      self$name <- name
      self$error_on_mismatch <- error_on_mismatch
      self$progress <- progress
    },

    print = function(...) {
      private$c(paste0("<Experiment> ", self$name))
      private$c(paste0("  control: ", private$sp(self$control_block)))
      if (!is.null(self$candidate_blocks)) {
        for (i in seq_along(self$candidate_blocks)) {
          private$c(paste0("  candidate: ", private$sp(self$candidate_blocks[i])))
        }
      } else {
        private$c("  candidate: <not assigned>")
      }
      invisible(self)
    },

    call_block = function(...) {
      ctrlt <- system.time(self$control_result <- private$r(self$control_block))
      candt <- vector(mode = "list", length = length(self$candidate_blocks))
      for (i in seq_along(self$candidate_blocks)) {
        candt[[i]] <- system.time(
          self$candidate_results[[ names(self$candidate_blocks)[i] %|i|% i ]] <- 
            private$r(self$candidate_blocks[[i]])
        )
      }
      self$candidate_results <- as.list(self$candidate_results)
      self$times <- list(
        control_time = ctrlt[["elapsed"]],
        candidate_times = vapply(candt, "[[", numeric(1), "elapsed")
      )
    },

    control = function(...) self$control_block <- lazyeval::lazy_dots(...),
    candidate = function(...) self$candidate_blocks <- lazyeval::lazy_dots(...),
    run = function() {
      self$call_block()
      self$do_comparison()
    },
    compare = function(x = NULL) {
      self$compare_fun <- x
    },
    do_comparison = function() {
      if (!is.null(self$compare_fun)) {
        stopifnot(is.function(self$compare_fun))
        comp <- eval(self$compare_fun)
        stopifnot(is.logical(comp) && length(comp) == 1)
      } else {
        comp <- identical(self$control_result, self$candidate_results)
      }

      if (!comp && self$error_on_mismatch) {
        stop("mismatch! re-run and check results", call. = FALSE)
      }
      self$comparison_result <- comp
    },
    result = function() {
      list(
        name = self$name,
        control = list(
          result = self$control_result,
          time = self$times$control_time
        ),
        candidates = Map(function(a, b, d) list(result = a, time = b, name = d),
          self$candidate_results,
          self$times$candidate_times,
          names(self$candidate_results) %||% rep(NA_character_, length(self$candidate_results))
        ),
        comparison = self$comparison_result
      )
    },
    publish = function(browse = TRUE) {
      # FIXME: use self$diff() here to prepare diffs for viewing
      check_for_a_pkg("whisker")
      # html <- glue::glue(html_template, .envir = self$result())
      res <- self$result()
      res$candidates <- unname(res$candidates)
      html <- whisker::whisker.render(html_template, data = res)
      file <- tempfile(fileext = ".html")
      writeLines(html, file)
      if (browse) browseURL(file) else file
    },
    diff = function() {
      res <- self$result()
      # if no difference, nothing to do
      if (res$comparison) message("no difference")
      # check that classes of returned data are the same
      clz_control <- class(res$control$result[[1]])
      clz_candidates <- vapply(res$candidates, function(w) class(w$result), "")
      if (any(clz_control != clz_candidates)) 
        stop("at least some object classes not the same", call. = FALSE)
      # FIXME
      # do the diff
      res$control$result
    }

  ),

  private = list(
    r = function(m) {
      callr::r(function(w) lazyeval::lazy_eval(w), args = list(m))
    },
    sp = function(x) {
      if (is.null(x)) "<not assigned>" else names(x) %||% "<unnamed>"
    },
    c = function(...) cat(..., sep = "\n")
  )
)
