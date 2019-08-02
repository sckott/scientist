#' Set up an experiment
#'
#' @export
#' @param name The name of the experiment
#' @param error_on_mismatch (logical) whether to error on mismatch of
#' results. default: `FALSE`
#' @param wait (logical) wait for code to execute. if `FALSE`, code is run in the
#' background, and you have to run `$collect()` to collect results.
#' default: `TRUE`
#' @param progress (logical) whether to turn on progress information or not,
#' default: `TRUE` (IGNORED RIGHT NOW)
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
#'     \item{\code{run(...)}}{
#'       execute code, runs `call_block()`, then `do_comparison()`
#'       `...`: pass on parameters through `call_block()` to
#'         [callr::r()] (`wait=TRUE`) or [callr::r_bg()] (`wait=FALSE`)
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
#' res
#' res$control(stuff = {
#'   Sys.sleep(2)
#'   x = 5
#'   x^2
#' })
#' res$candidate(foo = {
#'   Sys.sleep(2)
#'   y = 5
#'   y^3
#' }, bar = {
#'   Sys.sleep(2)
#'   w = 1000
#'   (w - 20) / 34
#' })
#' res
#' res$run()
#' res$control_result
#' res$candidate_results
#' res$result()
#' res$times
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
#' # if not waiting, run $status() and $collect()
#' res <- Experiment$new(name = "junipers", wait = FALSE)
#' res$control({
#'   x = 5
#'   x^2
#' })
#' res$candidate({
#'   y = 5
#'   y^3
#' })
#' \dontrun{res$status()}
#' res$run()
#' res
#' res$status()
#' res$collect()
#' res$result()
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
    waiting = TRUE,

    initialize = function(name, error_on_mismatch = FALSE,
      wait = TRUE, progress = FALSE) {

      assert(name, "character")
      assert(error_on_mismatch, "logical")
      assert(wait, "logical")
      assert(progress, "logical")
      self$name <- name
      self$error_on_mismatch <- error_on_mismatch
      self$waiting <- wait
      self$progress <- progress
    },

    print = function(...) {
      private$c(paste0("<Experiment> ", self$name))
      private$c(paste0(" error on mismatch?: ", self$error_on_mismatch))
      private$c(paste0(" waiting?: ", self$waiting))
      private$c(paste0(" progress?: ", self$progress))
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

    # call_block = function(...) {
    #   ctrlt <- system.time(self$control_result <- private$r(self$control_block))
    #   candt <- vector(mode = "list", length = length(self$candidate_blocks))
    #   for (i in seq_along(self$candidate_blocks)) {
    #     candt[[i]] <- system.time(
    #       self$candidate_results[[ names(self$candidate_blocks)[i] %|i|% i ]] <-
    #         private$r(self$candidate_blocks[[i]])
    #     )
    #   }
    #   self$candidate_results <- as.list(self$candidate_results)
    #   self$times <- list(
    #     control_time = ctrlt[["elapsed"]],
    #     candidate_times = vapply(candt, "[[", numeric(1), "elapsed")
    #   )
    # },

    call_block = function(...) {
      private$control_bg <- private$r_bg(self$control_block, ...)
      candt <- vector(mode = "list", length = length(self$candidate_blocks))
      for (i in seq_along(self$candidate_blocks)) {
          private$candidate_bg[[ names(self$candidate_blocks)[i] %|i|% i ]] <-
            private$r_bg(self$candidate_blocks[[i]], ...)
      }
      if (self$waiting) {
        message("waiting ...")
        self$times <- list(control_time = private$wait_time(private$control_bg))
        self$times <- c(self$times,
          list(candidate_times = lapply(private$candidate_bg, private$wait_time)))
        message("done ...")

        self$control_result <- private$control_bg$get_result()
        for (i in seq_along(private$candidate_bg)) {
          self$candidate_results[[ names(self$candidate_blocks)[i] %|i|% i ]] <-
            private$candidate_bg[[i]]$get_result()
        }
      }
    },

    status = function() {
      if (is.null(private$control_bg)) stop("experiment not run yet")
      structure(list(
        control = !private$control_bg$is_alive(),
        candidates = lapply(private$candidate_bg, function(z) !z$is_alive())
      ), class = "exp_status")
    },
    collect = function() {
      private$all_done_check()
      self$times <- list(control_time = private$run_time(private$control_bg))
      self$times <- c(self$times,
        list(candidate_times = lapply(private$candidate_bg, private$run_time)))
      self$control_result <- private$control_bg$get_result()
      for (i in seq_along(private$candidate_bg)) {
        self$candidate_results[[ names(self$candidate_blocks)[i] %|i|% i ]] <-
          private$candidate_bg[[i]]$get_result()
      }
    },

    control = function(...) self$control_block <- lazyeval::lazy_dots(...),
    candidate = function(...) self$candidate_blocks <- lazyeval::lazy_dots(...),
    run = function(...) {
      self$call_block(...)
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
      private$all_done_check()
      res <- self$result()
      res$control$time <- res$control$time$duration
      res$candidates <- unname(res$candidates)
      for (i in seq_along(res$candidates)) res$candidates[[i]]$time <- res$candidates[[i]]$time$duration
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
    control_bg = NULL,
    candidate_bg = NULL,
    r = function(m, ...) {
      callr::r(function(w) lazyeval::lazy_eval(w), args = list(m), ...)
    },
    r_bg = function(m, ...) {
      callr::r_bg(function(w) lazyeval::lazy_eval(w), args = list(m), ...)
    },
    sp = function(x) {
      if (is.null(x)) "<not assigned>" else names(x) %||% "<unnamed>"
    },
    c = function(...) cat(..., sep = "\n"),
    wait_time = function(x) {
      x$wait()
      et <- Sys.time()
      end_time <- as.POSIXlt(et, tz = "GMT")
      start_time <- x$get_start_time()
      list(start = start_time, end = end_time,
        duration = as.numeric(end_time - start_time))
    },
    run_time = function(x) {
      start_time <- x$get_start_time()
      list(start = start_time, end = NA, duration = NA)
    },
    all_done = function() all(unlist(self$status(), TRUE)),
    all_done_check = function(x) if (!private$all_done()) stop("not all experiments done, see $status()")
  )
)

print.exp_status <- function(x, ...) {
  cat("<experiment status>", sep = "\n")
  cat(paste0("  control done?: ", x$control), sep = "\n")
  cat("  candidates done?: ", sep = "\n")
  for (i in seq_along(x$candidates)) {
    cat(sprintf("    %s: %s", names(x$candidates)[i] %||% NA_character_,
      x$candidates[[i]]),
      sep = "\n")
  }
}
