scientist
=========



[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)

__Experiment with changes in an R project__


Inspired by the Ruby gem [scientist](https://github.com/github/scientist) -
but instead of targeted at web apps, this project targets researchers/etc. 
that want to compare changes in their code in a rigorous fashion.

## Install


```r
remotes::install_github("ropenscilabs/scientist")
```


```r
library(scientist)
```

## Usage

Initialize an experiment


```r
res <- Experiment$new(name = "jane")
```

Set your control code block


```r
res$control({
  x = 5
  x^2
})
```

Set your candidate code block. You can have 1 or more candidates, which are 
compared against the control.


```r
res$candidate({
  y = 5
  y^3
})
```

Now you can see some control and candidate details


```r
res
#> <Experiment> jane
#>  error on mismatch?: FALSE
#>  waiting?: TRUE
#>  progress?: FALSE
#>   control: <unnamed>
#>   candidate: <unnamed>
```

Run the experiment


```r
res$run()
```

Get the results


```r
res$control_result
#> [[1]]
#> [1] 25
res$candidate_results
#> [1] 125
```

Get all results plus timing data


```r
res$result()
#> $name
#> [1] "jane"
#> 
#> $control
#> $control$result
#> $control$result[[1]]
#> [1] 25
#> 
#> 
#> $control$time
#> $control$time$start
#> [1] "2019-08-02 01:18:45 GMT"
#> 
#> $control$time$end
#> [1] "2019-08-02 01:18:45 GMT"
#> 
#> $control$time$duration
#> [1] 0.2278159
#> 
#> 
#> 
#> $candidates
#> $candidates[[1]]
#> $candidates[[1]]$result
#> [1] 125
#> 
#> $candidates[[1]]$time
#> $candidates[[1]]$time$start
#> [1] "2019-08-02 01:18:45 GMT"
#> 
#> $candidates[[1]]$time$end
#> [1] "2019-08-02 01:18:45 GMT"
#> 
#> $candidates[[1]]$time$duration
#> [1] 0.222332
#> 
#> 
#> $candidates[[1]]$name
#> [1] NA
#> 
#> 
#> 
#> $comparison
#> [1] FALSE
```

Publish results - opens a page in your default browser


```r
res$publish()
```

![img](tools/publish_eg.png)

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/scientist/issues)
* License: MIT
* Get citation information for `scientist` in R doing `citation(package = 'scientist')`
* Please note that this project is released with a [Contributor Code of Conduct][coc]. By participating in this project you agree to abide by its terms.

[![ropensci_footer](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)


[coc]: https://github.com/ropenscilabs/scientist/blob/master/CODE_OF_CONDUCT.md
