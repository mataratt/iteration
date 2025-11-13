Simulation
================

Load key packages and source necessary files.

``` r
library(tidyverse)
source("source/sim_mean_sd.R")
```

We can “simulate” by running our function.

``` r
sim_mean_sd(n_subj = 400)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   3.08      1.94

Can I “verify” the Central Limit Theorem?

First with a `for` loop

``` r
output = vector("list", length = 100)

for (i in 1:100) {
  
  output[[i]] = sim_mean_sd(30)
}

output |> 
  bind_rows() |> 
  ggplot(aes(x = mu_hat)) +
  geom_density()
```

<img src="simulation_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

Try to repeat with a `map` statement

``` r
sim_results_df =
  expand_grid(
    iter = 1:100,
    sample_size = c(30, 60, 90, 120)
  ) |> 
  mutate(
    results = map(iter, sim_mean_sd)
  ) |> 
  unnest(results)
```
