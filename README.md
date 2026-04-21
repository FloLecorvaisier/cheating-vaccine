# README

## Information

This repository contains the code needed to produce the figures and simuations in Lecorvaisier & Martin (xxxx).

To produce the different figures, you must first create a `fig/` directory in the root of your session path and uncomment the lines in the `code.R` file starting with `ggsave`. On Linux-like systems, it can be done using the following command:

```
mkdir ./fig/
```

## Session info

```
R version 4.5.3 (2026-03-11)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 24.04.4 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.12.0 
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.12.0  LAPACK version 3.12.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8    LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

time zone: Europe/Paris
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] tidyr_1.3.2     patchwork_1.3.2 ggplot2_4.0.2  

loaded via a namespace (and not attached):
 [1] labeling_0.4.3     RColorBrewer_1.1-3 R6_2.6.1           tidyselect_1.2.1   farver_2.1.2       magrittr_2.0.4    
 [7] gtable_0.3.6       glue_1.8.0         tibble_3.3.1       pkgconfig_2.0.3    dplyr_1.2.0        generics_0.1.4    
[13] lifecycle_1.0.5    cli_3.6.5          S7_0.2.1           scales_1.4.0       grid_4.5.3         vctrs_0.7.2       
[19] textshaping_1.0.5  withr_3.0.2        systemfonts_1.3.2  compiler_4.5.3     purrr_1.2.1        tools_4.5.3       
[25] ragg_1.5.2         pillar_1.11.1      rlang_1.1.7
```