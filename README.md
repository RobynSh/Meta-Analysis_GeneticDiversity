# Custom code and supporting data for manuscript: "*Global meta-analysis shows action is needed to halt genetic diversity loss"*

This repository contains the following supporting data and R code (with
example data):

-   **Supporting_Data:**

    -   *Supporting Data 1 (bibliography).pdf*

        Bibliography for systematic review dataset.

    -   *Supporting Data 2 (PRISMA).xlsx*

        Source data for preferred reporting items for systematic reviews
        and meta-analysis (PRISMA) steps. Data are provided as an Excel
        workbook, with a detailed "READ ME" on sheet 1.

    -   *Supporting Data 3 (systematic review dataset).xlsx*

        Source data for systematic review and meta-analysis. Data are
        provided as an Excel workbook, with a detailed "READ ME" on
        sheet 1.

    -   *Supporting Data 4 (WOS advanced search string).pdf*

        Web of Science advanced search string.

    -   *Supporting Data 5 (generation length).xlsx*

        Generation length information for unique species in our dataset.
        Data are provided as an Excel workbook, with a detailed "READ
        ME" on sheet 1.

-   **Text_Mining:**

    -   *TextMining_CustomScripts.R*

        R script defining custom functions, with example code outlining
        the use of these functions with a test data set (found in the
        subfolder entitled "Batch_1")

    -   *Batch_1*

        Folder containing a test set of five PDF articles to be used as
        example input for custom R functions. PLOS ONE papers from
        co-authors have been provided as examples, as their licensing
        agreement permits redistribution. These papers may not be
        included in the final manuscript, but they contain genetic
        metrics, making them suitable for testing the 'genetic metric
        search' component of the text-mining process.

    -   *TextMining_Outputs:*

        -   *StartStop_Page_Search.csv*

            Output of the "Find.Section.Pg.Nos" custom function. A csv
            file containing the "start/stop" location for further
            searching. Note that the page numbers do not match exact
            numbers in actual pdfs, but are relative to the way in which
            pdfs are read into R, as described further in the relevant
            sections of the code.

        -   *PDFs_Genetic_Diversity_Hits*

            Output of the "Metric.Search" custom function. A subfolder
            containing PDFs of papers where genetic diversity metrics
            were detected.

        -   *PDFs_Metric_Hits.csv*

            Output of the "Metric.Search" custom function. A csv file
            listing specific hits for each pdf file name.

### **R Session Information for all analyses:**

*Note: for the manuscript, text-mining was performed in R v 3.5.2, using
the packages pdfsearch v 0.2.2, dplyr v 0.8.0, and stringi v 1.3.1. We
also tested the code using updated version of R and relevant packages,
provided below:*

R version 4.3.1 (2023-06-16).

Platform: x86_64-apple-darwin20 (64-bit)

Running under: macOS Ventura 13.6.9

Matrix products: default

BLAS:
/System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib

LAPACK:
/Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;
LAPACK version 3.11.0

locale: [1]
en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Australia/Sydney

tzcode source: internal

attached base packages:

-   stats

-   graphics

-   grDevices

-   utils

-   datasets

-   methods

-   base

-   other attached packages: stringi_1.8.3

-   dplyr_1.1.4

-   pdfsearch_0.3.0

loaded via a namespace (and not attached):

-   vctrs_0.6.5

-   knitr_1.45

-   cli_3.6.2

-   xfun_0.42

-   rlang_1.1.3

-   generics_0.1.3

-   glue_1.7.0

-   SnowballC_0.7.1

-   askpass_1.2.0

-   qpdf_1.3.4

-   htmltools_0.5.7

-   tokenizers_0.3.0

-   fansi_1.0.6

-   rmarkdown_2.25

-   evaluate_0.23

-   tibble_3.2.1

-   fastmap_1.1.1

-   yaml_2.3.8

-   lifecycle_1.0.4

-   compiler_4.3.1

-   Rcpp_1.0.12

-   pkgconfig_2.0.3

-   rstudioapi_0.15.0

-   digest_0.6.34

-   R6_2.5.1

-   tidyselect_1.2.0

-   utf8_1.2.4

-   pillar_1.9.0

-   pdftools_3.4.1

-   magrittr_2.0.3

-   tools_4.3.1

-   withr_3.0.0

### 
