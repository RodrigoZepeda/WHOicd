
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WHOicd

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/WHOicd)](https://CRAN.R-project.org/package=WHOicd)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/RodrigoZepeda/WHOicd/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RodrigoZepeda/WHOicd?branch=main)
[![R-CMD-check](https://github.com/RodrigoZepeda/WHOicd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RodrigoZepeda/WHOicd/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Access the World Health Organization’s (WHO) International
Classification of Diseases (ICD) [API](https://icd.who.int/icdapi). This
allows you to use ICD-10 and ICD-11 codes, encode text directly into
ICD-11, search for diseases in ICD-10, and detect underlying causes of
death using the [DORIS system](https://icd.who.int/doris).

## Installation

You can install the development version of WHOicd using the `remotes`
package:

``` r
#install.packages("remotes")
remotes::install_github("RodrigoZepeda/WHOicd")
```

## Setup

For the examples we’ll assume you already have a `CLIENT_ID` and
`CLIENT_SECRET` for the WHO API as obtained in the [**Obtaining your
token**](#obtaining-your-token) section.

To interact with the API you’ll need to continuously create a token
using the `get_token` function:

``` r
library(WHOicd)

#Substitute CLIENT_ID and CLIENT_SECRET by your credentials
token <- get_token(CLIENT_ID, CLIENT_SECRET)
```

> **Note** These tokens last for 1 hour and once the hour passes you’ll
> need to generate a new token. The package will try to auto-generate
> one for you.

## ICD-11 examples

## ICD-10 examples

The main function relating to ICD-10 is `icd10_search()` which searches
for the titles and parents of codes, blocks of chapters. As an example,
we can search for the following vector and obtain a `data.frame`:

``` r
#Search for code, specific code, chapter and block 
codes <- c("D60", "IX", "I10-I15")
icd10_search(token, codes)
#>   searched code                                          code_title   block
#> 1      D60  D60 Acquired pure red cell aplasia [erythroblastopenia] D60-D64
#> 2       IX <NA>                                                <NA>    <NA>
#> 3  I10-I15 <NA>                                                <NA> I10-I15
#>                   block_title chapter
#> 1 Aplastic and other anaemias     III
#> 2                        <NA>      IX
#> 3       Hypertensive diseases      IX
#>                                                                                         chapter_title
#> 1 Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism
#> 2                                                                  Diseases of the circulatory system
#> 3                                                                  Diseases of the circulatory system
```

If you only want to get the title of the current code/chapter/block you
can use `icd10_title` which is faster as it requires less requests to
the API:

``` r
#Search for code, specific code, chapter and block 
codes <- c("D60", "IX", "I10-I15")
icd10_title(token, codes)
#>   searched                                               title
#> 1      D60 Acquired pure red cell aplasia [erythroblastopenia]
#> 2       IX                  Diseases of the circulatory system
#> 3  I10-I15                               Hypertensive diseases
```

#### Top-down search

Given a chapter you can also list all the blocks in a chapter

``` r
icd10_blocks(token, chapter = "III")
#>     codes                                                          title
#> 1 D50-D53                                           Nutritional anaemias
#> 2 D55-D59                                            Haemolytic anaemias
#> 3 D60-D64                                    Aplastic and other anaemias
#> 4 D65-D69 Coagulation defects, purpura and other haemorrhagic conditions
#> 5 D70-D77               Other diseases of blood and blood-forming organs
#> 6 D80-D89               Certain disorders involving the immune mechanism
```

As well as all of the chapters in a block:

``` r
icd10_codes(token, block = "D55-D59")
#>   codes                                title
#> 1   D55      Anaemia due to enzyme disorders
#> 2   D56                         Thalassaemia
#> 3   D57                Sickle-cell disorders
#> 4   D58 Other hereditary haemolytic anaemias
#> 5   D59          Acquired haemolytic anaemia
```

The same command allows you to search inside a code:

``` r
icd10_codes(token, block = "D55")
#>   codes                                                              title
#> 1 D55.0 Anaemia due to glucose-6-phosphate dehydrogenase [G6PD] deficiency
#> 2 D55.1           Anaemia due to other disorders of glutathione metabolism
#> 3 D55.2                     Anaemia due to disorders of glycolytic enzymes
#> 4 D55.3                  Anaemia due to disorders of nucleotide metabolism
#> 5 D55.8                             Other anaemias due to enzyme disorders
#> 6 D55.9                        Anaemia due to enzyme disorder, unspecified
```

### Search for code in releases

Not all codes are available across releases. For example, the `C80.0`
code was not in the `2008` release of the ICD-10. Hence if you are using
that release you will not find it:

``` r
icd10_search(token, "C80.0", release = 2008)
#> Warning in value[[3L]](cond): Request not found. Possibly any of release,
#> chapter/block/code or language is not available or incorrectly specified.
#> Warning in value[[3L]](cond): Request not found. Possibly any of release,
#> chapter/block/code or language is not available or incorrectly specified.
#>   searched chapter chapter_title
#> 1    C80.0    <NA>          <NA>
```

However you can use the `icd10_code_search_release` to search for a
release containing that code:

``` r
icd10_code_search_release(token, code = "C80.0")
#> [1] "2019" "2016" "2010"
```

and use one of those releases instead:

``` r
icd10_search(token, "C80.0", release = 2016)
#>   searched level_0                                             title_0 level_1
#> 1    C80.0   C80.0 Malignant neoplasm, primary site unknown, so stated     C80
#>                                             title_1 level_2
#> 1 Malignant neoplasm, without specification of site C76-C80
#>                                                               title_2 level_3
#> 1 Malignant neoplasms of ill-defined, secondary and unspecified sites C00-C97
#>               title_3 level_4   title_4
#> 1 Malignant neoplasms      II Neoplasms
```

### Additional information on releases

The `icd10_releases` function lists all available ICD-10 releases

``` r
icd10_releases(token)
#> [1] "2019" "2016" "2010" "2008"
```

The default is 2019. You can change it with the `release` parameter
across all functions.

To obtain the complete information on a certain release you can use the
`icd10_release_info` function:

``` r
icd10_release_info(token, release = 2016)
#>                                                                                                                    context 
#>                                                                   "http://id.who.int/icd/contexts/contextForTopLevel.json" 
#>                                                                                                                         id 
#>                                                                                    "http://id.who.int/icd/release/10/2016" 
#>                                                                                                             title.language 
#>                                                                                                                       "en" 
#>                                                                                                                title.value 
#> "International Statistical Classification of Diseases and Related Health Problems 10th Revision (ICD-10) Version for 2016" 
#>                                                                                                                releaseDate 
#>                                                                                                               "2016-11-01" 
#>                                                                                                                 browserUrl 
#>                                                                 "http://apps.who.int/classifications/icd10/browse/2016/en"
```

## Doris examples

## Obtaining your token

Go to the WHO ICD API website at <https://icd.who.int/icdapi> and click
on `Register` inside the `API Access` section:

<figure>
<img src="man/figures/api1.png"
alt="Image of the ICD API website indicating where the Register button for the API lies" />
<figcaption aria-hidden="true">Image of the ICD API website indicating
where the Register button for the API lies</figcaption>
</figure>

Fill out your information and verify your email.

<figure>
<img src="man/figures/api2.png"
alt="Image of the API website with the register form" />
<figcaption aria-hidden="true">Image of the API website with the
register form</figcaption>
</figure>

Once your email is verified go to the `Login` page. Enter your email and
password:

<figure>
<img src="man/figures/api2_5.png"
alt="API login page requesting the user for email and password" />
<figcaption aria-hidden="true">API login page requesting the user for
email and password</figcaption>
</figure>

Read and accept the terms and conditions for the API

<figure>
<img src="man/figures/api3.png"
alt="Image of API website indicating where is the button to accept terms and conditions" />
<figcaption aria-hidden="true">Image of API website indicating where is
the button to accept terms and conditions</figcaption>
</figure>

Under **API Access** click on `View API access key(s)`

![Image of API website indicating where is the button to View the API
access keys](man/figures/api4.png) Your client id and secret will be
required by the `WHOicd` library. Copy them, they are the equivalent to
a user and password for this API. You’ll need them to interact with the
WHO ICD. **Don’t share them!!**

<figure>
<img src="man/figures/api5.png"
alt="Image of API website indicating where is the button to View the API access keys" />
<figcaption aria-hidden="true">Image of API website indicating where is
the button to View the API access keys</figcaption>
</figure>

## Support

This is not an official product of the WHO. However we are happy to
provide support if you [raise an
issue](https://docs.github.com/en/issues/tracking-your-work-with-issues/creating-an-issue).
