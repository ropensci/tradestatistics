---
title: "How to use this package"
author: "Mauricio Vargas S."
date: "2019-04-04"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{How to use this package}
  %\VignetteEngine{knitr::knitr}
  %\VignetteEncoding{UTF-8}
---



# Package data

## Available tables

Provided this package takes data from an API, it is useful to know which tables can be accessed:


```r
library(tradestatistics)

ots_attributes_tables
#> # A tibble: 15 x 3
#>    table      description                        source                    
#>    <chr>      <chr>                              <chr>                     
#>  1 countries  Countries metadata                 UN Comtrade               
#>  2 products   Product metadata                   UN Comtrade               
#>  3 reporters  Reporting countries                UN Comtrade               
#>  4 communiti… Product communities                Center for International …
#>  5 product_s… Product short names                The Observatory of Econom…
#>  6 country_r… Ranking of countries               UN Comtrade               
#>  7 product_r… Ranking of products                Open Trade Statistics     
#>  8 yrpc       Bilateral trade at product level … Open Trade Statistics     
#>  9 yrp        Bilateral trade at aggregated lev… Open Trade Statistics     
#> 10 yrc        Bilateral trade at aggregated lev… Open Trade Statistics     
#> 11 yrc_expor… Bilateral trade at aggregated lev… Open Trade Statistics     
#> 12 yrc_impor… Reporter trade at product level (… Open Trade Statistics     
#> 13 yr         Reporter trade at aggregated leve… Open Trade Statistics     
#> 14 yr_short   Reporter trade at aggregated leve… Open Trade Statistics     
#> 15 yc         Product trade at aggregated level… Open Trade Statistics
```

## Country codes

If you don't know a certain country code you can explore this data before using the functions.


```r
ots_attributes_countries
#> # A tibble: 249 x 6
#>    country_iso country_name_en… country_fullnam… continent_id continent
#>    <chr>       <chr>            <chr>                   <int> <chr>    
#>  1 afg         Afghanistan      Afghanistan                 1 Asia     
#>  2 alb         Albania          Albania                     2 Europe   
#>  3 dza         Algeria          Algeria                     3 Africa   
#>  4 asm         American Samoa   American Samoa              4 Oceania  
#>  5 and         Andorra          Andorra                     2 Europe   
#>  6 ago         Angola           Angola                      3 Africa   
#>  7 aia         Anguilla         Anguilla                    5 Americas 
#>  8 atg         Antigua and Bar… Antigua and Bar…            5 Americas 
#>  9 arg         Argentina        Argentina                   5 Americas 
#> 10 arm         Armenia          Armenia                     1 Asia     
#> # … with 239 more rows, and 1 more variable: eu28_member <int>
```

## Product codes



Open Trade Statistics (aka OTS) uses data classified under either HS92, HS96, HS02 and HS07 (Harmonized System) and SITC rev. 1 and rev. 2 (Standard International Trade Classification). A part of our project is to combine all of this under HS07 codes, and to make it available for the period 1962-2017 even for the years before 2007 when HS07 was not in use.

As [UN Comtrade](https://comtrade.un.org/db/mr/rfglossarylist.aspx) explains both HS and SITC are trade classifications that are updated by releasing new revisions, and when a classification is released then it remains unaltered. For example, HS92 was released in 1992 and its product codes remain unaltered until today, while newer classifications (e.g. HS07) were created to include newer products (e.g. it was important to provide more details about electronic products that did not exist when HS92 was created).

Each of these classifications consists in numeric codes and a detailed product name associated to each code, for example HS92 (and all HS revisions) contain 4 and 6 digits long codes while SITC does only contain 4 digits long codes in our cleaned datasets (there are 5 digits long SITC codes that we are not using):


```r
ots_attributes_countries
#> # A tibble: 249 x 6
#>    country_iso country_name_en… country_fullnam… continent_id continent
#>    <chr>       <chr>            <chr>                   <int> <chr>    
#>  1 afg         Afghanistan      Afghanistan                 1 Asia     
#>  2 alb         Albania          Albania                     2 Europe   
#>  3 dza         Algeria          Algeria                     3 Africa   
#>  4 asm         American Samoa   American Samoa              4 Oceania  
#>  5 and         Andorra          Andorra                     2 Europe   
#>  6 ago         Angola           Angola                      3 Africa   
#>  7 aia         Anguilla         Anguilla                    5 Americas 
#>  8 atg         Antigua and Bar… Antigua and Bar…            5 Americas 
#>  9 arg         Argentina        Argentina                   5 Americas 
#> 10 arm         Armenia          Armenia                     1 Asia     
#> # … with 239 more rows, and 1 more variable: eu28_member <int>
```

Each product belongs to a group with a given name and number according to HS classification.

# Package functions

## Country code

The end user can use this function to find an ISO code by providing a country name. This works by implementing partial search.

Basic examples:

```r
# Single match with no replacement
ots_country_code("Chile")
#> # A tibble: 1 x 6
#>   country_iso country_name_en… country_fullnam… continent_id continent
#>   <chr>       <chr>            <chr>                   <int> <chr>    
#> 1 chl         Chile            Chile                       5 Americas 
#> # … with 1 more variable: eu28_member <int>

# Single match with replacement
ots_country_code("America")
#> # A tibble: 1 x 6
#>   country_iso country_name_en… country_fullnam… continent_id continent
#>   <chr>       <chr>            <chr>                   <int> <chr>    
#> 1 usa         USA              USA, Puerto Ric…            5 Americas 
#> # … with 1 more variable: eu28_member <int>

# Double match with no replacement
ots_country_code("Germany")
#> # A tibble: 2 x 6
#>   country_iso country_name_en… country_fullnam… continent_id continent
#>   <chr>       <chr>            <chr>                   <int> <chr>    
#> 1 ddr         Fmr Dem. Rep. o… Fmr Dem. Rep. o…            2 Europe   
#> 2 deu         Germany          Germany (former…            2 Europe   
#> # … with 1 more variable: eu28_member <int>
```

## Product code

The end user can use this function to find a product code by providing a product name. This works by implementing partial search.


```r
ots_product_code("wine")
#> # A tibble: 58 x 5
#>    product_code product_fullname_engli… group_code group_name  type_product
#>    <chr>        <chr>                   <chr>      <chr>       <chr>       
#>  1 0103         Swine; live             01         Animals; l… wine        
#>  2 010310       Swine; live, pure-bred… 01         Animals; l… wine        
#>  3 010391       Swine; live, (other th… 01         Animals; l… wine        
#>  4 010392       Swine; live, (other th… 01         Animals; l… wine        
#>  5 0203         Meat of swine; fresh, … 02         Meat and e… wine        
#>  6 020311       Meat; of swine, carcas… 02         Meat and e… wine        
#>  7 020312       Meat; of swine, hams, … 02         Meat and e… wine        
#>  8 020319       Meat; of swine, n.e.c.… 02         Meat and e… wine        
#>  9 020321       Meat; of swine, carcas… 02         Meat and e… wine        
#> 10 020322       Meat; of swine, hams, … 02         Meat and e… wine        
#> # … with 48 more rows
```

## Trade data

This function downloads data for a single year and needs (at least) some filter parameters according to the query type.
 
### Bilateral trade at product level (Year - Reporter - Partner - Product Code)

If we want Chile-Argentina bilateral trade at product level in 1965:

```r
ots_create_tidy_data(years = 1965, reporters = "chl", partners = "arg")
#> # A tibble: 317 x 16
#>     year reporter_iso partner_iso reporter_fullna… partner_fullnam…
#>    <int> <chr>        <chr>       <chr>            <chr>           
#>  1  1965 chl          arg         Chile            Argentina       
#>  2  1965 chl          arg         Chile            Argentina       
#>  3  1965 chl          arg         Chile            Argentina       
#>  4  1965 chl          arg         Chile            Argentina       
#>  5  1965 chl          arg         Chile            Argentina       
#>  6  1965 chl          arg         Chile            Argentina       
#>  7  1965 chl          arg         Chile            Argentina       
#>  8  1965 chl          arg         Chile            Argentina       
#>  9  1965 chl          arg         Chile            Argentina       
#> 10  1965 chl          arg         Chile            Argentina       
#> # … with 307 more rows, and 11 more variables: product_code <chr>,
#> #   product_code_length <int>, product_fullname_english <chr>,
#> #   group_code <chr>, group_name <chr>, export_value_usd <int>,
#> #   import_value_usd <int>, export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>
```

Which is the same as:

```r
ots_create_tidy_data(years = 1965, reporters = "chl", partners = "arg", table = "yrpc")
#> # A tibble: 317 x 16
#>     year reporter_iso partner_iso reporter_fullna… partner_fullnam…
#>    <int> <chr>        <chr>       <chr>            <chr>           
#>  1  1965 chl          arg         Chile            Argentina       
#>  2  1965 chl          arg         Chile            Argentina       
#>  3  1965 chl          arg         Chile            Argentina       
#>  4  1965 chl          arg         Chile            Argentina       
#>  5  1965 chl          arg         Chile            Argentina       
#>  6  1965 chl          arg         Chile            Argentina       
#>  7  1965 chl          arg         Chile            Argentina       
#>  8  1965 chl          arg         Chile            Argentina       
#>  9  1965 chl          arg         Chile            Argentina       
#> 10  1965 chl          arg         Chile            Argentina       
#> # … with 307 more rows, and 11 more variables: product_code <chr>,
#> #   product_code_length <int>, product_fullname_english <chr>,
#> #   group_code <chr>, group_name <chr>, export_value_usd <int>,
#> #   import_value_usd <int>, export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>
```

If we want Chile-Argentina bilateral trade at product level in 1965-1967:

```r
ots_create_tidy_data(years = 1965:1967, reporters = "chl", partners = "arg")
#> # A tibble: 1,029 x 20
#>     year reporter_iso partner_iso reporter_fullna… partner_fullnam…
#>    <int> <chr>        <chr>       <chr>            <chr>           
#>  1  1965 chl          arg         Chile            Argentina       
#>  2  1965 chl          arg         Chile            Argentina       
#>  3  1965 chl          arg         Chile            Argentina       
#>  4  1965 chl          arg         Chile            Argentina       
#>  5  1965 chl          arg         Chile            Argentina       
#>  6  1965 chl          arg         Chile            Argentina       
#>  7  1965 chl          arg         Chile            Argentina       
#>  8  1965 chl          arg         Chile            Argentina       
#>  9  1965 chl          arg         Chile            Argentina       
#> 10  1965 chl          arg         Chile            Argentina       
#> # … with 1,019 more rows, and 15 more variables: product_code <chr>,
#> #   product_code_length <int>, product_fullname_english <chr>,
#> #   group_code <chr>, group_name <chr>, export_value_usd <int>,
#> #   import_value_usd <int>, export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>,
#> #   export_value_usd_change_5_years <int>,
#> #   export_value_usd_percentage_change_5_years <dbl>,
#> #   import_value_usd_change_5_years <int>,
#> #   import_value_usd_percentage_change_5_years <dbl>
```

If we want Chile-Argentina bilateral trade at product level in 1965, 1966 and 1968:

```r
ots_create_tidy_data(years = c(1965,1966,1968), reporters = "chl", partners = "arg")
#> # A tibble: 1,041 x 20
#>     year reporter_iso partner_iso reporter_fullna… partner_fullnam…
#>    <int> <chr>        <chr>       <chr>            <chr>           
#>  1  1965 chl          arg         Chile            Argentina       
#>  2  1965 chl          arg         Chile            Argentina       
#>  3  1965 chl          arg         Chile            Argentina       
#>  4  1965 chl          arg         Chile            Argentina       
#>  5  1965 chl          arg         Chile            Argentina       
#>  6  1965 chl          arg         Chile            Argentina       
#>  7  1965 chl          arg         Chile            Argentina       
#>  8  1965 chl          arg         Chile            Argentina       
#>  9  1965 chl          arg         Chile            Argentina       
#> 10  1965 chl          arg         Chile            Argentina       
#> # … with 1,031 more rows, and 15 more variables: product_code <chr>,
#> #   product_code_length <int>, product_fullname_english <chr>,
#> #   group_code <chr>, group_name <chr>, export_value_usd <int>,
#> #   import_value_usd <int>, export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>,
#> #   export_value_usd_change_5_years <int>,
#> #   export_value_usd_percentage_change_5_years <dbl>,
#> #   import_value_usd_change_5_years <int>,
#> #   import_value_usd_percentage_change_5_years <dbl>
```

If we want Chile-Argentina bilateral trade at product level in 1965 and 1966 with respect to product "0101" (Horses, asses, mules and hinnies; live):

```r
ots_create_tidy_data(years = 1965:1966, reporters = "chl", partners = "arg", products = "0101")
#> # A tibble: 2 x 16
#>    year reporter_iso partner_iso reporter_fullna… partner_fullnam…
#>   <int> <chr>        <chr>       <chr>            <chr>           
#> 1  1965 chl          arg         Chile            Argentina       
#> 2  1966 chl          arg         Chile            Argentina       
#> # … with 11 more variables: product_code <chr>, product_code_length <int>,
#> #   product_fullname_english <chr>, group_code <chr>, group_name <chr>,
#> #   export_value_usd <int>, import_value_usd <int>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>
```

But if we want Chile-Argentina bilateral trade at product level in 1965 with respect to all the codes that match the string "horse" at product or group name:

```r
ots_create_tidy_data(years = 1965, reporters = "chl", partners = "arg", products = "horse")
#> # A tibble: 10 x 17
#>     year reporter_iso partner_iso reporter_fullna… partner_fullnam…
#>    <int> <chr>        <chr>       <chr>            <chr>           
#>  1  1965 chl          arg         Chile            Argentina       
#>  2  1965 chl          arg         Chile            Argentina       
#>  3  1965 chl          arg         Chile            Argentina       
#>  4  1965 chl          arg         Chile            Argentina       
#>  5  1965 chl          arg         Chile            Argentina       
#>  6  1965 chl          arg         Chile            Argentina       
#>  7  1965 chl          arg         Chile            Argentina       
#>  8  1965 chl          arg         Chile            Argentina       
#>  9  1965 chl          arg         Chile            Argentina       
#> 10  1965 chl          arg         Chile            Argentina       
#> # … with 12 more variables: product_code <chr>, product_code_length <int>,
#> #   product_fullname_english <chr>, group_code <chr>, group_name <chr>,
#> #   export_value_usd <int>, import_value_usd <int>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>, observation <chr>
```

Another option is to pass more than one year, reporter, partner and/or product (here "0101" means to perform an exact search while "apple" will do string matching):

```r
ots_create_tidy_data(years = 1965, reporters = c("chl", "per"), partners = c("arg", "bra"), products = c("0101", "apple"))
#> # A tibble: 3 x 17
#>    year reporter_iso partner_iso reporter_fullna… partner_fullnam…
#>   <int> <chr>        <chr>       <chr>            <chr>           
#> 1  1965 chl          arg         Chile            Argentina       
#> 2  1965 chl          arg         Chile            Argentina       
#> 3  1965 chl          arg         Chile            Argentina       
#> # … with 12 more variables: product_code <chr>, product_code_length <int>,
#> #   product_fullname_english <chr>, group_code <chr>, group_name <chr>,
#> #   export_value_usd <int>, import_value_usd <int>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>, observation <chr>
```

This latter option works with a different number of reporters and partners:

```r
ots_create_tidy_data(years = 1965, reporters = "chl", partners = c("arg", "bra", "per"), products = c("0101", "apple"))
#> # A tibble: 3 x 17
#>    year reporter_iso partner_iso reporter_fullna… partner_fullnam…
#>   <int> <chr>        <chr>       <chr>            <chr>           
#> 1  1965 chl          arg         Chile            Argentina       
#> 2  1965 chl          arg         Chile            Argentina       
#> 3  1965 chl          arg         Chile            Argentina       
#> # … with 12 more variables: product_code <chr>, product_code_length <int>,
#> #   product_fullname_english <chr>, group_code <chr>, group_name <chr>,
#> #   export_value_usd <int>, import_value_usd <int>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>, observation <chr>
```

Here some fields deserve an explanation:

* `product_code`: HS07 product codes (e.g. according to the \code{products} table within this package, 0101 stands for "Horses, etc.")
* `product_code_length`: How many digits does `product_code` contain, this can be useful to filter by depth when using HS codes (HS 6 digits is a more detailed version of HS 4 digits, and therefore you don't have to sum both or you'll be counting exports/imports twice)
* `group_code`: International categorization of group products defined after product ID
* `group_name`: English name corresponding to `group_id`
* `export_value_usd`: Exports measured in nominal United States Dollars (USD)
* `import_value_usd`: Imports measured in nominal United States Dollars (USD)
* `export_value_usd_percentage_change_1_year`: Nominal increase/decrease in exports measured as percentage with respect to last year
* `export_value_usd_percentage_change_5_years`: Nominal increase/decrease in exports measured as percentage with respect to five years ago
* `export_value_usd_change_1_year`: Nominal increase/decrease in exports measured in USD with respect to last year
* `export_value_usd_change_5_years`: Nominal increase/decrease in exports measured in USD with respect to five years ago

### Bilateral trade at aggregated level (Year - Reporter - Partner)

If we want Chile-Argentina bilateral trade at aggregated level in 1965:

```r
ots_create_tidy_data(years = 1965, reporters = "chl", partners = "arg", table = "yrp")
#> # A tibble: 1 x 11
#>    year reporter_iso partner_iso reporter_fullna… partner_fullnam…
#>   <int> <chr>        <chr>       <chr>            <chr>           
#> 1  1965 chl          arg         Chile            Argentina       
#> # … with 6 more variables: export_value_usd <int>, import_value_usd <int>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>
```

If we want Chile-Argentina bilateral trade at aggregated level in 1965 and 1967:

```r
ots_create_tidy_data(years = c(1965,1967), reporters = "chl", partners = "arg", table = "yrp")
#> # A tibble: 2 x 15
#>    year reporter_iso partner_iso reporter_fullna… partner_fullnam…
#>   <int> <chr>        <chr>       <chr>            <chr>           
#> 1  1965 chl          arg         Chile            Argentina       
#> 2  1967 chl          arg         Chile            Argentina       
#> # … with 10 more variables: export_value_usd <int>,
#> #   import_value_usd <int>, export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>,
#> #   export_value_usd_change_5_years <int>,
#> #   export_value_usd_percentage_change_5_years <dbl>,
#> #   import_value_usd_change_5_years <int>,
#> #   import_value_usd_percentage_change_5_years <dbl>
```

Another option is to pass more than one year, reporter and/or partner:

```r
ots_create_tidy_data(years = 1965:1966, reporters = c("chl", "per"), partners = "arg", table = "yrp")
#> # A tibble: 2 x 11
#>    year reporter_iso partner_iso reporter_fullna… partner_fullnam…
#>   <int> <chr>        <chr>       <chr>            <chr>           
#> 1  1965 chl          arg         Chile            Argentina       
#> 2  1966 chl          arg         Chile            Argentina       
#> # … with 6 more variables: export_value_usd <int>, import_value_usd <int>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>
```

### Reporter trade at product level (Year - Reporter - Product Code) 

If we want Chilean trade at product level in 1965:

```r
ots_create_tidy_data(years = 1965, reporters = "chl", table = "yrc")
#> # A tibble: 907 x 16
#>     year reporter_iso reporter_fullna… product_code product_code_le…
#>    <int> <chr>        <chr>            <chr>                   <int>
#>  1  1965 chl          Chile            0101                        4
#>  2  1965 chl          Chile            0102                        4
#>  3  1965 chl          Chile            0103                        4
#>  4  1965 chl          Chile            0104                        4
#>  5  1965 chl          Chile            0105                        4
#>  6  1965 chl          Chile            0106                        4
#>  7  1965 chl          Chile            0201                        4
#>  8  1965 chl          Chile            0203                        4
#>  9  1965 chl          Chile            0204                        4
#> 10  1965 chl          Chile            0206                        4
#> # … with 897 more rows, and 11 more variables:
#> #   product_fullname_english <chr>, group_code <chr>, group_name <chr>,
#> #   export_value_usd <int>, import_value_usd <int>,
#> #   export_rca_4_digits_product_code <dbl>,
#> #   import_rca_4_digits_product_code <dbl>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>
```

If we want Chilean trade at product level in 1965 with respect to product "0101" (Horses, asses, mules and hinnies; live):

```r
ots_create_tidy_data(years = 1965, reporters = "chl", products = "0101", table = "yrc")
#> # A tibble: 1 x 16
#>    year reporter_iso reporter_fullna… product_code product_code_le…
#>   <int> <chr>        <chr>            <chr>                   <int>
#> 1  1965 chl          Chile            0101                        4
#> # … with 11 more variables: product_fullname_english <chr>,
#> #   group_code <chr>, group_name <chr>, export_value_usd <int>,
#> #   import_value_usd <int>, export_rca_4_digits_product_code <dbl>,
#> #   import_rca_4_digits_product_code <dbl>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>
```

But if we want just products exported by Chile in 1965 that also match the string "horse" at product or group name:

```r
ots_create_tidy_data(years = 1965, reporters = "chl", products = "horse", table = "yrc")
#> # A tibble: 30 x 19
#>     year reporter_iso reporter_fullna… product_code product_code_le…
#>    <int> <chr>        <chr>            <chr>                   <int>
#>  1  1965 chl          Chile            0101                        4
#>  2  1965 chl          Chile            0206                        4
#>  3  1965 chl          Chile            5606                        4
#>  4  1965 chl          Chile            5101                        4
#>  5  1965 chl          Chile            510111                      6
#>  6  1965 chl          Chile            510121                      6
#>  7  1965 chl          Chile            5102                        4
#>  8  1965 chl          Chile            510211                      6
#>  9  1965 chl          Chile            510220                      6
#> 10  1965 chl          Chile            5103                        4
#> # … with 20 more rows, and 14 more variables:
#> #   product_fullname_english <chr>, group_code <chr>, group_name <chr>,
#> #   export_value_usd <int>, import_value_usd <int>,
#> #   export_rca_4_digits_product_code <dbl>,
#> #   import_rca_4_digits_product_code <dbl>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>, observation <chr>,
#> #   export_rca_6_digits_product_code <dbl>,
#> #   import_rca_6_digits_product_code <dbl>
```

Another option is to pass more than one year, reporter and/or product:

```r
ots_create_tidy_data(years = c(1965,1967), reporters = c("chl", "per"), products = "apple", table = "yrc")
#> # A tibble: 4 x 21
#>    year reporter_iso reporter_fullna… product_code product_code_le…
#>   <int> <chr>        <chr>            <chr>                   <int>
#> 1  1965 chl          Chile            0804                        4
#> 2  1967 chl          Chile            0804                        4
#> 3  1965 chl          Chile            0808                        4
#> 4  1967 chl          Chile            0808                        4
#> # … with 16 more variables: product_fullname_english <chr>,
#> #   group_code <chr>, group_name <chr>, export_value_usd <int>,
#> #   import_value_usd <int>, export_rca_4_digits_product_code <dbl>,
#> #   import_rca_4_digits_product_code <dbl>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>,
#> #   export_value_usd_change_5_years <int>,
#> #   export_value_usd_percentage_change_5_years <dbl>,
#> #   import_value_usd_change_5_years <int>,
#> #   import_value_usd_percentage_change_5_years <dbl>, observation <chr>
```

Here the `export_rca*` and `import_rca*` fields contain the Revealed Comparative Advantage (RCA) of an exported product with respect to all the products with the same number of digits. The definition of RCA is detailed on [Open Trade Statistics Documentation](https://docs.tradestatistics.io/).

### Reporter trade at aggregated level (Year - Reporter)

If we want Chilean trade at aggregated level in 1965:

```r
ots_create_tidy_data(years = 1965, reporters = "chl", table = "yr")
#> # A tibble: 1 x 16
#>    year reporter_iso reporter_fullna… export_value_usd import_value_usd
#>   <int> <chr>        <chr>                       <int>            <int>
#> 1  1965 chl          Chile                  1012377535        800177433
#> # … with 11 more variables: eci_4_digits_product_code <dbl>,
#> #   eci_rank_4_digits_commodity_code <int>,
#> #   eci_rank_4_digits_commodity_code_delta_1_year <int>,
#> #   top_export_product_code <chr>, top_export_trade_value_usd <int>,
#> #   top_import_product_code <chr>, top_import_trade_value_usd <int>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>
```

Another option is to pass more than one year and/or reporter:

```r
ots_create_tidy_data(years = c(1965,1967), reporters = c("chl", "arg", "per"), table = "yr")
#> # A tibble: 2 x 21
#>    year reporter_iso reporter_fullna… export_value_usd import_value_usd
#>   <int> <chr>        <chr>                       <int>            <int>
#> 1  1965 chl          Chile                  1012377535        800177433
#> 2  1967 chl          Chile                  1269317121        882236802
#> # … with 16 more variables: eci_4_digits_product_code <dbl>,
#> #   eci_rank_4_digits_commodity_code <int>,
#> #   eci_rank_4_digits_commodity_code_delta_1_year <int>,
#> #   top_export_product_code <chr>, top_export_trade_value_usd <int>,
#> #   top_import_product_code <chr>, top_import_trade_value_usd <int>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>,
#> #   eci_rank_4_digits_commodity_code_delta_5_years <int>,
#> #   export_value_usd_change_5_years <int>,
#> #   export_value_usd_percentage_change_5_years <dbl>,
#> #   import_value_usd_change_5_years <int>,
#> #   import_value_usd_percentage_change_5_years <dbl>
```

Here some fields deserve an explanation:

* `eci_4_digits_product_code`: Economic Complexity Index (ECI) which is detailed on [Open Trade Statistics Documentation](https://docs.tradestatistics.io/). This index is built by using just four digits product codes.
* `eci_rank_4_digits_product_code`: The rank of a country given its ECI (e.g. the highest ECI obtains the #1)
* `eci_rank_4_digits_product_code_delta_1_year`: How many places a country increased or decreased with respect to last year

### Product trade at aggregated level (Year - Product Code)

If we want all products trade in 1965:

```r
ots_create_tidy_data(years = 1965, table = "yc")
#> # A tibble: 991 x 19
#>     year product_code product_code_le… product_fullnam… group_code
#>    <int> <chr>                   <int> <chr>            <chr>     
#>  1  1965 0101                        4 Horses, asses, … 01        
#>  2  1965 0102                        4 Bovine animals;… 01        
#>  3  1965 0103                        4 Swine; live      01        
#>  4  1965 0104                        4 Sheep and goats… 01        
#>  5  1965 0105                        4 Poultry; live, … 01        
#>  6  1965 0106                        4 Animals, n.e.c.… 01        
#>  7  1965 0201                        4 Meat of bovine … 02        
#>  8  1965 0203                        4 Meat of swine; … 02        
#>  9  1965 0204                        4 Meat of sheep o… 02        
#> 10  1965 0205                        4 Meat of horses,… 02        
#> # … with 981 more rows, and 14 more variables: group_name <chr>,
#> #   export_value_usd <dbl>, import_value_usd <dbl>,
#> #   pci_4_digits_product_code <dbl>, pci_rank_4_digits_product_code <int>,
#> #   pci_rank_4_digits_product_code_delta_1_year <int>,
#> #   top_exporter_iso <chr>, top_exporter_trade_value_usd <int>,
#> #   top_importer_iso <chr>, top_importer_trade_value_usd <int>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>
```

If we want traded values of product "0101" (Horses, asses, mules and hinnies; live) in 1965:

```r
ots_create_tidy_data(years = 1965, products = "0101", table = "yc")
#> # A tibble: 1 x 19
#>    year product_code product_code_le… product_fullnam… group_code
#>   <int> <chr>                   <int> <chr>            <chr>     
#> 1  1965 0101                        4 Horses, asses, … 01        
#> # … with 14 more variables: group_name <chr>, export_value_usd <int>,
#> #   import_value_usd <int>, pci_4_digits_product_code <dbl>,
#> #   pci_rank_4_digits_product_code <int>,
#> #   pci_rank_4_digits_product_code_delta_1_year <int>,
#> #   top_exporter_iso <chr>, top_exporter_trade_value_usd <int>,
#> #   top_importer_iso <chr>, top_importer_trade_value_usd <int>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>
```

If we want traded values of all products that match the string "horse" at product or group name:

```r
ots_create_tidy_data(years = 1965, products = "horse", table = "yc")
#> # A tibble: 43 x 23
#>     year product_code product_code_le… product_fullnam… group_code
#>    <int> <chr>                   <int> <chr>            <chr>     
#>  1  1965 0101                        4 Horses, asses, … 01        
#>  2  1965 0205                        4 Meat of horses,… 02        
#>  3  1965 0206                        4 Edible offal of… 02        
#>  4  1965 5110                        4 Yarn of coarse … 51        
#>  5  1965 5113                        4 Woven fabrics o… 51        
#>  6  1965 5606                        4 Yarn and strip … 56        
#>  7  1965 5101                        4 Wool, not carde… 51        
#>  8  1965 510111                      6 Wool; (not card… 51        
#>  9  1965 510121                      6 Wool; (not card… 51        
#> 10  1965 5102                        4 Fine or coarse … 51        
#> # … with 33 more rows, and 18 more variables: group_name <chr>,
#> #   export_value_usd <int>, import_value_usd <int>,
#> #   pci_4_digits_product_code <dbl>, pci_rank_4_digits_product_code <int>,
#> #   pci_rank_4_digits_product_code_delta_1_year <int>,
#> #   top_exporter_iso <chr>, top_exporter_trade_value_usd <int>,
#> #   top_importer_iso <chr>, top_importer_trade_value_usd <int>,
#> #   export_value_usd_change_1_year <int>,
#> #   export_value_usd_percentage_change_1_year <dbl>,
#> #   import_value_usd_change_1_year <int>,
#> #   import_value_usd_percentage_change_1_year <dbl>, observation <chr>,
#> #   pci_6_digits_product_code <dbl>, pci_rank_6_digits_product_code <int>,
#> #   pci_rank_6_digits_product_code_delta_1_year <int>
```

Here some fields deserve an explanation:

* `pci_4_digits_product_code`: Product Complexity Index (PCI) which is detailed on [Open Trade Statistics Documentation](https://docs.tradestatistics.io/). This index is built by using just four digits product codes.
* `pci_6_digits_product_code`: Similar to the previous field but built by using just six digits product codes.
* `pci_rank_4_digits_product_code`: The rank of a product given its PCI (e.g. the highest PCI obtains the #1)
* `pci_rank_4_digits_product_code_delta_1_year`: How many places a country increased or decreased with respect to last year
