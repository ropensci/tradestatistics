# Version 3.0

Updates
* Removes all references to tables using communities or short names 
 (both unofficial), reflecting changes in the API
* The functionality remains the same, but now the end user functions don't
 add a 21-colors palette to the data (i.e. see the data section)

Data

* Switches from HS92  to HS12 to reflect product changes with less aggregation
* Drops any data from Harvard (communities and short product names) as these
 depend on using HS92 4 digits, therefore the color palettes were removed as
 these depended on the communities table
* The inflation data was trimmed to a window since the year 2000
* The commodities data now contains information for +5000 products instead of
 +1200 as the aggregation level changed in the API
* Adds RTAs and MFN tariffs for gravity modelling

# Version 2.0

Updates 

* Uses ISO codes as is (affects Aruba, Roumania, Timor-Leste, Antarctica, 
 Saint Barthelemy, Curacao, Sint Maarten and South Sudan)
  
# Version 1.0

Updates

* Reflects API changes with less aggregated data
* Follows UN COMTRADE notation (i.e. commodity instead of product)
* Does not impute data before hand, which is better for most of gravity models use cases
* Provides the data exactly as in the API, returning commodity level data to allow users to do their own aggregation
* Does not drop reference year with inflation adjustment (https://github.com/ropensci/tradestatistics/issues/38)
* Takes max and min available years from the API instead of hardcoded values (https://github.com/ropensci/tradestatistics/pull/39)

# Version 0.4.0

Updates

* Includes `yrpc-ga`, `yrpc-sa`, `yrc-ga` and `yr-sa` tables reflecting API updates
* Simplifies end-user functions a bit (i.e. removes `include_groups` option)
* Optimizes the code a bit, specially at the joins with tables in the package
* Fixes codes duplication when both product and group/community match for a search
* Includes both official and shortened section names

# Version 0.3.1

Updates

* Removes `yrp_short` option reflecting last DB changes

# Version 0.3

Updates

* Much improved coverage to detect almost any possible error
* Fixes case in inflation adjustment when year = reference year

# Version 0.2.8

Updates

* Adds caching (in memory or on disk) option
* Lists Daniela de los Santos and Elio Campitelli as new contributors
* Includes forwards and backwards testing for inflation adjustment
* Testing for in memory caching

# Version 0.2.7

Updates

* Adds feedback provided by Daniela de los Santos
* Now ots_create_tidy_data() has both reporter and partner set to "all" by default

# Version 0.2.5

Updates

* Added dependency on R >= 3.5.0 because serialized objects in serialize/load version 3 cannot be read in older versions of R
* Minimal changes in `ots_create_tidy_data()` to allow multiple countries as arguments, in line with API changes from September 2019

# Version 0.2.4

Updates

* Removes `product_code_length`
* The API was updated with simplified parameters and 2018 data

# Version 0.2.3

Updates

* Fixtures for testthat evaluation

Fixes

* Specific Windows error during check

# Version 0.2.2

Adds

* Inflation data
* Inflation adjustment function
* Minor changes in vignettes

# Version 0.2.1

Fixes

* Consistent use of colour vs color, color is used from now
* Fixed available tables description
* Adds `yrp_short` to available tables
* Adds `use_localhost` option for our own server or users who want to clone the
  database locally, therefore avoid having a separate branh for server installation
  
