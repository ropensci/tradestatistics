# version 4.1

Breaking changes
* New port for local instance (8080 -> 4949)

# version 4.0

Updates
* Provides ysrpc table to visualize products, replacing yc tables.
* The new default option is to download imputed data, as there is no
  direct API access to raw data.
* A new method to correct import/exports mismatches was applied and the datasets
  now start from 1980 until I test enough the model results for older years.
* Drops yr-sections and yr-groups tables
* Drops 'group' columns in the final data (and replaces it for 'section' 
  columns)
* Allows to access both raw and imputed data from the API
* Allows to search for special codes in the API (i.e., e-490 and other codes)
* All these changes have resulted in large speedups with both data downloading 
  and the Open Trade Statistics dashboard

Breaking changes
* Most of the tables in the API were renamed, as now I made available an
  imputation method to remove transportation costs (and correct mismatching
  flows)

# version 3.0.3

Minor fixes
* Adds attributes to parquet tables (i.e., it now does the joins to
  add product name, section color, etc.)

Updates
* Uses "_name" instead of "_fullname_english" in final tables colnames

# version 3.0.2

Updates
* Allows to obtain tables in Parquet format from the API, giving a speed-up
  of ~50% for the final user.
* Uses tibble instead of DT to produce lighter vignettes
  
# version 3.0.1

Updates
* Adds section colors data for visualization, this is taken from the palette
  used in shiny.tradestatistics.io

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
* Does not impute data before hand, which is better for most of gravity models 
  use cases
* Provides the data exactly as in the API, returning commodity level data to 
  allow users to do their own aggregation
* Does not drop reference year with inflation adjustment 
  (https://github.com/ropensci/tradestatistics/issues/38)
* Takes max and min available years from the API instead of hardcoded values 
  (https://github.com/ropensci/tradestatistics/pull/39)

# Version 0.4.0

Updates

* Includes `yrpc-ga`, `yrpc-sa`, `yrc-ga` and `yr-sa` tables reflecting API 
  updates
* Simplifies end-user functions a bit (i.e. removes `include_groups` option)
* Optimizes the code a bit, specially at the joins with tables in the package
* Fixes codes duplication when both product and group/community match for a 
  search
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

* Adds caching (in memory or on disk) option (partial contributions from 
  @eliocamp)
* Includes forwards and backwards testing for inflation adjustment
* Testing for in memory caching

# Version 0.2.7

Updates

* Adds feedback provided by @danidlsa
* Now ots_create_tidy_data() has both reporter and partner set to "all" by 
  default

# Version 0.2.5

Updates

* Added dependency on R >= 3.5.0 because serialized objects in serialize/load 
  version 3 cannot be read in older versions of R
* Minimal changes in `ots_create_tidy_data()` to allow multiple countries as 
 arguments, in line with API changes from September 2019

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
  database locally, therefore avoid having a separate branh for server 
  installation
  
