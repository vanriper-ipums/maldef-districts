# prepare_v20201116.r
# Author: David Van Riper
# # Date: 2020-11-16
# 
# Load in the crosswalk that Steven Ochoa (MALDEF) provided me on 2020-09-26. This crosswalk allows me to create demographic summaries for 2018 legislative
# districts for the v20201116 DP data. 

require(data.table)
require(tidyverse)

#### Constants #### 
data_path <- "data/"
crosswalk_file <- "BlockEquiv_StateLegDistricts.csv.gz"
col_class <- c("character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character" )

#### Load file ####
xwalk <- fread(paste0(data_path, crosswalk_file), sep = ",", colClasses = col_class)

#### Create GISJOIN on crosswalk file #### 
xwalk[, cty := substr(County, 3, 5)]
xwalk[, tct := substr(Tract, 6, 11)]
xwalk[, blk := substr(Block, 12, 15)]

xwalk[, gisjoin := paste0("G", State, "0", cty, "0", tct, blk)]

#### Set key on gisjoin #### 
setkey(xwalk, gisjoin)

#### QA/QC for county changes #### 
# Looks like the block county codes reflect those from 2010 decennial sf 
#cty_count <- xwalk[, .N, by = .(State, cty)][order(State, cty)]

#### Load in NHGIS SF redistricting data #### 
sf <- fread("/pkg/ipums/misc/census-das/ppmf-tabs-20201116/data/sf1/nhgis1364_csv/sf_nhgis1364_ds172_2010_block.csv.gz", sep = ",")

#### Set the key field as gisjoin ####
setkey(sf, gisjoin)

#### How many blocks have zero population ####
# 4,897,539 blocks with zero pops 
#zero <- sf[H72001_sf == 0]

#### Load in necessary tables from Nov 2020 release #### 
#### Constant "character" vector #### 
char_all <- rep("character", 4)
geog_vars <- c("TABBLKST", "TABBLKCOU", "TABTRACTCE", "TABBLK")

#### Create vector if "integer" values #### 
int_p8 <- rep("integer", 71)
int_p9 <- rep("integer", 73)
int_p10 <- rep("integer", 71)
int_p11 <- rep("integer", 73)
int_p42 <- rep("integer", 10)

#### Column classes ####
col_classes_p8 <- c(char_all, int_p8)
col_classes_p9 <- c(char_all, int_p9)
col_classes_p10 <- c(char_all, int_p10)
col_classes_p11 <- c(char_all, int_p11)
col_classes_p42 <- c(char_all, int_p42)

#### Load data in using correct col_classes #### 
p8 <- fread("/pkg/ipums/misc/census-das/ppmf-tabs-20201116/data/output/block_p8.csv.gz", colClasses = col_classes_p8)
p9 <- fread("/pkg/ipums/misc/census-das/ppmf-tabs-20201116/data/output/block_p9.csv.gz", colClasses = col_classes_p9)
p10 <- fread("/pkg/ipums/misc/census-das/ppmf-tabs-20201116/data/output/block_p10.csv.gz", colClasses = col_classes_p10)
p11 <- fread("/pkg/ipums/misc/census-das/ppmf-tabs-20201116/data/output/block_p11.csv.gz", colClasses = col_classes_p11)
p42 <- fread("/pkg/ipums/misc/census-das/ppmf-tabs-20201116/data/output/block_p42.csv.gz", colClasses = col_classes_p42)

#### Set keys #### 
setkeyv(p8, geog_vars)
setkeyv(p9, geog_vars)
setkeyv(p10, geog_vars)
setkeyv(p11, geog_vars)
setkeyv(p42, geog_vars)

#### Merge together data.tables #### 
dp <-p8[p9][p10][p11][p42]

#### Generate gisjoin #### 
dp[, gisjoin := paste0("G", TABBLKST, "0", TABBLKCOU, "0", TABTRACTCE, TABBLK)]

#### Remove TABBLKST, TABBLKCOU, TABTRACTCE, and TABBLK from dp #### 
dp[, c("TABBLKST", "TABBLKCOU", "TABTRACTCE", "TABBLK") := NULL]

#### Set key on gisjoin for dp] ####
setkey(dp, "gisjoin")

#### Merge the nhgis crosswalk with dp data #### 
# # I need to join the DP counts onto the SF1 counts so that I don't miss any SF data. There are blocks with SF counts but no people in DP.
# After doing the join, I need to replace all NAs with 0
dp <- dp[sf]

#### Convert NAs to zeroes in dp dt #### 
dp[is.na(dp)] = 0

#### Join up the xwalk to create the district counts #### 
dp_xwalk <- xwalk[dp]

#### Remove "name" from dp_xwalk ####
dp_xwalk[, name := NULL]

#### Create the summaries #### 
congress2016 <- dp_xwalk[, lapply(.SD, sum),
                         by = .(State, Congress116),
                         .SDcols = H72001_dp:H80010_sf]

lowerSLD2018 <- dp_xwalk[, lapply(.SD, sum),
                         by = .(State, LowerSLD2018),
                         .SDcols = H72001_dp:H80010_sf]

upperSLD2018 <- dp_xwalk[, lapply(.SD, sum),
                         by = .(State, UpperSLD2018),
                         .SDcols = H72001_dp:H80010_sf]

#### Write out to CSV #### 
fwrite(congress2016, "data/congress2016_v20201116.csv")
fwrite(lowerSLD2018, "data/lowerSLD2018_v20201116.csv")
fwrite(upperSLD2018, "data/upperSLD2018_v20201116.csv")

#### Create ZIP archive ####
zipr("data/v20201116_maldef_districts.zip", c("data/upperSLD2018_v20201116.csv","data/lowerSLD2018_v20201116.csv", "data/congress2016_v20201116.csv", "/pkg/ipums/misc/census-das/ppmf-tabs-20201116/data/output/nhgis_ppdd_20201116_codebook.txt"))



