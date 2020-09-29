# prepare_cd113_sld2012_v20200527.r
# Author: David Van Riper
# # Date: 2020-09-29
# 
# Load in the crosswalks that James Whitehorne (Census Redistricting Office ) provided me on 2020-09-26. This crosswalk allows me to create 
# demographic summaries for post-2010 round of redistricting. 
# 
# There are three crosswalks (CD113, SLDL, SLDU), but I want to create a single crosswalk for ease of processing.

require(data.table)

#### Constants #### 
data_path <- "data/"
cd113 <- "National_CD113.txt.gz"
sldl <- "National_SLDL.txt.gz"
sldu <- "National_SLDU.txt.gz"

col_classes <- c("character", "character")

#### Read in data files ####
xwalk_cd113 <- fread(paste0(data_path, cd113), colClasses = c("character", "character"))
xwalk_sldl <- fread(paste0(data_path, sldl), colClasses = c("character", "character", "character"))
xwalk_sldu <- fread(paste0(data_path, sldu), colClasses = c("character", "character", "character"))

#### Rename columns in sldl and sldu so they don't conflict #### 
setnames(xwalk_sldl, "DISTRICT", "DISTRICT_SLDL")
setnames(xwalk_sldl, "NAME", "NAME_SLDL")
setnames(xwalk_sldu, "DISTRICT", "DISTRICT_SLDU")
setnames(xwalk_sldu, "NAME", "NAME_SLDU")

#### Set keys ####
setkey(xwalk_cd113, BLOCKID)
setkey(xwalk_sldl, BLOCKID)
setkey(xwalk_sldu, BLOCKID)

#### Join to create a single crosswalk ####
xwalk_leg <- xwalk_sldl[xwalk_sldu][xwalk_cd113]

#### Create GISJOIN on crosswalk file #### 
xwalk_leg[, state := substr(BLOCKID, 1, 2)] 
xwalk_leg[, cty := substr(BLOCKID, 3, 5)]
xwalk_leg[, tct := substr(BLOCKID, 6, 11)]
xwalk_leg[, blk := substr(BLOCKID, 12, 15)]

xwalk_leg[, gisjoin := paste0("G", state, "0", cty, "0", tct, blk)]

#### Remove unnecessary fields #### 
xwalk_leg[, c("BLOCKID", "cty", "tct", "blk") := NULL]

#### Set key on gisjoin #### 
setkey(xwalk_leg, gisjoin)

#### Load in necessary tables from Sept 2020 release #### 
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
p8 <- fread("/pkg/ipums/misc/census-das/ppmf-tabs/data/output/block_p8.csv.gz", colClasses = col_classes_p8)
p9 <- fread("/pkg/ipums/misc/census-das/ppmf-tabs/data/output/block_p9.csv.gz", colClasses = col_classes_p9)
p10 <- fread("/pkg/ipums/misc/census-das/ppmf-tabs/data/output/block_p10.csv.gz", colClasses = col_classes_p10)
p11 <- fread("/pkg/ipums/misc/census-das/ppmf-tabs/data/output/block_p11.csv.gz", colClasses = col_classes_p11)
p42 <- fread("/pkg/ipums/misc/census-das/ppmf-tabs/data/output/block_p42.csv.gz", colClasses = col_classes_p42)

#### Set keys #### 
setkeyv(p8, geog_vars)
setkeyv(p9, geog_vars)
setkeyv(p10, geog_vars)
setkeyv(p11, geog_vars)
setkeyv(p42, geog_vars)

#### Merge together data.tables #### 
dp <-p8[p9][p10][p11][p42]

#### Rename vars in dp dt #### 
# temp_from <- grep("^H", names(dp), value = TRUE)
# temp_to <- paste0(temp_from, "_dp")
# 
# correct_columns <- c(geog_vars, temp_to)
# 
# setnames(dp, names(dp), correct_columns)

#### Generate gisjoin #### 
dp[, gisjoin := paste0("G", TABBLKST, "0", TABBLKCOU, "0", TABTRACTCE, TABBLK)]

#### Remove TABBLKST, TABBLKCOU, TABTRACTCE, and TABBLK from dp #### 
dp[, c("TABBLKST", "TABBLKCOU", "TABTRACTCE", "TABBLK") := NULL]

#### Set key on gisjoin for dp] ####
setkey(dp, "gisjoin")

#### Join up the xwalk to create the district counts #### 
dp_xwalk <- xwalk_leg[dp]

#### Create the summaries #### 
congress2013 <- dp_xwalk[, lapply(.SD, sum),
                         by = .(state, CD113),
                         .SDcols = H72001_dp:H80010_dp]

lowerSLD2012 <- dp_xwalk[, lapply(.SD, sum),
                         by = .(state, DISTRICT_SLDL, NAME_SLDL),
                         .SDcols = H72001_dp:H80010_dp]

upperSLD2012 <- dp_xwalk[, lapply(.SD, sum),
                         by = .(state, DISTRICT_SLDU, NAME_SLDU),
                         .SDcols = H72001_dp:H80010_dp]

#### Write out to CSV #### 
fwrite(congress2013, "data/congress2013_v20200527.csv")
fwrite(lowerSLD2012, "data/lowerSLD2012_v20200527.csv")
fwrite(upperSLD2012, "data/upperSLD2012_v20200527.csv")