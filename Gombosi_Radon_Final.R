# Krystian Gombosi
# EOS 192

library(maps)
library(sf)
library(jsonlite)
library(RColorBrewer)
library(sp)
library(Hmisc)
library(corrplot)

setwd("~/Desktop/Statistical Analysis in EOS/radon")

# ----------------------------- Acquire and clean data -------------------------
# Turn off spherical coordinates
sf_use_s2(FALSE)

# Load in list of zip codes, radon data by zip code, US zip codes shapefile, 
# and iowa geology shapefile
iazip.list = read.csv("iaZips.csv")
radon.2019 = read.csv("Radon_ZipCode_2019.csv")
zc = st_read("ZIP/tl_2021_us_zcta520.shp") 
ia.geo = st_read("IA/IA_geol_poly.shp") 

# extract only example zip codes
ia.zip <- zc[is.element(zc$ZCTA5CE20, iazip.list$zip),]

# Check coordinate systems
st_crs(ia.geo) # uses WGS84
st_crs(ia.zip) # uses NAD 83

# Convert to WGS84
ia.zip <- st_transform(ia.zip, crs = 4326) # the code for WGS84 is 4326
st_crs(ia.zip) # now in WGS84

# Plot the data so far to make sure the zip codes are looking right
par(mgp=c(1,1,0), mar = c(3,3,4,3))
map('state', 'iowa')
plot(ia.zip$geometry, add=T, main = "Iowa Zip Codes")

# get intersection of zipcodes and geological maps
# output is an sf object--each row is the clipped geologic unit polygon from the geologic map
# the output includes for each row the zip code ("ZCTA5CE20") and geologic unit id ("GEOID20") 
zip.geo <- st_intersection(ia.geo, ia.zip)
ia.zip$Zip_Code = ia.zip$GEOID20

# plot geology data to make sure everything looks good
coul2 = brewer.pal(4, "Accent")
map('state','iowa')
plot(zip.geo$geometry, add=T)

# now you can do thing like calculate the area of each geologic map polygon
geo.area <- st_area(zip.geo)
length(geo.area)

# ---------------------------- Stop here ---------------------------------------

# If you want to run the loop calling the API, run the next block of code. This
# loop works, but takes a VERY long time (on the order of an hour). If you wish
# to skip the loop, I have included the output of the loop in my data files. 
# Skip to line 130 if you want to save some time. However, the code will work 
# either way.

# ------------------------------ loop -----------------------------------------
# extract geologic data from api in json format--use the "UNIT_LINK" field in 
# zip.geo
base.url <- "https://mrdata.usgs.gov/geology/state/json/"

# Initialize data frames for API loop
units <- zip.geo$UNIT_LINK
zips <- zip.geo$ZCTA5CE20
lith.data1 <- data.frame() # empty data frame to store lithological data
lith.data1 <- data.frame()
lith.data2 <- data.frame()

# For some reason, loop stops at i = 3162 (stack overflow error?). Fix this 
# by running two iterations of the loop and then using rbind to combine
# the results
for(i in 1:length(units)) {
  unit.url <- paste0(base.url, units[i])
  temp <- fromJSON(unit.url)
  lith <- temp$lith # pulls out just the lithology data
  # column lith1 gives sedimentary, igneous, metamorphic
  # column lith_rank gives major, minor, incidental
  # column lith_low gives the finest lithological classification
  
  # add columns for zip code and unit area
  lith$zip <- zips[i]
  lith$area <- geo.area[i]
  
  # new lithology append 
  lith.data1 <- rbind(lith.data, lith)
  
  print(i) # keep track of loop progress
}

for(i in 3162:length(units)) {
  unit.url <- paste0(base.url, units[i])
  temp <- fromJSON(unit.url)
  lith <- temp$lith # pulls out just the lithology data
  # column lith1 gives sedimentary, igneous, metamorphic
  # column lith_rank gives major, minor, incidental
  # column lith_low gives the finest lithological classification
  
  # add columns for zip code and unit area
  lith$zip <- zips[i]
  lith$area <- geo.area[i]
  
  # new lithology append 
  lith.data2 <- rbind(lith.data, lith)
  
  print(i) # keep track of loop progress
}

# Make data names easier to understand
lith.data3162to7596 <-lith.data2
lith.data1to3161 <- lith.data1

# bind lithology data for whole state
lith.data = rbind(lith.data1to3161,lith.data3162to7596)

# Save lithology data to external file to prevent having to run the loop again.
# Loop takes a very long time to run (on the order of an hour). If you want to 
# skip the loop and read in the data file that I created after running the loop,
# run line 130.
write.csv(lith.data,"ZIP_Lithology.csv", row.names = FALSE) 

# ------------------ Start here again if you skipped the loop ------------------

lith.data = read.csv("ZIP_Lithology.csv")

# ------------------------- Initial Observations -------------------------------

# Find overall counts of primary lithologies
lith1.table = data.frame(table(lith.data$lith1))
colnames(lith1.table)=c("Lithology", "Count")

# Not too interesting. Maybe area will be better instead
barplot(lith1.table$Count, names.arg=c(lith1.table$Lithology))

# remove duplicate lithologies
lith_unique = lith.data
lith_unique = lith_unique[!duplicated(lith_unique$objectid),]
# check to see all unique lithologies
length(unique(lith.data$objectid)) # 167 instances, all unique ID's found

# Create subsets of unique primary lithologies 
sed = subset(lith_unique, lith1=="Sedimentary")
ig = subset(lith_unique, lith1=="Igneous")
unc = subset(lith_unique, lith1 == "Unconsolidated")
met = subset(lith_unique, lith1 == "Metamorphic")

# Sum the area of each area of primary lithology
sum_sed = sum(sed$area)
sum_ig = sum(ig$area)
sum_unc = sum(unc$area)
sum_met = sum(met$area)
c2 = c(sum_sed, sum_ig,sum_unc,sum_met)
c1 = c("Sedimentary", "Igneous","Unconsolidated","Metamorphic")

lith_sums = data.frame(c1,c2)
lith_sums$c2 = as.numeric(lith_sums$c2)

# Plot areas
coul <- brewer.pal(4, "Accent")
par(mgp=c(2.5,1,0), mar = c(3,4,3,1))
barplot(log10(lith_sums$c2),names=c(lith_sums$c1), cex.axis=1, cex.names = 1, 
        ylim=c(0,10), col = coul[1:4], 
        ylab = expression(paste("log"[10]," Area, ", m^2)), main =
          "Primary Lithologies by Area, Iowa")


# -------------------------- start looking at secondary lithology --------------

# might be enough variation is secondary lithology to look at statistical 
# difference
lith2.tab=data.frame(table(lith_unique$lith2))
lith3.tab = data.frame(table(lith_unique$lith3))

# --------------------- Are there differences in radon by ZIP? -----------------

# Read in radon data, subset to only data points were there are no mitigation
# in place to prevent high radon concentrations 
radon = read.csv("Radon_ZipCode_2019.csv")
rad_unm = subset(radon, Mitigation == "Unmitigated")

# Create a map of radon concentrations by zip for the available unmitigated 
# data
m <- merge(ia.zip, rad_unm, by='Zip_Code')
m2 = as(m, Class = "Spatial")

zip1 = as(ia.zip$geometry, Class = "Spatial")
zip2 = list("sp.lines", zip1, col = "grey", lwd = 0.5)

spplot(m2, "MEDIAN_of_Test_Result_Radon",  
       par.settings = list(axis.line = list(col = "transparent")),
       main = "Radon Concentration by Zip Code",
       sub = "Median Radon Concentration (pCi/L)", 
       col = "transparent",
       sp.layout = zip2)

# Test for normal distribution
shapiro.test(rad_unm$MEDIAN_of_Test_Result_Radon)
# Normality test fails

# Make a plot to visualize distributions with various transformations
par(mfrow=c(1,3))
par(mgp=c(2.5,1,0), mar = c(5,4,3,1))
hist(rad_unm$MEDIAN_of_Test_Result_Radon, 
     xlab = expression(paste("Median Radon Concentration (", pCi/L,")")),
     ylab = "Count", 
     main = "")
hist(log10(rad_unm$MEDIAN_of_Test_Result_Radon), 
     xlab = expression(paste("log"[10]," Median Radon Concentration (", pCi/L,")")),
     ylab = "Count", 
     main = "Transformations")
hist(log(rad_unm$MEDIAN_of_Test_Result_Radon), 
     xlab = expression(paste("log"[e]," Median Radon Concentration (", pCi/L,")")),
     ylab = "Count", 
     main = "")

# Make a plot of the best transformation
par(mfrow=c(1,1))
hist(log(rad_unm$MEDIAN_of_Test_Result_Radon), ylim = c(0,150),
     xlab = expression(paste("log"[e]," Median Radon Concentration (", pCi/L,")")),
     ylab = "Count", 
     main = "")
segments(log(4), 0,log(4),150, col = "red", lwd = 3)
text("EPA Action Level (4 pCi/L)", x = 2.35, y = 145, col = "red")
# Data is close to a normal distribution. Add a column to the data frame 
# with the transformed data to make future analysis easier
rad_unm$medtrans.e = log(rad_unm$MEDIAN_of_Test_Result_Radon)
rad_unm$count = c(1:length(rad_unm$Zip_Code))


# Scatter plot of concentration by zip code
par(mgp=c(2.5,1,0), mar = c(4,4,3,1))
plot(MEDIAN_of_Test_Result_Radon~count, data=rad_unm, xaxt = "n", pch = 18,
     xlab = "Zip Code, Not Individually Labeled", 
     ylab = "Median Radon Concentration (pCi/L)",
     main = "Radon Concentrations by Zip Code")
segments(0,4,rad_unm$count, 4, col = "red", lwd = 3)

# There seems to be a difference by Zip code in median radon concentrations in 
# radon unmitigated homes

# ------------------ find dominant major lithology by ZIP ----------------------

# Screen for only major lithologies
major = subset(lith.data, lith_rank == "Major")
unique(lith.data$lith_rank)
# Sets up vectors for the loop for dominant lithology, lithology area, and 
# zip codes
dom.lith = vector(mode ="character", length = length(unique(major$zip)))
area = vector(mode = "numeric", length = length(unique(major$zip)))
allzip = unique(major$zip)

# Loop to search for every zip code and find the major lithology with the 
# largest area.
for(i in 1:length(allzip)) {
  tempzip = subset(major, zip == allzip[i])
  tempzip = tempzip[!duplicated(tempzip),]
  tempmax=aggregate(x = tempzip$area,               
               by = list(tempzip$lith1),              
               FUN = sum) 
  
  sumtab=tempmax[which(tempmax$x == max(tempmax$x)), ]
  
  dom.lith[i] = sumtab$Group.1
  area[i] = sumtab$x
}

# Creates a data frame of the dominant primary lithologies for each area
# code
dominant_lith1 = data.frame(allzip, dom.lith, area)
colnames(dominant_lith1) = c("ZIP", "Dominant_lithology_1", "Area (m^2)")
unique(dominant_lith1$Dominant_lithology_1)
Lith1 = c("Sedimentary", "Metamorphic", "Igneous", "Unconsolidated")
l1.counts = c(length(dominant_lith1$Dominant_lithology_1), 0,0,0)
l1counts = data.frame(Lith1, l1.counts)

# Plot dominant primary lithology by area
coul <- brewer.pal(4, "Accent")
par(mgp=c(2.5,1,0), mar = c(3,4,3,1))
barplot(l1counts$l1.counts,names=c(l1counts$Lith1), cex.axis=1, cex.names = 1, 
        ylim=c(0,1000), col = coul[1:4], 
        ylab = "Number of Zip Codes", main =
          "Dominant Primary Lithology by Zip Code")
# Unfortunately, the dominant primary lithology for every zip code is 
# sedimentary.It will be necessary to look at the secondary lithology.

# -------------------- Investigate secondary lithology -------------------------

# Initialize vectors for dominant secondary lithology loop
dom.lith2 = vector(mode = "character", length = length(unique(major$zip)))
area2 = vector(mode = "numeric", length = length(unique(major$zip)))
area2 = -99
two_dominant_lith2 = data.frame(matrix(nrow=0,ncol=3))


for(i in 1:length(allzip)) {
  tempzip = subset(major, zip == allzip[i])
  # remove erroneous duplicates
  tempzip = tempzip[!duplicated(tempzip),]
  tempmax=aggregate(x = tempzip$area,               
                    by = list(tempzip$lith2),              
                    FUN = sum) 
  
  # Find dominant secondary lithology by area. If two lithologies have the 
  # same area, set these data points aside in a new data frame
  sumtab=tempmax[which(tempmax$x == max(tempmax$x)), ]
  if(length(sumtab$Group.1) != 1) {
    tempcount = length(sumtab$Group.1)
    lt = vector(mode = "character", length = tempcount)
    at = vector(mode = "numeric", length = tempcount)
    zt = vector(mode = "numeric", length = tempcount)
    zt = rep(c(allzip[i]), each = tempcount)
    lt = sumtab$Group.1
    at = sumtab$x
    dft = data.frame(zt,lt,at)
    two_dominant_lith2 = rbind(two_dominant_lith2, dft)
  }
  else {
  dom.lith2[i] = sumtab$Group.1
  area2[i] = sumtab$x 
  }
}

# Clean up results of loop
colnames(two_dominant_lith2) = c("Zip", "Lithologies", "Areas")
dominant_lith2 = data.frame(allzip, dom.lith2, area2)
colnames(dominant_lith2) = c("Zip", "Lithology", "Area")

# Remove NA values introduced by looping
dominant_lith2 = dominant_lith2[complete.cases(dominant_lith2),]

# Create a plot of dominant secondary lithologies
colL2 = brewer.pal(3, "Dark2")
l2.tab = data.frame(table(dominant_lith2$Lithology))
barplot(l2.tab$Freq, names = c(l2.tab$Var1), ylim = c(0,600), 
        ylab = "Number of Zip Codes", 
        main = "Dominant Secondary Lithology by Zip Code",
        col = colL2[l2.tab$Var1])

# Final datasets: dominant_lith2, which is a data frame containing the single
# dominant lithology and area for each zip code, and two_dominant_lithologies
# which is a data frame that contains the lithologies and areas for zip codes
# where two lithologies were both dominant by area (a tie).

# ---------------------- statistical analysis, lith 2 --------------------------

# Merge radon data with dominant secondary lithology data
radon_unmitigated = rad_unm
colnames(radon_unmitigated) = c("Test_year", "Zip", "Mitigation", "Count_test", 
                                "Mean", "Max", "Min", "Median", "logemed", 
                                "count")
# Test for equal variance
radon_lith2 = merge(dominant_lith2, radon_unmitigated, by = "Zip", rm.na = T)
bartlett.test(logemed~Lithology, data = radon_lith2)
# Bartlett test rejects the null hypothesis that the variance is the same
# in both the Carbonate and Clastic lithologies (p = 0.00441).

# Run a t test by lithology
t.test(logemed~Lithology, data = radon_lith2)
# There is a significant difference between clastic and carbonate radon risk. 
# By two-sample T test of unequal variance, the true difference in means is 
# not equal to zero (p = 0.000595)

# Retrieve data from t tests for plotting
clastic = subset(radon_lith2, Lithology == "Clastic")
carbonate = subset(radon_lith2 , Lithology == "Carbonate")
clas_t = t.test(clastic$logemed)
clas_lower = clas_t$conf.int[1]
clas_upper = clas_t$conf.int[2]

carb_t = t.test(carbonate$logemed, conf.level = .95, paired = F)
carb_lower = carb_t$conf.int[1]
carb_upper = carb_t$conf.int[2]

# Make a bar plot of radon by lithology including confidence intervals
par(mgp=c(2,1,0), mar = c(4,3.5,3,2))
boxplot((radon_lith2$logemed)~radon_lith2$Lithology, xlab = "",
        ylab = expression(paste("Log"[e]," Median Radon Concentration (pCi/L)")),
        main = "Distribution of Radon Concentrations by Secondary Lithology",
        col = colL2[l2.tab$Var1], notch = T, range = 0, ylim = c(-1.5, 3))
text(paste("95% CI: [", round(carb_lower,2), ",",
           round(carb_upper,2), "]"),  x = 1, y= -1.5, cex = .8)
text(paste("95% CI: [", round(clas_lower,2), ",",
           round(clas_upper,2), "]"),  x = 2, y= -1.5, cex = .8)
radon_lith2$count = c(1:length(radon_lith2$Zip))

# Make a scatterplot of the same data
plot(Median~count, data=radon_lith2, xaxt = "n", pch = 16,
     xlab = "Zip Code, Not Individually Labeled", 
     ylab = "Median Radon Concentration (pCi/L)",
     main = "Radon Concentrations by Zip Code",
     col = colL2[l2.tab$Var1])
segments(0,4,radon_lith2$count, 4, col = "red", lwd = 3)
legend("topleft", legend=levels(l2.tab$Var1),  
       fill = colL2, bty = "n", ncol = 1, pt.cex = 2)
# --------------------------- logistic regression ------------------------------

# Create binary variables for clastic and carbonate lithologies and high and 
# low radon concentrations. Definitions below loop

for(i in 1:length(radon_lith2$Median)) {
  if(radon_lith2$Median[i] >=4) {radon_lith2$highradon[i] = 1}
  else {radon_lith2$highradon[i] = 0}
  
  if(radon_lith2$Lithology[i] == "Clastic") { radon_lith2$lithcat[i] = 1}
  else {radon_lith2$lithcat[i] = 0}
}

# Carbonate = 0
# Below exposure risk = 0

# Run a logistic regression of high radon grouped by lithology
logreg1 = glm(formula = radon_lith2$highradon~radon_lith2$lithcat,
              family = "binomial")
summary(logreg1)
exp(coef(logreg1))
# Individuals living above clastic lithology have 2.56 times the odds of having
# high radon, defined as higher than 4 pCi/L
exp(confint(logreg1))
# 95% confidence interval from 1.73 - 3.87 x the odds.

# ------------------- logistic regression, minor lithology ---------------------

# redefine a variable for zip codes
zips = unique(lith.data$zip)

# Set up data frames for loop
percentages = as.data.frame(matrix(nrow = length(zips), ncol = 5))
colnames(percentages) = c("Zip_Code", "Sedimentary", "Igneous", "Metamorphic", "Unconsolidated")

# Find the areas of each primary lithology by zip code
for(i in 1:length(zips)) {
  temp = subset(lith.data, zip == zips[i])
  area = sum(temp$area)
  sed = subset(temp,lith1 == "Sedimentary")
  seda = sum(sed$area)
  ig = subset(temp,lith1 == "Igneous")
  iga = sum(ig$area)
  met = subset(temp,lith1 == "Metamorphic")
  meta = sum(met$area)
  unc = subset(temp,lith1 == "Unconsolidated")
  unca = sum(unc$area)
  percentages$Zip_Code[i] = zips[i]
  percentages$Sedimentary[i] = (seda/area)
  percentages$Igneous[i] = (iga/area)
  percentages$Metamorphic[i] = (meta/area)
  percentages$Unconsolidated[i] = (unca/area)
}

# Merge lithology areas from loop with radon data
minor_rad_lith = merge(percentages, rad_unm)

# Create binary variables for high radon 
for(i in 1:length(minor_rad_lith$Zip_Code)) {
  if(minor_rad_lith$MEDIAN_of_Test_Result_Radon[i] >= 4) 
    {minor_rad_lith$highradon[i] = 1}
  else {minor_rad_lith$highradon[i] = 0}
}

# Run a correlation test for homoscedastity
cortest = minor_rad_lith[,2:5]

# Create a correlation plot to visualize correlation test
par(mfrow=c(1,1))
radcor <- rcorr(as.matrix(cortest), type = "spearman")
radcor
corrplot(radcor$r, type = "upper", tl.col = "black", tl.srt = 45)

# Run a logistic regression for high radon grouped by area of each dominant 
# primary lithology. Two glm calls are necessary due to multicollinearity 
# concerns
minor_lrg_sed = glm(highradon ~ Sedimentary + Igneous + Metamorphic,
                family = "binomial", minor_rad_lith)
summary(minor_lrg_sed)
exp(coef(minor_lrg_sed))

minor_lrg_unc = glm(highradon ~ Unconsolidated + Igneous + Metamorphic,
                    family = "binomial", minor_rad_lith)
summary(minor_lrg_unc)
exp(coef(minor_lrg_unc))
# All results insignificant to a significance level of 95%