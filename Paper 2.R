#Advance Analysis in PLS-PM
#Obsevred Heterogeneity 
#Product Terms Approach, Mutli-Group Analysis and Pathmox
#Analysis of Interactions Terms, Age and Gender Effects
#Working Directory
setwd("~/GitHub/Advances Analysis/.git")
#Moderations
#Packages
require(plspm)
require(tester)
require(pathmox)
require(usethis)
require(genpathmox)
#library
library(plspm)
library(tester)
library(pathmox)
install.packages("devtools")
library(devtools)
library(usethis)
library(genpathmox)
#getting data into R 
mydata=read.table("C:/Users/bwabo/OneDrive/Desktop/Public Money/Governace3.csv",header = T,sep = ",", stringsAsFactors = FALSE )
str(mydata)
#Model A
#path matrix 
Transparency =rep(0,4)
Accountability=rep(0,4)
Legal =c(1,1,0,0)
Value =c(1,1,1,0)
#bind the matrix 
Gov_path =rbind(Transparency,Accountability,Legal,Value)
#let see 
Gov_path
# plot the path matrix 
innerplot(Gov_path)
#outer model 
# list of blocks (outer model)
Gov_blocks =list(1:9, 10:27, 27:34, 35:41)

# vector of reflective modes
Gov_modes =rep("A",4)

# apply plspm
Govpls = plspm(mydata, Gov_path, Gov_blocks, modes = Gov_modes, scheme="centroid", scaled=FALSE)
#what's in Gov_pls?
Govpls
summary(Govpls)
class(Govpls)
# path coefficients 
Govpls$path_coefs
# inner model
Govpls$inner_model
# summarized results 
summary(Govpls)
# plotting results (inner model)
plot(Govpls)
# running bootstrap validation 
Gov_val = plspm(mydata,Gov_path, Gov_blocks, modes = Gov_modes, boot.val = TRUE, br = 5000)
# bootstrap results
Gov_val[["boot"]]
Gov_val$boot$paths
Gov_val$boot$loadings
Gov_val$boot$total.efs
print(Gov_val)
Gov_val[["manifests"]]
plot(Gov_val)
#Model B
# duplicate Governance 3 as Governance 1
mydata = Governace_3
#create a product indicator between Transparency and Legal
#T1
Governance1$inter1 = mydata$T1 * mydata$L1
Governance1$inter2 = mydata$T1 * mydata$L2
Governance1$inter3 = mydata$T1 * mydata$L3
Governance1$inter4 = mydata$T1 * mydata$L4
Governance1$inter5 = mydata$T1 * mydata$L5
Governance1$inter6 = mydata$T1 * mydata$L6
Governance1$inter7 = mydata$T1 * mydata$L7
Governance1$inter8 = mydata$T1 * mydata$L9
#T2
Governance1$inter9  = mydata$T2 * mydata$L1
Governance1$inter10 = mydata$T2 * mydata$L2
Governance1$inter11 = mydata$T2 * mydata$L3
Governance1$inter12 = mydata$T2 * mydata$L4
Governance1$inter13 = mydata$T2 * mydata$L5
Governance1$inter14 = mydata$T2 * mydata$L6
Governance1$inter15 = mydata$T2 * mydata$L7
Governance1$inter16 = mydata$T2 * mydata$L9
#T3
Governance1$inter11 = mydata$T3 * mydata$L1
Governance1$inter18 = mydata$T3 * mydata$L2
Governance1$inter19 = mydata$T3 * mydata$L3
Governance1$inter20 = mydata$T3 * mydata$L4
Governance1$inter21 = mydata$T3 * mydata$L5
Governance1$inter22 = mydata$T3 * mydata$L6
Governance1$inter23 = mydata$T3 * mydata$L7
Governance1$inter24 = mydata$T3 * mydata$L9
#T5
Governance1$inter25 = mydata$T5 * mydata$L1
Governance1$inter26 = mydata$T5 * mydata$L2
Governance1$inter27 = mydata$T5 * mydata$L3
Governance1$inter28 = mydata$T5 * mydata$L4
Governance1$inter29 = mydata$T5 * mydata$L5
Governance1$inter30 = mydata$T5 * mydata$L6
Governance1$inter31 = mydata$T5 * mydata$L7
Governance1$inter32 = mydata$T5 * mydata$L9
#T6
Governance1$inter33 = mydata$T6 * mydata$L1
Governance1$inter34=  mydata$T6 * mydata$L2
Governance1$inter35 = mydata$T6 * mydata$L3
Governance1$inter36 = mydata$T6 * mydata$L4
Governance1$inter37 = mydata$T6 * mydata$L5
Governance1$inter38 = mydata$T6 * mydata$L6
Governance1$inter39 = mydata$T6 * mydata$L7
Governance1$inter40 = mydata$T6 * mydata$L9
#T7
Governance1$inter41 = mydata$T7 * mydata$L1
Governance1$inter42 = mydata$T7 * mydata$L2
Governance1$inter43 = mydata$T7 * mydata$L3
Governance1$inter44 = mydata$T7 * mydata$L4
Governance1$inter45 = mydata$T7 * mydata$L5
Governance1$inter46 = mydata$T7 * mydata$L6
Governance1$inter47 = mydata$T7 * mydata$L7
Governance1$inter48 = mydata$T7 * mydata$L9
#T8
Governance1$inter49 = mydata$T8 * mydata$L1
Governance1$inter50 = mydata$T8 * mydata$L2
Governance1$inter51 = mydata$T8 * mydata$L3
Governance1$inter52 = mydata$T8 * mydata$L4
Governance1$inter53 = mydata$T8 * mydata$L5
Governance1$inter54 = mydata$T8 * mydata$L6
Governance1$inter55 = mydata$T8 * mydata$L7
Governance1$inter56 = mydata$T8 * mydata$L9
#T9
Governance1$inter57 = mydata$T9 * mydata$L1
Governance1$inter58 = mydata$T9 * mydata$L2
Governance1$inter59 = mydata$T9 * mydata$L3
Governance1$inter60 = mydata$T9 * mydata$L4
Governance1$inter61 = mydata$T9 * mydata$L5
Governance1$inter62 = mydata$T9 * mydata$L6
Governance1$inter63 = mydata$T9 * mydata$L7
Governance1$inter64 = mydata$T9 * mydata$L9
#T10
Governance1$inter65 = mydata$T10 * mydata$L1
Governance1$inter66 = mydata$T10 * mydata$L2
Governance1$inter67 = mydata$T10 * mydata$L3
Governance1$inter68 = mydata$T10 * mydata$L4
Governance1$inter69 = mydata$T10 * mydata$L5
Governance1$inter70 = mydata$T10 * mydata$L6
Governance1$inter71 = mydata$T10 * mydata$L7
Governance1$inter72 = mydata$T10 * mydata$L9
#check
edit(Governance1)
str(Governance1)
#Path Matrix 
# create path matrix
r1 =c(0, 0, 0, 0,0)
r2 =c(0, 0, 0, 0,0)
r3 =c(0, 0, 0, 0,0)
r4 =c(1, 1, 1, 0,0)
r5 =c(1, 1, 1, 1,0)
prod_path =rbind(r1, r2, r3, r4,r5)
rownames(prod_path) =c("Transparency","Inter","Accountabiliy","Legal","Value")
colnames(prod_path) =c("Transparency","Inter","Accountabiliy","Legal","Value")
# define outer model list
prod_blocks =list(4:12, 45:115,13:20, 30:37,38:44)
# define reflective indicators
prod_modes =rep("A", 5)

# run plspm analysis with bootstrap validation
prod_pls =plspm(Governance1, prod_path, prod_blocks, modes = prod_modes,
                boot.val = TRUE, br = 5000)

summary(prod_pls)
#step 3 
# check path coefficients
prod_pls$path_coefs
# plot inner model
plot(prod_pls)
# check bootstrapped path coefficients
prod_pls$boot$paths
#Role of Gender effects including moderating Variables(Model B) 
Transparency=rep(0,5)
Inter=rep(0,5)
Accountability=rep(0,5)
Legal=c(1,1,1,0,0)
Value=c(1,1,1,1,0)
#bind the matrix 
value_path =rbind(Transparency, Inter, Accountability, Legal,Value)

# list of blocks (outer model)
value_blocks =list(4:12, 45:115, 13:20,30:37, 38:44)

# vector of reflective modes
value_modes =rep("A", 5)

# apply plspm
value_pls =plspm(Governance1, value_path, value_blocks, modes = value_modes,
                 boot.val = TRUE)
# plot path coefficients
plot(value_pls)

# bootstrapped path coefficients
value_pls$boot$paths
# select data of female 
female = Governance1[Governance1$Gender =="FEMALE", ]

# female plspm
female_value_pls =plspm(female, value_path, value_blocks, modes = value_modes)
female_val=plspm(female,value_path, value_blocks, modes=value_modes, boot.val=TRUE,br=5000)
female_val
summary(female_val)
# Gender (male & Female) 
male = Governance1[Governance1$Gender =="MALE", ]
#male plspm
male_value_pls =plspm(male, value_path, value_blocks, modes = value_modes)
male_val=plspm(male,value_path, value_blocks, modes=value_modes, boot.val=TRUE,br=5000)
male_val
summary(male_val)
# Invoke as.factor method on dataframe$columnName
Governance1$Gender = as.factor(Governance1$Gender)
# apply plspm.groups premutation
value_perm =plspm.groups(value_pls, Governance1$Gender, method ="permutation")

# see the results
value_perm
# plotting results (inner model)
plot(value_pls, arr.pos = 0.35)
# select data for Age (Junior|Senior) 
Junior = Governance1[Governance1$Age =="JUNIOR",]
senior = Governance1[Governance1$Age=="SENIOR",]
# female plspm
junior_prod_pls =plspm(Junior, prod_path, prod_blocks,modes = prod_modes)
summary(junior_prod_pls)
junior_val=plspm(Junior,prod_path, prod_blocks,modes = prod_modes, boot.val=TRUE,br=5000)
junior_val
summary(junior_val)
#Senior 
senior_prod_pls =plspm(senior, prod_path, prod_blocks,modes = prod_modes)
summary(senior_prod_pls)
senior_val=plspm(senior,prod_path, prod_blocks,modes = prod_modes, boot.val=TRUE,br=5000)
junior_val
summary(senior_val)
# apply plspm.groups premutation on Age 
prod_pls_perm =plspm.groups(prod_pls, Governance1$Age, method ="permutation")
# matrix of path coefficients
Paths = value_pls$path_coefs
arrow_lwd = 10 *round(Paths, 2)
# how does it look like?
arrow_lwd
# arrows of different sizes reflecting the values of the path coeffs
plot(value_pls, arr.pos = 0.35, arr.lwd = arrow_lwd)

# plot path coefficients
plot(female_value_pls, box.size = 0.05)
plot(male_value_pls, box.size = 0.05)

# apply plspm.groups bootstrap
value_boot =plspm.groups(value_pls, Governance1$Gender, method ="bootstrap")

# Invoke as.factor method on dataframe$columnName
Governance1$Gender = as.factor(Governance1$Gender)
# apply plspm.groups bootstrap
value_boot =plspm.groups(value_pls, Governance1$Gender, method ="bootstrap")

# see the results
value_boot

# apply plspm.groups premutation
value_perm =plspm.groups(value_pls, Governance1$Gender, method ="permutation")

# see the results
value_perm

# path coefficients between female and male
barplot(t(as.matrix(value_boot$test[,2:3])), border = NA, beside = TRUE,
        col =c("#FEB24C","#74A9CF"), las = 2, ylim =c(-0.1, 2),
        cex.names = 0.6, col.axis ="gray30", cex.axis = 0.6)
# add horizontal line
abline(h = 0, col ="gray50")
# add itle
title("Path coefficients of Female and Male",
      cex.main = 0.75, col.main ="gray30")
# add legend
legend("top", legend =c("female","male"), pt.bg =c("#FEB24C","#A6BDDB"),
       ncol = 2, pch = 22, col =c("#FEB24C","#74A9CF"), bty ="n",
       text.col ="gray40")
#Pathmox 
#bind the matrix 
inner.gov =rbind(Transparency,Accountability, Inter,Legal,Value)
colnames(inner.gov) = rownames(inner.gov)
# list of blocks (outer model)
outer.gov = list(1:12, 13:20,45:115, 30:37, 38:44)
# vector of modes (reflective indicators)
modes.gov = rep("A",5)
# apply plspm
pls.gov = plspm(Governance1, inner.gov, outer.gov, modes.gov)
#segmentation
Seg.gov = mydata[,1:2]
Seg.gov$Age = factor(mydata$Age, ordered=TRUE)
Seg.gov$Age =factor(mydata$Age,
                    levels=c("<10yk","15y","25y",">35k"),ordered=T)
Seg.gov$Gender=factor(mydata$Gender,ordered=TRUE)

# Pathmox Analysis
pls.gov.pathmox=pathmox(Gov_pls,Seg.gov,signif=0.05,deep=2,size=0.2)
gov.pathmox=pls.pathmox(pls.gov,Seg.gov,signif=0.05,deep=2,size=0.2,n.node=20)
#Graphics
#Conditioning (Lattice)
install.packages("latticeExtra")
library(latticeExtra)
attach(Governace_3)
Governace_3$Gender = as.factor(Governace_3$Gender)
Governace_3$Age=as.factor(Governace_3$Age)
str(Governace_3)
edit(Governace_3)
xyplot(Junior ~ Senior | Gender, data = Governace_3,
       type = c("g", "p", "smooth"),
       xlab = "Junior", ylab = "Senior",
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.rug(x = x[is.na(y)], y = y[is.na(x)])
       })
#
library(grid)
qqmath(~ Age | factor(Experience), Governace_3, groups = Gender,
       f.value = function(n) ppoints(100),
       aspect = "xy",
       page = function(n) {
         cat("Click on plot to place legend", fill = TRUE)
         ll <- grid.locator(unit = "npc")
         if (!is.null(ll))
           draw.key(simpleKey(levels(factor(Governace_3$Gender))),
                    vp = viewport(x = ll$x, y = ll$y),
                    draw = TRUE)
       })

