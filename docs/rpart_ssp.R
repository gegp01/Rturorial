# From georeferenced dataset including a group of interest and other observations to contrast the group of interest, and historical observations on land use (see data_compiler.R) 
# This R code aims at: 
# (1) Forming a subset with the class of interest and a random selection of a null model (no_class)
# (2) Performing recursive partitioning on the data, and the historical variables of Land Use

##################################################
# PART 1. READ DATA (example) AND LIST THE GOUP OF CLASSES OF INTEREST
# Example dataset of shorebirds with speciment in museum collections and reported in GBIF, including the historical data of land use change.
D = readRDS("data_gbif_luc.rds") 

# Note: there are 7259 data with NA in the variables states, hence we exlude these observations.
D = D[is.na(D$primf)==F,]

# LIST OF CLASSES OF INTEREST, BASED ON A VARIABLE IN THE DATASET D.
#taxa = unique(D$species)
taxa = readRDS("sp Mx.rds")

# SELECT A GROUP OF INTEREST (e.g. "Charadrius nivosus", which is number 31 in the list taxa).
v = 31

# FORM A BINARY CLASS BASED ON THE GROUP OF INTEREST
class = ifelse(D$species==taxa[v], 1, 0)

# SPLIT THE DATASET IN THE CLASS AND NO_CLASS
Y1 = D[class==1,]
Y0 = D[class==0,]

# MODELO NULO A NIVEL DE COLECTA
#idx = 1:nrow(Y0)
#Y0_subset = Y0[sample(idx, nrow(Y1)),]

# SELECT A SUBSET OF DATA BASED ON A GROUP OF INTEREREST WITHIN THE NO_CLASS. For instance we might want to compare the class, with the no_class members of the same family.
g = unique(D$family[D$species == taxa[v]]) # g is the family group
Y0_subset_ = Y0[Y0$family==g,]

idx = 1:nrow(Y0_subset_)
Y0_subset = Y0_subset_[sample(idx, nrow(Y1)),]

# LABEL THE TWO SUBSETS AS CLASS (1) AND NO_CLASS (0)
Y1$class = 1
Y0_subset$class = 0

# PUT ALL DATA TOGETHER IN A DATAFRAME Q, TO PERFORM RECURSIVE PARTITIONING
Q = rbind(Y1, Y0_subset)

##########################################
# PART 2. PERFORM THE GEOGRAPHIC MODEL. 
require(rpart)
require(rpart.plot)

modelo = clase ~ primn + secdn + range + pastr + c3per + c3ann + c3nfx + c4per + c4ann + urban + secma
tree = rpart(modelo, data = Q, method = "class")
b <- tree$cptable[which.min(tree$cptable[, "xerror"]), "CP"]
pruned_tree <- prune(tree, cp = b)

# par(font.main=3)
# rpart.plot(pruned_tree, tweak = 1.2, main=taxa[v])








