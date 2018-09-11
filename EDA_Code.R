# Read in raw data and caluclate demographic proportions
Train_Dat <- read.csv("Cumulative_Training_Set.csv")
for (i in 11:17) {
  Train_Dat[, i] <- Train_Dat[, i]/Train_Dat$Pop
}

# Remove index columns
Train_Dat <- Train_Dat[, -c(1)]

# Calculate voting proportions
Train_Dat$Romney <- Train_Dat$Romney/Train_Dat$Total.Vote
Train_Dat$Obama <- Train_Dat$Obama/Train_Dat$Total.Vote

# Subset numerical data
Cluster_Dat <- Train_Dat[complete.cases(Train_Dat), -c(1:5, 7:9)]
Cluster_Dat <- Cluster_Dat[, -c(10)]
Train_Dat$Sol_by_Pop <- log(Train_Dat$X2015)/log(Train_Dat$Pop)
Cluster_Dat <- left_join(Cluster_Dat, Train_Dat[, c(6, 24)], by = "Full_FIPS")


# Create distance matrix
dist_dem <- dist(scale(Cluster_Dat[, -1]))

# Calculate numbers to be used in scree plot
kmeans_dat <- data.frame(integer(), double())
for (i in 2:10) {
  dem_obj <- kmeans(dist_dem, centers = i)
  kmeans_dat <- rbind(kmeans_dat, c(i, dem_obj$tot.withinss))
}

# Make scree plot
plot(kmeans_dat)

# Use four clusters based on scree plot 
dem_obj <- kmeans(dist_dem, centers = 4)
Cluster_Dat$cluster <- factor(dem_obj$cluster)

# Create PCA and color based on cluster
Sol_PCA <- prcomp(Cluster_Dat[, -c(1, 10, 16)], scale = T, center = T)
autoplot(Sol_PCA, colour = "cluster", data = Cluster_Dat, loadings = T, loadings.label = T)

# Develop regional map from cluster analysis
To_map <- Cluster_Dat[, c(1, 16)]
names(To_map) <- c("region", "value")
county_choropleth(To_map)

# Add region column to dataset, to be used in model testing
Train_Dat$Region <- ""
North <- c("Pennsylvania", "New York", "New Jersey", "Massachusetts", "Vermont", 
           "Connecticut", "New Hampshire", "Delaware", "Maine", "Ohio", "Wisconsin", "Minnesota")
West <- c("Arizona", "California", "New Mexico", "Nevada")

Train_Dat$Region <- ifelse(Train_Dat$State %in% North, "North", "Other")
Train_Dat$Region <- ifelse(Train_Dat$State %in% West, "West", Train_Dat$Region)

