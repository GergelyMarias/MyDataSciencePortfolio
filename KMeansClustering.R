library(ggplot2)
library(factoextra)
library(gridExtra)
library(cluster)
library(dplyr)
library(fpc)
library(gridExtra)
library(kableExtra)

load("C:/Users/gergo/Desktop/COURSES/II. Seminar Data Science for Marketing Analytics/I. Cluster Analysis/Assignment/CustomerSegmentation.RData")

#Convert to factor
df$children<- as.factor(df$children)
#Creating dummy columns from each factor and scale the numeric variables
dummy_df_scaled<- as.data.frame(model.matrix(~ education + occupation + townsize + children -1, data = df))
#Add the initial dummy variables
dummy_df_scaled$sex<-df$sex
dummy_df_scaled$married <- df$married
dummy_df_scaled$sex<- as.numeric(dummy_df_scaled$sex)-1
dummy_df_scaled$married<- as.numeric(dummy_df_scaled$married)-1
#Add the numeric variables and scale them
dummy_df_scaled$age<- df$age
dummy_df_scaled$income <- df$income
dummy_df_scaled$age <- scale(dummy_df_scaled$age)
dummy_df_scaled$income <- scale(dummy_df_scaled$income)

#Create a non scaled data frame for the later interpretation
dummy_df<- as.data.frame(model.matrix(~ education + occupation + townsize + children - 1, data = df))
dummy_df$sex<-df$sex
dummy_df$married <- df$married
dummy_df$age<- df$age
dummy_df$income <- df$income
dummy_df$sex<- as.numeric(dummy_df$sex)-1
dummy_df$married<- as.numeric(dummy_df$married)-1
for (i in 1:15) {
  dummy_df[, i] <- as.factor(dummy_df[, i])
}

################################ K MEANS CLUSTER ANALYSIS ###########################

###WCSS
wcss <- matrix(numeric(), nrow = 20, ncol = 10)
for (seed in 1:10) {
  set.seed(seed)
  for (cluster_n in 1:20) {
    kmeans_model <- kmeans(dummy_df_scaled, centers = cluster_n)
    wcss[cluster_n,seed] <- kmeans_model$tot.withinss
  }
}
wcss<-data.frame(wcss)
wcss$means <- rowMeans(wcss[, 1:10])

###SILHOUETTE
#Silhouette
silhouette_scores1 <- vector("numeric", length = 20)
set.seed(1)
for (i in 2:20) {  
  kmeans_model <- kmeans(dummy_df_scaled, centers = i)
  silhouette_scores1[i] <- silhouette(kmeans_model$cluster, dist(dummy_df_scaled))
}
#
set.seed(2)
silhouette_scores2 <- vector("numeric", length = 20)
for (i in 2:20) { 
  kmeans_model <- kmeans(dummy_df_scaled, centers = i)
  silhouette_scores2[i] <- silhouette(kmeans_model$cluster, dist(dummy_df_scaled))
}
#
set.seed(3)
silhouette_scores3 <- vector("numeric", length = 20)
for (i in 2:20) {  
  kmeans_model <- kmeans(dummy_df_scaled, centers = i)
  silhouette_scores3[i] <- silhouette(kmeans_model$cluster, dist(dummy_df_scaled))
}
#
set.seed(4) 
silhouette_scores4 <- vector("numeric", length = 20)
for (i in 2:20) { 
  kmeans_model <- kmeans(dummy_df_scaled, centers = i)
  silhouette_scores4[i] <- silhouette(kmeans_model$cluster, dist(dummy_df_scaled))
}
#
set.seed(5)
silhouette_scores5 <- vector("numeric", length = 20)
for (i in 2:20) {   
  kmeans_model <- kmeans(dummy_df_scaled, centers = i)
  silhouette_scores5[i] <- silhouette(kmeans_model$cluster, dist(dummy_df_scaled))
}
#
set.seed(6)
silhouette_scores6 <- vector("numeric", length = 20)
for (i in 2:20) {  
  kmeans_model <- kmeans(dummy_df_scaled, centers = i)
  silhouette_scores6[i] <- silhouette(kmeans_model$cluster, dist(dummy_df_scaled))
}
#
set.seed(7)
silhouette_scores7 <- vector("numeric", length = 20)
for (i in 2:20) {  
  kmeans_model <- kmeans(dummy_df_scaled, centers = i)
  silhouette_scores7[i] <- silhouette(kmeans_model$cluster, dist(dummy_df_scaled))
}
#
set.seed(8)
silhouette_scores8 <- vector("numeric", length = 20)
for (i in 2:20) {  
  kmeans_model <- kmeans(dummy_df_scaled, centers = i)
  silhouette_scores8[i] <- silhouette(kmeans_model$cluster, dist(dummy_df_scaled))
}
#
set.seed(9)
silhouette_scores9 <- vector("numeric", length = 20)
for (i in 2:20) {  
  kmeans_model <- kmeans(dummy_df_scaled, centers = i)
  silhouette_scores9[i] <- silhouette(kmeans_model$cluster, dist(dummy_df_scaled))
}
#
set.seed(10)
silhouette_scores10 <- vector("numeric", length = 20)
for (i in 2:20) { 
  kmeans_model <- kmeans(dummy_df_scaled, centers = i)
  silhouette_scores10[i] <- silhouette(kmeans_model$cluster, dist(dummy_df_scaled))
}
scores<- data.frame(cbind(silhouette_scores1,silhouette_scores2,silhouette_scores3,silhouette_scores4,silhouette_scores5,
                          silhouette_scores6,silhouette_scores7,silhouette_scores8,silhouette_scores9,silhouette_scores10))

scores$means <- rowMeans(scores[, 1:10])

#####
wcss_values8 <- numeric(200)
for (i in 1:200) {
  set.seed(i) 
  kmeans_result <- kmeans(dummy_df_scaled, centers = 8)
  wcss_values8[i] <- kmeans_result$tot.withinss
}

wcss_value8<-data.frame(wcss_values8)
w<-data.frame(summary(wcss_value8))
w<-data.frame(w[,-c(1,2)])
colnames(w)<- "WCSS value for K8"

###

#CREATE CLUSTERS
set.seed(121)
kmeans_model <- kmeans(dummy_df_scaled, centers = 8)
#PLOT THE CLUSTERS MAP
cluster_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")
plot1<-fviz_cluster(kmeans_model, dummy_df_scaled, ellipse.type = "norm", geom = "point") + scale_colour_manual(values = cluster_colors) + 
  scale_fill_manual(values = cluster_colors) +
  ggtitle("")+
  theme(legend.text = element_text(size = 8),  
        axis.text = element_blank(),  
        axis.title = element_blank(),  
        axis.ticks = element_blank())

#BARPLOT
sizes8k <- data.frame(Size = kmeans_model[["size"]],
                      Cluster = c("1", "2", "3", "4",
                                  "5", "6", "7", "8"))
cluster_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")
plot2<-ggplot(sizes8k, aes(x = reorder(factor(Cluster), desc(Size)), y = Size, fill = factor(Cluster))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cluster_colors) +  # Set the cluster colors
  xlab("Cluster") + ylab("Size") +
  geom_text(aes(label = round(Size, digits = 0)),
            size = 2, colour = "black",
            position = position_stack(vjust = 0.5))+
  theme(legend.text = element_text(size = 8),  
        axis.text = element_blank(),  
        axis.title = element_blank(),  
        axis.ticks = element_blank())+
  labs(fill = "Cluster")

#INTERPRETATIONS
dummy_df$k8Cluster = kmeans_model[["cluster"]]
summarystats.percluster_8k = dummy_df %>% group_by(k8Cluster) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

summary_counts <- dummy_df %>%
  group_by(k8Cluster) %>%
  summarise(across(everything(), ~ sum(. == "1", na.rm = TRUE)))

summary_counts$sum<- kmeans_model[["size"]]
column_sums <- colSums(summary_counts)
df_sums <- rbind(summary_counts, column_sums)


plot3<-ggplot(summarystats.percluster_8k, aes(x = reorder(factor(k8Cluster), desc(income)), y = income, fill = factor(k8Cluster))) +
  geom_col() +
  xlab("Clusters") + ylab("Income") +
  geom_text(aes(label = round(income, digits = -3)),
            size = 2.5, colour = "black",
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = cluster_colors)+
  ggtitle("Avarage income")+
  theme(legend.text = element_text(size = 8),  
        axis.text = element_blank(),  
        axis.title = element_blank())+
  labs(fill = "Clusters")

plot4<-ggplot(summarystats.percluster_8k, aes(x = reorder(factor(k8Cluster), age), y = age, fill = factor(k8Cluster))) +
  geom_col() +
  xlab("Clusters") + ylab("Age") +
  geom_text(aes(label = round(age, digits = 0)),
            size = 2.5, colour = "black",
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = cluster_colors) +
  ggtitle("Avarage age") +
  theme(legend.text = element_text(size = 8),  
        axis.text = element_blank(),  
        axis.title = element_blank())+
  labs(fill = "Clusters")

####

#ALL CUSTOMER %
all_customer<- df_sums[9,-c(1,17,18,19)]
all_customer$occupation0<-633
all_customer$townsize0<-989
all_customer$children0<-1133
colnames(all_customer)[14]<- "female"
new_rows <- data.frame(matrix(NA, ncol = ncol(all_customer), nrow = 8))
column_names <- colnames(all_customer)
colnames(new_rows) <- column_names
all_customer <- rbind(all_customer, new_rows)
all_customer$education0[2:9]<-df_sums$education0[1:8]
all_customer$education1[2:9]<-df_sums$education1[1:8]
all_customer$education2[2:9]<-df_sums$education2[1:8]
all_customer$education3[2:9]<-df_sums$education3[1:8]
all_customer$occupation1[2:9]<-df_sums$occupation1[1:8]
all_customer$occupation2[2:9]<-df_sums$occupation2[1:8]
all_customer$townsize1[2:9]<-df_sums$townsize1[1:8]
all_customer$townsize2[2:9]<-df_sums$townsize2[1:8]
all_customer$children1[2:9]<-df_sums$children1[1:8]
all_customer$children2[2:9]<-df_sums$children2[1:8]
all_customer$children3[2:9]<-df_sums$children3[1:8]
all_customer$children4[2:9]<-df_sums$children4[1:8]
all_customer$children5[2:9]<-df_sums$children5[1:8]
all_customer$female[2:9]<-df_sums$sex[1:8]
all_customer$married[2:9]<-df_sums$married[1:8]
all_customer$sum<- c(2000,df_sums$sum[1:8])
all_customer$occupation0[1:9]<- c(all_customer$sum-(all_customer$occupation1+all_customer$occupation2))
all_customer$townsize0[1:9]<- c(all_customer$sum-(all_customer$townsize1+all_customer$townsize2))
all_customer$children0[1:9]<- c(all_customer$sum-(all_customer$children1+all_customer$children2+all_customer$children3+all_customer$children4+all_customer$children5))
all_customer<-all_customer[,-19]


#INTERPRETATION Cluster by Cluster descanding
#
k6 <- all_customer[7,]
k6 <- data.frame(Variable = colnames(k6), Value = unlist(k6))
k6$Percentage<- round((k6$Value / sizes8k[6,1]),2)*100
k6$Percentage <- paste0(k6$Percentage, "%")
k6$Variable <- reorder(k6$Variable, -k6$Value)
p6<-ggplot(k6, aes(x = Variable, y = Value)) +
  geom_col(fill = "#8c564b") +
  geom_text(aes(label = Percentage), size = 2, colour = "black",
            position = position_stack(vjust = 0.5)) +
  labs(title = "C6 - 503 Customer | Age~31 | $~62000", x = "Variable", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())
#
k8 <- all_customer[9,]
k8 <- data.frame(Variable = colnames(k8), Value = unlist(k8))
k8$Percentage<- round((k8$Value / sizes8k[8,1]),2)*100
k8$Percentage <- paste0(k8$Percentage, "%")
k8$Variable <- reorder(k8$Variable, -k8$Value)
p8<-ggplot(k8, aes(x = Variable, y = Value)) +
  geom_col(fill = "#7f7f7f") +
  geom_text(aes(label = Percentage), size = 2, colour = "black",
            position = position_stack(vjust = 0.5)) +
  labs(title = "C8 - 320 Customer | Age~46 | $~78000", x = "Variable", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())
#
k2 <- all_customer[3,]
k2 <- data.frame(Variable = colnames(k2), Value = unlist(k2))
k2$Percentage<- round((k2$Value / sizes8k[2,1]),2)*100
k2$Percentage <- paste0(k2$Percentage, "%")
k2$percentage<- (k2$Value / sum(k2$Value))
k2$Variable <- reorder(k2$Variable, -k2$Value)
p2<-ggplot(k2, aes(x = Variable, y = Value)) +
  geom_col(fill = "#ff7f0e") +
  geom_text(aes(label = Percentage), size = 2, colour = "black",
            position = position_stack(vjust = 0.5)) +
  labs(title = "C2 - 248 Customer | Age~30 | $~54000", x = "Variable", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())
#
k1 <- all_customer[2,]
k1 <- data.frame(Variable = colnames(k1), Value = unlist(k1))
k1$Percentage<- round((k1$Value / sizes8k[1,1]),2)*100
k1$Percentage <- paste0(k1$Percentage, "%")
k1$Variable <- reorder(k1$Variable, -k1$Value)
p1<-ggplot(k1, aes(x = Variable, y = Value)) +
  geom_col(fill = "#1f77b4") +
  geom_text(aes(label = Percentage), size = 2, colour = "black",
            position = position_stack(vjust = 0.5)) +
  labs(title = "C1 - 242 Customer | Age~29 | $~38000", x = "Variable", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())
#
k4 <- all_customer[5,]
k4 <- data.frame(Variable = colnames(k4), Value = unlist(k4))
k4$Percentage<- round((k4$Value / sizes8k[4,1]),2)*100
k4$Percentage <- paste0(k4$Percentage, "%")
k4$Variable <- reorder(k4$Variable, -k4$Value)
p4<-ggplot(k4, aes(x = Variable, y = Value)) +
  geom_col(fill = "#d62728") +
  geom_text(aes(label = Percentage), size = 2, colour = "black",
            position = position_stack(vjust = 0.5)) +
  labs(title = "C4 - 233 Customer | Age~58 | $~71000", x = "Variable", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())
#
k5 <- all_customer[6,]
k5 <- data.frame(Variable = colnames(k5), Value = unlist(k5))
k5$Percentage<- round((k5$Value / sizes8k[5,1]),2)*100
k5$Percentage <- paste0(k5$Percentage, "%")
k5$Variable <- reorder(k5$Variable, -k5$Value)
p5<-ggplot(k5, aes(x = Variable, y = Value)) +
  geom_col(fill = "#9467bd") +
  geom_text(aes(label = Percentage), size = 2, colour = "black",
            position = position_stack(vjust = 0.5)) +
  labs(title = "C5 - 181 Customer | Age~40 | $~102000", x = "Variable", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())
#
k7 <- all_customer[8,]
k7 <- data.frame(Variable = colnames(k7), Value = unlist(k7))
k7$Percentage<- round((k7$Value / sizes8k[7,1]),2)*100
k7$Percentage <- paste0(k7$Percentage, "%")
k7$Variable <- reorder(k7$Variable, -k7$Value)
p7<-ggplot(k7, aes(x = Variable, y = Value)) +
  geom_col(fill = "#e377c2") +
  geom_text(aes(label = Percentage), size = 2, colour = "black",
            position = position_stack(vjust = 0.5)) +
  labs(title = "C7 - 180 Customer | Age~42 | $~53000", x = "Variable", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())
#
k3 <- all_customer[4,]
k3 <- data.frame(Variable = colnames(k3), Value = unlist(k3))
k3$Percentage<- round((k3$Value / sizes8k[3,1]),2)*100
k3$Percentage <- paste0(k3$Percentage, "%")
k3$Variable <- reorder(k3$Variable, -k3$Value)
p3<-ggplot(k3, aes(x = Variable, y = Value)) +
  geom_col(fill = "#2ca02c") +
  geom_text(aes(label = Percentage), size = 2, colour = "black",
            position = position_stack(vjust = 0.5)) +
  labs(title = "C3 - 93 Customer | Age~62 | $~124000", x = "Variable", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

###

# Extract the first row as a data frame
row_data <- data.frame(all_customer[1, ])
# Extract the values from the first row as a numeric vector
values <- as.numeric(unlist(row_data))

# Sort the values in descending order and get the corresponding order
sorted_order <- order(-values)
sorted_values <- values[sorted_order]
sorted_names <- colnames(row_data)[sorted_order]


all_customer_table<-data.frame(cbind(sorted_names,sorted_values))
colnames(all_customer_table)<- c("Category","Numbers")
all_customer_table$Numbers<- as.numeric(all_customer_table$Numbers)
all_customer_table$Percentage<- c(round((all_customer_table$Numbers/2000)*100,0))
all_customer_table$Percentage <- paste0(all_customer_table$Percentage, "%")
all_customer_table<- all_customer_table[1:6,]

#PLOT
par(mfrow = c(1, 2))
par(mar = c(4, 4, 0.5, 0.1))
plot(1:20, wcss[,11], type = "b", xlab = "Number of Clusters", ylab = "WCSS", cex.axis = 0.7, cex.lab= 0.7)
abline(v = 8, col = "red")
axis(1, at = 8, labels = "8", col = "red",cex.axis = 0.8)
plot(1:20, scores$means, type = "b", xlab = "Number of Clusters", ylab = "Silhouette Score",cex.axis = 0.7, cex.lab= 0.7)
abline(v = 8, col = "red")
axis(1, at = 8, labels = "8", col = "red",cex.axis = 0.8)

#TABLE
kable(list(w,all_customer_table), caption = "Stability of the cluster number 8 (left) and the most customers from each category (right)")

grid.arrange(plot1, plot2, ncol = 2)
grid.arrange(plot3, plot4, ncol = 2)
grid.arrange(p6, p8, p2, p1, ncol = 2)
grid.arrange(p4, p5, p7, p3, ncol = 2)