mtcars
?mtcars
plot(mtcars[,1],mtcars[,4])
plot(mtcars$mpg,mtcars$hp,xlab = "Miles/(US)gallon",ylab = "Gross horsepower",pch = 16,col = c("orange","magenta"))
pairs(mtcars[,1:5])
pairs(mtcars[,1:5],upper.panel = NULL)
cor(mtcars[,1],mtcars[,4],method = "spearman")
?cor
cor(mtcars)

data <-as.matrix(ddd)
cor(data)
install.packages("htmlTable")
library(htmlTable)
htmlTable(round(cor(mtcars),2))
cov(mtcars[,1],mtcars[,4])
htmlTable(round(cov(mtcars),2))
install.packages("ggm")
library(ggm)
pcor(c("hp","mpg","drat"),var(mtcars))
cor.test(mtcars$mpg,mtcars$hp)
dd= kk[3,8,]
dat<-as.matrix(hwhw)
#cor(test[1:6])
#cor.test(test$1,test$2)

datanew<-data.frame(hwhw)
attach(datanew)
print("XXXXXXXXXXXXXXXXXXXXXXXXXXX")
cor.test(TR,GI)
cor.test(TR,GE)
cor.test(TR,FINF)
cor.test(TR,FOUTF)
cor.test(GI,GE)
cor.test(GI,FINF)
cor.test(GI,FOUTF)
cor.test(GE,FINF)
cor.test(GE,FOUTF)
cor.test(FINF,FOUTF)

cor.test(GDP,GI)
cor.test(GDP,GE)
cor.test(GDP,FINF)

cor.test(TR,GDP)

cor.test(GDP,FOUTF)
p<-data.frame(pure)
cor(p)

na<-data.frame(no_anom1)
cor(na)
attach(na)

cor.test(TR,GI)
cor.test(TR,GE)
cor.test(TR,FINF)
cor.test(TR,FOUTF)
cor.test(GI,GE)
cor.test(GI,FINF)
cor.test(GI,FOUTF)
cor.test(GE,FINF)
cor.test(GE,FOUTF)
cor.test(FINF,FOUTF)

cor.test(GDP,GI)
cor.test(GDP,GE)
cor.test(GDP,FINF)

cor.test(TR,GDP)

aa<-cor.test(GDP,FOUTF)
str(aa)


df_numeric  <- p[, c(1,2:6)]

pairs(df_numeric)

cor(df_numeric)

fit  <- corr.test(df_numeric)
fit$r
fit$p
fit$adjust



corr.test(p, y = NULL, use = "pairwise",method="pearson",adjust="holm", 
          alpha=.05,ci=TRUE,minlength=5)
corr.p(r,n,adjust="holm",alpha=.05,minlength=5,ci=TRUE)





d <- dist(noanom) 

install.packages("cluster")
library(cluster)

# РџР°СЂР°РјРµС‚СЂ hang = -1  РІС‹СЂР°РІРЅРёРІР°РµС‚ РјРµС‚РєРё
#РњРµС‚РѕРґ РґР°Р»СЊРЅРµРіРѕ СЃРѕСЃРµРґР°
plot(hclust(d, method = "complete"), cex = 0.5, hang = -1)
#РњРµС‚РѕРґ Р±Р»РёР¶РЅРµРіРѕ СЃРѕСЃРµРґР°
plot(hclust(d, method = "single"), cex = 0.5, hang = -1)
#РњРµС‚РѕРґ СЃСЂРµРґРЅРµР№ СЃРІСЏР·Рё
plot(hclust(d, method = "average"), cex = 0.5, hang = -1)
#РњРµС‚РѕРґ С†РµРЅС‚СЂРѕРІ С‚СЏР¶РµСЃС‚Рё
plot(hclust(d, method = "centroid"), cex = 0.5, hang = -1)
#Ward РјРµС‚РѕРґ
plot(hclust(d, method = "ward.D"), cex = 0.5, hang = -1)

#Approach 2
hcd <- as.dendrogram(hclust(d, method = "ward.D2" ))
nodePar <- list(lab.cex = 0.7, pch = c(NA, 19), cex = 0.7, col = "blue")
plot(hcd,  xlab = "Height", nodePar = nodePar, horiz = TRUE,
     edgePar = list(col = 2:3, lwd = 2:1))

#k-means


library(cluster)

k <- kmeans(noanom, 2)
summary(k)

for (i in 1:length(x)){
  cluster[i] <- k$cluster[i]
}
data <- matrix(c(x,y,cluster), ncol = 3)
data

plot(data[,1],data[,2],col = data[,3], pch = 16)


library(cluster) 
clusplot(noanom, data[,3], color = TRUE, shade = TRUE, labels = 2, lines = 0)
noanom<-data.frame(no_anom1)
rownames(noanom)<-c("Afghanistan",'Argentina','Armenia',
                'Australia',
                'Austria',
                'Azerbaijan',
                'Bahrain',
                'Bangladesh',
                'Belarus',
                'Belize',
                'Bhutan',
                'Botswana',
                'Brazil',
                'Brunei Darussalam',
                'Bulgaria',
                'Burundi',
                'Cabo Verde',
                'Cambodia',
                'Cameroon',
                'Canada',
                'Chile',
                'China',
                'Colombia',
                'Comoros',
                'Congo, Dem. Rep.',
                'Costa Rica',
                'Croatia',
                'Czech Republic',
                'Denmark',
                'Ecuador',
                'Egypt, Arab Rep.',
                'Ethiopia',
                'Euro area',
                'France',
                'Gambia, The',
                'Georgia',
                'Germany',
                'Ghana',
                'Greece',
                'Grenada',
                'Guatemala',
                'Guyana',
                'Haiti',
                'Honduras',
                'Hong Kong SAR, China',
                'India',
                'Indonesia',
                'Ireland',
                'Israel',
                'Italy',
                'Jamaica',
                'Japan',
                'Korea, Rep.',
                'Kyrgyz Republic',
                'Lao PDR',
                'Lebanon',
                'Lesotho',
                'Liberia',
                'Lithuania',
                'Madagascar',
                'Malaysia',
                'Maldives',
                'Mauritius',
                'Mexico',
                'Moldova',
                'Mongolia',
                'Montenegro',
                'Morocco',
                'Myanmar',
                'Namibia',
                'Nepal',
                'New Zealand',
                'Nigeria',
                'Oman',
                'Pakistan',
                'Peru',
                'Philippines',
                'Poland',
                'Portugal',
                'Romania',
                'Russian Federation',
                'Rwanda',
                'Samoa',
                'San Marino',
                'Sao Tome and Principe',
                'Saudi Arabia',
                'Seychelles',
                'Sierra Leone',
                'Singapore',
                'Slovak Republic',
                'Slovenia',
                'Solomon Islands',
                'South Africa',
                'Spain',
                'St. Kitts and Nevis',
                'St. Lucia',
                'St. Vincent and the Grenadines',
                'Suriname',
                'Tajikistan',
                'Tanzania',
                'Thailand',
                'Trinidad and Tobago',
                'Tunisia',
                'Turkey',
                'Uganda',
                'Ukraine',
                'United States',
                'Uruguay',
                'Uzbekistan',
                'Vanuatu',
                'West Bank and Gaza')
install.packages("factoextra")
library('factoextra')
g2<-fviz_nbclust(noanom,kmeans,method='silhouette')
g2
pairs(noanom)

km<-data.frame(KMEANS)
rownames(km)<-c("CLUSTER0",'CLUSTER1')
km
kkk<-dist(km) 
kkk
pcor(noanom)
install.packages('ppcor')
library('ppcor')
pcor(noanom)
attach(noanom)
pcor.test(TR,GI,FINF)
pcor.test(TR,GI,FOUTF)
pcor.test(TR,GI,GE)
pcor.test(TR,FINF,c(GE,GI,FOUTF,GDP))
pcor.test(TR,FOUTF,c(GE,GI,FINF,GDP))
pcor.test(GI,GE,c(TR,FINF,FOUTF,GDP))
pcor.test(GI,FINF)
pcor.test(GI,FOUTF)
pcor.test(GE,FINF)
pcor.test(GE,FOUTF)
pcor.test(FINF,FOUTF)

pcor.test(GDP,GI)
pcor.test(GDP,GE)
pcor.test(GDP,FINF)

pcor.test(TR,GDP)

pcor.test(GDP,FOUTF)
pcor(noanom)



#part two
install.packages("partykit")
install.packages("CHAID", repos="http://R-Forge.R-project.org")
require(rsample) # for dataset and splitting also loads broom and tidyr
require(dplyr)
require(ggplot2)
theme_set(theme_bw()) # set theme
require(CHAID)
require(purrr)

install.packages("caret", repos="http://R-Forge.R-project.org")

da<-data.frame(bb)
rownames(da)<-c("Afghanistan",'Argentina','Armenia',
                    'Australia',
                    'Austria',
                    'Azerbaijan',
                    'Bahrain',
                    'Bangladesh',
                    'Belarus',
                    'Belize',
                    'Bhutan',
                    'Botswana',
                    'Brazil',
                    'Brunei Darussalam',
                    'Bulgaria',
                    'Burundi',
                    'Cabo Verde',
                    'Cambodia',
                    'Cameroon',
                    'Canada',
                    'Chile',
                    'China',
                    'Colombia',
                    'Comoros',
                    'Congo, Dem. Rep.',
                    'Costa Rica',
                    'Croatia',
                    'Czech Republic',
                    'Denmark',
                    'Ecuador',
                    'Egypt, Arab Rep.',
                    'Ethiopia',
                    'Euro area',
                    'France',
                    'Gambia, The',
                    'Georgia',
                    'Germany',
                    'Ghana',
                    'Greece',
                    'Grenada',
                    'Guatemala',
                    'Guyana',
                    'Haiti',
                    'Honduras',
                    'Hong Kong SAR, China',
                    'India',
                    'Indonesia',
                    'Ireland',
                    'Israel',
                    'Italy',
                    'Jamaica',
                    'Japan',
                    'Korea, Rep.',
                    'Kyrgyz Republic',
                    'Lao PDR',
                    'Lebanon',
                    'Lesotho',
                    'Liberia',
                    'Lithuania',
                    'Madagascar',
                    'Malaysia',
                    'Maldives',
                    'Mauritius',
                    'Mexico',
                    'Moldova',
                    'Mongolia',
                    'Montenegro',
                    'Morocco',
                    'Myanmar',
                    'Namibia',
                    'Nepal',
                    'New Zealand',
                    'Nigeria',
                    'Oman',
                    'Pakistan',
                    'Peru',
                    'Philippines',
                    'Poland',
                    'Portugal',
                    'Romania',
                    'Russian Federation',
                    'Rwanda',
                    'Samoa',
                    'San Marino',
                    'Sao Tome and Principe',
                    'Saudi Arabia',
                    'Seychelles',
                    'Sierra Leone',
                    'Singapore',
                    'Slovak Republic',
                    'Slovenia',
                    'Solomon Islands',
                    'South Africa',
                    'Spain',
                    'St. Kitts and Nevis',
                    'St. Lucia',
                    'St. Vincent and the Grenadines',
                    'Suriname',
                    'Tajikistan',
                    'Tanzania',
                    'Thailand',
                    'Trinidad and Tobago',
                    'Tunisia',
                    'Turkey',
                    'Uganda',
                    'Ukraine',
                    'United States',
                    'Uruguay',
                    'Uzbekistan',
                    'Vanuatu',
                    'West Bank and Gaza')
attach(da)

set.seed(290875)
b<-da[,2:9]
ctrl <- chaid_control(minsplit = 50, minprob = 0.1)
chaidUS <- chaid(EC ~ ., data = b, control = ctrl)
print(chaidUS)

b
library(rpart)
(ch <- chaid(GDP ~ ., data = b))
print(ch)
printcp(ch)
plot(ch)
prettyTree()
ch