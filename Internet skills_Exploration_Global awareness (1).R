# Load data and package
data1<-read.csv("D:/Dropbox/Research/@_Team/@_Fandom/BMF collaborative projects/2023/Datasets/Global citizenship/Global citizenship_Data from Vietnam.csv",header = TRUE,stringsAsFactors = TRUE)
library(bayesvl)


library(ltm)

Group_1<-data.frame(data1$Q1_1,data1$Q1_2,data1$Q1_3,data1$Q1_4,data1$Q1_5,data1$Q1_6,
                    data1$Q1_7,data1$Q1_8,data1$Q1_9,data1$Q1_10,data1$Q1_11,data1$Q1_12)


Group_1<-na.omit(Group_1) 


cronbach.alpha(Group_1)


data1$Curiosity<-data1$Q2i_1
data1$InternetInforSeeking<-data1$Q2iii_9
data1$InternetInforSelection<-data1$Q2iii_10
data1$InternetForeignInforExchange<-data1$Q2iii_11

data1$GlobalIssueAwareness<-(data1$Q1_1+data1$Q1_2+data1$Q1_3+data1$Q1_4+data1$Q1_5+data1$Q1_6+
                   data1$Q1_7+data1$Q1_8+data1$Q1_9+data1$Q1_10+data1$Q1_11+data1$Q1_12)/12



keeps <- c("Curiosity","InternetInforSeeking","InternetInforSelection","InternetForeignInforExchange","GlobalIssueAwareness")
data1 <- data1[keeps]
data1<-na.omit(data1) 


# Model construction
model1a<-bayesvl()
model1a<-bvl_addNode(model1a,"Curiosity","norm")
model1a<-bvl_addNode(model1a,"InternetInforSeeking","norm")
model1a<-bvl_addNode(model1a,"InternetInforSelection","norm")
model1a<-bvl_addNode(model1a,"InternetForeignInforExchange","norm")

model1a<-bvl_addArc(model1a,"InternetInforSeeking","Curiosity","slope")
model1a<-bvl_addArc(model1a,"InternetInforSelection","Curiosity","slope")
model1a<-bvl_addArc(model1a,"InternetForeignInforExchange","Curiosity","slope")

bvl_bnPlot(model1a)

# Generate Stan code
model_string1a<- bvl_model2Stan(model1a)
cat(model_string1a) 

# Model Fit
model1a<-bvl_modelFit(model1a, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4) 

bvl_plotTrace(model1a)
bvl_plotGelmans(model1a,NULL,2,2)
bvl_plotAcfs(model1a,NULL,2,2)

bvl_plotIntervals(model1a,c("b_InternetInforSeeking_Curiosity",
                                            "b_InternetInforSelection_Curiosity","b_InternetForeignInforExchange_Curiosity"))+theme_bw()
bvl_plotDensity(model1a,c("b_InternetInforSeeking_Curiosity",
                                          "b_InternetInforSelection_Curiosity","b_InternetForeignInforExchange_Curiosity"))+theme_bw()

bvl_plotDensOverlay(model1a, n = 50)

bvl_plotParams(model1a,2,2,credMass=0.90,NULL)
loo1a<-bvl_stanLoo(model1a)
plot(loo1a)
bvl_plotPPC(model1a, fun = "stat", stat = "mean", color_scheme = "blue")

# Model construction
model2a<-bayesvl()
model2a<-bvl_addNode(model2a,"GlobalIssueAwareness","norm")
model2a<-bvl_addNode(model2a,"Curiosity","norm")

model2a<-bvl_addArc(model2a,"Curiosity","GlobalIssueAwareness","slope")

bvl_bnPlot(model2a)

# Generate Stan code
model_string2a<- bvl_model2Stan(model2a)
cat(model_string2a) 

# Model Fit
model2a<-bvl_modelFit(model2a, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4) 

bvl_plotTrace(model2a)
bvl_plotGelmans(model2a,NULL,1,2)
bvl_plotAcfs(model2a,NULL,1,2)

bvl_plotIntervals(model2a,c("b_Curiosity_GlobalIssueAwareness"))+theme_bw()
bvl_plotDensity(model2a,c("b_Curiosity_GlobalIssueAwareness"))+theme_bw()

bvl_plotParams(model2a,1,2,credMass=0.90,NULL)
loo2a<-bvl_stanLoo(model2a)
plot(loo2a)
bvl_plotPPC(model2a, fun = "stat", stat = "mean", color_scheme = "blue")

# Model construction
model3a<-bayesvl()
model3a<-bvl_addNode(model3a,"GlobalIssueAwareness","norm")
model3a<-bvl_addNode(model3a,"InternetInforSeeking","norm")
model3a<-bvl_addNode(model3a,"InternetInforSelection","norm")
model3a<-bvl_addNode(model3a,"InternetForeignInforExchange","norm")
model3a<-bvl_addNode(model3a,"Curiosity","norm")

model3a<-bvl_addArc(model3a,"InternetInforSeeking","GlobalIssueAwareness","slope")
model3a<-bvl_addArc(model3a,"InternetInforSelection","GlobalIssueAwareness","slope")
model3a<-bvl_addArc(model3a,"InternetForeignInforExchange","GlobalIssueAwareness","slope")
model3a<-bvl_addArc(model3a,"Curiosity","GlobalIssueAwareness","slope")

bvl_bnPlot(model3a)

# Generate Stan code
model_string3a<- bvl_model2Stan(model3a)
cat(model_string3a) 

# Model Fit
model3a<-bvl_modelFit(model3a, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4) 

bvl_plotTrace(model3a)
bvl_plotGelmans(model3a,NULL,2,3)
bvl_plotAcfs(model3a,NULL,2,3)

bvl_plotIntervals(model3a,c("b_InternetInforSeeking_GlobalIssueAwareness","b_InternetInforSelection_GlobalIssueAwareness",
                            "b_InternetForeignInforExchange_GlobalIssueAwareness","b_Curiosity_GlobalIssueAwareness"))+theme_bw()

bvl_plotDensity(model3a,c("b_InternetInforSeeking_GlobalIssueAwareness","b_InternetInforSelection_GlobalIssueAwareness",
                         "b_InternetForeignInforExchange_GlobalIssueAwareness","b_Curiosity_GlobalIssueAwareness"))+theme_bw()

bvl_plotParams(model3a,2,3,credMass=0.90,NULL)
loo3a<-bvl_stanLoo(model3a)
plot(loo3a)
bvl_plotPPC(model3a, fun = "stat", stat = "mean", color_scheme = "blue")

