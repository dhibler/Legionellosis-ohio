set.seed(37)

setwd("C:/Users/hible/Documents/School/Pubhealth/PUBHLTH 8802 - GISD sem")      

my_data <- read.csv("ohio_ld_2018.csv", header=TRUE)
summary(my_data)

l_2017_v <- my_data$l_2017
pop_2018_v <- my_data$pop_2018
pop_density_v <- my_data$pop_density
ln_2018_pop_v <- my_data$ln_2018_pop
ln_pop_density_v <- my_data$ln_pop_density


linearMod_pop <- lm(l_2017 ~ pop_2018, data=my_data)  # build linear regression model on full data
print(linearMod_pop)

kruskal.test(l_2017 ~ pop_2018, data=my_data)

linearMod_popd <- lm(l_2017 ~ pop_density, data=my_data)  # build linear regression model on full data
print(linearMod_popd)

kruskal.test(l_2017 ~ pop_density, data=my_data)

linearMod_lnpop <- lm(l_2017 ~ ln_2018_pop, data=my_data)  # build linear regression model on full data
print(linearMod_lnpop)

kruskal.test(l_2017 ~ ln_2018_pop, data=my_data)

linearMod_lnpopd <- lm(l_2017 ~ ln_pop_density, data=my_data)  # build linear regression model on full data
print(linearMod_lnpopd)

kruskal.test(l_2017 ~ ln_pop_density, data=my_data)

logMod_pop <- logit(l_2017 ~ pop_2018, data=my_data)  # build logistic regression model on full data
logMod_popfit <- predict(logMod_pop)
print(logMod_pop)

logMod_popd <- logit(l_2017 ~ pop_density, data=my_data)  # build logistic regression model on full data
logMod_popdfit <- predict(logMod_popd)
print(logMod_popd)

logMod_lnpop <- logit(l_2017 ~ ln_2018_pop, data=my_data)  # build logistic regression model on full data
logMod_lnpopfit <- predict(logMod_lnpop)
print(logMod_lnpop)

logMod_lnpopd <- logit(l_2017 ~ ln_pop_density, data=my_data)  # build logistic regression model on full data
logMod_lnpopdfit <- predict(logMod_lnpopd)
print(logMod_lnpopd)


cor_p_values <- matrix(nrow=3, ncol=4)
colnames(cor_p_values) <- c("Infections vs Population", "Infections vs Pop_density", "Infections vs Ln_Pop", "Infections vs Ln_Pop_density")
rownames(cor_p_values) <- c("Pearson", "Kendall", "Spearman")

i_p_p <- cor.test(l_2017_v,pop_2018_v, method = "pearson")
cor_p_values[1,1] <- i_p_p$p.value
i_p_k <- cor.test(l_2017_v,pop_2018_v, method = "kendall")
cor_p_values[2,1] <- i_p_k$p.value
i_p_s <- cor.test(l_2017_v,pop_2018_v, method = "spearman")
cor_p_values[3,1] <- i_p_s$p.value

i_p_p <- cor.test(l_2017_v,pop_density_v, method = "pearson")
cor_p_values[1,2] <- i_p_p$p.value
i_p_k <- cor.test(l_2017_v,pop_density_v, method = "kendall")
cor_p_values[2,2] <- i_p_k$p.value
i_p_s <- cor.test(l_2017_v,pop_density_v, method = "spearman")
cor_p_values[3,2] <- i_p_s$p.value

i_p_p <- cor.test(l_2017_v,ln_2018_pop_v, method = "pearson")
cor_p_values[1,3] <- i_p_p$p.value
i_p_k <- cor.test(l_2017_v,ln_2018_pop_v, method = "kendall")
cor_p_values[2,3] <- i_p_k$p.value
i_p_s <- cor.test(l_2017_v,ln_2018_pop_v, method = "spearman")
cor_p_values[3,3] <- i_p_s$p.value

cor.test(l_2017_v,ln_pop_density_v, method = "pearson")
i_p_p <- cor.test(l_2017_v,ln_pop_density_v, method = "pearson")
cor_p_values[1,4] <- i_p_p$p.value
i_p_k <- cor.test(l_2017_v,ln_pop_density_v, method = "kendall")
cor_p_values[2,4] <- i_p_k$p.value
i_p_s <- cor.test(l_2017_v,ln_pop_density_v, method = "spearman")
cor_p_values[3,4] <- i_p_s$p.value

print(cor_p_values)
write.csv(cor_p_values, file="cor_p_values.csv")


x11()
scatter.smooth(x=my_data$pop_2018, y=my_data$l_2017, main="l_2017 ~ pop_2018")  # scatterplot
x11()
scatter.smooth(x=my_data$pop_density, y=my_data$l_2017, main="l_2017 ~ pop_density")  # scatterplot
x11()
scatter.smooth(x=my_data$ln_2018_pop, y=my_data$l_2017, main="l_2017 ~ ln_2018_pop")  # scatterplot
x11()
scatter.smooth(x=my_data$ln_pop_density, y=my_data$l_2017, main="l_2017 ~ ln_pop_density")  # scatterplot


x11()
hist(my_data$l_2017)
x11()
hist(my_data$pop_2018)
x11()
hist(my_data$pop_density)


x11()
plot(x=my_data$pop_2018, y=my_data$l_2017, main="l_2017 ~ pop_2018")  # scatterplot with linear regression line
abline(linearMod_pop, col = 2)
x11()
plot(x=my_data$pop_density, y=my_data$l_2017, main="l_2017 ~ pop_density")  # scatterplot with linear regression line
abline(linearMod_popd, col = 2)
x11()
plot(x=my_data$ln_2018_pop, y=my_data$l_2017, main="l_2017 ~ ln_2018_pop")  # scatterplot with linear regression line
abline(linearMod_lnpop, col = 2)
x11()
plot(x=my_data$ln_pop_density, y=my_data$l_2017, main="l_2017 ~ ln_pop_density")  # scatterplot with linear regression line
abline(linearMod_lnpopd, col = 2)


x11()
plot(x=my_data$pop_2018, y=my_data$l_2017, main="l_2017 ~ pop_2018")  # scatterplot with logistic regression line
lines(pop_2018_v, logMod_popfit)
x11()
plot(x=my_data$pop_density, y=my_data$l_2017, main="l_2017 ~ pop_density")  # scatterplot with logistic regression line
lines(pop_density_v, logMod_popdfit)
x11()
plot(x=my_data$ln_2018_pop, y=my_data$l_2017, main="l_2017 ~ ln_2018_pop")  # scatterplot with logistic regression line
lines(ln_2018_pop_v, logMod_lnpopfit)
x11()
plot(x=my_data$ln_pop_density, y=my_data$l_2017, main="l_2017 ~ ln_pop_density")  # scatterplot with logistic regression line
lines(ln_pop_density_v, logMod_lnpopdfit)


x11()
scatter.smooth(x=my_data$pop_dif, y=my_data$l_2017, main="l_2017 ~ pop_dif")  # scatterplot
x11()
scatter.smooth(x=my_data$pop_dif, y=my_data$inf_dif, main="inf_dif ~ pop_dif")  # scatterplot

x11()
scatter.smooth(x=my_data$abs_pop_dif, y=my_data$l_2017, main="l_2017 ~ abs_pop_dif")  # scatterplot
x11()
scatter.smooth(x=my_data$abs_pop_dif, y=my_data$inf_dif, main="inf_dif ~ abs_pop_dif")  # scatterplot

