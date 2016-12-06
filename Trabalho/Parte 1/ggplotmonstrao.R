
fit.model=lm(Peso~Altura,data=dados_1)

X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
lms <- summary(fit.model)
s <- lms$sigma
r <- resid(lms)
ts <- r/(s*sqrt(1-h))
di <- (1/p)*(h/(1-h))*(ts^2)
si <- lm.influence(fit.model)$sigma
tsi <- r/(si*sqrt(1-h))
tam <- 1:length(tsi)
a <- max(tsi)
b <- min(tsi)

g1 = ggplot(data=data.frame(tam,tsi),aes(tam,tsi))+ geom_point() +
      scale_y_continuous(name = "Resíduo Studentizado",limits=c(b-1,a+1)) +
      scale_x_continuous(name = "Índice") + theme_light()+
      geom_hline(yintercept=0)+geom_hline(yintercept=2)+
      geom_hline(yintercept=-2)
#plot(tsi,xlab="Índice", ylab="Resíduo Studentizado",
#     ylim=c(b-1,a+1), pch=16,cex=1.1,cex.axis=1.1,cex.lab=1.1)

g2 =  ggplot(data=data.frame(fitted(fit.model),tsi),aes(fitted(fit.model),tsi))+ geom_point() +
      scale_y_continuous(name = "Resíduo Studentizado",limits=c(b-1,a+1)) +
      scale_x_continuous(name = "Valores Ajustados") + theme_light()+
      geom_hline(yintercept=0)+geom_hline(yintercept=2)+
      geom_hline(yintercept=-2)

g3 = ggplot(data=data.frame(tsi),aes(tsi,col=I("black"),fill=I("white")))
            +geom_histogram(binwidth = 1)




g4 = ggplot(data=data.frame(tsi),aes(tsi,tsi,col=I("black"),fill=I("white")))+ geom_boxplot()+scale_x_continuous(name = "")+scale_y_continuous(name = "Resíduo Studentizado")+ theme(axis.text.x = element_blank())



#plot(fitted(fit.model),tsi,xlab="Valores Ajustados", 
#     ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16,cex=1.1,cex.axis=1.1,cex.lab=1.1)
#



