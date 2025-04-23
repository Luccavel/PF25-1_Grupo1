require(car)
require(lmtest)
require(MASS)
require(tseries)
require(nortest)
library(xtable)
library(knitr)
library(tidyverse)
library(ggplot2)
library(dplyr)

inf <- read.csv("C:/Users/lmarc/OneDrive/Documentos/GitHub/PF25-1_Grupo1/inflacao.csv")
summary(inf)
attach(inf)

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}



###################           PROJETO 1


plot(salario_minimo	)
plot(ipca_acumulado_ano)

plot(ano, log(ipca_acumulado_ano))
plot(ano, log(salario_minimo))

####
plot(log(salario_minimo),log(ipca_acumulado_ano), ylab = "Logar?timo da infla??o acumulada ao ano", xlab = "Logar?timo do sal?rio m?nimo" )

ggplot(inf) +
  aes(x = log(salario_minimo), y = log(ipca_acumulado_ano) +
        geom_point(colour = "#A11D21", size = 3) +
        labs(
          x = "Logar?timo do sal?rio m?nimo",
          y = "Logar?timo da infla??o acumulada ao ano"
        ) +
        theme_estat())
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")
####

hist(ipca_acumulado_ano)
hist(salario_minimo)

plot(ano, salario_minimo, log = "y", 
     main = "Sal?rio M?nimo por Ano (escala log)", 
     xlab = "Ano", ylab = "Sal?rio M?nimo", 
     pch = 21, bg = "lightblue")

ggplot(inf) +
  aes(x=ano, y=salario_minimo, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Sal?rio m?nimo")

ggplot(inf) +
  aes(x=ano, y=ipca_acumulado_ano, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Infla??o")

ggplot(inf) +
  aes(x = ano, y = preco, group = produto, colour = produto) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Ano", y = "Pre?o") +
  theme_estat()
ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")

ggplot(inf) +
  aes(x = ano) +
  
  # Linha e pontos para sal?rio m?nimo
  geom_line(aes(y = salario_minimo, colour = "Sal?rio M?nimo"), size = 1) +
  geom_point(aes(y = salario_minimo, colour = "Sal?rio M?nimo"), size = 2) +
  
  # Linha e pontos para imposto
  geom_line(aes(y = ipca_acumulado_ano, colour = "Imposto"), size = 1, linetype = "dashed") +
  geom_point(aes(y = ipca_acumulado_ano, colour = "Imposto"), size = 2, shape = 17) +
  
  # T?tulos e r?tulos
  labs(x = "Ano", y = "Valor (R$)", colour = "Legenda") +
  theme_estat()

fit <- lm(salario_minimo~ipca_acumulado_ano); summary(fit) # nao precisa Montar a tabela

fit1 <- lm(log(salario_minimo)~log(ipca_acumulado_ano)); summary(fit1) # nao precisa Montar a tabela


cor.test(salario_minimo, ipca_acumulado_ano, method = "pearson")

cor.test(salario_minimo, ipca_acumulado_ano, method = "spearman")

fisher.test(table(salario_minimo, ipca_acumulado_ano))



###################           PROJETO 2

plot(selic_meta)

graf_disp <- ggplot(inf) +
  aes(x = log(ipca_acumulado_ano), y = log(selic_meta)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Logaritimo do SELIC",
    y = "Logaritimo so IPCA acumulado"
  )

################################################################################


fit <- lm(log(ipca_acumulado_ano)~selic_meta); summary(fit)
tabela_fit <- xtable(summary(fit))

qqPlot(fit)
envelope_LR(fit,  main.title = "Envelope", number.id =1) #prop

# normalidade
jarque.bera.test(rstudent(fit))

#*** Avaliação da correlação entre os erros aleatórios via teste de Durbin-Watson ****#
dwtest(fit, alternative = "two.sided")

gqtest(fit)


cor.test(selic_meta, ipca_acumulado_ano, method = "pearson")

cor.test(selic_meta, ipca_acumulado_ano, method = "spearman")

ggplot(inf) +
  aes(x = ano) +
  
  # Linha e pontos para sal?rio m?nimo
  geom_line(aes(y = log(selic_meta), colour = "SELIC")) +
  geom_point(aes(y = log(selic_meta), colour = "SELIC"), size = 2) +
  
  # Linha e pontos para imposto
  geom_line(aes(y = log(ipca_acumulado_ano), colour = "IPCA"), size = 1) +
  geom_point(aes(y = log(ipca_acumulado_ano), colour = "IPCA"), size = 2) +
  
  # T?tulos e r?tulos
  labs(x = "Ano", y = "Logaritimo das variáveis", colour = "Legenda") +
  
  # Cores personalizadas
  scale_colour_manual(
    values = c(
      "SELIC" = "#A11D21",    # Vermelho escuro
      "IPCA" = "#003366"      # Azul Dodger
    )
  )
