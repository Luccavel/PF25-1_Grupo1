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
plot(log(salario_minimo),log(ipca_acumulado_ano), ylab = "Logarítimo da inflação acumulada ao ano", xlab = "Logarítimo do salário mínimo" )

ggplot(inf) +
  aes(x = log(salario_minimo), y = log(ipca_acumulado_ano) +
        geom_point(colour = "#A11D21", size = 3) +
        labs(
          x = "Logarítimo do salário mínimo",
          y = "Logarítimo da inflação acumulada ao ano"
        ) +
        theme_estat())
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")
####

hist(ipca_acumulado_ano)
hist(salario_minimo)

plot(ano, salario_minimo, log = "y", 
     main = "Salário Mínimo por Ano (escala log)", 
     xlab = "Ano", ylab = "Salário Mínimo", 
     pch = 21, bg = "lightblue")

ggplot(inf) +
  aes(x=ano, y=salario_minimo, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Salário mínimo")

ggplot(inf) +
  aes(x=ano, y=ipca_acumulado_ano, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Inflação")

ggplot(inf) +
  aes(x = ano, y = preco, group = produto, colour = produto) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Ano", y = "Preço") +
  theme_estat()
ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")

ggplot(inf) +
  aes(x = ano) +
  
  # Linha e pontos para salário mínimo
  geom_line(aes(y = salario_minimo, colour = "Salário Mínimo"), size = 1) +
  geom_point(aes(y = salario_minimo, colour = "Salário Mínimo"), size = 2) +
  
  # Linha e pontos para imposto
  geom_line(aes(y = ipca_acumulado_ano, colour = "Imposto"), size = 1, linetype = "dashed") +
  geom_point(aes(y = ipca_acumulado_ano, colour = "Imposto"), size = 2, shape = 17) +
  
  # Títulos e rótulos
  labs(x = "Ano", y = "Valor (R$)", colour = "Legenda") +
  theme_minimal()

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

fit <- lm(selic_meta~ipca_acumulado_ano); summary(fit)
tabela_fit <- xtable(summary(fit))

qqPlot(fit)

# normalidade
jarque.bera.test(rstudent(fit))

#*** AvaliaÃ§Ã£o da correlaÃ§Ã£o entre os erros aleatÃ³rios via teste de Durbin-Watson ****#
dwtest(fit, alternative = "two.sided")


cor.test(selic_meta, ipca_acumulado_ano, method = "pearson")

cor.test(selic_meta, ipca_acumulado_ano, method = "spearman")
