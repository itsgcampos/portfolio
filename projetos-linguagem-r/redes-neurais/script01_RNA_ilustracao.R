
###########################################
# Gerando os dados

# Gerar x como uma sequencia de valores entre 0 e 1
x1 <- seq(0,1, length.out=1000)

# y segue uma relação quadrática com estes parâmetros
a <- 0
b <- 12.5
c <- -10

# Gerar y
set.seed(1729)
y1 <- a + b*x1 + c*x1**2 + rnorm(length(x1), mean=0, sd=.1)
df1 <- data.frame(x1, y1) # criar um dataframe
colnames(df1) <- c('x', 'y') #renomear colunas

# Gráfico dos dados gerados
p0 <- ggplot(df1, aes(x,y)) + 
  geom_point(aes(colour='Observado'), alpha=.5) +
  viridis::scale_color_viridis(discrete=TRUE, begin=0, end=.85, name = "Valor") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.spacing.x = unit(0, 'cm'))
p0

########################
# Perceptron linear

# Primeira rede neural
set.seed(1729)
rn0 <- neuralnet::neuralnet(y ~ x, 
                 data=df1, 
                 threshold = 0.01,
                 act.fct = 'logistic'
                 )

plot(rn0)

df1['pred0'] <- predict(rn0, df1)

# Valores esperados e observados
x_vs_y_EspObs <- ggplot(df1, aes(x,y)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,pred0, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  # theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados") +
  # scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")

x_vs_y_EspObs


########################
# Perceptron Multicamada
set.seed(1729)
tempo_ini <- Sys.time()
rn1 <- neuralnet(y ~ x, 
                 data=df1, 
                 hidden = c(3,2))
tempo_fim <- Sys.time()
tempo_fim - tempo_ini

plot(rn1)

df1['pred1'] <- predict(rn1, df1)


# Valores esperados e observados
x_vs_y_EspObs_rna2 <- ggplot(df1, aes(x,y)) + # gráfico base >> x vs y <<
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,pred1, colour='Esperado')) + # Gráfico sobreposto >> x vs pred
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados") +
  scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")

x_vs_y_EspObs_rna2
