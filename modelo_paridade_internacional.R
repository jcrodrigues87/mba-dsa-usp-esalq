################################################################################
# INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS
################################################################################

#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp","readxl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

################################################################################
# CARGA E TRATAMENTO DE DADOS
################################################################################

dados_etanol_blr = read.csv("dados/etanol_semanal_anp_blr.csv")
dados_acucar_usd = read.csv("dados/acucar_diario_ny_usd.csv")
dados_dolar_blr = read.csv("dados/dolar_diario_blr.csv")
dados_brent_usd = read.csv("dados/brent_diario_usd.csv")
dados_rbob_usd = read.csv("dados/rbob_diario_usd.csv")

dados_etanol_blr$data <- as.Date(dados_etanol_blr$data_fim,format="%d/%m/%Y")
dados_etanol_blr$data_inicio <- NULL
dados_etanol_blr$data_fim <- NULL

dados_acucar_usd$data_ajustada <- as.Date(dados_acucar_usd$data,format="%d/%m/%Y")
dados_acucar_usd$data <- dados_acucar_usd$data_ajustada + 1
dados_acucar_usd$data_ajustada <- NULL

dados_dolar_blr$data_ajustada <- as.Date(dados_dolar_blr$data,format="%d/%m/%Y")
dados_dolar_blr$data <- dados_dolar_blr$data_ajustada + 1
dados_dolar_blr$data_ajustada <- NULL

dados_brent_usd$data_ajustada <- as.Date(dados_brent_usd$data,format="%d/%m/%Y")
dados_brent_usd$data <- dados_brent_usd$data_ajustada + 1
dados_brent_usd$data_ajustada <- NULL

dados_rbob_usd$data_ajustada <- as.Date(dados_rbob_usd$data,format="%d/%m/%Y")
dados_rbob_usd$data <- dados_rbob_usd$data_ajustada + 1
dados_rbob_usd$data_ajustada <- NULL

dados = dados_etanol_blr

dados = merge(x = dados, 
              y = dados_acucar_usd, 
              by = "data", all.x = TRUE)

dados = merge(x = dados, 
              y = dados_dolar_blr, 
              by = "data", all.x = TRUE)

dados = merge(x = dados, 
              y = dados_brent_usd, 
              by = "data", all.x = TRUE)

dados = merge(x = dados,
              y = dados_rbob_usd,
              by = "data", all.x = TRUE)

dados = dados[dados$data >= '2017-01-01', ]

dados = na.omit(dados)

#Listar base de dados
dados %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 16)

#Visualizando as observações e as especificações referentes às variáveis do dataset
glimpse(dados)

#Estatísticas univariadas
summary(dados)

#A função correlation do pacote correlation faz com que seja estruturado um
#diagrama interessante que mostra a inter-relação entre as variáveis e a
#magnitude das correlações entre elas
#Requer instalação e carregamento dos pacotes see e ggraph para a plotagem
dados %>%
  correlation(method = "pearson") %>%
  plot()

#A função chart.Correlation() do pacote PerformanceAnalytics apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
chart.Correlation((dados[2:6]), histogram = TRUE)

#A função corr_plot do pacote metan também apresenta as distribuições
#das variáveis, scatters, valores das correlações e suas respectivas
#significâncias
dados %>%
  corr_plot(etanol_blr, acucar_usd, dolar_blr, brent_usd,
            shape.point = 21,
            col.point = "black",
            fill.point = "#FDE725FF",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 4,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#440154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#440154FF",
            pan.spacing = 0,
            lab.position = "bl")

#Estimando a Regressão Múltipla
modelo <- lm(formula = etanol_blr ~ . -data, data = dados)

#Parâmetros do modelo
summary(modelo)
export_summs(modelo, scale = F, digits = 5)

#Aplicando o procedimento Stepwise, temos o seguinte código:
step_modelo <- step(modelo, k = 3.841459)

#De onde vem o argumento k = 3.841459?
qchisq(p = 0.05, df = 1, lower.tail = F)
round(pchisq(3.841459, df = 1, lower.tail = F),7)

summary(step_modelo)

export_summs(step_modelo, scale = F, digits = 5)

#Parâmetros reais do modelo com procedimento Stepwise
confint(step_modelo, level = 0.95) # siginificância 5%
plot_summs(step_modelo, colors = "#440154FF") #função plot_summs do pacote ggstance

#Parâmetros padronizados
plot_summs(step_modelo, scale = TRUE, colors = "#440154FF")

#Adicionando a caracterização da distribição normal no IC de cada parâmetro beta
plot_summs(step_modelo, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#440154FF")

#Comparando os ICs dos betas dos modelos sem e com procedimento Stepwise
plot_summs(modelo, step_modelo, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#FDE725FF", "#440154FF"))

################################################################################
# TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE
#  SHAPIRO-FRANCIA
################################################################################
#Shapiro-Wilk: n <= 30
## shapiro.test(modelo_linear$residuals)

#Shapiro-Francia: n > 30
sf.test(step_modelo$residuals) #função sf.test do pacote nortest

#Histograma dos resíduos do modelo OLS linear
dados %>%
  mutate(residuos = step_modelo$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_modelo$residuals),
                            sd = sd(step_modelo$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


##################################################################################
#                            TRANSFORMAÇÃO DE BOX-COX                            #
##################################################################################
#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(dados$etanol_blr) #função powerTransform do pacote car#
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
dados$etanol_bc <- (((dados$etanol_blr ^ lambda_BC$lambda) - 1) / 
                     lambda_BC$lambda)

#Visualizando a nova variável na base de dados
dados %>%
  select(data, etanol_blr, etanol_bc, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

#Estimando um novo modelo múltiplo com variável dependente transformada por Box-Cox
modelo_bc <- lm(formula = etanol_bc ~ . -data -etanol_blr, 
                data = dados)

#Parâmetros do modelo
summary(modelo_bc)

#Aplicando o procedimento Stepwise
step_modelo_bc <- step(modelo_bc, k = 3.841459)

summary(step_modelo_bc)
export_summs(step_modelo, scale = F, digits = 5)

#Verificando a normalidade dos resíduos do modelo step_modelo_bc
sf.test(step_modelo_bc$residuals) #função sf.test do pacote nortest


#Plotando os novos resíduos do step_modelo_bc
dados %>%
  mutate(residuos = step_modelo_bc$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..),
                 color = "white",
                 fill = "#287D8EFF",
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_modelo_bc$residuals),
                            sd = sd(step_modelo_bc$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()


#Salvando os fitted values dos dois modelos (modelo_linear e modelo_bc)
dados$yhat_linear <- step_modelo$fitted.values
dados$yhat_modelo_bc <- (((step_modelo_bc$fitted.values*(lambda_BC$lambda))+
                            1))^(1/(lambda_BC$lambda))

#Ajustes dos modelos: valores previstos (fitted values) X valores reais
dados %>%
  ggplot() +
  geom_smooth(aes(x = etanol_blr, y = yhat_linear, color = "OLS Linear"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = etanol_blr, y = yhat_linear),
             color = "#FDE725FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = etanol_blr, y = yhat_modelo_bc, color = "Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = etanol_blr, y = yhat_modelo_bc),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = etanol_blr, y = etanol_blr), method = "lm", 
              color = "gray30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#440154FF", "#FDE725FF")) +
  labs(x = "etanol_blr", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


