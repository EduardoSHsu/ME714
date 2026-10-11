# DISCIPLINA: ME714 - Análise de Dados Discretos
# PROJETO: Eficácia da Medicina Chinesa (Decocção CGD)
# AUTORES: Yejee Kim, Marcel Adati, Eduardo Hsu, Joaquim Nucci

# Lista de pacotes necessários 
pacotes <- c("tidyverse", "readxl", "gtsummary", "car", "patchwork", "hnp", "pROC")

for (p in pacotes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

dados <- read_excel("dataset.xlsx")

aba_indicadores <- read_excel("dataset.xlsx", sheet = "Baseline indicators") %>%
  mutate(`Admission number` = as.character(`Admission number`))

aba_pressoes <- read_excel("dataset.xlsx", sheet = "Blood pressures") %>%
  mutate(`Admission number` = as.character(`Admission number`))
dados_completos <- inner_join(aba_indicadores, aba_pressoes, by = "Admission number")

# 2. LIMPEZA E FORMATAÇÃO DE TODAS AS VARIÁVEIS

dados_eda <- dados_completos %>%
  rename(
    Pressao_Alvo = `Achieving enhanced BP reduction (0, no; 1, yes)`,
    Grupo = `Group (1, AHD; 2, CGD+AHD).x`, 
    Idade = `Age, yr`,
    IMC = `BMI, kg/m²`,
    Sexo = `Sex  (1, male; 2, female)`,
    Pressao_Basal = `SBP (before treatment), mm Hg`,
    Tempo_Doenca = `Duration of illness (0,＜10 yr; 1, ≥10 yr)`,
    Fumo = `Smoking (0, no; 1, yes)`,
    Bebe = `Drinking (0, no; 1, yes)`,
    Diabetes = `Diabetes mellitus (0, no; 1, yes)`,
    Doenca_Coronaria = `Coronary heart disease (0, no; 1, yes)`,
    Disritmia = `Cardiac Dysrhythmia (0, no; 1, yes)`,
    Enfarte_Cerebral = `Cerebral infarction (0, no; 1, yes)`,
    Hipotireoidismo = `Hypothyroidism (0, no; 1, yes)`,
    Hiperlipemia = `Hyperlipemia (0, no; 1, yes)`,
    Hiperuricemia = `Hyperuricemia (0, no; 1, yes)`
  ) %>%
  dplyr::select(Pressao_Alvo, Grupo, Idade, IMC, Sexo, Pressao_Basal, Tempo_Doenca, Fumo, Bebe, 
         Diabetes, Doenca_Coronaria, Disritmia, Enfarte_Cerebral, Hipotireoidismo, 
         Hiperlipemia, Hiperuricemia) %>%
  filter(Idade > 0, IMC > 0, Pressao_Basal > 0) %>%
  drop_na() %>%
  mutate(
    Pressao_Alvo = factor(Pressao_Alvo, levels = c(0, 1), labels = c("Falha", "Sucesso")),
    Grupo = factor(Grupo, levels = c(1, 2), labels = c("AHD", "CGD+AHD")),
    Sexo = factor(Sexo, levels = c(1, 2), labels = c("Masc", "Fem")),
    Tempo_Doenca = factor(Tempo_Doenca, levels = c(0, 1), labels = c("<10 Anos", "≥10 Anos")),

    across(c(Fumo, Bebe, Diabetes, Doenca_Coronaria, Disritmia, Enfarte_Cerebral, 
             Hipotireoidismo, Hiperlipemia, Hiperuricemia), 
           ~ factor(.x, levels = c(0, 1), labels = c("Não", "Sim")))
  )

# 3. TABELAS CLÍNICAS

# TABELA 1: Características Basais - Compara Tratamentos
tabela_1 <- dados_eda %>%
  dplyr::select(-Pressao_Alvo) %>% 
  tbl_summary(
    by = Grupo,
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 1
  ) %>%
  add_p() %>% 
  modify_header(label = "**Característica Clínica**") %>%
  modify_caption("**Tabela 1:** Características Basais por Grupo de Tratamento")

print(tabela_1)

# TABELA 2: Fatores Associados ao Desfecho 
tabela_desfecho <- dados_eda %>%
  dplyr::select(-Grupo) %>% 
  tbl_summary(
    by = Pressao_Alvo,
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 1
  ) %>%
  add_p() %>% 
  modify_header(label = "**Fatores de Risco / Clínicos**") %>%
  modify_caption("**Tabela 2:** Variáveis vs. Sucesso na Redução da Pressão")

print("--- A processar a Tabela de Desfecho ---")
print(tabela_desfecho)

# 4. ANÁLISE VISUAL

# 4.1 Gráfico Multi-Painel para Variáveis Contínuas (Idade, IMC, Pressão Basal)
dados_continuos <- dados_eda %>%
  dplyr::select(Pressao_Alvo, Idade, IMC, Pressao_Basal) %>%
  pivot_longer(cols = -Pressao_Alvo, names_to = "Variavel", values_to = "Valor")

grafico_continuas <- ggplot(dados_continuos, aes(x = Pressao_Alvo, y = Valor, fill = Pressao_Alvo)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red") +
  facet_wrap(~ Variavel, scales = "free_y") + 
  scale_fill_manual(values = c("#e74c3c", "#2ecc71")) +
  theme_minimal() +
  labs(title = "Variáveis Contínuas vs. Desfecho", x = "", y = "Valores Medidos") +
  theme(legend.position = "none")

print(grafico_continuas)

# 4.2 Gráfico Multi-Painel para todas as Variáveis Categóricas/Doenças
dados_categoricos <- dados_eda %>%
  dplyr::select(-Idade, -IMC, -Pressao_Basal, -Grupo) %>%
  pivot_longer(cols = -Pressao_Alvo, names_to = "Condicao", values_to = "Resposta")

grafico_doencas <- ggplot(dados_categoricos, aes(x = Resposta, fill = Pressao_Alvo)) +
  geom_bar(position = "fill", alpha = 0.85) + 
  facet_wrap(~ Condicao, scales = "free_x", ncol = 4) + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#e74c3c", "#2ecc71")) +
  theme_minimal() +
  labs(title = "Prevalência de Sucesso Terapêutico por Comorbidade e Estilo de Vida", 
       x = "", y = "Proporção de Pacientes (%)", fill = "Desfecho") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_doencas)

# 1. ESTRATIFICAÇÃO CLÍNICA (CRIANDO OS SUBGRUPOS)

dados_estratificados <- dados_eda %>%
  mutate(
    # Fatiar o IMC pelos pontos de corte da OMS
    IMC_Categoria = case_when(
      IMC < 25 ~ "1. Peso Adequado (<25)",
      IMC >= 25 ~ "2. Sobrepeso/Obesidade (≥25)"
    ),
    
    Pressao_Categoria = case_when(
      Pressao_Basal < 160 ~ "1. Hipertensão Estágio 1 (<160)",
      Pressao_Basal >= 160 ~ "2. Hipertensão Estágio 2 ou mais (≥160)"
    )
  ) %>%
  mutate(
    IMC_Categoria = as.factor(IMC_Categoria),
    Pressao_Categoria = as.factor(Pressao_Categoria)
  )

# 2. TESTES DE MANTEL-HAENSZEL

print("TESTE 1: MANTEL-HAENSZEL ESTRATIFICADO POR IMC")

tabela_3d_imc <- xtabs(~ Grupo + Pressao_Alvo + IMC_Categoria, data = dados_estratificados)

resultado_mh_imc <- mantelhaen.test(tabela_3d_imc)
print(resultado_mh_imc)

print("TESTE 2: MANTEL-HAENSZEL ESTRATIFICADO POR PRESSÃO BASAL")

tabela_3d_pressao <- xtabs(~ Grupo + Pressao_Alvo + Pressao_Categoria, data = dados_estratificados)

resultado_mh_pressao <- mantelhaen.test(tabela_3d_pressao)
print(resultado_mh_pressao)

# 3. TESTE DE MANTEL-HAENSZEL ESTRATIFICADO POR SEXO

print("TESTE 3: MANTEL-HAENSZEL ESTRATIFICADO POR SEXO")

tabela_3d_sexo <- xtabs(~ Grupo + Pressao_Alvo + Sexo, data = dados_eda)

resultado_mh_sexo <- mantelhaen.test(tabela_3d_sexo)
print(resultado_mh_sexo)

# MODELAGEM PREDITIVA: REGRESSÃO LOGÍSTICA COM SELEÇÃO FORWARD

# 1. Definir o Modelo Nulo 
modelo_nulo <- glm(Pressao_Alvo ~ 1, 
                   data = dados_eda, 
                   family = binomial(link = "logit"))

# 2. Definir o Escopo Máximo 
# Deixamos de fora hábitos e doenças raras para respeitar o Princípio da Parcimônia e a EPV.
modelo_cheio <- glm(Pressao_Alvo ~ Grupo + IMC + Idade + Sexo + Pressao_Basal, 
                    data = dados_eda, 
                    family = binomial(link = "logit"))

# 3. Executar o Algoritmo Stepwise Forward
print("--- Iniciando a Seleção Stepwise Forward (Critério AIC) ---")
modelo_forward <- step(modelo_nulo, 
                       scope = list(lower = modelo_nulo, upper = modelo_cheio), 
                       direction = "forward", 
                       trace = 1) 

# RESULTADOS E INFERÊNCIA

print("--- Resumo do Modelo Selecionado ---")
summary(modelo_forward)

print("--- Razões de Chance (Odds Ratios) e Intervalos de Confiança (95%) ---")

exp(cbind(OR = coef(modelo_forward), confint(modelo_forward)))

print("--- Análise de Deviance (ANOVA Binária) ---")

anova(modelo_forward, test = "Chisq")

# TESTE DE MODELOS MANUAIS 

dados_eda$Idade_Centralizada <- dados_eda$Idade - mean(dados_eda$Idade, na.rm = TRUE)
dados_eda$Pressao_Centralizada <- dados_eda$Pressao_Basal -mean(dados_eda$Pressao_Basal)

# MODELO 0
modelo_0 <- glm(Pressao_Alvo ~ Grupo, 
                data = dados_eda, 
                family = binomial(link = "logit"))
# MODELO 1
modelo_1 <- glm(Pressao_Alvo ~ Grupo + IMC, 
                                  family = binomial(link = "logit"), 
                                  data = dados_eda)

# MODELO 2
modelo_2 <- glm(Pressao_Alvo ~ Grupo + IMC + Sexo, 
                data = dados_eda, 
                family = binomial(link = "logit"))
# MODELO3 
modelo_3 <- glm(Pressao_Alvo ~ Grupo + Pressao_Centralizada, 
                data = dados_eda, 
                family = binomial(link = "logit"))

# MODELO 4
modelo_4 <- glm(Pressao_Alvo ~ Grupo + Pressao_Centralizada + IMC, 
                data = dados_eda, 
                family = binomial(link = "logit"))

# MODELO 5
modelo_5 <- glm(Pressao_Alvo ~ Grupo + IMC + Sexo*Idade_Centralizada, 
                data = dados_eda, 
                family = binomial(link = "logit"))

# MODELO 6
modelo_6 <- glm(Pressao_Alvo ~ Grupo + Sexo*Idade_Centralizada, 
                data = dados_eda, 
                family = binomial(link = "logit"))

# MODELO 7
modelo_7 <- glm(Pressao_Alvo ~ Grupo + IMC + Sexo + Idade_Centralizada + Pressao_Centralizada, 
                data = dados_eda, 
                family = binomial(link = "logit"))

# RESULTADOS E COMPARAÇÃO

print("--- Comparação de AIC (Critério de Akaike) ---")
AIC(modelo_0, modelo_1, modelo_2, modelo_3, modelo_4, modelo_5, modelo_6, modelo_7)

print("--- Análise de Deviance (ANOVA Binária) entre Modelos ---")
anova(modelo_0, modelo_1, modelo_2, modelo_5, test = "Chisq")

print("--- Análise de Deviance (ANOVA Binária) entre Modelos ---")
anova(modelo_0, modelo_3, modelo_4, test = "Chisq")

print("--- Análise de Deviance (ANOVA Binária) entre Modelos ---")
anova(modelo_0, modelo_6, test = "Chisq")

print("--- Análise de Deviance (ANOVA Binária) entre Modelos ---")
anova(modelo_0, modelo_7, test = "Chisq")

print("--- Resumo do Modelo 0 ---")
summary(modelo_0)

print("--- Odds Ratios (OR) do Modelo 0 ---")
exp(cbind(OR = coef(modelo_0), confint(modelo_0)))

print("--- Odds Ratios (OR) do Modelo 1 ---")
exp(cbind(OR = coef(modelo_1), confint(modelo_1)))

print("--- Odds Ratios (OR) do Modelo 2 ---")
exp(cbind(OR = coef(modelo_2), confint(modelo_2)))

print("--- Odds Ratios (OR) do Modelo 3 ---")
exp(cbind(OR = coef(modelo_3), confint(modelo_3)))

print("--- Odds Ratios (OR) do Modelo 4 ---")
exp(cbind(OR = coef(modelo_4), confint(modelo_4)))

print("--- Odds Ratios (OR) do Modelo 5 ---")
exp(cbind(OR = coef(modelo_5), confint(modelo_5)))

#Diagnóstico do modelo selecionado (modelo_0)
#Gerar o Gráfico de Envelope Simulado

hnp(modelo_0, 
    resid.type = "deviance", 
    halfnormal = TRUE,
    print.on = TRUE, 
    paint.out = TRUE, 
    col.paint.out = "red",   
    main = "",
    xlab = "Quantis Teóricos",
    ylab = "Resíduos de Deviance")

residuos_deviance <- residuals(modelo_forward, type = "deviance")
residuos_pearson <- residuals(modelo_forward, type = "pearson")

summary(residuos_deviance)

# DISTÂNCIA DE COOK
op <- par(mar = c(5, 4, 1, 2))  

plot(modelo_0,
     which = 4,
     caption = "",   
     sub.caption = "", 
     main = "")

par(op)

#Curva roc e AUC
prob_modelo_0 <- predict(modelo_0,
                         newdata = dados_eda,
                         type = "response")


roc_modelo_0 <- roc(response = dados_eda$Pressao_Alvo,
                    predictor = prob_modelo_0)

auc_modelo_0 <- auc(roc_modelo_0)

plot(
  roc_modelo_0,
  col = "#E69F00",
  lwd = 3,
  main = paste()
)

abline(a = 0, b = 1,
       lty = 2,
       col = "gray50")

legend(
  "bottomright",
  legend = paste("AUC =", round(auc_modelo_0, 3)),
  bty = "n",
  text.col = "#E69F00",
  cex = 1.1
)
