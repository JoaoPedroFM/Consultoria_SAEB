require(readr)
require(ggplot2)
require(dplyr)
require(gridExtra)
base = readr::read_csv2("base_saeb.csv") 
base = base |> 
  mutate(turno = ordered(turno,levels=c("Manh?","Tarde","Noite"), labels=c("Manh?","Tarde","Noite")))

#####
#TESTES ESTAT?STICOS
masc = filter(base, sexo == "Masculino")
fem = filter(base, sexo != "Masculino")
t.test(fem$prof_mat, masc$prof_mat) #PODE CONSIDERAR IGUAL
t.test(fem$prof_port, masc$prof_port) #PODE CONSIDERAR IGUAL

branco = filter(base, raca == "Branco")
nb = filter(base, raca != "Branco")
t.test(branco$prof_mat, nb$prof_mat) #N?O PODE CONSIDERAR IGUAL
t.test(branco$prof_port, nb$prof_port) #NAO PODE CONSIDERAR IGUAL

munic = filter(base, depADM == "Municipal")
fed = filter(base, depADM == "Federal")
est = filter(base, depADM == 'Estadual')
t.test(munic$prof_mat, fed$prof_mat)   # PODE CONSIDERAR IGUAL
t.test(munic$prof_port, fed$prof_port) # PODE CONSIDERAR IGUAL
t.test(munic$prof_mat, est$prof_mat)   #
t.test(munic$prof_port, est$prof_port) #
t.test(fed$prof_mat, est$prof_mat)     #
t.test(fed$prof_port, est_prof_port)   #

ce = filter(base, local == "CE")
es = filter(base, local == "ES")
rj = filter(base, local == "RJ")
sp = filter(base, local == "SP")
t.test(ce$prof_mat, es$prof_mat)   # NAO PODE CONSIDERAR IGUAL
t.test(ce$prof_port, es$prof_port) # NAO PODE CONSIDERAR IGUAL
t.test(ce$prof_mat, sp$prof_mat)   # NAO PODE CONSIDERAR IGUAL
t.test(ce$prof_port, sp$prof_port) # NAO PODE CONSIDERAR IGUAL
t.test(ce$prof_mat, rj$prof_mat)   # NAO PODE CONSIDERAR IGUAL
t.test(ce$prof_port, rj$prof_port) # NAO PODE CONSIDERAR IGUAL
t.test(sp$prof_mat, rj$prof_mat)   # PODE CONSIDERAR IGUAL
t.test(sp$prof_port, rj$prof_port) # NAO PODE CONSIDERAR IGUAL
t.test(sp$prof_mat, es$prof_mat)   # PODE CONSIDERAR IGUAL
t.test(sp$prof_port, es$prof_port) # NAO PODE CONSIDERAR IGUAL
t.test(rj$prof_mat, es$prof_mat)   # PODE CONSIDERAR IGUAL
t.test(rj$prof_port, es$prof_port) # PODE CONSIDERAR IGUAL


manha = filter(base, turno == "Manh?")
tarde = filter(base, turno == "Tarde")
noite = filter(base, turno == "Noite")
t.test(manha$prof_port, tarde$prof_port) # PODE CONSIDERAR IGUAL
t.test(manha$prof_mat, tarde$prof_mat)   # PODE CONSIDERAR IGUAL
t.test(manha$prof_port, noite$prof_port) # PODE CONSIDERAR IGUAL
t.test(manha$prof_mat, noite$prof_mat)   # PODE CONSIDERAR IGUAL
t.test(tarde$prof_port, noite$prof_port) # PODE CONSIDERAR IGUAL
t.test(tarde$prof_mat, noite$prof_mat)   # PODE CONSIDERAR IGUAL



dados = stack(base)
bartlett.test(prof_port~esc_pai, data = dados, center =
                  "mean")
bartlett.test(prof_mat~esc_pai, data = dados, center =
               "mean")

oneway.test(prof_mat ~ esc_pai, data = dados, var.equal = T)
oneway.test(prof_port ~ esc_pai, data = dados, var.equal = T)


#####
#HISTOGRAMAS
#####TURNOS (FIGURAS 1 E 2)
#PORTUGU?S
g1= base |> filter(turno %in% c("Manh?", "Tarde")) |>
  ggplot(mapping = aes(x = prof_port)) +
  geom_histogram(fill= "darkred", col="white", bins = 8)  +
  scale_y_continuous(limits = c(0,35), breaks = seq(0,35,5)) +
  labs(y= "Frequ?ncia", x="Profici?ncia em portugu?s",
       title="Profici?ncia em portugu?s para os alunos dos turnos de manh? e tarde",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + theme_classic()+ theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


g2= base |> filter(turno == "Noite") |>
  ggplot(base, mapping = aes(x = prof_port)) +
  geom_histogram(fill= "darkred", col="white", bins = 8) +
  scale_x_continuous(breaks = seq(240, 360, 15), limits = c(242.0635, 358.0635)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
  labs(y= "Frequ?ncia", x="Profici?ncia em portugu?s",
       title="Profici?ncia em portugu?s para os alunos do turno da noite",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + theme_classic()+ theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


#MATEM?TICA
g3= base |> filter(turno %in% c("Manh?", "Tarde")) |>
  ggplot(mapping = aes(x = prof_mat)) +
  geom_histogram(fill= "darkred", col="white", bins = 10)  +
  scale_x_continuous(breaks = seq(210, 420, 30)) +
  scale_y_continuous(limits = c(0,35), breaks = seq(0,35,5)) +
  labs(y= "Frequ?ncia", x="Profici?ncia em matem?tica",
       title="Profici?ncia em matem?tica para os alunos dos turnos de manh? e tarde",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + theme_classic()+ theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


g4= base |> filter(turno == "Noite") |>
  ggplot(base, mapping = aes(x = prof_mat)) +
  geom_histogram(fill= "darkred", col="white", bins = 10) +
  scale_x_continuous(breaks = seq(210, 420, 12), limits = c(210, 420)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
  labs(y= "Frequ?ncia", x="Profici?ncia em matem?tica",
       title="Profici?ncia em matem?tica para os alunos do turno da noite",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + theme_classic()+ theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))

grid.arrange(g1,g3,nrow=2)
grid.arrange(g2,g4,nrow=2)

#####SEXO (FIGURAS 3 E 4)
#PORTUGU?S
g5= base |> filter(sexo == "Masculino") |>
  ggplot(mapping = aes(x = prof_port)) +
  geom_histogram(fill= "darkred", col="white", bins = 8)  +
  scale_y_continuous(limits = c(0,35), breaks = seq(0,35,5)) +
  labs(y= "Frequ?ncia", x="Profici?ncia em portugu?s",
       title="Profici?ncia em portugu?s para os alunos do sexo masculino",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + 
  theme_classic() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))

g6= base |> filter(sexo == "Feminino") |>
  ggplot(mapping = aes(x = prof_mat)) +
  geom_histogram(fill= "darkred", col="white", bins = 8)  +
  scale_y_continuous(limits = c(0,35), breaks = seq(0,35,5)) +
  labs(y= "Frequ?ncia", x="Profici?ncia em portugu?s",
       title="Profici?ncia em portugu?s para os alunos do sexo feminino",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + 
  theme_classic() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


#MATEM?TICA
g7= base |> filter(sexo == "Masculino") |>
  ggplot(mapping = aes(x = prof_mat)) +
  geom_histogram(fill= "darkred", col="white", bins = 10)  +
  scale_x_continuous(breaks = seq(210, 420, 30)) +
  scale_y_continuous(limits = c(0,25), breaks = seq(0,25,5)) +
  labs(y= "Frequ?ncia", x="Profici?ncia em matem?tica",
       title="Profici?ncia em matem?tica para os alunos do sexo masculino",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + 
  theme_classic() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


g8= base |> filter(sexo == "Feminino") |>
  ggplot(mapping = aes(x = prof_mat)) +
  geom_histogram(fill= "darkred", col="white", bins = 10)  +
  scale_x_continuous(breaks = seq(210, 420, 30)) +
  scale_y_continuous(limits = c(0,25), breaks = seq(0,25,5)) +
  labs(y= "Frequ?ncia", x="Profici?ncia em matem?tica",
       title="Profici?ncia em matem?tica para os alunos do sexo feminino",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + 
  theme_classic() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))

grid.arrange(g5,g7,nrow=2)
grid.arrange(g6,g8,nrow=2)

#####
#DISPERS?O
#ESTADO
ggplot(base, aes(x = prof_port, y = prof_mat, colour = local)) +
  geom_point() + 
  theme_classic() +
  labs(x = "Profici?ncia em Portugu?s", y = "Profici?ncia em Matem?tica", title = "Rela??o de profici?ncia de portugu?s e 
       matem?tica segundo estado", colour = "Localiza??o") +
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))

#SEXO
ggplot(base, aes(x = prof_port, y = prof_mat, colour = sexo)) +
  geom_point() + 
  theme_classic() +
  labs(x = "Profici?ncia em Portugu?s", y = "Profici?ncia em Matem?tica", title = "Rela??o de profici?ncia de portugu?s e 
       matem?tica segundo sexo", colour = "Sexo") +
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))

#TURNO
ggplot(base, aes(x = prof_port, y = prof_mat, colour = turno)) +
  geom_point() + 
  theme_classic() +
  labs(x = "Profici?ncia em Portugu?s", y = "Profici?ncia em Matem?tica", title = "Rela??o de profici?ncia de portugu?s e 
       matem?tica segundo turno", colour = "Turno") +
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))

#depADM
ggplot(base_aux, aes(x = prof_port, y = prof_mat, colour = depADM)) +
  geom_point() + 
  theme_classic() +
  labs(x = "Profici?ncia em Portugu?s", y = "Profici?ncia em Matem?tica", title = "Rela??o de profici?ncia de portugu?s e 
       matem?tica segundo depend?ncia administrativa", colour = "Depend?ncia Administrativa") +
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))

#ra?a
ggplot(base, aes(x = prof_port, y = prof_mat, colour = raca)) +
  geom_point() + 
  theme_classic() +
  labs(x = "Profici?ncia em Portugu?s", y = "Profici?ncia em Matem?tica", title = "Rela??o de profici?ncia de portugu?s e 
       matem?tica segundo ra?a", colour = "Ra?a") +
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


#####
#BOXPLOTS
####TURNOS (FIGURAS 1 E 2)
#portugues
base |> 
  ggplot(mapping = aes(y = prof_port, x = factor(turno))) +
  geom_boxplot(fill = c("blue", "yellow", "purple")) +
  labs(x = "Turno", y = "Profici?ncia em portugu?s", title="Profici?ncia em portugu?s segundo turno",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") +
  theme_classic() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))

#matematica
base |> 
  ggplot(mapping = aes(y = prof_mat, x = factor(turno))) +
  geom_boxplot(fill = c("blue", "yellow", "purple")) +
  labs(x = "Turno", y = "Profici?ncia em matem?tica", title="Profici?ncia em matem?tica segundo turno",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") +
  theme_classic() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))

####SEXO (FIGURAS 3 E 4)
#portugues
base |> 
  ggplot(mapping = aes(y = prof_port, x = factor(sexo, labels = c("Feminino","Masculino")))) +
  geom_boxplot(fill = c("pink", "blue")) +
  labs(x = "Sexo", y = "Profici?ncia em portugu?s", title="Profici?ncia em portugu?s segundo sexo",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + 
  theme_classic() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


#matematica
base |> 
  ggplot(mapping = aes(y = prof_mat, x = factor(sexo, labels = c("Feminino","Masculino")))) +
  geom_boxplot(fill = c("pink", "blue")) +
  labs(x = "Sexo", y = "Profici?ncia em matem?tica", title="Profici?ncia em matem?tica segundo sexo",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + 
  theme_classic() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


####ESTADO (FIGURAS 5 A 8)
#portugues
base |> 
  ggplot(mapping = aes(y = prof_port, x = factor(local))) +
  geom_boxplot(fill = c("red", "blue", "yellow", "green")) +
  labs(x = "Estado", y = "Profici?ncia em portugu?s", title="Profici?ncia em portugu?s segundo sexo",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + 
  theme_classic() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


#matem?tica
base |> 
  ggplot(mapping = aes(y = prof_mat, x = factor(local))) +
  geom_boxplot(fill = c("red", "blue", "yellow", "green")) +
  labs(x = "Estado", y = "Profici?ncia em matem?tica", title="Profici?ncia em matem?tica segundo estado",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + 
  theme_classic() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


####RA?A (FIGURAS 9 E 10)
#portugues
base |> 
  ggplot(mapping = aes(y = prof_port, x = factor(raca))) +
  geom_boxplot(fill = c("white", "black")) +
  labs(x = "Ra?a", y = "Profici?ncia em portugu?s", title="Profici?ncia em portugu?s segundo ra?a",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + 
  theme_classic() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


#matem?tica
base |> 
  ggplot(mapping = aes(y = prof_mat, x = factor(raca))) +
  geom_boxplot(fill = c("white", "black")) +
  labs(x = "Ra?a", y = "Profici?ncia em matem?tica", title="Profici?ncia em matem?tica segundo ra?a",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + 
  theme_classic() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))


####ESTADUAIS E FEDERAIS + PELO MENOS UM DOS PAIS DE EM COMPLETO (FIGURAS 11 E 12)
base_aux = filter(base, esc_mae == "EM completo" | esc_pai %in% c("EM completo", "Ensino superior completo ou mais"))

for (i in 1:76){
  if(base_aux$depADM[i] == "Municipal"){
    base_aux$depADM[i] = "Municipal"
  }else{
    base_aux$depADM[i] = "Estadual/Federal"
  }
}

#portugues
base_aux |>
  ggplot(mapping = aes(y = prof_port, x = factor(depADM))) +
  geom_boxplot(fill = c("red", "blue")) +
  labs(x = "Depend?ncia administrativa", y = "Profici?ncia em portugu?s", title="Profici?ncia em portugu?s segundo depend?ncia administrativa",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + 
  theme_classic() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))

#matematica
base_aux |>
  ggplot(mapping = aes(y = prof_mat, x = factor(depADM))) +
  geom_boxplot(fill = c("red", "blue")) +
  labs(x = "Depend?ncia administrativa", y = "Profici?ncia em matem?tica", title="Profici?ncia em matem?tica segundo depend?ncia administrativa",
       subtitle="Dados referentes aos realizantes do SAEB 2017 (5? ano)") + 
  theme_classic() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size=14, face="bold"), 
        text = element_text(size=15), plot.subtitle = element_text(hjust = 0.5, size=12))

