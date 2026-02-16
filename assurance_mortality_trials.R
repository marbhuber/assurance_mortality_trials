#############################################
# Assurance illustrated in multicentre trials
#############################################

rm(list=ls())
library(tidyverse)
library(compareGroups)
set.seed(2343)

# data - provided in the Supplementary Material of Sidebotham, Popovich and Lumley (DOI: 10.1016/j.bja.2021.06.026)

data_bja <- readxl::read_excel("data_bja.xlsx") %>% 
  rownames_to_column()

df <- data_bja %>% 
  transmute(
    Title,
    id=rowname,
    N = as.integer(`Planned sample size`),
    p.control   = as.numeric(`Expected control group mortality`),
    p.treatment = as.numeric(`Expected control group mortality`) - as.numeric(`Anticipated Absolute Mortality Difference`),
    power = `Design power`,
    effect.estimated = `Anticipated Absolute Mortality Difference` %>% as.numeric(),
    effect.measured = `Absolute mortality reduction` %>% as.numeric()
  ) %>% 
  na.omit()


# Use uniform prior, 10'000 simulation for each choice of prior sample from the uniform prior

n.sim = 1e4
df.ap      <- data.frame()
for (study in 1:nrow(df)){
    print(study)
    rm(myrange)
    pcontrol   = df$p.control[study]
    ptreatment = df$p.treatment[study]
    mylower    = ptreatment
    myupper    = pcontrol                 
    myrange    = seq(mylower,myupper,length.out=100)
    delta.p    = 1/length(myrange) # they are all equally likely
    mean.success = rep(NA,length(myrange))
    n = 1
    for (myr in myrange){
      # now run 1000 trials
      rm(trial.p.control,trial.p.treatment)
      trial.p.control   = rbinom(n.sim,size=round(df$N[study]/2),prob=pcontrol)
      trial.p.treatment = rbinom(n.sim,size=round(df$N[study]/2),prob=myr)
      results <- rep(NA,n.sim)
      for (ens in 1:n.sim){
        rm(test)
        test = prop.test(x = c(trial.p.control[ens], trial.p.treatment[ens]), n = c(round(df$N[study]/2), round(df$N[study]/2)))
        results[ens] <- ifelse(test$p.value<0.05,1,0)
      }
      mean.success[n] = mean(results,na.rm=T)
      n=n+1
    }
    df.ap <- rbind(
      df.ap,
      data.frame(
        study, 
        assurance = sum(mean.success*delta.p))
    )
  }

# Illustration for a single trial

df.example <- df %>% filter(Title == "Liberal or Conservative Oxygen Therapy for Acute Respiratory Distress Syndrome.")
df.ap.full <- data.frame()
for (study in 1:nrow(df.example)){
  print(study)
  rm(myrange)
  pcontrol   = df.example$p.control[study]
  ptreatment = df.example$p.treatment[study]
  mylower    = ptreatment#
  myupper    = pcontrol                 
  myrange    = seq(mylower,myupper,length.out=100)
  delta.p    = 1/length(myrange) # they are all equally likely
  mean.success = rep(NA,length(myrange))
  n = 1
  for (myr in myrange){
    # now run 1000 trials
    rm(trial.p.control,trial.p.treatment)
    trial.p.control   = rbinom(n.sim,size=round(df.example$N[study]/2),prob=pcontrol)
    trial.p.treatment = rbinom(n.sim,size=round(df.example$N[study]/2),prob=myr)
    results <- rep(NA,n.sim)
    for (ens in 1:n.sim){
      rm(test)
      test = prop.test(alternative = c("greater"),x = c(trial.p.control[ens], trial.p.treatment[ens]), n = c(round(df.example$N[study]/2), round(df.example$N[study]/2)))
      results[ens] <- ifelse(test$p.value<0.05,1,0)
    }
    mean.success[n] = mean(results,na.rm=T)
    n=n+1
  }
  df.ap.full <- rbind(
    df.ap.full,
    data.frame(
      study, 
      parameter = rev(myrange)-ptreatment,
      powers    = mean.success,
      assurance = sum(mean.success*delta.p))
  )
}

########################
########################
# Summary measures
########################
########################

createTable(
  compareGroups(~power,data=df %>% mutate(power=100*as.numeric(power)),method=2)
)

createTable(
  compareGroups(~assurance,data=df.ap %>% mutate(assurance = 100*assurance),method=2),digits=0
)

createTable(
  compareGroups(~effect.estimated,data=df %>% mutate(effect.estimated = 100*effect.estimated),method=2),digits=1
)

createTable(
  compareGroups(~effect.measured,data=df %>% mutate(effect.measured = 100*effect.measured),method=2),digits=1
)


########################
########################
# Figure
########################
########################

# (A) Illustration

figA <- ggplot()+
  geom_ribbon(
    data = data.frame(x=seq(0.21,0.3,by=0.01),ymin=0,ymax=1),
    aes(x=x,ymin=ymin,ymax=ymax),
    alpha=0.3
  )+
  scale_x_continuous(labels = scales::percent_format(accuracy = 5L),n.breaks=10,limits = c(0.1,0.4))+
  theme_classic()+
  theme(
    legend.position = "none",
    text = element_text(size=16),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())+
  xlab("Mortality incidence (primary outcome)")+
  ylab("")+
  scale_y_continuous(limits = c(0,1.6))+
  annotate("text", x = 0.25, y = 0.6, label = "Prior",size=8)+
  geom_segment(aes(x = 0.21, y = 0, xend = 0.21, yend = 1.2),size=1)+
  geom_point(aes(x=0.21,y=1.2,size=2))+
  geom_segment(aes(x = 0.3, y = 0, xend = 0.3, yend = 1.2),size=1)+
  geom_point(aes(x=0.3,y=1.2,size=2))+
  annotate("text", x = 0.25, y = 0.6, label = "Prior",size=8)+
  annotate(geom = "text", x = 0.21, y = 1.4, label = "liberal\noxygen", color = "black",angle = 0,size=5)+
  annotate(geom = "text", x = 0.3, y = 1.4, label = "conservative\noxygen", color = "black",angle = 0,size=5)
  
# (B) Power as function of effect size -> integrating gives the assurance
figB <- df.ap.full %>% 
  ggplot(aes(x=parameter,y=powers))+
  geom_line(linewidth=1)+
  theme_bw()+
  scale_y_continuous(labels = scales::percent,n.breaks=10,limits = c(0,1))+
  theme(
    legend.position = "none",
    text = element_text(size=16))+
  scale_x_continuous(breaks = seq(0,0.09,by=0.01),labels = paste0(seq(0,9),"%"))+
    # labels = scales::percent,n.breaks=10,limits = c(0,0.09))+
  xlab("Anticipated risk reduction")+
  ylab("Power")+
  geom_ribbon(
    aes(x=parameter,ymin=0,ymax=powers),alpha=0.3
  )+
  geom_hline(yintercept = unique(df.ap.full$assurance),linetype="dashed",color="orange",size=1)+
  annotate(geom = "text", x = 0.02, y = unique(df.ap.full$assurance)+0.1, label = "Assurance\n(expected power)", color = "orange",angle = 0,size=4.4)
  

figAB <- cowplot::plot_grid(figA,figB,labels = c("A","B"),align="hv")

# (C) Conditional versus unconditional power

figC <- df.ap %>% 
  left_join(
    df %>% select(id,power) %>% mutate(power = as.numeric(power),study=as.integer(id))
  ) %>% 
  na.omit() %>% 
  mutate(
    power.conditional = power,
    power.unconditional = assurance
  ) %>% 
  select(-power) %>% 
  pivot_longer(contains("power")) %>% 
  mutate(
    name.new = case_when(
      name=="power.conditional"~"Conditional power in sample size calculations",
      name=="power.unconditional"~"Expected power in sample size calculations")
  ) %>% 
  ggplot(aes(x=value,color=name.new,fill=name.new))+
  # geom_vline(xintercept = 0,linetype="dashed",color="darkgrey")+
  geom_histogram(alpha=0.7,color="white")+
  theme_bw()+
  theme(
    legend.position = "none",
    text = element_text(size=16)
  )+
  facet_wrap(~name.new,nrow=2)+
  ggsci::scale_color_jama()+
  ggsci::scale_fill_jama()+
  ylab("Number of studies")+
  xlab("Power")+
  scale_x_continuous(labels = scales::percent,n.breaks=10,limits = c(0,1))


# (D) Assumed and reported
figD <- df %>% 
  pivot_longer(contains("effect")) %>% 
  mutate(
    name.new = case_when(
      name=="effect.estimated"~"Assumed effects in sample size calculation",
      name=="effect.measured"~"Reported effects in trial"
    )
  ) %>% 
  ggplot(aes(x=value,color=name.new,fill=name.new))+
  geom_vline(xintercept = 0,linetype="dashed",color="darkgrey")+
  geom_histogram(alpha=0.7,color="white")+
  theme_bw()+
  theme(
    legend.position = "none",
    text = element_text(size=16)
  )+
  facet_wrap(~name.new,nrow=2)+
  ggsci::scale_color_jama()+
  ggsci::scale_fill_jama()+
  ylab("Number of studies")+
  xlab("Reduction in mortality")+
  scale_x_continuous(labels = scales::percent,n.breaks=10,limits = c(-.22,0.22))


figCD <- cowplot::plot_grid(figC,figD,align = "hv",labels = c("C","D"))

ggpubr::ggarrange(figAB,figCD,nrow=2,heights = c(1,1.4))

ggsave("figure1.png",dpi=600,width=9.5,height=8)

