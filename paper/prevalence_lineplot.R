
source("hrs/g2ach01_input_data.R")
source("elsa/g2ach01_input_data.R")
source("charls/g2ach01_input_data.R")

# Calculate all prevalence by wave
hrs_prevalences <- hrs_long_svy %>%
  group_by(wave) %>%
  summarise(
    h_htn_prevalence = survey_mean(h_htn == 1, vartype = "ci"),
    w_htn_prevalence = survey_mean(w_htn == 1, vartype = "ci"),
    joint_htn_prevalence = survey_mean(h_htn == 1 & w_htn == 1, vartype = "ci")
  ) %>%
  ungroup() %>% 
  mutate(data = "hrs",country = "USA")

elsa_prevalences <- elsa_long_svy %>%
  group_by(wave) %>%
  summarise(
    h_htn_prevalence = survey_mean(h_htn == 1, vartype = "ci"),
    w_htn_prevalence = survey_mean(w_htn == 1, vartype = "ci"),
    joint_htn_prevalence = survey_mean(h_htn == 1 & w_htn == 1, vartype = "ci")
  ) %>%
  ungroup() %>% 
  mutate(data = "elsa",country = "England")

charls_prevalences <- charls_long_svy %>%
  group_by(wave) %>%
  summarise(
    h_htn_prevalence = survey_mean(h_htn == 1, vartype = "ci"),
    w_htn_prevalence = survey_mean(w_htn == 1, vartype = "ci"),
    joint_htn_prevalence = survey_mean(h_htn == 1 & w_htn == 1, vartype = "ci")
  ) %>%
  ungroup() %>% 
  mutate(data = "charls",country = "China")

prevalences <- bind_rows(hrs_prevalences,
                         elsa_prevalences,
                         charls_prevalences)

prevalences_long <- bind_rows(prevalences %>%
                                dplyr::select(wave,data,country,starts_with("h_")) %>%
                                dplyr::rename_with(~ gsub("^h_", "", .), starts_with("h_")) %>%
                                mutate(gender = "male"),
                              prevalences %>%
                                dplyr::select(wave,data,country,starts_with("w_")) %>%
                                dplyr::rename_with(~ gsub("^w_", "", .), starts_with("w_")) %>%
                                mutate(gender = "female"),
                              prevalences %>%
                                dplyr::select(wave,data,country,starts_with("joint_")) %>%
                                dplyr::rename_with(~ gsub("^joint_", "", .), starts_with("joint_")) %>%
                                mutate(gender = "couple")) %>%
  group_by(data) %>%
  mutate(
    wave = case_when(
      data == "hrs" ~ wave - 7,
      data == "elsa" ~ wave / 2,
      TRUE ~ wave
    )
  ) %>%
  ungroup()


(fig_A = prevalences_long  %>% 
    dplyr::filter(gender == "male") %>% 
    ggplot(data=.,aes(x=wave,group=country,color=country,y=htn_prevalence)) +
    geom_point() +
    geom_path() +
    theme_bw() +
    scale_color_manual(name="",values=c("red","#56B4E9","#E69F00","#009E73")) +
    scale_y_continuous(limits=c(0.2,0.9),breaks=seq(0.2,0.9,0.1)) +
    scale_x_continuous(limits=c(0,8),breaks=seq(0,8,2)) +
    xlab("Wave") +ylab("Prevalence (%)") +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14)) + guides(fill=guide_legend(nrow=2,byrow=TRUE))
)

(fig_B = prevalences_long  %>% 
    dplyr::filter(gender == "female") %>% 
    ggplot(data=.,aes(x=wave,group=country,color=country,y=htn_prevalence)) +
    geom_point() +
    geom_path() +
    theme_bw() +
    scale_color_manual(name="",values=c("red","#56B4E9","#E69F00","#009E73")) +
    scale_y_continuous(limits=c(0.2,0.9),breaks=seq(0.2,0.9,0.1)) +
    scale_x_continuous(limits=c(0,8),breaks=seq(0,8,2)) +
    xlab("Wave") +ylab("Prevalence (%)") +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14)) + guides(fill=guide_legend(nrow=2,byrow=TRUE))
)

(fig_C = prevalences_long  %>% 
    dplyr::filter(gender == "couple") %>% 
    ggplot(data=.,aes(x=wave,group=country,color=country,y=htn_prevalence)) +
    geom_point() +
    geom_path() +
    theme_bw() +
    scale_color_manual(name="",values=c("red","#56B4E9","#E69F00","#009E73")) +
    scale_y_continuous(limits=c(0.2,0.9),breaks=seq(0.2,0.9,0.1)) +
    scale_x_continuous(limits=c(0,8),breaks=seq(0,8,2)) +
    xlab("Wave") +ylab("Prevalence (%)") +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14)) + guides(fill=guide_legend(nrow=2,byrow=TRUE))
)

library(ggpubr)
ggarrange(fig_A,
          fig_B,
          fig_C,
          nrow = 2,
          ncol = 2,
          labels = c("A","B","C"),
          legend = "bottom",
          common.legend = TRUE) %>% 
  ggsave(.,filename=paste0(path_htn_family_folder,"/figures/lineplot by country and gender.png"),width=9, height = 8)



