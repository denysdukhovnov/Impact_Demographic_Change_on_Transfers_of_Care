library(tidyverse)
library(ggpubr)

setwd("../Output")

## Figure 1: Transfers matrices plot
matrix.plot.df <- as_tibble(transfers.array) %>% 
  pivot_longer(1:ncol(.)) %>% 
  mutate(`Caregiver Age` = factor(rep(4:18, each = dim(transfers.array)[2] * dim(transfers.array)[3]), 
                                  levels = 1:18, labels = ColAgeLab),
         `Caregiver Sex` = factor(rep(rep(1:2, each = dim(transfers.array)[2] * dim(transfers.array)[3]/2), dim(transfers.array)[1]), 
                                  levels = 1:2, labels = c("Male", "Female")),
         `Care Recipient Age` = factor(rep(1:18, dim(transfers.array)[1] * dim(transfers.array)[3]), levels = 1:18, labels = ColAgeLab),
         `Care Recipient Sex` = factor(rep(rep(1:2, each = dim(transfers.array)[2]), dim(transfers.array)[1] * dim(transfers.array)[3]/2),
                                       levels = 1:2, labels = c("Male", "Female"))) %>% 
  arrange(`Caregiver Sex`, `Caregiver Age`, `Care Recipient Age`, `Care Recipient Sex`) %>% 
  mutate(Transfer = factor(case_when(`Caregiver Sex` == "Male" & `Care Recipient Sex` == "Male" ~ 1,
                                     `Caregiver Sex` == "Male" & `Care Recipient Sex` == "Female" ~ 2,
                                     `Caregiver Sex` == "Female" & `Care Recipient Sex` == "Male" ~ 3,
                                     `Caregiver Sex` == "Female" & `Care Recipient Sex` == "Female" ~ 4,
                                     TRUE ~ NA_real_),
                           levels = 1:4, labels = c("a            Male-to-Male",
                                                    "b            Male-to-Female",
                                                    "c         Female-to-Male", 
                                                    "d         Female-to-Female")),
         `Minutes per day` = value) %>% 
  select(-name, -value)

Fig.1 <- ggplot(matrix.plot.df, aes(y = `Caregiver Age`, x = `Care Recipient Age`, fill = `Minutes per day`)) +
  geom_raster() +
  scale_fill_gradient(low = "grey95", high = "grey20") +
  facet_wrap(~Transfer, ncol = 2, scales = "free") +
  xlab("Ages of care recipients") +
  ylab("Ages of caregivers") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = 'bold', vjust = 0.95, hjust = -1),
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(2, "cm"),
    legend.text = element_text(size = 14),
    legend.box.margin = margin(0.5, 0, 0, 0, "cm"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x = element_text(size = 16, vjust = -1),
    axis.title.y = element_text(size = 16, vjust = 2.5),
    strip.text = element_text(size = 16, hjust = 0, face = "bold"))

Fig.1

ggsave(plot = Fig.1, filename = "Fig1_matrices.tiff", width = 4, height = 4, units = "in", dpi = 300, scale = 2, compression = "lzw")



## Figure 2: Transfers matrices by age and sex for caregiver subset

transfers.matrices.caregivers <- read.csv("care_transfer_matrices_by_age_and_sex_caregivers_only.csv", header = FALSE, stringsAsFactors = FALSE)
transfers.array.caregivers <- array(unlist(split(transfers.matrices.caregivers, rep(1:4, each = 15))), dim = c(15, 18, 4))

matrix.plot.df.caregivers <- as_tibble(transfers.array.caregivers) %>% 
  pivot_longer(1:ncol(.)) %>% 
  mutate(`Caregiver Age` = factor(rep(4:18, each = dim(transfers.array.caregivers)[2] * dim(transfers.array.caregivers)[3]), 
                                  levels = 1:18, labels = ColAgeLab),
         `Caregiver Sex` = factor(rep(rep(1:2, each = dim(transfers.array.caregivers)[2] * dim(transfers.array.caregivers)[3]/2), dim(transfers.array.caregivers)[1]), 
                                  levels = 1:2, labels = c("Male", "Female")),
         `Care Recipient Age` = factor(rep(1:18, dim(transfers.array.caregivers)[1] * dim(transfers.array.caregivers)[3]), levels = 1:18, labels = ColAgeLab),
         `Care Recipient Sex` = factor(rep(rep(1:2, each = dim(transfers.array.caregivers)[2]), dim(transfers.array.caregivers)[1] * dim(transfers.array.caregivers)[3]/2),
                                       levels = 1:2, labels = c("Male", "Female"))) %>% 
  arrange(`Caregiver Sex`, `Caregiver Age`, `Care Recipient Age`, `Care Recipient Sex`) %>% 
  mutate(Transfer = factor(case_when(`Caregiver Sex` == "Male" & `Care Recipient Sex` == "Male" ~ 1,
                                     `Caregiver Sex` == "Male" & `Care Recipient Sex` == "Female" ~ 2,
                                     `Caregiver Sex` == "Female" & `Care Recipient Sex` == "Male" ~ 3,
                                     `Caregiver Sex` == "Female" & `Care Recipient Sex` == "Female" ~ 4,
                                     TRUE ~ NA_real_),
                           levels = 1:4, labels = c("a            Male-to-Male",
                                                    "b            Male-to-Female",
                                                    "c         Female-to-Male", 
                                                    "d         Female-to-Female")),
         `Minutes per day` = value) %>% 
  select(-name, -value)

Fig.2 <- ggplot(matrix.plot.df.caregivers, aes(y = `Caregiver Age`, x = `Care Recipient Age`, fill = `Minutes per day`)) +
  geom_raster() +
  scale_fill_gradient(low = "grey95", high = "grey20") +
  facet_wrap(~Transfer, ncol = 2, scales = "free") +
  xlab("Ages of care recipients") +
  ylab("Ages of caregivers") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = 'bold', vjust = 0.95, hjust = -1),
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(2, "cm"),
    legend.text = element_text(size = 14),
    legend.box.margin = margin(0.5, 0, 0, 0, "cm"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x = element_text(size = 16, vjust = -1),
    axis.title.y = element_text(size = 16, vjust = 2.5),
    strip.text = element_text(size = 16, hjust = 0, face = "bold"))

Fig.2

ggsave(plot = Fig.2, filename = "Fig2_matrices_for_caregivers.tiff", width = 4, height = 4, units = "in", dpi = 300, scale = 2, compression = "lzw")



## Figure 3: Childcare and adult care emotions from ATUS and PSID

## creating the childcare and adult care emotion comparison plot for ATUS

# Standard palette
std.pal <- scales::grey_pal()(4)
names(std.pal) <- c("Childcare", "Adult Care")

# ATUS plot
atus.compare.plot <- ggplot(wb.estimates, aes(x = Mean, y = Emotion, 
                                              color = interaction(Sex, Activity, sep = " ", lex.order = T), 
                                              shape = interaction(Sex, Activity, sep = " ", lex.order = T))) + 
  geom_hline(yintercept = seq(1.5, 0.5 + (length(levels(wb.estimates$Emotion)) - 1), 1), color = "grey90") +
  geom_errorbarh(aes(xmin = Lower,
                     xmax = Upper,
                     height = 0), 
                 size = 1.1,
                 position = position_dodge(0.75)) +
  geom_point(position = position_dodge(width = 0.75), 
             size = 3.5) + 
  scale_shape_manual(values = c(16, 17, 16, 17)) +
  scale_color_manual(values = c("grey20", "grey20", "grey70", "grey70")) +
  scale_y_discrete(limits = rev(levels(wb.estimates$Emotion))) +
  scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(0,6,1), limits = c(0,6)) +
  labs(y = "",
       x = "Intensity") +
  guides(shape = guide_legend(title = "Activity by sex", reverse = T), color = guide_legend(title = "Activity by sex", reverse = T)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.2, hjust = 0.5, size = 16),
        axis.text.y = element_text(angle = 0, size = 16),
        axis.title.x = element_text(size = 16, margin = margin(t = 0.2, r = 0, l = 0, b = 0, unit = "in")),
        axis.title.y = element_text(size = 16, vjust = 1, margin = margin(t = 0, r = 0.25, l = 0, b = 0, unit = "in")),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.key.size = unit(1, "cm"),
        legend.margin = margin(0, 0.25, 0, 1, "cm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())

atus.compare.plot


# PSID plot
psid.compare.plot <- ggplot(graph.df.psid %>% 
                              mutate(Emotion = fct_relevel(Emotion, "Happy", "Calm", "Frustrated", "Worried", "Sad", "Pained", "Tired")),
                            aes(x = Mean, y = Emotion, 
                                color = interaction(Sex, Activity, sep = " ", lex.order = T), 
                                shape = interaction(Sex, Activity, sep = " ", lex.order = T))) +
  geom_hline(yintercept = seq(1.5, 0.5 + (length(levels(graph.df.psid$Emotion)) - 1), 1), color = "grey90") +
  geom_errorbarh(aes(xmin = Lower,
                     xmax = Upper,
                     height = 0), 
                 size = 1.1,
                 position = position_dodge(0.75)) +
  geom_point(position = position_dodge(width = 0.75), 
             size = 3.5) + 
  scale_shape_manual(values = c(16, 17, 16, 17)) +
  scale_color_manual(values = c("grey20", "grey20", "grey70", "grey70")) +
  scale_y_discrete(limits = rev(levels(graph.df.psid %>% 
                                         mutate(Emotion = fct_relevel(Emotion, "Happy", "Calm", "Frustrated", "Worried", "Sad", "Pained", "Tired")) %>% 
                                         pull(Emotion)))) +
  scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(0,6,1), limits = c(0,6)) +
  labs(y = "",
       x = "Intensity") +
  guides(shape = guide_legend(title = "Activity by sex", reverse = T), color = guide_legend(title = "Activity by sex", reverse = T)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.2, hjust = 0.5, size = 16),
        axis.text.y = element_text(angle = 0, size = 16),
        axis.title.x = element_text(size = 16, margin = margin(t = 0.2, r = 0, l = 0, b = 0, unit = "in")),
        axis.title.y = element_text(size = 16, vjust = 1, margin = margin(t = 0, r = 0.25, l = 0, b = 0, unit = "in")),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.key.size = unit(1, "cm"),
        legend.margin = margin(0, 0.25, 0, 1, "cm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())

psid.compare.plot


# Plot and save the combined ATUS and PSID activity by emotion intensity
Fig.3 <- ggarrange(atus.compare.plot, psid.compare.plot, ncol = 1,
                   labels = c("a", "b"), font.label = list(size = 28),
                   common.legend = T, legend = "right")
Fig.3

ggsave(plot = Fig.3, filename = "Fig3_dotplots.tiff", width = 7, height = 8, units = "in", dpi = 300, scale = 1.3, compression = "lzw")




## Figure 4: Marginal positive and negative production and consumption by age and sex (both in the total population and caregiver sample)

# Produce marginal production and consumption plots
marg.plot.male.df <- tibble(Feeling = factor(rep(rep(c("Positive", "Negative", "Total"), each = 18), 2), levels = c("Positive", "Negative", "Total")), 
                            Age = factor(rep(1:18, 6), levels = 1:18, labels = ColAgeLab),
                            Type = factor(rep(c("Production", "Consumption"), each = 18 * 3), levels = c("Production", "Consumption")),
                            Value = unname(c(pos.male.prod.marg, neg.male.prod.marg, overall.male.prod.marg, 
                                             pos.male.cons.marg, neg.male.cons.marg, overall.male.cons.marg)))

marg.plot.female.df <- tibble(Feeling = factor(rep(rep(c("Positive", "Negative", "Total"), each = 18), 2), levels = c("Positive", "Negative", "Total")), 
                              Age = factor(rep(1:18, 6), levels = 1:18, labels = ColAgeLab),
                              Type = factor(rep(c("Production", "Consumption"), each = 18 * 3), levels = c("Production", "Consumption")),
                              Value = unname(c(pos.female.prod.marg, neg.female.prod.marg, overall.female.prod.marg, 
                                               pos.female.cons.marg, neg.female.cons.marg, overall.female.cons.marg)))

marg.plot.male <- ggplot(marg.plot.male.df, 
                         aes(x = Age, y = Value, color = interaction(Feeling, Type), 
                             linetype = interaction(Feeling, Type), group = interaction(Feeling, Type))) +
  geom_line(size = 1) +
  scale_y_continuous(expand = c(0.01,0.01),
                     breaks = seq(0, 550, 50),
                     limits = c(0, 550)) +
  scale_x_discrete(expand = c(0,0.5)) +
  scale_linetype_manual(name = "", 
                        labels = c("Positive production", "Negative production", "Total production", 
                                   "Positive consumption", "Negative consumption", "Total consumption"),
                        values = c(2,3,1,2,3,1)) +
  scale_color_manual(name = "", 
                     labels = c("Positive production", "Negative production", "Total production", 
                                "Positive consumption", "Negative consumption", "Total consumption"),
                     values = c("black", "black", "black", "grey60", "grey60", "grey60")) +
  theme_minimal() +
  ylab("Mean minutes per day") +
  xlab("Age group") +
  guides(color = guide_legend(ncol = 3)) +
  theme(
    legend.position = "none",
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 16, vjust = 2.5),
    axis.ticks.y = element_line(color = "black"),
    axis.title.x = element_text(size = 16, vjust = -1.5),
    axis.text.x = element_text(vjust = 0.5, hjust = 1, angle = 90),
    axis.ticks.x = element_line(color = "black"),
    axis.text = element_text(size = 13),
    axis.line = element_line(size = 0.5), 
    plot.margin = margin(0.2,0.6,0.6,0.2, unit = "cm")
  )


marg.plot.female <- ggplot(marg.plot.female.df, 
                           aes(x = Age, y = Value, color = interaction(Feeling, Type), 
                               linetype = interaction(Feeling, Type), group = interaction(Feeling, Type))) + 
  geom_line(size = 1) +
  scale_y_continuous(expand = c(0.01,0.01),
                     breaks = seq(0, 550, 50),
                     limits = c(0, 550)) +
  scale_x_discrete(expand = c(0,0.5)) +
  scale_linetype_manual(name = "", 
                        labels = c("Positive production", "Negative production", "Total production", 
                                   "Positive consumption", "Negative consumption", "Total consumption"),
                        values = c(2,3,1,2,3,1)) +
  scale_color_manual(name = "", 
                     labels = c("Positive production", "Negative production", "Total production", 
                                "Positive consumption", "Negative consumption", "Total consumption"),
                     values = c("black", "black", "black", "grey60", "grey60", "grey60")) +
  theme_minimal() +
  xlab("Age group") +
  guides(color = guide_legend(ncol = 3)) +
  theme(
    #legend.position = "none",
    legend.title = element_text(size = 14, face = "bold"),
    legend.key.size = unit(1.25, "cm"),
    legend.text = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.ticks.y = element_line(color = "black"),
    axis.title.x = element_text(size = 16, vjust = -1.5),
    axis.text.x = element_text(vjust = 0.5, hjust = 1, angle = 90),
    axis.ticks.x = element_line(color = "black"),
    axis.text = element_text(size = 13),
    axis.line = element_line(size = 0.5), 
    plot.margin = margin(0.2,0.6,0.6,0.6, unit = "cm")
  )

marg.plot.male
marg.plot.female 



## Marginal positive and negative production and consumption by age and sex and positive caregiver status

# Produce marginal production and consumption plots
marg.plot.male.df.cg <- tibble(Feeling = factor(rep(rep(c("Positive", "Negative", "Total"), each = 18), 2), levels = c("Positive", "Negative", "Total")), 
                               Age = factor(rep(1:18, 6), levels = 1:18, labels = ColAgeLab),
                               Type = factor(rep(c("Production", "Consumption"), each = 18 * 3), levels = c("Production", "Consumption")),
                               Value = unname(c(pos.male.prod.marg.cg, neg.male.prod.marg.cg, overall.male.prod.marg.cg, 
                                                pos.male.cons.marg.cg, neg.male.cons.marg.cg, overall.male.cons.marg.cg)))

marg.plot.female.df.cg <- tibble(Feeling = factor(rep(rep(c("Positive", "Negative", "Total"), each = 18), 2), levels = c("Positive", "Negative", "Total")), 
                                 Age = factor(rep(1:18, 6), levels = 1:18, labels = ColAgeLab),
                                 Type = factor(rep(c("Production", "Consumption"), each = 18 * 3), levels = c("Production", "Consumption")),
                                 Value = unname(c(pos.female.prod.marg.cg, neg.female.prod.marg.cg, overall.female.prod.marg.cg, 
                                                  pos.female.cons.marg.cg, neg.female.cons.marg.cg, overall.female.cons.marg.cg)))

marg.plot.male.cg <- ggplot(marg.plot.male.df.cg, 
                            aes(x = Age, y = Value, color = interaction(Feeling, Type), 
                                linetype = interaction(Feeling, Type), group = interaction(Feeling, Type))) +
  geom_line(size = 1) +
  scale_y_continuous(expand = c(0.01,0.01),
                     breaks = seq(0, 550, 50),
                     limits = c(0, 550)) +
  scale_x_discrete(expand = c(0,0.5)) +
  scale_linetype_manual(name = "", 
                        labels = c("Positive production", "Negative production", "Total production", 
                                   "Positive consumption", "Negative consumption", "Total consumption"),
                        values = c(2,3,1,2,3,1)) +
  scale_color_manual(name = "", 
                     labels = c("Positive production", "Negative production", "Total production", 
                                "Positive consumption", "Negative consumption", "Total consumption"),
                     values = c("black", "black", "black", "grey60", "grey60", "grey60")) +
  theme_minimal() +
  ylab("Mean minutes per day") +
  xlab("Age group") +
  guides(color = guide_legend(ncol = 3)) +
  theme(
    legend.position = "none",
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 16, vjust = 2.5),
    axis.ticks.y = element_line(color = "black"),
    axis.title.x = element_text(size = 16, vjust = -1.5),
    axis.text.x = element_text(vjust = 0.5, hjust = 1, angle = 90),
    axis.ticks.x = element_line(color = "black"),
    axis.text = element_text(size = 13),
    axis.line = element_line(size = 0.5), 
    plot.margin = margin(0.2,0.6,0.6,0.2, unit = "cm")
  )


marg.plot.female.cg <- ggplot(marg.plot.female.df.cg, 
                              aes(x = Age, y = Value, color = interaction(Feeling, Type), 
                                  linetype = interaction(Feeling, Type), group = interaction(Feeling, Type))) + 
  geom_line(size = 1) +
  scale_y_continuous(expand = c(0.01,0.01),
                     breaks = seq(0, 550, 50),
                     limits = c(0, 550)) +
  scale_x_discrete(expand = c(0,0.5)) +
  scale_linetype_manual(name = "", 
                        labels = c("Positive production", "Negative production", "Total production", 
                                   "Positive consumption", "Negative consumption", "Total consumption"),
                        values = c(2,3,1,2,3,1)) +
  scale_color_manual(name = "", 
                     labels = c("Positive production", "Negative production", "Total production", 
                                "Positive consumption", "Negative consumption", "Total consumption"),
                     values = c("black", "black", "black", "grey60", "grey60", "grey60")) +
  theme_minimal() +
  xlab("Age group") +
  guides(color = guide_legend(ncol = 3)) +
  theme(
    #legend.position = "none",
    legend.title = element_text(size = 14, face = "bold"),
    legend.key.size = unit(1.25, "cm"),
    legend.text = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.ticks.y = element_line(color = "black"),
    axis.title.x = element_text(size = 16, vjust = -1.5),
    axis.text.x = element_text(vjust = 0.5, hjust = 1, angle = 90),
    axis.ticks.x = element_line(color = "black"),
    axis.text = element_text(size = 13),
    axis.line = element_line(size = 0.5), 
    plot.margin = margin(0.2,0.6,0.6,0.6, unit = "cm")
  )

marg.plot.male.cg
marg.plot.female.cg



# Combine and save the plots

Fig.4 <- ggarrange(ggplot() + theme_void(), marg.plot.male, marg.plot.female,
                   ggplot() + theme_void(), marg.plot.male.cg, marg.plot.female.cg,
                   common.legend = T, hjust = 0.5,
                   legend = "right",
                   labels = c("", "a", "b", "", "c", "d"),
                   widths = c(0.1,4.5,4.5,0.1,4.5,4.5), 
                   font.label = list(size = 28), 
                   ncol = 3,
                   nrow = 2)
Fig.4

ggsave(plot = Fig.4, filename = "Fig4_marg_plot_total_and_caregivers_only.tiff", width = 6.5, height = 6, units = "in", dpi = 300, scale = 1.8, compression = "lzw")


## Figure 5: Care Support ratio plot

# Combine all CSR into one tibble
CSR.df <- bind_rows(CSR.pos.male, CSR.neg.male, CSR.total.male, CSR.pos.female, CSR.neg.female, CSR.total.female, CSR.pos, CSR.neg, CSR.total) %>% 
  mutate(Ratio = factor(Ratio, levels = 1:3, labels = c("Positive", "Negative", "Total")),
         `Caregiver sex` = factor(Sex, levels = 1:3, labels = c("Male", "Female", "Total"))) %>% 
  filter(Year >= 2010, Ratio != "Total")


## Graph the production and consumption projection schedules as well as CSR
CSR.pos.plot <- ggplot(CSR.df %>% filter(Ratio == "Positive"), aes(x = Year, y = CSR, linetype = `Caregiver sex`)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("Male" = "dashed", "Female" = "dotted", "Total" = "solid")) +
  scale_y_continuous(breaks = seq(0, 1.1, 0.1), expand = c(0,0), limits = c(0, 0.9)) +
  scale_x_continuous(breaks = seq(2010, 2100, 10), expand = c(0,0)) +
  ylab("Care Support Ratio (CSR)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 16, vjust = 2.5),
    axis.ticks.y = element_line(color = "black"),
    axis.title.x = element_text(size = 16, vjust = -1.5),
    axis.text.x = element_text(vjust = 1, hjust = 1, angle = 45),
    axis.ticks.x = element_line(color = "black"),
    axis.text = element_text(size = 14),
    axis.line = element_line(size = 0.5), 
    plot.margin = margin(0.6,0.6,0.6,0.2, unit = "cm")
  )

CSR.pos.plot

CSR.neg.plot <- ggplot(CSR.df %>% filter(Ratio == "Negative"), aes(x = Year, y = CSR, linetype = `Caregiver sex`)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("Male" = "dashed", "Female" = "dotted", "Total" = "solid")) +
  scale_y_continuous(breaks = seq(0, 1.1, 0.1), expand = c(0,0), limits = c(0, 0.9)) +
  scale_x_continuous(breaks = seq(2010, 2100, 10), expand = c(0,0)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    axis.title.y = element_blank(), #element_text(size = 16, vjust = 1),
    axis.ticks.y = element_line(color = "black"),
    axis.title.x = element_text(size = 16, vjust = -1.5),
    axis.text.x = element_text(vjust = 1, hjust = 1, angle = 45),
    axis.ticks.x = element_line(color = "black"),
    axis.text = element_text(size = 14),
    axis.line = element_line(size = 0.5), 
    plot.margin = margin(0.6,0.6,0.6,0.6, unit = "cm")
  )

CSR.neg.plot


# Combine the Care Support Ratio plots into one and export
Fig.5 <- ggarrange(CSR.pos.plot, CSR.neg.plot, labels = c("a", "b"), font.label = list(size = 24), hjust = 0, common.legend = T, legend = "bottom")
Fig.5

ggsave(plot = Fig.5, filename = "Fig5_CSRplot_by_sex.tiff", width = 6, height = 4, units = "in", dpi = 300, scale = 1.6, compression = "lzw")


