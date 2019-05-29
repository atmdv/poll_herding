# DK re-do of herding analysis
# http://freerangestats.info/blog/2019/05/15/polls-misc

options(repos = getOption("repos")["CRAN"])
devtools::install_github("mikkelkrogsholm/pollsDK")
pacman::p_load(pollsDK, tidyverse, lubridate)

data <- get_polls()

data %>% 
  filter(letter=="V") %>%
  filter(month(datetime)>=5) %>% 
  ggplot(aes(x = datetime, y = percent)) +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_smooth(se = FALSE, method = "gam") +
  geom_point(aes(colour = pollster)) +
  #facet_wrap(~election_year, scales = "free_x", nrow = 1) +
  ggtitle("2019 polls overestimated the ALP result somewhat more than usual",
          "Showing polls in the 28 days up to the election. 2019 result is provisional, at 16 hours.") +
  labs(x = "Polling date",
       y = "Venstres valgresultat, pct.",
       colour = "",
       caption = "freerangestats.info")

data_stats <- data %>% 
  filter(letter=="V") %>%
  filter(month(datetime)>=5) %>%
  summarise(n_distinct(pollster, datetime))

# underdispersion test
set.seed(123)

d <- data %>%
  filter(letter=="V") %>%
  filter(month(datetime)>=5)

n <- nrow(d)
reps <- 100000

sims <- matrix(round(rbinom(n * reps, 1000, mean(d$percent) / 100), 3) /10,
               ncol = n, nrow = reps)

d4 <- tibble(standard_deviations = apply(sims, 1, sd)) 

d4 %>%
  ggplot(aes(x = standard_deviations)) +
  geom_density(fill = "grey", alpha = 0.5) +
  geom_vline(xintercept = sd(d$percent), colour = "red") +
  annotate("text", x = 0.45, y = 0.5, hjust = 0, label = paste0(
    "Only ", round(mean(d4 < sd(d$percent)), 3),
    " of simulated poll\nsequences have a standard\ndeviation less than has been\nobserved in the ",
    n, " polls in 2019."
  )) +
  annotate("text", x = 1, y = 1.5, colour = "red", label = "Observed", hjust = 1) +
  labs(x = paste0("Standard deviations of simulated sequences of ", 
                  n, 
                  " surveys with 1,000 respondents, with survey results rounded")) +
  ggtitle("The polls published in 2019 vary somewhat less than they should if random",
          "The amount of variance is surprisingly low, but not impossibly so.")


