rm(list=ls())


### Implementation of synthetic control with spillovers for Libya ###

# Packages

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tibble)

# Data for building the weigts (w and W). Comes from IMF

data_imf <- read_excel("weights for libya.xlsx")

treated <- "Libya"

control_countries <- setdiff(unique(c(data_imf$country1, data_imf$country2)), treated)

# Note that there are more countries in the data from IMF than in the final control group .
# Thereby, we will select less countries after.

# Building of vector w
# This vector contains the weight of every control country with Libya

w_lib <- data_imf %>%
  filter(country1 == treated | country2 == treated) %>%
  mutate(country = ifelse(country1 == treated, country2, country1)) %>%
  dplyr::select(country, poids)

# Now we select the right countries to be in the donor pool. We should have 23 countries.
# Note that we did not modify countries for w_lib yet, but we will do it after.
all_countries <- c(sort(control_countries))
all_countries <- setdiff(sort(control_countries), c("Burundi", "Iraq", "Mexico", "Ecuador", 
                                                    "United Arab Emirates", "Burkina Faso", 
                                                    "Botswana", "Central African Republic", "Congo, Rep.", 
                                                    "Ghana", "Iran, Islamic Rep.", "Kenya", "Madagascar", "Mali", "Mauritius", "Mauritania", "Niger", "Uganda", 
                                                    "Mauritania, Islamic Republic of", "Congo, Republic of", 
                                                    "Madagascar, Republic of" ))

# W is the matrix of weights for all control countries

W_mat <- matrix(0, nrow = length(all_countries), ncol = length(all_countries))
rownames(W_mat) <- colnames(W_mat) <- all_countries

for (i in 1:nrow(data_imf)) {
  from <- data_imf$country1[i]
  to <- data_imf$country2[i]
  weight <- data_imf$poids[i]
  if (from %in% all_countries & to %in% all_countries) {
    W_mat[from, to] <- weight
  }
}

w_vec <- numeric(length(all_countries))
names(w_vec) <- all_countries

for (i in 1:nrow(data_imf)) {
  from <- data_imf$country1[i]
  to <- data_imf$country2[i]
  weight <- data_imf$poids[i]
  if (from %in% all_countries & to %in% all_countries) {
    W_mat[from, to] <- weight
    W_mat[to, from] <- weight  
  }
}

W_mat <- as.data.frame(W_mat)



### Computation of GDP of Libya ###

data <- read_excel("fichier_nettoye2.xlsx")
summary(data)

# Now we can firstly have a look to some descriptive statistics. Here we do some violin plots for each covariate 
# and we represent Libya's GDP per capita.

resume <- data %>%
  filter(`Country Name` == "Libya") %>%
  mutate(period = case_when(
    Time >= 2000 & Time <= 2010 ~ "2000-2010",
    Time >= 2011 & Time <= 2023 ~ "2011-2023"
  )) %>%
  group_by(period) %>%
  summarise(
    across(
      .cols = where(is.numeric) & !Time, 
      .fns = list(mean = ~mean(.x, na.rm = TRUE),
                  sd   = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  )

# Mean over 2000-2011
libya_means <- tibble(
  Variable = c("GDP per capita",
               "Population growth",
               "Access to electricity",
               "External balance",
               "Exports of goods and services",
               "Trade",
               "Merchandise trade",
               "Asylum-seekers",
               "Birth rate",
               "Death rate",
               "Unemployment"),
  Libya_Mean = c(12061.16, 1.99, 90.27, 28.97, 57.86, 86.75, 57.46, 693.27, 23.11, 4.67, 19.02)
)

# Calculation of the mean per country over 2000-2011
donor_means <- data %>%
  filter(`Country Name` != "Libya", Time >= 2000, Time <= 2010) %>%
  group_by(`Country Name`) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

donor_long <- donor_means %>%
  pivot_longer(-`Country Name`, names_to = "Variable", values_to = "Moyenne") %>%
  filter(Variable != "Time")

donor_long$Variable <- str_replace_all(donor_long$Variable, "\\s*\\(.*?\\)", "")

# We rename some variables for lisibility
donor_long$Variable <- recode(donor_long$Variable,
                              `Access to electricity` = "Access to electricity",
                              `Asylum-seekers by country or territory of origin` = "Asylum-seekers",
                              `Birth rate, crude` = "Birth rate",
                              `Death rate, crude` = "Death rate",
                              `Unemployment, total` = "Unemployment",
                              `External balance on goods and services` = "External balance"
)

# Violin plots of the means of each covariate over 2000-2010.
# Note the position of the means of Libya over the period regarding the general distribution of means.
ggplot(donor_long, aes(x = "", y = Moyenne)) +
  geom_violin(fill = "skyblue", alpha = 0.6, color = "black") +
  geom_boxplot(width = 0.1, outlier.size = 0.5, alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free_y") +
  geom_hline(data = libya_means, 
             aes(yintercept = Libya_Mean, color = "Libya Mean 2000-2010"),  # juste changer ici
             linetype = "dashed", size = 1, show.legend = TRUE) +
  scale_color_manual(name = "", values = c("Libya Mean 2000-2010" = "orange")) +  # et ici
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold")) +
  labs(x = NULL, 
       y = "Distribution of means",
       title = "Distribution of country means for each variable in the donor pool in the period 2000-2010")  # et ici


# Also we can represent the gdp per capita for Libya over the entire pre-treatment period.
libya_gdp <- data %>%
  filter(`Country Name` == "Libya", Time >= 2000, Time <= 2010)

ggplot(libya_gdp, aes(x = Time, y = `GDP per capita (constant 2015 US$)`)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  scale_x_continuous(breaks = seq(2000, 2010, by = 1)) +   # années entières
  labs(
    title = "GDP per capita of Libya (2000–2010)",
    x = "Year",
    y = "GDP per capita (constant 2015 US$)"
  ) +
  theme_minimal()


### Synthetic control part

df_libya <- data %>%
  filter(`Country Name` == "Libya") %>%
  rename(
    year = Time,
    gdp_libya = `GDP per capita (constant 2015 US$)`
  ) %>%
  dplyr::select(year, gdp_libya)

Y0 <- df_libya  # Y0 (vector for treated)
print(round(Y0, 2))

# ---- Creation of Yc (control countries) ----

df_controls <- data %>%
  filter(!`Country Name` %in% c("Libya")) %>%
  rename(
    country = `Country Name`,
    year = Time,
    gdp_per_capita = `GDP per capita (constant 2015 US$)`
  ) %>%
  dplyr::select(country, year, gdp_per_capita) %>%
  filter(year %in% 2000:2010)

df_controls_all <- data %>%
  filter(!`Country Name` %in% c("Libya")) %>%
  rename(
    country = `Country Name`,
    year = Time,
    gdp_per_capita = `GDP per capita (constant 2015 US$)`
  ) %>%
  dplyr::select(country, year, gdp_per_capita)


df_controls$country <- as.character(df_controls$country)
df_controls$year <- as.integer(df_controls$year)


Yc <- df_controls %>%
  pivot_wider(names_from = country, values_from = gdp_per_capita) %>%
  column_to_rownames(var = "year")

Yc_all <- df_controls_all %>%
  pivot_wider(names_from = country, values_from = gdp_per_capita) %>%
  column_to_rownames(var = "year")


# Column names

# Here we need to make some modifications because the countries are not written the same way in the data from IMF and World bank website
colnames(W_mat) <- rownames(W_mat) <- recode(
  colnames(W_mat),
  "Azerbaijan, Republic of"         = "Azerbaijan",
  "Congo, Republic of"              = "Congo, Rep.",
  "Côte d'Ivoire"                   = "Cote d'Ivoire",
  "Egypt, Arab Republic of"         = "Egypt, Arab Rep.",
  "Iran, Islamic Republic of"       = "Iran, Islamic Rep.",
  "Kazakhstan, Republic of"         = "Kazakhstan",
  "Madagascar, Republic of"         = "Madagascar",
  "Mauritania, Islamic Republic of" = "Mauritania",
  "Tanzania, United Republic of"    = "Tanzania"
)
Yc <- Yc[, colnames(W_mat)]
Yc_all <- Yc_all[, colnames(W_mat)]


### Estimation of alpha coefficients ###


# 1. Prepare our data
Y0_pre <- Y0 %>% filter(year %in% 2000:2010) %>% pull(gdp_libya) %>% as.numeric()
Yc_pre <- Yc[as.character(2000:2010), ]
Yc_pre <- as.data.frame(Yc_pre)
Y0_pre_df <- data.frame(libya = Y0_pre)

control_names <- colnames(Yc_pre)


### Sampling function for alpha

estimate_alpha_full_bayes <- function(Y0_pre, Yc_pre, n_iter = 50000, burn_in = 20000, verbose = TRUE) {
  T0 <- length(Y0_pre)
  N <- ncol(Yc_pre)
  
  # Initialization of parameters
  alpha <- rep(1, N)
  lambda2 <- rep(1, N)
  nu_lambda <- 100
  tau2 <- 100
  nu_tau <- 100
  sigma2 <- 1
  nu_sigma <- 1
  
  
  alpha_chain <- matrix(0, nrow = n_iter - burn_in, ncol = N)
  colnames(alpha_chain) <- colnames(Yc_pre)
  
  Yc_pre_t <- t(Yc_pre)
  
  for (iter in 1:n_iter) {
    ### [1] Update of alpha | rest
    penalty_diag <- diag(1 / lambda2)
    A <- Yc_pre_t %*% Yc_pre + sigma2 * penalty_diag  # stabilisation
    b <- Yc_pre_t %*% Y0_pre
    A_inv <- tryCatch(solve(A), error = function(e) MASS::ginv(A))
    alpha_mean <- A_inv %*% b
    alpha_cov <- sigma2 * A_inv
    alpha <- as.numeric(MASS::mvrnorm(1, mu = alpha_mean, Sigma = alpha_cov))
    
    ### [2] lambda_i^2 | rest ~ IG(1, α_i² / 2 + 1 / ν_λ)
    for (i in 1:N) {
      rate <- max(alpha[i]^2 / 2 + 1 / nu_lambda, 1e-8)
      lambda2[i] <- 1 / rgamma(1, shape = 1, rate = rate)
    }
    
    ### [3] ν_λ | rest ~ IG(1, ∑ 1 / λ_i² + 1 / τ²)
    rate <- max(sum(1 / lambda2) + 1 / tau2, 1e-8)
    nu_lambda <- 1 / rgamma(1, shape = 1, rate = rate)
    
    ### [4] τ² | rest ~ IG(1, 1 / ν_λ + 1 / ν_τ)
    rate <- max(1 / nu_lambda + 1 / nu_tau, 1e-8)
    tau2 <- 1 / rgamma(1, shape = 1, rate = rate)
    
    ### [5] σ² | rest ~ IG(1 + T0/2, RSS/2 + α'Λ⁻¹α/2 + 1/ν_σ)
    residuals <- Y0_pre - Yc_pre %*% alpha
    rss <- sum(residuals^2)
    penalty <- sum((alpha^2) / lambda2)
    shape <- 1 + T0 / 2
    rate <- max(0.5 * rss + 0.5 * penalty + 1 / nu_sigma, 1e-8)
    sigma2 <- 1 / rgamma(1, shape = shape, rate = rate)
    
    ### [6] ν_σ | rest ~ IG(1, 1 / σ² + 1 / 100)
    rate <- max(1 / sigma2 + 1 / 100, 1e-8)
    nu_sigma <- 1 / rgamma(1, shape = 1, rate = rate)
    
    # Stockage
    if (iter > burn_in) {
      alpha_chain[iter - burn_in, ] <- alpha
    }
    
    if (verbose && iter %% 500 == 0) {
      cat("Iteration", iter, "\n")
    }
  }
  
  alpha_mean <- colMeans(alpha_chain)
  return(list(alpha_samples = alpha_chain, alpha_mean = alpha_mean))
}

### Visualization for the alpha chains

res_alpha <- estimate_alpha_full_bayes(Y0_pre, as.matrix(Yc_pre), n_iter = 50000) # vector of alpha weigths for control countries
alpha <- matrix(res_alpha$alpha_mean, ncol = 1)

alpha_chain_df <- as.data.frame(res_alpha$alpha_samples)
alpha_chain_df$iteration <- 1:nrow(alpha_chain_df)

alpha_long <- alpha_chain_df %>%
  pivot_longer(-iteration, names_to = "alpha_index", values_to = "value")


Y0_pre <- Y0 %>% filter(year %in% 2000:2010) %>% pull(gdp_libya) %>% as.numeric()
Yc_pre <- Yc[as.character(2000:2010), ]
Yc_pre <- as.data.frame(Yc_pre)
Y0_pre_df <- data.frame(libya = Y0_pre)
Yc_pre <- apply(Yc_pre, 2, as.numeric)
Y0_synth <- Yc_pre %*% alpha


# We can check that the estimated weights for alpha lead to a good estimation of Libya's GDP
plot(2000:2010, Y0_pre, type = "l", col = "black", lwd = 2, ylim = range(c(Y0_pre, Y0_synth)),
     ylab = "GDP per capita", xlab = "Year", main = "GDP Libya vs Synthetic")
lines(2000:2010, Y0_synth, col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Libya", "Synthetic"), col = c("black", "blue"), lty = c(1,2))

# The estimation seems to be good. We can proceed.

### Results


### Treatment effects ###

covariate_names <- c(
  "Population growth (annual %)",
  "Asylum-seekers by country or territory of origin",
  "Access to electricity (% of population)",
  "External balance on goods and services (% of GDP)",
  "Exports of goods and services (% of GDP)",
  "Trade (% of GDP)",
  "Merchandise trade (% of GDP)",
  "Birth rate, crude (per 1,000 people)",
  "Death rate, crude (per 1,000 people)",
  "Unemployment, total (% of total labor force) (modeled ILO estimate)"
)

rename_map <- c(
  "Côte d'Ivoire" = "Cote d'Ivoire",
  "Egypt, Arab Republic of" = "Egypt, Arab Rep.",
  "Iran, Islamic Republic of" = "Iran, Islamic Rep.",
  "Azerbaijan, Republic of" = "Azerbaijan",
  "Congo, Republic of" = "Congo, Rep.",
  "Kazakhstan, Republic of" = "Kazakhstan",
  "Madagascar, Republic of" = "Madagascar",
  "Mauritania, Islamic Republic of" = "Mauritania",
  "Tanzania, United Republic of" = "Tanzania"
)

data <- data %>%
  mutate(`Country Name` = recode(`Country Name`, !!!rename_map))

years <- 2000:2023

T_all <- length(years)
N <- length(control_names)
K <- length(covariate_names)

data_filtered <- data %>%
  filter(`Country Name` %in% control_names, Time %in% years) %>%
  dplyr::select(country = `Country Name`, year = Time, all_of(covariate_names))


### We have some NA's. We can replace them with 

data_filtered <- data_filtered %>%
  mutate(
    `External balance on goods and services (% of GDP)` = ifelse(
      country == "United Arab Emirates" & year == 2000,
      82,
      `External balance on goods and services (% of GDP)`
    ),
    `Trade (% of GDP)` = ifelse(
      country == "United Arab Emirates" & year == 2000,
      140,
      `Trade (% of GDP)`
    ),
    `Exports of goods and services (% of GDP)` = ifelse(
      country == "United Arab Emirates" & year == 2000,
      49,
      `Exports of goods and services (% of GDP)`
    )
  )

data_filtered <- data_filtered %>%
  mutate(
    `Inflation, consumer prices (annual %)` = case_when(
      country == "Oman" & year == 2000 ~ -0.8,
      country == "United Arab Emirates" & year >= 2000 & year <= 2007 ~ 3,
      TRUE ~ `Inflation, consumer prices (annual %)`
    )
  )


data_filtered <- data_filtered %>%
  mutate(
    `External balance on goods and services (% of GDP)` = case_when(
      country == "Qatar" & year == 2023 ~ 37,
      country == "Oman" & year == 2023 ~ 18,
      TRUE ~ `External balance on goods and services (% of GDP)`
    ),
    `Exports of goods and services (% of GDP)` = case_when(
      country == "Qatar" & year == 2023 ~ 70,
      country == "Oman" & year == 2023 ~ 60,
      TRUE ~ `Exports of goods and services (% of GDP)`
    ),
    `Trade (% of GDP)` = case_when(
      country == "Qatar" & year == 2023 ~ 103,
      country == "Oman" & year == 2023 ~ 102,
      TRUE ~ `Trade (% of GDP)`
    )
  )


colSums(is.na(data_filtered)) # We have no NA anymore


# Get the same order for countries
data_filtered$country <- factor(data_filtered$country, levels = colnames(Yc))
data_filtered <- data_filtered %>% arrange(country, year)


# Initialize X_array
X_array <- array(NA, dim = c(T_all, N, K),
                 dimnames = list(as.character(years), control_names, covariate_names))

for (k in seq_along(covariate_names)) {
  cov_k <- data_filtered %>%
    dplyr::select(country, year, value = all_of(covariate_names[k])) %>%
    pivot_wider(names_from = country, values_from = value) %>%
    column_to_rownames("year")
  
  # Sécurité : ordre exact des années et pays
  cov_k <- cov_k[as.character(years), control_names, drop = FALSE]
  
  # Ajout dans X_array : attention au transpose
  X_array[, , k] <- t(as.matrix(cov_k))
}
# Trimatrix T_all x N x K

# --- Data --- 
Y0_vec <- as.vector(Y0_pre_df$libya) # Vector on all the period
Y0_all <- Y0$gdp_libya
Yc_mat <- as.matrix(Yc_pre)
Yc_mat_all <- as.matrix(Yc_all)
storage.mode(Yc_mat) <- "numeric"
W <- W_mat # Weight matrix 23 x 23
W <- as.matrix(W)
w <- as.vector(w_lib[-c(4, 5, 7, 9, 11, 14, 19, 20, 21, 22, 23, 24, 26, 36,37),]$poids)  # Vector of weights 1 x 23. As we did not change the control contries from previously we do it here.
# The positions removed here are the ones for the countries that we do not keep in the donor pool.
years_control <- 2000 : 2010
T <- length(years_control)


# Function of estimation for rho
estimate_rho_bayes_full <- function(n_iter = 20000, burn_in = 3000, k_rho, verbose = TRUE) {
  library(MASS)
  
  rho_accept <- 0  # acceptations for rho
  
  W <- as.matrix(W)
  storage.mode(W) <- "numeric"
  N <- ncol(Yc_mat)
  T <- nrow(Yc_mat)
  T_all <- nrow(Yc_mat_all)
  
  # --- Initialization of parameters ---
  
  p <- 2
  
  omega2 <- rep(1, p)
  nu_omega <- rep(1, p)
  
  eta_mat <- matrix(0, nrow = N, ncol = p) # N x p
  gamma_mat <- matrix(rnorm(T_all * p), nrow = T_all, ncol = p) # T0 x p (we pick one element one line to do the product with eta_mat)
  beta <- rep(1, K) # 1 x K
  lambda2 <- rep(1, K) # 1 x K
  nu_lambda <- 1
  tau2 <- 1
  nu_tau <- 1
  eta <- rep(0, N)
  phi <- 1
  sigma_gamma2 <- 1
  nu_gamma <- 1
  sigma_eta2 <- 1
  sigma2 <- 1
  nu_sigma <- 1
  rho <- 1
  
  rho_chain <- numeric(n_iter - burn_in)
  
  for (iter in 1:n_iter) {
    
    A <- diag(N) - rho * W
    
    # --- [1] Update beta | rest --- VERIFIED (Watch out the u_t is multiplied by X_t)
    XtX <- matrix(0, K, K)
    Xty <- rep(0, K)
    for (t in 1:T_all) {
      Yct <- matrix(as.numeric(Yc_mat_all[t, ]), ncol = 1)
      X_t <- X_array[t, , ]
      
      gamma_t <- gamma_mat[t, ]
      gamma_t_vec <- matrix(gamma_mat[t, ], ncol = 1)
      u_t <- A %*% Yct - rho * w * Y0_all[t] - X_t %*% beta - eta_mat %*% gamma_t_vec
      
      XtX <- XtX + t(X_t) %*% X_t  # K x K
      Xty <- Xty + t(X_t) %*% u_t  # K x 1
    }
    
    A_beta <- XtX + sigma2 * diag(1 / lambda2) # K x K
    A_beta_inv <- tryCatch(chol2inv(chol(A_beta)), error = function(e) MASS::ginv(A_beta))
    mu_beta <- A_beta_inv %*% Xty
    beta <- as.numeric(mvrnorm(1, mu = mu_beta, Sigma = sigma2 * A_beta_inv))
    
    # --- [2] lambda_j^2 --- VERIFIED (Let's consider nu_lambda0 is nu_lambda)
    for (j in 1:K) {
      lambda2[j] <- 1 / rgamma(1, shape = 1, rate = beta[j]^2 / 2 + 1 / nu_lambda)
    }
    
    # --- [3] nu_lambda --- VERIFIED
    nu_lambda <- 1 / rgamma(1, shape = 1, rate = sum(1 / lambda2) + 1 / tau2)
    
    # --- [4] tau^2 --- VERIFIED
    tau2 <- 1 / rgamma(1, shape = 1, rate = 1 / nu_lambda + 1 / nu_tau)
    
    # --- [5] nu_tau --- VERIFIED
    nu_tau <- 1 / rgamma(1, shape = 1, rate = 1 / tau2 + 1 / sigma2)
    
    # --- [6] phi_gamma --- VERIFIED
    
    numerateur <- sum(rowSums(gamma_mat[2:T, ] * gamma_mat[1:(T - 1), ]))
    denominateur <- sum(rowSums(gamma_mat[1:(T - 1), ]^2))
    
    phi <- rnorm(1,
                 mean = numerateur / denominateur,
                 sd = sqrt(sigma_gamma2 / denominateur))
    # As we take the transpose we still have a scalar
    
    # --- [7] sigma_gamma2 --- VERIFIED
    e <- gamma_mat[2:T,] - phi * gamma_mat[1:(T- 1),] # Basically gamma_vec[1:T] is gamma_mat[1:(T_all - 1),]
    sigma_gamma2 <- 1 / rgamma(1, shape =  0.5 * (T - 1) * p + 1, rate = 0.5 * sum(e^2) + 1 / nu_gamma) # Here it is T0
    
    
    # --- [8] nu_gamma --- VERIFIED
    nu_gamma <- 1 / rgamma(1, shape = 1, rate = 1 / sigma_gamma2 + 1 / 100)
    
    
    
    # --- [9] gamma_t --- VERIFIED
    v_t <- matrix(rnorm(T_all * p, mean = 0, sd = sqrt(sigma_gamma2)), nrow = T_all, ncol = p)
    # vt ~ N(0p, sigma_gamma2) each column of v_t correspond to one generation of normal law for p vector
    
    
    gamma_mat[1,] <- v_t[1,]  # gamma_1 ~ N(0, sigma_gamma2)
    
    for (t in 2:T_all) {
      gamma_mat[t, ] <- phi * gamma_mat[t - 1, ] + v_t[t, ]
      
    }
    
    # --- [10] eta_i --- VERIFIED
    Sigma_eta_inv <- diag(1 / omega2)  # p x p
    C_eta <- matrix(0, nrow = p, ncol = p) # p x p # Note that the transpose is not in the same disposition than before so here we have a matrix
    
    # Part 1 : sum of γ_t γ_t^T
    for (t in 1:T) { # Here we keep T because it is T0
      gamma_t_vec <- gamma_mat[t,]  # p x 1
      C_eta <- C_eta + gamma_t_vec %*% t(gamma_t_vec)
    }
    # Part 2 : + sigma2 * sigma_eta2^{-1} * Σ_η^{-1}
    C_eta <- C_eta + sigma2 * sigma_eta2^(-1) * Sigma_eta_inv
    # Inversion de C_eta
    C_eta_inv <- tryCatch(solve(C_eta), error = function(e) MASS::ginv(C_eta))
    
    # Update of each eta_i (vector p x 1)
    for (i in 1:N) {
      sum_matrix <- matrix(0, nrow = p, ncol = 1) # p x 1
      
      for (t in 1:T) {
        Yct_i <- as.numeric(Yc_mat[t, i])              # Y^c_{it}, scalar
        X_t <- X_array[t, , ]               # N x K
        X_it_beta <- X_t[i, ] %*% beta      # scalar
        
        # u_it complete according to papier
        u_it <- rho * w[i] * Y0_vec[t] + # 1 x 1
          rho * sum(W[i, ] * Yc_mat[t, ]) +
          X_it_beta
        
        gamma_t_vec <- matrix(gamma_mat[t, ], ncol = 1)
        scalar_resid <- matrix(Yct_i - u_it, nrow = 1, ncol = 1)
        sum_matrix <- sum_matrix + gamma_t_vec %*% scalar_resid  # (2x1) %*% (1x1) = (2x1)
        
      }
      
      mu_eta <- C_eta_inv %*% sum_matrix # p x 1
      eta_mat[i, ] <- mvrnorm(1, mu = as.vector(mu_eta), Sigma = sigma2 * C_eta_inv)
    }
    
    # --- [11] sigma_eta2 --- VERIFIED
    shape_sigma_eta <- 0.5 + (p * N) / 2
    
    rate_sigma_eta <- 1 / nu_sigma + 0.5 * sum(sapply(1:N, function(i) {
      eta_i <- matrix(eta_mat[i, ], ncol = 1)
      as.numeric(t(eta_i) %*% Sigma_eta_inv %*% eta_i)
    }))
    
    sigma_eta2 <- 1 / rgamma(1, shape = shape_sigma_eta, rate = rate_sigma_eta)
    
    
    # --- [13] omega_j^2 --- VERIFIED
    for (j in 1:p) {
      shape <- 0.5 + N / 2
      rate <- 0.5 * sum(eta_mat[, j]^2) / sigma_eta2 + 1 / nu_omega[j]
      omega2[j] <- 1 / rgamma(1, shape = shape, rate = rate)
    }
    
    # --- [14] nu_omega_j --- VERIFIED
    for (j in 1:p) {
      nu_omega[j] <- 1 / rgamma(1, shape = 1, rate = 1 / omega2[j] + 1 / 100)
    }
    
    # --- [15] sigma2 --- VERIFIED
    total_u_sq <- 0
    for (t in 1:T) {
      Yct <- matrix(as.numeric(Yc_mat[t, ]), ncol = 1)
      X_t <- X_array[t, , ]
      u_t <- Yct - rho * w * Y0_vec[t] - rho * W %*% Yct - X_t %*% beta - eta_mat %*% matrix(gamma_mat[t,], ncol = 1)
      
      total_u_sq <- total_u_sq + sum(u_t^2)
    }
    
    shape_sigma2 <- 1 + (N * T) / 2
    rate_sigma2 <- 1 / nu_lambda + 1 / nu_sigma + total_u_sq
    sigma2 <- 1 / rgamma(1, shape = shape_sigma2, rate = rate_sigma2)
    
    # --- [16] nu_sigma --- VERIFIED
    nu_sigma <- 1 / rgamma(1, shape = 1, rate = 1 / sigma2 + 1 / 100)
    
    # --- [17] rho
    # tuning parameter (as suggested, to aim for 40–60% acceptance rate)
    
    u <- rnorm(1, mean = 0,sd = 1)
    rho_prop <- rho + k_rho * u
    if (abs(rho_prop) < 0.99) { # Basically if rho is between -1 and 1
      A_curr <- diag(N) - rho * W
      A_prop <- diag(N) - rho_prop * W
      
      det_curr <- tryCatch(abs(det(A_curr)), error = function(e) NA_real_)
      det_prop <- tryCatch(abs(det(A_prop)), error = function(e) NA_real_)
      
      if (!is.na(det_curr) && !is.na(det_prop)) {
        res_curr <- 0
        res_prop <- 0
        
        for (t in 1:T) {
          Yct <- matrix(as.numeric(Yc_mat[t, ]), ncol = 1)
          X_t <- X_array[t, , ]
          
          u_curr <- A_curr %*% Yct - rho * w * Y0_vec[t] - X_t %*% beta - eta_mat %*% matrix(gamma_mat[t,], ncol = 1)
          
          u_prop <- A_prop %*% Yct - rho_prop * w * Y0_vec[t] - X_t %*% beta - eta_mat %*% matrix(gamma_mat[t,], ncol = 1)
          
          
          res_curr <- res_curr + sum(u_curr^2)
          res_prop <- res_prop + sum(u_prop^2)
        }
        
        # Apply |A|^T in the acceptance ratio
        num <- (det_prop^T) * exp(-0.5 / sigma2 * res_prop)
        den <- (det_curr^T) * exp(-0.5 / sigma2 * res_curr)
        
        acceptance_prob <- min(1, num / den)
        
        if (runif(1) < acceptance_prob) {
          rho <- rho_prop
          rho_accept <- rho_accept + 1
        }
      }
    }
    
    # --- Stockage ---
    if (iter > burn_in) {
      rho_chain[iter - burn_in] <- rho
    }
    
    if (verbose && iter %% 500 == 0) {
      cat("Iteration", iter, "- rho:", round(rho, 4),
          "- acceptation rate:", round(rho_accept / iter, 3), "\n")
    }
    
    # --- k_rho adjustment ---
    if (iter > 250 && rho_accept > 0) { # We want to keep an acceptation rate between 0.4 and 0.6
      accept_rate <- rho_accept / iter
      if (accept_rate < 0.4) {
        k_rho <- k_rho / 1.1
      }
      if (accept_rate > 0.6) {
        k_rho <- k_rho * 1.1
      }
    }
  }  # end of for loop
  
  cat("rho acceptation rate :", round(rho_accept / n_iter, 3), "\n")
  return(list(rho_chain = rho_chain, rho_estimate = mean(rho_chain)))
}

res_rho <- estimate_rho_bayes_full(n_iter = 6000, burn_in = 1000, k_rho = 0.2) # Do at least 5000 iterations
rho_hat <- res_rho$rho_estimate

rho_cummean <- cumsum(res_rho$rho_chain) / seq_along(res_rho$rho_chain)

plot(rho_cummean, type = "l", main = "Markov chain for ρ", 
     ylab = expression(paste("Value ", rho)), xlab = "Iteration")
abline(h = res_rho$rho_estimate, col = "red", lty = 2) # The chain converged


rho <- rho_hat
I_N <- diag(length(w))
W <- as.matrix(W_mat[colnames(Yc), colnames(Yc)])

w <- matrix(w, ncol = 1)
# Calculation of inverse matrix A_inv
A_inv <- solve(I_N - rho * w %*% t(alpha) - rho * W)

# Initialization of treatment effects
xi_vec <- numeric(nrow(Yc_all))
years <- as.numeric(rownames(Yc_all))

for (i in 1:nrow(Yc_all)) {
  Y0t <- Y0$gdp_libya[Y0$year == years[i]]
  Yct <- as.numeric(Yc_all[i, ])
  Yct <- matrix(Yct, ncol = 1)
  
  correction_term <- (I_N - rho * W) %*% Yct - rho * w * Y0t
  synthetic_effect <- t(alpha) %*% A_inv %*% correction_term
  xi <- Y0t - synthetic_effect
  xi_vec[i] <- xi
}

# Results
effects_df <- data.frame(
  year = years,
  treatment_effect = as.numeric(xi_vec)
)


effects_df_post <- subset(effects_df, year > 2010)
print(effects_df_post) # We can get a look at the estimated treatment effects after 2010.

### Spillover effects ###


spillover_mat <- matrix(NA, nrow = nrow(Yc_all), ncol = ncol(Yc_all))
colnames(spillover_mat) <- colnames(Yc_all)
years <- as.numeric(rownames(Yc_all))

# Loop over time
for (i in 1:nrow(Yc_all)) {
  Yct <- as.numeric(Yc_all[i, ])
  Yct <- matrix(Yct, ncol = 1)
  Y0t <- Y0$gdp_libya[Y0$year == years[i]]
  
  correction_term <- (I_N - rho * W) %*% Yct - rho * w * Y0t
  Yct0 <- A_inv %*% correction_term
  spillover_effects <- Yct - Yct0
  
  spillover_mat[i, ] <- spillover_effects
}

spillover_df <- data.frame(
  year = rep(years, times = ncol(Yc_all)),  
  country = rep(colnames(Yc_all), each = nrow(Yc_all)), 
  spillover_effect = as.vector(spillover_mat)
)

# Filter only the post-treatment period
spillover_post <- subset(spillover_df, year > 2010)

# Spillovers for all countries
ggplot(spillover_post, aes(x = year, y = spillover_effect)) +
  geom_line(color = "steelblue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ country, scales = "free_y") +
  labs(
    title = "Estimated spillover effects on GDP by country",
    x = "Year",
    y = "Effect on GDP"
  )

# We conclude that there are spillovers, especially for Tunisia and Egypt.
