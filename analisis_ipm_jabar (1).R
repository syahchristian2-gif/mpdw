# =====================================================================
# TUGAS: Regresi IPM Jawa Barat 2010-2021 (X = Tahun, Y = IPM)
# Cek autokorelasi -> jika ada, tangani dengan Cochran-Orcutt & Hildreth-Lu
# =====================================================================

library(lmtest)

# -------------------------
# 1. INPUT DATA
# -------------------------
tahun <- 2010:2021
ipm   <- c(66.15, 66.67, 67.32, 68.25, 68.80, 69.50,
           70.05, 70.69, 71.30, 72.03, 72.09, 72.45)

data_jabar <- data.frame(Tahun = tahun, IPM = ipm)
print(data_jabar)

# -------------------------
# 2. REGRESI LINEAR SEDERHANA (OLS)
# -------------------------
model_ols <- lm(IPM ~ Tahun, data = data_jabar)
cat("\n=================== MODEL OLS AWAL ===================\n")
print(summary(model_ols))

# -------------------------
# 3. UJI AUTOKORELASI (Durbin-Watson)
# -------------------------
cat("\n=================== UJI DURBIN-WATSON ===================\n")
dw_result <- dwtest(model_ols)
print(dw_result)

residual_ols <- residuals(model_ols)
cat("\nResidual model OLS:\n")
print(round(residual_ols, 4))

# -------------------------
# 4. PENANGANAN AUTOKORELASI DENGAN COCHRAN-ORCUTT (MANUAL)
# -------------------------
cochran_orcutt <- function(x, y, max_iter = 50, tol = 1e-6) {
  n <- length(y)
  rho_old <- 0
  for (i in 1:max_iter) {
    y_t <- y[2:n] - rho_old * y[1:(n-1)]
    x_t <- x[2:n] - rho_old * x[1:(n-1)]
    model_t <- lm(y_t ~ x_t)

    b0 <- coef(model_t)[1] / (1 - rho_old)
    b1 <- coef(model_t)[2]
    resid_asli <- y - (b0 + b1 * x)

    rho_new <- sum(resid_asli[2:n] * resid_asli[1:(n-1)]) / sum(resid_asli[1:(n-1)]^2)

    if (abs(rho_new - rho_old) < tol) break
    rho_old <- rho_new
  }
  list(rho = rho_old, model_transformasi = model_t, b0 = b0, b1 = b1, iterasi = i)
}

hasil_co <- cochran_orcutt(data_jabar$Tahun, data_jabar$IPM)

cat("\n=================== COCHRAN-ORCUTT ===================\n")
cat("Rho (estimasi autokorelasi) :", round(hasil_co$rho, 5), "\n")
cat("Jumlah iterasi              :", hasil_co$iterasi, "\n")
cat("Model transformasi (y* ~ x*):\n")
print(summary(hasil_co$model_transformasi))
cat("\nPersamaan regresi setelah koreksi Cochran-Orcutt:\n")
cat(sprintf("IPM_hat = %.5f + %.5f * Tahun\n", hasil_co$b0, hasil_co$b1))

cat("\nUji Durbin-Watson pada model hasil transformasi Cochran-Orcutt:\n")
print(dwtest(hasil_co$model_transformasi))

# -------------------------
# 5. PENANGANAN AUTOKORELASI DENGAN HILDRETH-LU (GRID SEARCH)
# -------------------------
hildreth_lu <- function(x, y, rho_seq = seq(-0.99, 0.99, by = 0.01)) {
  n <- length(y)
  ssr_list <- numeric(length(rho_seq))

  for (i in seq_along(rho_seq)) {
    rho <- rho_seq[i]
    y_t <- y[2:n] - rho * y[1:(n-1)]
    x_t <- x[2:n] - rho * x[1:(n-1)]
    model_t <- lm(y_t ~ x_t)
    ssr_list[i] <- sum(residuals(model_t)^2)
  }

  idx_min <- which.min(ssr_list)
  rho_optimal <- rho_seq[idx_min]

  y_t <- y[2:n] - rho_optimal * y[1:(n-1)]
  x_t <- x[2:n] - rho_optimal * x[1:(n-1)]
  model_final <- lm(y_t ~ x_t)

  b0 <- coef(model_final)[1] / (1 - rho_optimal)
  b1 <- coef(model_final)[2]

  list(rho = rho_optimal, ssr = ssr_list, rho_seq = rho_seq,
       model_transformasi = model_final, b0 = b0, b1 = b1)
}

hasil_hl <- hildreth_lu(data_jabar$Tahun, data_jabar$IPM)

cat("\n=================== HILDRETH-LU ===================\n")
cat("Rho optimal (SSR minimum) :", hasil_hl$rho, "\n")
cat("Model transformasi (y* ~ x*):\n")
print(summary(hasil_hl$model_transformasi))
cat("\nPersamaan regresi setelah koreksi Hildreth-Lu:\n")
cat(sprintf("IPM_hat = %.5f + %.5f * Tahun\n", hasil_hl$b0, hasil_hl$b1))

cat("\nUji Durbin-Watson pada model hasil transformasi Hildreth-Lu:\n")
print(dwtest(hasil_hl$model_transformasi))

# -------------------------
# 6. RINGKASAN PERBANDINGAN
# -------------------------
cat("\n=================== RINGKASAN PERBANDINGAN ===================\n")
cat(sprintf("Model OLS awal        : IPM_hat = %.5f + %.5f*Tahun | DW = %.4f\n",
            coef(model_ols)[1], coef(model_ols)[2], dw_result$statistic))
cat(sprintf("Model Cochran-Orcutt  : IPM_hat = %.5f + %.5f*Tahun | rho = %.5f\n",
            hasil_co$b0, hasil_co$b1, hasil_co$rho))
cat(sprintf("Model Hildreth-Lu     : IPM_hat = %.5f + %.5f*Tahun | rho = %.5f\n",
            hasil_hl$b0, hasil_hl$b1, hasil_hl$rho))

# -------------------------
# 7. SIMPAN PLOT (opsional, untuk lampiran laporan)
# -------------------------
png("plot_ipm_jabar.png", width = 900, height = 600)
plot(data_jabar$Tahun, data_jabar$IPM, pch = 19, col = "blue",
     xlab = "Tahun", ylab = "IPM", main = "IPM Jawa Barat 2010-2021 & Garis Regresi OLS")
abline(model_ols, col = "red", lwd = 2)
legend("topleft", legend = c("Data Aktual", "Garis Regresi OLS"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1))
dev.off()

png("plot_residual.png", width = 900, height = 600)
plot(data_jabar$Tahun, residual_ols, type = "b", pch = 19, col = "darkgreen",
     xlab = "Tahun", ylab = "Residual", main = "Plot Residual Model OLS")
abline(h = 0, col = "red", lty = 2)
dev.off()

cat("\nSelesai. Plot disimpan sebagai plot_ipm_jabar.png dan plot_residual.png\n")
