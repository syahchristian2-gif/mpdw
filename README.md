# Pemodelan ARIMA Harga Saham NSE (Praktikum MPDW)

Analisis deret waktu ARIMA untuk harga penutupan harian saham NSE (2.444 amatan, 4 Agustus 2014 - 5 Juli 2024).

## Tahapan
1. Pembagian data latih (2.200 amatan) dan data uji (244 amatan)
2. Uji kestasioneran data latih: ACF, uji ADF (rataan), Box-Cox (ragam; selang kepercayaan memuat 1 atau tidak)
3. Penanganan: transformasi log dan differencing orde 1
4. Identifikasi model potensial: ACF, PACF, EACF
5. Pendugaan parameter, uji signifikansi, dan AIC
6. Analisis sisaan
7. Uji overfitting
8. Peramalan dan akurasi

## Isi repositori
| File | Keterangan |
|---|---|
| `ARIMA_Saham_NSE.Rmd` | Kode R Markdown (sumber laporan) |
| `ARIMA_Saham_NSE.html` / `index.html` | Laporan hasil render (`index.html` untuk GitHub Pages) |
| `NSE_Stock_Historical_price_data_dian.xlsx` | Data mentah |

## Cara menjalankan ulang
```r
install.packages(c("readxl", "forecast", "tseries", "lmtest", "MASS", "rmarkdown", "knitr"))
# opsional: install.packages("TSA")  # untuk fungsi eacf() asli; jika tidak ada dipakai fungsi manual
rmarkdown::render("ARIMA_Saham_NSE.Rmd")
```
Ubah `kolom_saham` dan `n_test` pada bagian *Data* untuk memodelkan saham lain atau mengganti ukuran data uji.
