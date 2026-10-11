# =====================================================
# TUGAS 1 - DATA PREPROCESSING (RStudio)
# Dataset: dataset_tugas1_preprocessing.csv
# =====================================================

# -----------------------------------------------------
# TAHAP 1: LOAD DATASET
# -----------------------------------------------------
df <- read.csv("dataset_tugas1_preprocessing.csv",
               stringsAsFactors = FALSE,
               na.strings = c("", "NA"))

head(df, 10)

# -----------------------------------------------------
# TAHAP 2: EKSPLORASI AWAL (EDA)
# -----------------------------------------------------
# 2.1 Struktur dan tipe data
cat("Jumlah baris:", nrow(df), "| Jumlah kolom:", ncol(df), "\n")
str(df)

# 2.2 Jumlah nilai hilang per kolom
missing <- data.frame(
  Jumlah_Hilang  = colSums(is.na(df)),
  Persentase     = round(colMeans(is.na(df)) * 100, 2)
)
print(missing)

# 2.3 Statistik deskriptif
summary(df)

# 2.4 Pemeriksaan tambahan: format tanggal dan sebaran kategori
format_tgl <- ifelse(grepl("/", df$Tanggal_Ujian), "yyyy/mm/dd", "dd-mm-yyyy")
print(table(format_tgl))
print(table(df$Jenis_Kelamin, useNA = "ifany"))
print(table(df$Prodi,         useNA = "ifany"))
print(table(df$Status,        useNA = "ifany"))
print(table(df$Nilai_Akhir,   useNA = "ifany"))

# -----------------------------------------------------
# TAHAP 3: MENANGANI MISSING VALUES
# -----------------------------------------------------
# Umur (numerik)        -> imputasi MEDIAN
# Nilai_Akhir (kategori)-> imputasi MODUS

median_umur <- median(df$Umur, na.rm = TRUE)
modus_nilai <- names(which.max(table(df$Nilai_Akhir)))   # modus = nilai terbanyak

cat("Median Umur :", median_umur, "\n")
cat("Modus Nilai :", modus_nilai, "\n")

df$Umur[is.na(df$Umur)]               <- median_umur
df$Nilai_Akhir[is.na(df$Nilai_Akhir)] <- modus_nilai
df$Umur <- as.integer(df$Umur)

cat("\nSisa missing value per kolom:\n")
print(colSums(is.na(df)))

# -----------------------------------------------------
# TAHAP 4: NORMALISASI FORMAT TANGGAL
# -----------------------------------------------------
# Ada dua format: "yyyy/mm/dd" dan "dd-mm-yyyy".
# Dikonversi terpisah dengan format eksplisit agar hari/bulan tidak tertukar.
tgl_asli  <- df$Tanggal_Ujian
tgl_baru  <- as.Date(rep(NA, length(tgl_asli)))

idx_slash <- grepl("/", tgl_asli)
tgl_baru[idx_slash]  <- as.Date(tgl_asli[idx_slash],  format = "%Y/%m/%d")
tgl_baru[!idx_slash] <- as.Date(tgl_asli[!idx_slash], format = "%d-%m-%Y")

df$Tanggal_Ujian <- tgl_baru

class(df$Tanggal_Ujian)               
sum(is.na(df$Tanggal_Ujian))          
head(df[, c("ID", "Tanggal_Ujian")], 12)

# -----------------------------------------------------
# TAHAP 5: ENCODING LABEL (LABEL ENCODING)
# -----------------------------------------------------
# Kolom: Nama, Jenis_Kelamin, Prodi, Status, Nilai_Akhir
# Data asli disalin dulu supaya label teks tetap tersedia untuk visualisasi.
df_encoded <- df
kolom_kategorikal <- c("Nama", "Jenis_Kelamin", "Prodi", "Status", "Nilai_Akhir")

for (kol in kolom_kategorikal) {
  f <- factor(df[[kol]])                 # level diurutkan alfabetis
  df_encoded[[kol]] <- as.integer(f) - 1 # mulai dari 0 (seperti LabelEncoder)
  cat("\n", kol, ":\n", sep = "")
  print(setNames(seq_along(levels(f)) - 1, levels(f)))   # tabel pemetaan label
}

head(df_encoded, 10)

# -----------------------------------------------------
# TAHAP 6: SPLIT DATA (80% LATIH - 20% UJI)
# -----------------------------------------------------
set.seed(42)                                   # agar hasil reproducible
n          <- nrow(df_encoded)
idx_latih  <- sample(seq_len(n), size = floor(0.8 * n))

data_latih <- df_encoded[idx_latih, ]
data_uji   <- df_encoded[-idx_latih, ]

cat("Data latih:", nrow(data_latih), "baris\n")
cat("Data uji  :", nrow(data_uji),   "baris\n")
cat("Proporsi  :", round(nrow(data_latih) / n * 100), "% /",
                   round(nrow(data_uji)   / n * 100), "%\n")

# -----------------------------------------------------
# TAHAP 7: VISUALISASI
# -----------------------------------------------------
# 7.1 Histogram Umur
hist(df$Umur,
     breaks = seq(min(df$Umur) - 0.5, max(df$Umur) + 0.5, by = 1),
     col    = "steelblue", border = "black",
     main   = "Histogram Umur Mahasiswa",
     xlab   = "Umur (tahun)", ylab = "Frekuensi")

# 7.2 Grafik batang jumlah mahasiswa per Prodi (pakai data sebelum encoding)
jumlah_prodi <- sort(table(df$Prodi), decreasing = TRUE)
bp <- barplot(jumlah_prodi,
              col  = "seagreen", border = "black",
              main = "Jumlah Mahasiswa per Program Studi",
              xlab = "Program Studi", ylab = "Jumlah Mahasiswa",
              ylim = c(0, max(jumlah_prodi) + 5))
text(bp, jumlah_prodi, labels = jumlah_prodi, pos = 3)   # label angka di atas batang

# -----------------------------------------------------
# write.csv(df_encoded, "hasil_preprocessing.csv", row.names = FALSE)
# write.csv(data_latih, "data_latih.csv",          row.names = FALSE)
# write.csv(data_uji,   "data_uji.csv",            row.names = FALSE)
