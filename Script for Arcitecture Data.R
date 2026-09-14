library(GA)
library(readxl)
Data <- read_excel("GA Analysis 27 Oct.xlsx", sheet = "Data Arsitektur")
names(Data)

PairwiseMatrix <- function(x1, x2, x3, x4, x5, x6, x7, x8,
                           x9, x10, x11, x12, x13, x14, x15, x16){
  # Mengambil nilai dari data frame
  values <- as.numeric(c(x1, x2, x3, x4, x5, x6, x7, x8,
                         x9, x10, x11, x12, x13, x14, x15, x16))
  
  # Jumlah kriteria
  n <- length(values)
  
  # Membuat matriks kosong untuk pairwise comparison
  pairwise_matrix <- matrix(0, nrow = n, ncol = n)
  
  # Mengisi matriks pairwise comparison
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        pairwise_matrix[i, j] <- 1  # Membandingkan dengan dirinya sendiri
      } else {
        # Mengisi dengan rasio perbandingan
        pairwise_matrix[i, j] <- values[i] / values[j]
      }
    }
  }
  return(pairwise_matrix) 
}

# Fungsi untuk menghitung matriks prioritas ternormalisasi
normalize_matrix <- function(matrix) {
  # Menghitung jumlah setiap kolom
  col_sums <- colSums(matrix)
  
  # Menormalkan setiap elemen matriks dengan membagi dengan jumlah kolomnya
  normalized_matrix <- matrix / col_sums
  
  # Mengembalikan matriks ternormalisasi
  return(normalized_matrix)
}

# Fungsi untuk menghitung vektor prioritas
calculate_priority_vector <- function(normalized_matrix) {
  # Menghitung rata-rata setiap baris
  row_means <- rowMeans(normalized_matrix)
  
  # Mengembalikan vektor prioritas
  return(row_means)
}

# Fungsi untuk menghitung konsistensi matriks
consistency_check <- function(x1, x2, x3, x4, x5, x6, x7, x8,
                              x9, x10, x11, x12, x13, x14, x15, x16) {
  matrix <- PairwiseMatrix(x1, x2, x3, x4, x5, x6, x7, x8,
                           x9, x10, x11, x12, x13, x14, x15, x16)
  
  # Menghitung jumlah setiap kolom
  col_sums <- colSums(matrix)
  
  # Menghitung vektor prioritas
  priority_vector <- calculate_priority_vector(normalize_matrix(matrix))
  
  # Menghitung matriks konsistensi
  consistency_matrix <- matrix %*% priority_vector
  
  # Menghitung nilai konsistensi (CI)
  CI <- (sum(consistency_matrix) - nrow(matrix)) / (nrow(matrix) - 1)
  
  # Menghitung indeks konsistensi (RI)
  RI <- c(0, 0, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)
  
  # Menghitung rasio konsistensi (CR)
  if(nrow(matrix)>10){
    CR <- CI / RI[10]
  }else {
    CR <- CI / RI[nrow(matrix)] 
  }
  return(CR)
}

# Misalkan kita memiliki 16 kriteria
n_kriteria <- 16

# Inisialisasi populasi
set.seed(123)
ga_model <- ga(
  type = "real-valued",
  fitness = function(x) -consistency_check(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],
                                           x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16]),
  lower = rep(1, n_kriteria),
  upper = rep(9, n_kriteria),
  popSize = 100,      # Ukuran populasi
  maxiter = 100,     # Jumlah iterasi maksimum
  run = 100,         # Berhenti jika tidak ada perbaikan dalam 100 generasi
  monitor = TRUE     # Tampilkan progress
)
summary(ga_model)
plot(ga_model)

# Hasil
optimal_weights <- ga_model@solution
colnames(optimal_weights) <- c(names(Data)[2:17])
optimal_weights
# Menghitung vektor prioritas
priority_vector <- calculate_priority_vector(normalize_matrix(PairwiseMatrix(optimal_weights[1],optimal_weights[2],
                                                                             optimal_weights[3],optimal_weights[4],
                                                                             optimal_weights[5],optimal_weights[6],
                                                                             optimal_weights[7],optimal_weights[8],
                                                                             optimal_weights[9],optimal_weights[10],
                                                                             optimal_weights[11],optimal_weights[12],
                                                                             optimal_weights[13],optimal_weights[14],
                                                                             optimal_weights[15],optimal_weights[16])))
consistency_check(optimal_weights[1],optimal_weights[2],
                  optimal_weights[3],optimal_weights[4],
                  optimal_weights[5],optimal_weights[6],
                  optimal_weights[7],optimal_weights[8],
                  optimal_weights[9],optimal_weights[10],
                  optimal_weights[11],optimal_weights[12],
                  optimal_weights[13],optimal_weights[14],
                  optimal_weights[15],optimal_weights[16])
# Menampilkan bobot optimal
cat("Vektor Prioritas dari tingkat kepentingan optimal:\n")
print(priority_vector)
