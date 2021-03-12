uloha3 = function(filename,
                  c_ukolu,
                  output_data_dir,
                  hodnoty,
                  trizeni,
                  typy = " světelný tok v 5 °C ",
                  vyrobci = " výrobci ") {
  write(" <center><h1>Úkol 3.</h1></center> ",
        file = filename,
        append = TRUE)
  
  
  hodnoty_OP = hodnoty
  for (i in unique(trizeni)) {
    pom = boxplot(hodnoty_OP[trizeni == i])
    hodnoty_OP[(hodnoty_OP %in% pom$out) &
                 (trizeni == i)] = NaN
  }
  trizeni_OP = trizeni[!is.nan(hodnoty_OP)]
  hodnoty_OP = na.omit(hodnoty_OP)
  
  #---------------------------A--------------------------
  write(" <h2>a)</h2> ",
        file = filename,
        append = TRUE)
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/boxplot_multi.jpg"),
    width = 900,
    height = 350
  )
  par(mfrow = c(1, 2), pty = "s")
  boxplot(
    hodnoty ~ trizeni,
    main = paste0("Graf 15: ", typy, " (včetně OP)"),
    xlab = vyrobci,
    ylab = typy
  )
  boxplot(
    hodnoty_OP ~ trizeni_OP,
    main = paste0("Graf 16: ", typy, " (bez OP)"),
    xlab = vyrobci,
    ylab = typy
  )
  dev.off()
  write(
    paste0(
      "<center><img src=",
      "\"",
      paste0(output_data_dir, "du_", c_ukolu, "_img/boxplot_multi.jpg"),
      "\"",
      "></center>"
    ),
    file = filename,
    append = TRUE
  )
  
  
  #---------------------------B--------------------------
  write(" <h2>b)</h2> ",
        file = filename,
        append = TRUE)
  
  write(" Dále pracujeme bez OP. ",
        file = filename,
        append = TRUE)
  
  normality_exact_empiric <- function(x) {
    tmp = data.frame(
      Shapiro_Wilk = round(shapiro.test(x)$p.value, digits = 3),
      sikmost = round(skewness(x), digits =
                        5),
      spicatost = round(kurtosis(x) - 3, digits =
                          5)
    )
    return(tmp)
  }
  
  summary_stack = tapply(hodnoty_OP,
                         trizeni_OP,
                         normality_exact_empiric)
  summary_matrix <- matrix(unlist(t(summary_stack)), ncol = 4)
  colnames(summary_matrix) = colnames(t(summary_stack))
  rownames(summary_matrix) = colnames(summary_stack[[1]])
  
  summary_html = htmlTable(
    summary_matrix,
    col.columns = c("none", "#CCCCCC"),
    rgroup = c("Exaktní",
               "Explorační"),
    n.rgroup = c(1, 2),
    css.cell = "padding-left: .7em; padding-right: .7em;"
  )
  summary_html
  write(
    "<center><h4>Tabulka 3: Explorační a exaktní (Shapiro-Wilk test) posouzení normality</h4>",
    file = filename,
    append = TRUE
  )
  write(paste0(summary_html, "</center>"),
        file = filename,
        append = TRUE)
  
  
  #---------------------------C--------------------------
  write(" <h2>c)</h2> ",
        file = filename,
        append = TRUE)
  res_lev = levene.test(hodnoty_OP ,
                        trizeni_OP)
  res_bart = bartlett.test(hodnoty_OP ,
                           trizeni_OP)
  
  rozptyly_empiric <- function(x) {
    tmp = data.frame(rozptyl = round(var(x), digits = 5))
    return(tmp)
  }
  
  vars = tapply(hodnoty_OP ,
                trizeni_OP,
                rozptyly_empiric)
  summary_matrix <- matrix(unlist(t(vars)), ncol = 4)
  colnames(summary_matrix) = colnames(t(vars))
  rownames(summary_matrix) = "Rozptyl &nbsp"
  
  summary_html = htmlTable(summary_matrix,
                           col.columns = c("none", "#CCCCCC"),
                           css.cell = "padding-left: .7em; padding-right: .7em;")
  summary_html
  write(
    paste0(
      "<center><h4>Tabulka 4: Rozptyly jednotlivých ",
      vyrobci,
      "</h4>"
    ),
    file = filename,
    append = TRUE
  )
  write(paste0(summary_html, "</center>"),
        file = filename,
        append = TRUE)
  stds = tapply(hodnoty_OP ,
                trizeni_OP,
                sd)
  
  write(
    paste0(
      " Poměr největšího a nejmenšího rozptylu ",
      round((max(stds) / min(stds)) ^ 2, digits = 3),
      " "
    ),
    file = filename,
    append = TRUE
  )
  if (res_bart$p.value >= 0.0005) {
    t_p = paste0("= ", round(res_bart$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(paste0(" Bartlett test p-hodnota ", t_p, " "),
        file = filename,
        append = TRUE)
  if (res_lev$p.value >= 0.0005) {
    t_p = paste0("= ", round(res_lev$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(paste0(" Levene test p-hodnota ", t_p, " "),
        file = filename,
        append = TRUE)
  
  
  #---------------------------D--------------------------
  write(" <h2>d)</h2> ",
        file = filename,
        append = TRUE)
  
  
  IO_mean <- function(x) {
    res = t.test(x)
    data.frame(
      bodovy_odhad = round(mean(x), digits = 5),
      dolni_mez = round(res$conf.int[1], digits = 5),
      horni_mez = round(res$conf.int[2], digits = 5)
    )
  }
  
  IO_median <- function(x) {
    res = wilcox.test(x, conf.int = TRUE)
    data.frame(
      bodovy_odhad = round(median(x), digits = 5),
      dolni_mez = round(res$conf.int[1], digits = 5),
      horni_mez = round(res$conf.int[2], digits = 5)
    )
  }
  
  mean_stack = tapply(hodnoty_OP,
                      trizeni_OP,
                      IO_mean)
  summary_matrix_mean <- matrix(unlist(t(mean_stack)), ncol = 4)
  colnames(summary_matrix_mean) = colnames(t(mean_stack))
  rownames(summary_matrix_mean) = colnames(mean_stack[[1]])
  
  median_stack = tapply(hodnoty_OP,
                        trizeni_OP,
                        IO_median)
  summary_matrix_median <- matrix(unlist(t(median_stack)), ncol = 4)
  colnames(summary_matrix_median) = colnames(t(median_stack))
  rownames(summary_matrix_median) = colnames(median_stack[[1]])
  
  
  summary_html = htmlTable(
    rbind(summary_matrix_mean, summary_matrix_median),
    col.columns = c("none", "#CCCCCC"),
    rgroup = c("Odhady střední hodnoty",
               "Odhady mediánu"),
    n.rgroup = c(3, 3),
    css.cell = "padding-left: .7em; padding-right: .7em;"
  )
  
  write(
    "<center><h4>Tabulka 5: Bodové a 95% intervalové odhady střední hodnoty (t-test) a mediánu (Wilcox test)</h4>",
    file = filename,
    append = TRUE
  )
  write(paste0(summary_html, "</center>"),
        file = filename,
        append = TRUE)
  
  #---------------------------E--------------------------
  write(" <h2>e)</h2> ",
        file = filename,
        append = TRUE)
  
  anova_res = summary(aov(hodnoty_OP ~ trizeni_OP))
  
  if (anova_res[[1]]$`Pr(>F)`[1] >= 0.0005) {
    t_p = paste0("= ", round(anova_res[[1]]$`Pr(>F)`[1], digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(paste0(" Anova p-hodnota ", t_p, " "),
        file = filename,
        append = TRUE)
  trizeni_OP = as.factor(trizeni_OP)
  KW_res = kruskal.test(hodnoty_OP ,
                        trizeni_OP)
  if (KW_res$p.value >= 0.0005) {
    t_p = paste0("= ", round(KW_res$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0(" Kruskal-Wallis test p-hodnota ", t_p, " "),
    file = filename,
    append = TRUE
  )
  
  t_mean = mean(hodnoty_OP)
  t_median = quantile(hodnoty_OP, 0.5)
  
  efekty <- function(x) {
    data.frame(
      efekt_prumery = round(-t_mean + mean(x), digits = 5),
      efekt_median = round(-t_median + quantile(x, 0.5), digits = 5)
    )
  }
  efekty_stack = tapply(hodnoty_OP, trizeni_OP, efekty)
  summary_matrix_efekty <- matrix(unlist(t(efekty_stack)), ncol = 4)
  colnames(summary_matrix_efekty) = colnames(t(efekty_stack))
  rownames(summary_matrix_efekty) = colnames(efekty_stack[[1]])
  
  summary_html = htmlTable(
    summary_matrix_efekty,
    col.columns = c("none", "#CCCCCC"),
    rgroup = c("Efekty pro ANOVU",
               "Efekty pro Kruskal-Wallise"),
    n.rgroup = c(1, 1),
    css.cell = "padding-left: .7em; padding-right: .7em;"
  )
  
  write("<center><h4>Tabulka 6: Efekty</h4>",
        file = filename,
        append = TRUE)
  write(paste0(summary_html, "</center>"),
        file = filename,
        append = TRUE)
  write(
    paste0(
      "  V tabulkách níže jsou ",
      vyrobci,
      " setřízeny sestupně dle efektů.  "
    ),
    file = filename,
    append = TRUE
  )
  write(
    "<center><h4>Tabulka 7: Post-Hoc Tukey HSD &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp Tabulka 8: Dunové test (Bonferroni)</h4></center>",
    file = filename,
    append = TRUE
  )
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/post_hoc.jpg"),
    width = 800,
    height = 400,
    quality = 100,
    pointsize = 15
  )
  par(mfrow = c(1, 2), pty = "s")
  
  ############### ANOVA picture ###########################
  
  alpha = 0.05
  nazvy = unique(trizeni)
  
  anova_res = aov(hodnoty_OP ~ trizeni_OP)
  post_hoc_res = TukeyHSD(anova_res)
  p_vals_post_hoc = post_hoc_res[[1]][, 4]
  it = 1
  mat = matrix(0, 4, 4)
  for (i in 1:3) {
    for (j in (i + 1):4) {
      mat[i, j] = p_vals_post_hoc[it]
      mat[j, i] = p_vals_post_hoc[it]
      it = it + 1
    }
    
  }
  means_all = tapply(hodnoty_OP , trizeni_OP, mean)
  poradi = sort.list(means_all, decreasing = TRUE)
  mat = mat[poradi, poradi]
  
  normality_exact_empiric <- function(i, j, tag, text) {
    if (tag) {
      rect(i - 1, j - 1, i, j, col = "green")
    } else{
      rect(i - 1, j - 1, i, j, col = "orange")
    }
    if (text >= 0.0005) {
      if (text >= 0.999) {
        t_p = "0.999"
      } else{
        t_p = paste0(round(text, digits = 3))
      }
    } else {
      t_p = "<<0.001"
    }
    text(i - 0.5, j - 0.5, t_p)
  }
  
  
  
  plot(
    c(0, 4),
    c(0, 4),
    type = "l",
    xaxt = 'n',
    yaxt = 'n',
    xlim = c(0.15, 3.85),
    ylim = c(3.85, 0.15),
    xlab = "",
    ylab = ""
  )
  lines(c(0, 1), c(1, 0))
  lines(1 + c(0, 1), 1 + c(1, 0))
  lines(2 + c(0, 1), 2 + c(1, 0))
  lines(3 + c(0, 1), 3 + c(1, 0))
  axis(3,
       at = c(0.5, 1.5, 2.5, 3.5),
       labels = nazvy[poradi],
       las = 1)
  axis(2,
       at = c(0.5, 1.5, 2.5, 3.5),
       labels = nazvy[poradi],
       las = 1)
  grid(
    nx = NULL,
    ny = NULL,
    col = "black",
    lty = "solid"
  )
  
  
  for (i in 1:4) {
    for (j in 1:4) {
      if (i == j) {
        
      } else{
        normality_exact_empiric(i, j, mat[i, j] > alpha, mat[i, j])
      }
    }
  }
  
  
  ############### KW picture ###########################
  
  alpha = 0.05 / 2
  nazvy = unique(trizeni)
  
  
  library(dunn.test)
  post_hoc_res = dunn.test(hodnoty_OP , trizeni_OP, method = "bonferroni")
  p_vals_post_hoc = post_hoc_res$P.adjusted
  it = 1
  mat = matrix(0, 4, 4)
  for (j in 2:4) {
    for (i in (1):(j - 1)) {
      mat[i, j] = p_vals_post_hoc[it]
      mat[j, i] = p_vals_post_hoc[it]
      it = it + 1
    }
    
  }
  means_all = tapply(hodnoty_OP , trizeni_OP, quantile, 0.5)
  poradi = sort.list(means_all, decreasing = TRUE)
  mat = mat[poradi, poradi]
  
  normality_exact_empiric <- function(i, j, tag, text) {
    if (tag) {
      rect(i - 1, j - 1, i, j, col = "green")
    } else{
      rect(i - 1, j - 1, i, j, col = "orange")
    }
    if (text >= 0.0005) {
      if (text >= 0.999) {
        t_p = "0.999"
      } else{
        t_p = paste0(round(text, digits = 3))
      }
    } else {
      t_p = "<<0.001"
    }
    text(i - 0.5, j - 0.5, t_p)
  }
  
  
  plot(
    c(0, 4),
    c(0, 4),
    type = "l",
    xaxt = 'n',
    yaxt = 'n',
    xlim = c(0.15, 3.85),
    ylim = c(3.85, 0.15),
    xlab = "",
    ylab = ""
  )
  lines(c(0, 1), c(1, 0))
  lines(1 + c(0, 1), 1 + c(1, 0))
  lines(2 + c(0, 1), 2 + c(1, 0))
  lines(3 + c(0, 1), 3 + c(1, 0))
  axis(3,
       at = c(0.5, 1.5, 2.5, 3.5),
       labels = nazvy[poradi],
       las = 1)
  axis(2,
       at = c(0.5, 1.5, 2.5, 3.5),
       labels = nazvy[poradi],
       las = 1)
  grid(
    nx = NULL,
    ny = NULL,
    col = "black",
    lty = "solid"
  )
  
  
  for (i in 1:4) {
    for (j in 1:4) {
      if (i == j) {
        
      } else{
        normality_exact_empiric(i, j, mat[i, j] > alpha, mat[i, j])
      }
    }
  }
  
  
  ###############----------------###########################
  dev.off()
  write(
    paste0(
      "<center><img src=",
      "\"",
      paste0(output_data_dir, "du_", c_ukolu, "_img/post_hoc.jpg"),
      "\"",
      "></center>"
    ),
    file = filename,
    append = TRUE
  )
  
}
