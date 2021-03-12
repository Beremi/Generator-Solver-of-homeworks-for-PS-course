

uloha2 = function(filename,
                  c_ukolu,
                  output_data_dir,
                  data1,
                  data2,
                  typy=" pokles světelného toku výrobců ",
                  sloupce=c(" Amber ", " Bright "),
                  vyrobci=" výrobci ") {
  
  write(
    " <br><br><center><h1>Úkol 2.</h1></center> ",
    file = filename,
    append = TRUE
  )
  
  #---------------------------A--------------------------
  write(" <h2>a)</h2> ",
        file = filename,
        append = TRUE)
  
  data1_OP = data1
  data2_OP = data2
  pom = boxplot(data1_OP)
  data1_OP[data1_OP %in% pom$out] = NaN
  pom = boxplot(data2_OP)
  data2_OP[data2_OP %in% pom$out] = NaN
  data1_OP = na.omit(data1_OP)
  data2_OP = na.omit(data2_OP)
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/boxplot2.jpg"),
    width = 900,
    height = 350
  )
  par(mfrow = c(1, 2), pty = "s")
  boxplot(
    data1, data2,
    main = paste0("Graf 9a: ", typy, " (včetně OP)"),
    xlab = vyrobci,
    ylab = typy
  )
  boxplot(
    data1_OP, data2_OP,
    main = paste0("Graf 9b: ", typy, " (bez OP)"),
    xlab = vyrobci,
    ylab = typy
  )
  dev.off()
  write(paste0(
    "<center><img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/boxplot2.jpg"),
    "\"",
    " ></center><br> "
  ),
  file = filename,
  append = TRUE)
  
  data1=data1_OP
  data2=data2_OP
  
  
  
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/histogramy2.jpg"),
    width = 900,
    height = 350
  )
  par(mfrow = c(1, 2), pty = "s")
  hist(
    data1_OP,
    main = paste0("Graf 10a: ", sloupce[1], typy, " (bez OP)"),
    xlab = typy,
    ylab = "Absolutní četnosti",
    breaks = 20
  )
  hist(
    data2_OP,
    main = paste0("Graf 10b: ", sloupce[2], typy, " (bez OP)"),
    xlab = typy,
    ylab = "Absolutní četnosti",
    breaks = 20
  )
  dev.off()
  write(paste0(
    "<center><img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/histogramy2.jpg"),
    "\"",
    " ></center><br> "
  ),
  file = filename,
  append = TRUE)
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/qq_ploty2.jpg"),
    width = 900,
    height = 350
  )
  par(mfrow = c(1, 2), pty = "s")
  qqnorm(
    data1_OP,
    main = paste0(
      "Graf 10c: QQ graf ",
      typy,
      " ",
      sloupce[1]
    ),
    xlab = "Teoretické kvantily normálního rozdělení",
    ylab = "Naměřené kvantily výběru"
  )
  qqline(data1_OP)
  qqnorm(
    data2_OP,
    main = paste0(
      "Graf 10d: QQ graf ",
      typy,
      " ",
      sloupce[2]
    ),
    xlab = "Teoretické kvantily normálního rozdělení",
    ylab = "Naměřené kvantily výběru"
  )
  qqline(data2_OP)
  dev.off()
  write(paste0(
    "<center><img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/qq_ploty2.jpg"),
    "\"",
    " ></center><br> "
  ),
  file = filename,
  append = TRUE)
  
  
  
  write(" Vše dále pro data bez OP. <br>",
        file = filename,
        append = TRUE)
  
  write(
    paste0(
      "\n \n",
      "Střední hodnota ",typy,sloupce[1]," je ",
      round(mean(data1), digits = 4),
      " nm, medián je ",
      round(quantile(data1, 0.5), digits = 4),
      " nm.<br>",
      "Střední hodnota ",typy,sloupce[2]," je ",
      round(mean(data2), digits = 4),
      " nm, medián je ",
      round(quantile(data2, 0.5), digits = 4),
      " nm.<br>\n \n"
    ),
    file = filename,
    append = TRUE
  )
  

  #---------------------------B--------------------------
  write(" <h2>b)</h2> ",
        file = filename,
        append = TRUE)
  write(" Pro data bez OP. <br><br>",
        file = filename,
        append = TRUE)

  A1_SW = shapiro.test(data1)
  A1_SW$p.value
  if (A1_SW$p.value >= 0.0005) {
    t_p = paste0("= ", round(A1_SW$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0(
      " Shapiro-Wilk test normality pro typ ",sloupce[1]," vyšel s p-hodnotou ",
      t_p,
      " <br>"
    ),
    file = filename,
    append = TRUE
  )
  A2_SW = shapiro.test(data2)
  A2_SW$p.value
  if (A2_SW$p.value >= 0.0005) {
    t_p = paste0("= ", round(A2_SW$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0(
      " Shapiro-Wilk test normality pro typ ",sloupce[2]," vyšel s p-hodnotou ",
      t_p,
      " <br><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      " Šikmost ",sloupce[1]," je ",
      round(skewness(data1), digits = 2),
      ", šikmost ",sloupce[2]," je ",
      round(skewness(data2), digits = 2),
      " <br><br>"
    ),
    file = filename,
    append = TRUE
  )
  
  write(
    paste0(
      " Test symetrie (balíček \"lawstat\", funkce \"symmetry.test(data,boot=FALSE)\"). ",
      " <br>"
    ),
    file = filename,
    append = TRUE
  )
  
  library(lawstat)
  
  A1_symtest = symmetry.test(data1, boot = FALSE)
  if (A1_symtest$p.value >= 0.0005) {
    t_p = paste0("= ", round(A1_symtest$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0(" Test symetrie ",sloupce[1],", p-hodnota ", t_p,
           " <br>"),
    file = filename,
    append = TRUE
  )
  
  A2_symtest = symmetry.test(data2, boot = FALSE)
  if (A2_symtest$p.value >= 0.0005) {
    t_p = paste0("= ", round(A2_symtest$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0(" Test symetrie ",sloupce[2],", p-hodnota ", t_p,
           " <br>"),
    file = filename,
    append = TRUE
  )
  
  A1_ttest = t.test(data1)
  A1_ttest$conf.int
  A2_ttest = t.test(data2)
  A2_ttest$conf.int
  A1_wtest = wilcox.test(data1, conf.int = TRUE)
  A1_wtest$conf.int
  A2_wtest = wilcox.test(data2, conf.int = TRUE)
  A2_wtest$conf.int
  
  library(signmedian.test)
  A1_mtest = signmedian.test(
    data1,
    mu = 0,
    conf.level = 0.95,
    alternative = "two.sided",
    conf.int = TRUE
  )
  A1_mtest$conf.int
  A2_mtest = signmedian.test(
    data2,
    mu = 0,
    conf.level = 0.95,
    alternative = "two.sided",
    conf.int = TRUE
  )
  A2_mtest$conf.int
  write(
    " <h4>Odhad střední hodnoty dle t-testu:</h4> <br>",
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      " ",
      "",sloupce[1],": Bodový odhad = ",
      round(mean(data1), digits = 5),
      ", intervalový odhad=<",
      round(A1_ttest$conf.int[1], digits = 5),
      ", ",
      round(A1_ttest$conf.int[2], digits = 5),
      ">",
      " <br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      " ",
      "",sloupce[2],": Bodový odhad = ",
      round(mean(data2), digits = 5),
      ", intervalový odhad=<",
      round(A2_ttest$conf.int[1], digits = 5),
      ", ",
      round(A2_ttest$conf.int[2], digits = 5),
      ">",
      " <br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    " <h4>Odhad mediánu dle Wilcoxonova testu:</h4> <br>",
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      " ",
      "",sloupce[1],": Bodový odhad = ",
      round(quantile(data1, 0.5), digits = 5),
      ", intervalový odhad=<",
      round(A1_wtest$conf.int[1], digits = 5),
      ", ",
      round(A1_wtest$conf.int[2], digits = 5),
      ">",
      " <br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      " ",
      "",sloupce[2],": Bodový odhad = ",
      round(quantile(data2, 0.5), digits = 5),
      ", intervalový odhad=<",
      round(A2_wtest$conf.int[1], digits = 5),
      ", ",
      round(A2_wtest$conf.int[2], digits = 5),
      ">",
      " <br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    " <h4>Odhad mediánu dle mediánového testu:</h4> <br>",
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      " ",
      "",sloupce[1],": Bodový odhad = ",
      round(quantile(data1, 0.5), digits = 5),
      ", intervalový odhad=<",
      round(A1_mtest$conf.int[1], digits = 5),
      ", ",
      round(A1_mtest$conf.int[2], digits = 5),
      ">",
      " <br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      " ",
      "",sloupce[2],": Bodový odhad = ",
      round(quantile(data2, 0.5), digits = 5),
      ", intervalový odhad=<",
      round(A2_mtest$conf.int[1], digits = 5),
      ", ",
      round(A2_mtest$conf.int[2], digits = 5),
      ">",
      " <br>"
    ),
    file = filename,
    append = TRUE
  )
  
  

  #---------------------------C--------------------------
  write(" <h2>c)</h2> ",
        file = filename,
        append = TRUE)
  write(
    " Jedná se o jednovýběrové testy, mám na výběr <strong>t-test, Wilcoxonův test nebo mediánový test</strong>. <br>",
    file = filename,
    append = TRUE
  )

  
  alternatives = c("greater","two.sided","less")
  alternatives_h = c("mu>0","mu!=0","mu<0")
  for (kk in 1:3) {
    write(
      paste0(
        " <h2>",
        alternatives[kk],
        ": ",
        "  H0: mu=0",
        ", HA:",alternatives_h[kk],"</h4> "
      ),
      file = filename,
      append = TRUE
    )
    A1 = c(0, 0, 0)
    A2 = c(0, 0, 0)
    
    A1[1] = t.test(data1, mu = 0, alternative = alternatives[kk])$p.value
    A2[1] = t.test(data2, mu = 0, alternative = alternatives[kk])$p.value
    A1[2] = wilcox.test(data1, mu = 0, alternative = alternatives[kk])$p.value
    A2[2] = wilcox.test(data2, mu = 0, alternative = alternatives[kk])$p.value
    A1[3] = signmedian.test(data1, mu = 0, alternative = alternatives[kk])$p.value
    A2[3] = signmedian.test(data2, mu = 0, alternative = alternatives[kk])$p.value
    
    AA = rbind(A1, A2)
    testy = c("T-test ", "Wilcox ", "Sign-median ")
    
    for (j in 1:2) {
      write(
        paste0(" <h3>", sloupce[j], "</h3> "),
        file = filename,
        append = TRUE
      )
      for (i in 1:3) {
        if (AA[j, i] >= 0.0005) {
          t_p = paste0("= ", round(AA[j, i], digits = 3))
        } else {
          t_p = "<< 0.001"
        }
        
        write(
          paste0(
            " <b>",
            testy[i],
            ":</b>",
            " p-hodnota ",
            t_p,
            "",
            " <br>"
          ),
          file = filename,
          append = TRUE
        )
      }
    }
  }
  
  #---------------------------D--------------------------
  write(" <h2>d)</h2> ",
        file = filename,
        append = TRUE)
  
  write(
    paste0(
      "\n \n",
      "Rozdíl středních hodnot ",typy,sloupce[1]," a ",sloupce[2]," je ",
      round(mean(data1) - mean(data2), digits = 4),
      " nm, rozdíl mediánů je ",
      round(quantile(data1, 0.5) - quantile(data2, 0.5), digits = 4),
      " nm.<br>\n \n"
    ),
    file = filename,
    append = TRUE
  )
  
  write(
    " Normalita viz výše. Ověříme shodu rozptylů pomocí F-testu. <br>",
    file = filename,
    append = TRUE
  )
  f_test = var.test(data1, data2, conf.level = 0.95)
  
  if (f_test$p.value >= 0.0005) {
    t_p = paste0("= ", round(f_test$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  
  write(
    paste0(
      " Exploračně: poměr rozptylů = ",
      round(max(sd(data1) ^ 2, sd(data2) ^ 2) / min(sd(data1) ^ 2, sd(data2) ^
                                                          2), digits = 3),
      ". <br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(" Pro F-test vyšla p-hodnota ", t_p, ". <br>"),
    file = filename,
    append = TRUE
  )
  
  
  
  A1A2_ttest = t.test(data1, data2, var.equal = TRUE)
  A1A2_AWtest = t.test(data1, data2)
  A1A2_MWtest = wilcox.test(data1, data2, conf.int = TRUE)
  
  
  
  write(
    paste0(" <h4>Odhad rozdílu středních hodnot (",sloupce[1],"-",sloupce[2],") dle t-testu:</h4> <br>"),
    file = filename,
    append = TRUE
  )
  
  
  write(
    paste0(
      " ",
      "",sloupce[1],"-",sloupce[2],": Bodový odhad = ",
      round(mean(data1) - mean(data2), digits = 5),
      ", intervalový odhad=<",
      round(A1A2_ttest$conf.int[1], digits = 6),
      ", ",
      round(A1A2_ttest$conf.int[2], digits = 6),
      ">",
      " <br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(" <h4>Odhad rozdílu středních hodnot (",sloupce[1],"-",sloupce[2],") dle Aspinové-Welshova testu:</h4> <br>"),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      " ",
      "",sloupce[1],"-",sloupce[2],": Bodový odhad = ",
      round(mean(data1) - mean(data2), digits = 5),
      ", intervalový odhad=<",
      round(A1A2_AWtest$conf.int[1], digits = 6),
      ", ",
      round(A1A2_AWtest$conf.int[2], digits = 6),
      ">",
      " <br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(" <h4>Odhad rozdílu mediánů (",sloupce[1],"-",sloupce[2],") dle Mannova-Whitneyova testu:</h4> <br>"),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      " ",
      "",sloupce[1],"-",sloupce[2],": Bodový odhad = ",
      round(quantile(data1, 0.5) - quantile(data2, 0.5), digits = 5),
      ", intervalový odhad=<",
      round(A1A2_MWtest$conf.int[1], digits = 5),
      ", ",
      round(A1A2_MWtest$conf.int[2], digits = 5),
      ">",
      " <br>"
    ),
    file = filename,
    append = TRUE
  )
  
  #---------------------------E--------------------------
  write(" <h2>e)</h2> ",
        file = filename,
        append = TRUE)
  
 
  alternatives = c("greater","two.sided","less")
  alternatives_h = c(paste0("(",sloupce[1],"-",sloupce[2],")>0"), 
                     paste0("(",sloupce[1],"-",sloupce[2],")!=0"), 
                     paste0("(",sloupce[1],"-",sloupce[2],")<0"))
  for (kk in 1:3) {
    write(
      paste0(
        " <h2>",
        alternatives[kk],
        ": ",
        " H0: ","(",sloupce[1],"-",sloupce[2],")=0 ",
        ", HA:",
        alternatives_h[kk],
        "</h4> "
      ),
      file = filename,
      append = TRUE
    )
    A1 = c(0, 0, 0)
    
    A1[1] = t.test(data1, data2, var.equal = TRUE, alternative = alternatives[kk])$p.value
    A1[2] = t.test(data1, data2, alternative = alternatives[kk])$p.value
    A1[3] = wilcox.test(data1, data2, alternative = alternatives[kk])$p.value
    
    testy = c("T-test ", "A-W  ", "M-W ")
    
    
    for (i in 1:3) {
      if (A1[i] >= 0.0005) {
        t_p = paste0("= ", round(A1[i], digits = 3))
      } else {
        t_p = "<< 0.001"
      }
      
      write(
        paste0(
          " <b>",
          testy[i],
          ":</b>",
          " p-hodnota ",
          t_p,
          "",
          " <br>"
        ),
        file = filename,
        append = TRUE
      )
    }
    
  }
  
}