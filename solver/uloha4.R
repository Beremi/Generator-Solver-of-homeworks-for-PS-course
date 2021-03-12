library(lsr)

uloha4 = function(filename,
                  c_ukolu,
                  output_data_dir,
                  hodnoty,
                  trizeni,
                  hodnoty_OP,
                  trizeni_OP,
                  fail_name = " nedosažení deklarovaného světelného toku (800lm) ",
                  vyrobci = " výrobci ") {
  write("<center><h1><br>Úkol 4.</h1></center>",
        file = filename,
        append = TRUE)
  
  napisy = c(
    "<h1>Pokud jsou použita původní data (tedy i ta odstraněná v rámci úkolů 1-3)</h1>",
    "<h1>Pokud jsou použita data bez OP</h1>"
  )
  
  for (ii in 1:2) {
    write(napisy[ii],
          file = filename,
          append = TRUE)
    
    #---------------------------A--------------------------
    write("<h2>a)</h2>",
          file = filename,
          append = TRUE)
    if (ii == 1) {
      tab = table(trizeni, hodnoty)
    }
    if (ii == 2) {
      tab = table(trizeni_OP, hodnoty_OP)
    }
    
    
    cv = round(cramersV(tab), digits = 3)
    res = chisq.test(tab)
    cc = round(sqrt(res$statistic / (res$statistic + sum(tab))), digits = 3)
    cc_corig = round(cc / sqrt(1 / 2), digits = 3)
    
    abs_tab = addmargins(tab)
    rel_tab = round(addmargins(tab / sum(tab)), digits = 3)
    prob_fail = addmargins(t(tab[, 1] / abs_tab[1:4, 3]), 2)
    rownames(prob_fail) = "Relativní četnost"
    summary_html = htmlTable(
      rbind(t(abs_tab), t(rel_tab), round(prob_fail, digits = 3)),
      col.columns = c("none", "#CCCCCC"),
      rgroup = c(
        "Absolutní hodnoty",
        "Relativní hodnoty",
        paste0("Riziko ", fail_name)
      ),
      n.rgroup = c(3, 3, 1),
      css.cell = "padding-left: .7em; padding-right: .7em;"
    )
    
    
    write(
      "<center><h4>Tabulka 9: Kontingenční tabulka (absolutní a relativní hodnoty)</h4>",
      file = filename,
      append = TRUE
    )
    write(paste0(summary_html, "</center>"),
          file = filename,
          append = TRUE)
    
    write(
      paste0(
        "<br>",
        "CramersV = ",
        cv,
        ",<br>koeficient kontingence = ",
        cc,
        ",<br>korigovaný koeficient kontingence = ",
        cc_corig,
        "<br>"
      ),
      file = filename,
      append = TRUE
    )
    
    
    
    jpeg(
      paste0(output_data_dir, "du_", c_ukolu, "_img/mosaic",ii,".jpg"),
      width = 850,
      height = 400,
      quality = 100,
      pointsize = 15
    )
    par(mfrow = c(1, 1), pty = "s")
    mosaicplot(
      tab,
      main = paste0(
        "Graf 17: Mosaikový graf rozdělení ",
        fail_name,
        " mezi jednotlivé ",
        vyrobci
      )
    )
    dev.off()
    write(
      paste0(
        "<center><img src=",
        "\"",
        paste0(output_data_dir, "du_", c_ukolu, "_img/mosaic",ii,".jpg"),
        "\"",
        "></center>"
      ),
      file = filename,
      append = TRUE
    )
    
    #---------------------------b--------------------------
    write("<h2>b)</h2>",
          file = filename,
          append = TRUE)
    names = unique(trizeni)
    for (jj in 1:4) {
      p = tab[jj, 1] / (tab[jj, 1] + tab[jj, 2])
      write(
        paste0("<h3>",names[jj],"</h3>Bodový odhad pravděpodobnosti p= ",
               round(p, digits = 3),
               "<br>"),
        file = filename,
        append = TRUE
      )
      n0 = 9 / (p * (1 - p))
      write(
        paste0(
          "Přepoklad testu: minimální počet pozorování (9/(p*(1-p))) = ",
          ceiling(n0),
          ", skutečný počet pozorování = ",
          (tab[jj, 1] + tab[jj, 2]),
          "<br>"
        ),
        file = filename,
        append = TRUE
      )
      res = binom.test(tab[jj, 1], (tab[jj, 1] + tab[jj, 2]))
      
      write(
        paste0(
          "",
          "absolutní riziko: Bodový odhad = ",
          round(p, digits = 3),
          ", intervalový odhad=<",
          round(res$conf.int[1], digits = 3),
          ", ",
          round(res$conf.int[2], digits = 3),
          ">",
          "<br>"
        ),
        file = filename,
        append = TRUE
      )
    }
    #---------------------------c+d--------------------------
    write("<h2>c,d)</h2>",
          file = filename,
          append = TRUE)
    
    names = colnames(prob_fail)[-length(prob_fail)]
    a_tab = rbind(tab[which.max(prob_fail[-length(prob_fail)]),],
                  tab[which.min(prob_fail[-length(prob_fail)]),])
    rownames(a_tab) = c(names[which.max(prob_fail[-length(prob_fail)])],
                        names[which.min(prob_fail[-length(prob_fail)])])
    
    summary_html = htmlTable(a_tab,
                             col.columns = c("none", "#CCCCCC"),
                             css.cell = "padding-left: .7em; padding-right: .7em;")
    write(
      paste0("<center><h4>Tabulka 10: Asociační tabulka, Exposed=",rownames(a_tab)[1],",Outcome=",colnames(a_tab)[1],")</h4>"),
      file = filename,
      append = TRUE
    )
    write(paste0(summary_html), file = filename, append = TRUE)
    library(epiR)
    res = epi.2by2(a_tab)
    RR = round(res$massoc$RR.strata.wald, digits = 2)
    OR = round(res$massoc$OR.strata.wald, digits = 2)
    
    summary_html1 = htmlTable(res$tab,
                              col.columns = c("none", "#CCCCCC"),
                              css.cell = "padding-left: .7em; padding-right: .7em;")
    
    write(paste0(summary_html1, "</center>"),
          file = filename,
          append = TRUE)
    
    
    jpeg(
      paste0(output_data_dir, "du_", c_ukolu, "_img/mosaic2",ii,".jpg"),
      width = 850,
      height = 350,
      quality = 100,
      pointsize = 15
    )
    par(mfrow = c(1, 1), pty = "s")
    mosaicplot(a_tab, main = "Graf 18: Mosaikový graf pro asociační tabulku")
    dev.off()
    write(
      paste0(
        "<center><img src=",
        "\"",
        paste0(output_data_dir, "du_", c_ukolu, "_img/mosaic2",ii,".jpg"),
        "\"",
        "></center>"
      ),
      file = filename,
      append = TRUE
    )
    write(
      paste0(
        "<br>",
        "Relativní riziko: Bodový odhad = ",
        RR[1],
        ", intervalový odhad=<",
        RR[2],
        ", ",
        RR[3],
        ">",
        "<br>"
      ),
      file = filename,
      append = TRUE
    )
    write(
      paste0(
        "",
        "Poměr šancí: Bodový odhad = ",
        OR[1],
        ", intervalový odhad=<",
        OR[2],
        ", ",
        OR[3],
        ">",
        "<br>"
      ),
      file = filename,
      append = TRUE
    )
    #---------------------------e--------------------------
    write("<h2>e)</h2>",
          file = filename,
          append = TRUE)
    res = chisq.test(tab)
    
    
    abs_tab = round(addmargins(res$expected), digits = 3)
    rel_tab = round(addmargins(res$expected / sum(res$expected)), digits = 3)
    
    summary_html = htmlTable(rbind(t(abs_tab)),
                             col.columns = c("none", "#CCCCCC"),
                             css.cell = "padding-left: .7em; padding-right: .7em;")
    
    
    
    write(
      "<center><h4>Tabulka 11: Očekávané absolutní četnosti pro ChiSq test nezávislosti</h4>",
      file = filename,
      append = TRUE
    )
    write(paste0(summary_html, "</center>"),
          file = filename,
          append = TRUE)
    
    
    if (res$p.value >= 0.0005) {
      t_p = paste0("= ", round(res$p.value, digits = 3))
    } else {
      t_p = "<< 0.001"
    }
    
    
    write(
      paste0(
        "",
        "ChiSqtest p-hodnota",
        t_p,
        ", počet stupňů volnosti=",
        res$parameter,
        "<br>"
      ),
      file = filename,
      append = TRUE
    )
  }
}
