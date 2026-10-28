rNestmenu <-
  function(lang = "english") {
    if (lang == "english") {
      warninglabl <- "Warning"
      warningtextlabl <- paste0(
        "The technical information contained in rNest is general information that constitutes advice only. All persons must adhere to all pertinent laws (for example provincial or territorial laws), regulations and permit requirements including but not restricted to the Migratory Birds Convention Act, 1994 (MBCA) and the Migratory Birds Regulations (MBR). It is important to note that some species of birds protected under the MBCA have also been listed in Schedule 1 of the Species at Risk Act (SARA). These species receive protection from both the MBCA and SARA. This information does not provide an authorization for harming or killing migratory birds or for the disturbance, destruction or taking of nests or eggs as prohibited under the MBR. This information does not provide a guarantee that the activities will avoid contravening the MBR or other laws and regulations. This is general information not intended to be relied on as official advice concerning the legal consequences of any specific activity. It is not a substitute for the MBCA, the MBR, or any other legislation.",
        "\n",
        "\n",
        "It is the responsibility of individuals and companies to assess their risk with regards to migratory birds and design relevant avoidance and mitigation measures to reduce risks. It is possible that local nesting periods could have a different starting date and/or duration than dates provided in rNest due to micro-climatic conditions in specific areas (e.g. high elevation sites or coastal sites) as well as inter-annual variation due to factors such as early spring or cold, wet summer. rNest will be updated as new data become available, which could result in the changing of dates. Any time nests of migratory bird containing eggs or young are encountered, the immediate area should be avoided until the young have naturally left the vicinity of the nest. This protection measure should be taken even if the nest has been found outside the dates of the general nesting period for the area provided in rNest. Once out of the nest, young birds are still vulnerable therefore precautionary measures are recommended.",
        "\n",
        "\n",
        "For more detailed Avoidance Guidelines, please visit Environment Canada\u2019s website at www.ec.gc.ca/paom-itmb"
      )
      loadingmsglabl = "Data and rNest interface loading..."
    } else {
      warninglabl <- "Avertissement"
      warningtextlabl <- paste0(
        "L'information technique contenue dans rNest est de l'information g\u00e9n\u00e9rale ne constituant uniquement qu'un avis. Tous doivent se conformer aux lois pertinentes (e.g. lois provinciales ou territoriales), aux r\u00e8glements et aux permis requis, incluant entre autre la Loi de 1994 sur la convention concernant les oiseaux migrateurs (LCOM) et le R\u00e8glement sur les oiseaux migrateurs (ROM). Il est important de noter que certaines esp\u00e8ces d'oiseaux prot\u00e9g\u00e9es sous la LCOM sont \u00e9galement list\u00e9es sous l'annexe 1 de la Loi sur les esp\u00e8ces en p\u00e9ril (LEP). Ces esp\u00e8ces sont prot\u00e9g\u00e9es \u00e0 la fois sous la LCOM et la LEP. L'information pr\u00e9sent\u00e9e ici ne fournit aucune autorisation \u00e0 blesser ou \u00e0 tuer des oiseaux migrateurs ou \u00e0 perturber, d\u00e9truire ou pr\u00e9lever des nids ou des oeufs tel qu'il l'est interdit sous la LCOM. L'information pr\u00e9sent\u00e9e ne fournit pas de garantie que les activit\u00e9s vont \u00e9viter de contrevenir au ROM ou aux autres lois et r\u00e8glements en vigueur. Il s'agit d'information g\u00e9n\u00e9rale qui ne doit pas \u00eatre consid\u00e9r\u00e9e comme \u00e9tant un avis officiel concernant les cons\u00e9quences l\u00e9gales de toutes activit\u00e9s sp\u00e9cifiques. Il ne s'agit pas d'un substitut \u00e0 la LCOM, le ROM ou toute autre l\u00e9gislation en vigueur.",
        "\n",
        "\n",
        "C'est la responsabilit\u00e9 des individus et des entreprises d'\u00e9valuer leurs risques en regards aux oiseaux migrateurs et d'\u00e9tablir des mesures d'\u00e9vitements et d'att\u00e9nuations appropri\u00e9es afin de r\u00e9duire ces risques. Il est possible que les dates des p\u00e9riodes de nidification locales diff\u00e8rent en d\u00e9but ou en dur\u00e9e que ce qui est fourni par rNest en raison des conditions micro-climatiques d'un endroit sp\u00e9cifique (e.g. site en altitude ou site c\u00f4tier) ou en raison de la variation inter-annuelle associ\u00e9e aux variations climatiques. Le programme rNest sera mis \u00e0 jour en fonction des nouvelles informations disponibles et les dates fournies pourraient varier. D\u00e8s qu'un nid contenant des oeufs ou des jeunes d'oiseaux migrateurs est trouv\u00e9, l'endroit imm\u00e9diat devrait \u00eatre \u00e9vit\u00e9 jusqu'\u00e0 ce que les jeunes quittent les environs du nid de fa\u00e7on naturelle. Cette mesure de protection devrait \u00eatre appliqu\u00e9e m\u00eame si un nid est trouv\u00e9 hors de la p\u00e9riode g\u00e9n\u00e9rale de nidification indiqu\u00e9e par rNest. Une fois hors du nid, les jeunes oiseaux sont toujours vuln\u00e9rables, par cons\u00e9quent des mesures de pr\u00e9cautions sont recommand\u00e9es.",
        "\n",
        "\n",
        "Pour de plus amples renseignements concernant les R\u00e8gles en mati\u00e8re d'\u00e9vitement, consultez le site d'Environnement Canada au www.ec.gc.ca/paom-itm"
      )
      loadingmsglabl = "Chargement des donn\u00e9es et de l'interface rNest..."
    }
    tkmessageBox(
      title = warninglabl,
      message = warningtextlabl,
      icon = "info",
      type = "ok"
    )
    cat("\n", "  ", loadingmsglabl, "\n")
    flush.console()
    data(nestwatch)
    data(nestingparameters)
    data(birdgroupings)
    data(ecodistrictscsv)
    data(predictions)
    data(specodistricts)
    x <- dlply(ecodistrictscsv, .(group), function(i) {
      Polygon(i[c(nrow(i), 1:nrow(i)), 1:2], hole = i$hole[1])
    })
    x <- sapply(unique(ecodistrictscsv$id), function(i) {
      res <- x[
        names(x) %in% ecodistrictscsv$group[which(ecodistrictscsv$id == i)]
      ]
      Polygons(res, ID = i)
    })
    dat <- ecodistrictscsv[match(names(x), ecodistrictscsv$id), ]
    dat <- as.data.frame(do.call("rbind", strsplit(dat$id, "_")))
    names(dat) <- c(
      "PROVINCE",
      "ecodistrict",
      "breeding_zone2",
      "breeding_zone",
      "provincesEN",
      "provincesFR"
    )
    dat <- dat[, -1]
    dat$ecodistrict <- as.numeric(as.character(dat$ecodistrict))
    x <- SpatialPolygons(
      x,
      proj4string = CRS(
        "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
      )
    )
    x <- SpatialPolygonsDataFrame(x, data = dat, match.ID = FALSE)
    ecodistricts <- x
    rNest:::tk.nesting.calendar(ecodistricts = ecodistricts, lang = lang)
    #winMenuAddItem("rNest","single nest","rNesttest:::tk.single.nest()")
    #winMenuAddItem("rNest","single nest test","rNesttest:::tk.single.nest.test()")
    #winMenuAddItem("rNest","nesting calendar","rNesttest:::tk.nesting.calendar()")
    #winMenuAddItem("rNest","explore data","rNesttest:::tk.data()")
  }
