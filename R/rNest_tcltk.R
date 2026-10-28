rNest_tcltk <-
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
        "For more detailed Avoidance Guidelines, please visit Environment Canada's website at www.ec.gc.ca/paom-itmb",
        "\n",
        "\n",
        "When using rNest in observation mode, it is important to keep in mind that nesting observations from the Project NestWatch database are mostly concentrated in southern and inhabitated regions of Canada. Consequently, not enough information is available in several parts of Canada to obtain a representative portray of the nesting season solely based on nesting observations. Moreover, several species are overrepresented in the data and can have a disproportionate influence on nesting calendars when groups of birds are used. Use caution when interpreting nesting calendars in observation mode as scarce data, species overrepresentation and aberrant or extreme nesting events may severely affect their reliability."
      )
      loadingmsglabl = "Data and rNest interface loading..."
    } else {
      warninglabl <- "Avertissement"
      warningtextlabl <- paste0(
        "Les renseignements techniques pr\u00e9sent\u00e9s dans rNid ne le sont qu\u2019\u00e0 titre de conseils. Toute personne doit se conformer \u00e0 l'ensemble des exigences pertinentes des lois (par exemple les lois provinciales ou territoriales), des r\u00e8glements et des permis, y compris, entre autres, la Loi de 1994 sur la convention concernant les oiseaux migrateurs (LCOM) et le R\u00e8glement sur les oiseaux migrateurs (ROM). Il est important de noter que certaines esp\u00e8ces d'oiseaux prot\u00e9g\u00e9s en vertu de la LCOM ont \u00e9galement \u00e9t\u00e9 inscrites \u00e0 l'annexe 1 de la Loi sur les esp\u00e8ces en p\u00e9ril (LEP). Ces esp\u00e8ces sont prot\u00e9g\u00e9es \u00e0 la fois en vertu de LCOM et de la LEP. Ces conseils ne donnent pas l'autorisation de blesser ou de tuer des oiseaux migrateurs, ni de d\u00e9ranger, de d\u00e9truire ou de prendre des nids ou des \u0153ufs en vertu du ROM, et ils n'offrent pas non plus la garantie que les activit\u00e9s ne donnent pas lieu \u00e0 une violation ROM ou d'autres lois et r\u00e8glements. Les renseignements pr\u00e9sent\u00e9s ici sont de nature g\u00e9n\u00e9rale et ne doivent pas servir d'avis officiel au sujet des cons\u00e9quences juridiques de toute activit\u00e9 particuli\u00e8re. Ils ne remplacent pas la LCOM, le ROM ou toute autre loi.",
        "\n",
        "\n",
        "La responsabilit\u00e9 incombe aux particuliers et aux entreprises d\u2019\u00e9valuer leur propre niveau de risque en tenant compte des oiseaux migrateurs en pr\u00e9sence et d\u2019\u00e9tablir les mesures pertinentes d\u2019\u00e9vitement et d\u2019att\u00e9nuation appropri\u00e9es. Il est possible que localement la p\u00e9riode de nidification commence et se termine plus t\u00f4t ou plus tard que les dates fournies dans rNid en raison de conditions microclimatiques particuli\u00e8res \u00e0 certains lieux (p. ex. milieux c\u00f4tiers ou en altitude), ou en raison de variations climatiques interannuelles (p. ex. printemps h\u00e2tif, \u00e9t\u00e9 froid et pluvieux). Les renseignements techniques publi\u00e9s dans rNid seront mis \u00e0 jour \u2013 et possiblement modifi\u00e9s \u2013 \u00e0 mesure que de nouvelles donn\u00e9es seront disponibles. Chaque fois que des nids contenant des \u0153ufs ou des oisillons sont d\u00e9couverts, la zone imm\u00e9diate doit \u00eatre \u00e9vit\u00e9e jusqu'\u00e0 ce que les oisillons aient quitt\u00e9 naturellement l\u2019entourage imm\u00e9diat du nid. Cette mesure de protection doit \u00eatre appliqu\u00e9e m\u00eame si le nid a \u00e9t\u00e9 trouv\u00e9 en dehors de la p\u00e9riode g\u00e9n\u00e9rale de nidification fournie par rNid. Une fois hors du nid, les oisillons demeurent vuln\u00e9rables et il est recommand\u00e9 d\u2019agir avec pr\u00e9caution.",
        "\n",
        "\n",
        "Pour obtenir de plus amples renseignements, veuillez consulter le site Web d\u2019Environnement Canada \u00e0 l\u2019adresse suivante : www.ec.gc.ca/paom-itmb.",
        "\n",
        "\n",
        "Lorsque rNid est utilis\u00e9 en mode observation, il est important de mentionner que les observations de nids contenues dans la base de donn\u00e9es NestWatch sont concentr\u00e9es dans le sud et les r\u00e9gions habit\u00e9es du Canada. Par cons\u00e9quent, il n'y a pas assez d'information dans plusieurs parties du Canada pour obtenir un portrait repr\u00e9sentatif de la saison de nidification en se basant uniquement sur les observations de nids. De plus plusieurs esp\u00e8ces sont surrepr\u00e9sent\u00e9es dans les donn\u00e9es et peuvent avoir une influence disproportionn\u00e9e sur les calendriers de nidification lorsque des groupes sont utilis\u00e9s. En mode observation, les calendriers de nidification doivent \u00eatre interpr\u00e9t\u00e9s avec prudence en raison du manque de donn\u00e9es, des esp\u00e8ces surrepr\u00e9sent\u00e9es et des observations de nidification aberrantes ou extr\u00eames qui peuvent s\u00e9v\u00e8rement affecter leur fiabilit\u00e9."
      )
      loadingmsglabl = "Chargement des donn\u00e9es et de l'interface rNid..."
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
