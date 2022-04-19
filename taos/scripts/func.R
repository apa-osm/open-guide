# display debugging messages in R (if local) 
# and in the console log (if running in shiny)
debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) message(txt)
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}

debug_sprintf <- function(fmt, ...) {
  debug_msg(sprintf(fmt, ...))
}


#' CRediT Roles
#'
#' @param display Whether to display the category names, explanations, or abbreviations
#'
#' @return list of roles
#' @export
#'
#' @examples
#' credit_roles()
credit_roles <- function(display = c("explain", "names", "abbr")) {
  roles <- list(
    "Conceptualization" = "Ideas; formulation or evolution of overarching research goals and aims.",
    "Data curation" = "Management activities to annotate (produce metadata), scrub data and maintain research data (including software code, where it is necessary for interpreting the data itself) for initial use and later re-use.",
    "Formal analysis" = "Application of statistical, mathematical, computational, or other formal techniques to analyse or synthesize study data.",
    "Funding acquisition" = "Acquisition of the financial support for the project leading to this publication.",
    "Investigation" = "Conducting a research and investigation process, specifically performing the experiments, or data/evidence collection.",
    "Methodology" = "Development or design of methodology; creation of models.",
    "Project administration" = "Management and coordination responsibility for the research activity planning and execution.",
    "Resources" = "Provision of study materials, reagents, materials, patients, laboratory samples, animals, instrumentation, computing resources, or other analysis tools.",
    "Software" = "Programming, software development; designing computer programs; implementation of the computer code and supporting algorithms; testing of existing code components.",
    "Supervision" = "Oversight and leadership responsibility for the research activity planning and execution, including mentorship external to the core team.",
    "Validation" = "Verification, whether as a part of the activity or separate, of the overall replication/reproducibility of results/experiments and other research outputs.",
    "Visualization" = "Preparation, creation and/or presentation of the published work, specifically visualization/data presentation.",
    "Writing - original draft" = "Preparation, creation and/or presentation of the published work, specifically writing the initial draft (including substantive translation).",
    "Writing - review & editing" = "Preparation, creation and/or presentation of the published work by those from the original research group, specifically critical review, commentary or revision -- including pre- or post-publication stages."
  )
  
  abbr <- c(
    "con",
    "dat",
    "ana",
    "fun",
    "inv",
    "met",
    "adm",
    "res",
    "sof",
    "sup",
    "val",
    "vis",
    "dra",
    "edi"
  )
  
  if ("explain" == display[1]) {
    for (i in 1:length(roles)) {
      cname <- names(roles)[i]
      cdesc <- roles[[i]]
      paste0("[", i, "/", abbr[i], "] ", cname, ": ", cdesc, "\n") %>%
        cat()
    }
  } else if ("abbr" == display[1]) {
    abbr
  } else {
    names(roles)
  }
}

author_jats <- function(a) {
  contrib_author <- '<contrib>%s
  <string-name>
    <surname>%s</surname>
    <given-names>%s</given-names>
  </string-name>
  %s
</contrib>'
  
  orcid <- check_orcid(a$orcid)
  if (orcid != FALSE) {
    orcid <- paste0('\n  <contrib-id authenticated="true" contrib-id-type="orcid">https://orcid.org/', orcid, '</contrib-id>')
  } else {
    orcid <- ''
  }
  
  contrib_role <- '<role vocab="credit"
    vocab-identifier="http://credit.niso.org/"
    vocab-term="%s"
    vocab-term-identifier="http://credit.niso.org/contributor-roles/%s/">%s</role>'
  dash_roles <- gsub("\\W+", "-", a$roles) %>% tolower()
  
  sprintf(contrib_role,
                   a$roles,
                   dash_roles,
                   a$roles) %>%
    paste(collapse = "\n  ") %>%
    sprintf(contrib_author,
            orcid,
            a$family,
            a$given, .)
}

#' Check validity of ORCiD
#'
#' @param orcid a 16-character ORCiD in bare or URL format
#'
#' @return a formatted 16-character ORCiD or FALSE
#' @export
#'
#' @examples
#' check_orcid("0000-0002-7523-5539")
#' check_orcid("0000-0002-0247-239X")
#' check_orcid("https://orcid.org/0000-0002-0247-239X")
#' check_orcid("0000-0002-0247-2394") # incorrect, return FALSE
check_orcid <- function(orcid) {
  if (is.null(orcid)) { return(FALSE) }
  baseDigits <- gsub("[^0-9X]", "", orcid)
  
  if (nchar(baseDigits) != 16) {
    return(FALSE)
  }
  
  total <- 0
  for (i in 1:(nchar(baseDigits)-1)) {
    digit <- substr(baseDigits, i, i) %>% as.integer()
    total <- (total + digit) * 2
  }
  remainder <- total %% 11;
  result <- (12 - remainder) %% 11;
  result <- ifelse(result == 10, "X", result)
  
  if (result == substr(baseDigits, 16, 16)) {
    paste(substr(baseDigits, 1, 4),
          substr(baseDigits, 5, 8),
          substr(baseDigits, 9, 12),
          substr(baseDigits, 13, 16),
          sep = "-")
  } else {
    return(FALSE)
  }
}



#' Get ORCiD from Name
#'
#' If there is more than one match for the name, the function will return a list of potential matches with more info. Set `info_from` to the indices to return. The default is 1:5 because the lookup can take a few seconds for each potential match, but you can set this to a larger number. If you set `info_from` to a single index, you will only get the ORCiD returned.
#'
#' @param family The family (last) name to search for
#' @param given An optional given (first) name to search for. Initials will be converted from, e.g., L M to L\* M\*
#' @param info_from if there is more than one match, which indices to get info from
#'
#' @return A matching ORCiD or a list of potential author matches
#' @export
#'
#' @examples
#' get_orcid("DeBruine", "Lisa")
#'
get_orcid <- function(family, given = "*", info_from = 1:5) {
  if (is.null(family) || trimws(family) == "") {
    stop("You must include a family name")
  }
  
  if (is.null(given) || trimws(given) == "") {
    given <- "*"
  }
  
  query <- "https://pub.orcid.org/v3.0/search/?q=family-name:%s+AND+given-names:%s"
  
  given2 <- given %>%
    trimws() %>%
    gsub("^(\\w)\\.?$", "\\1\\*", .) %>% # single initial
    gsub("^(.)\\.?\\s", "\\1\\* ", .) %>% # initial initial
    gsub("\\s(.)\\.?$", " \\1\\*", .) %>% # ending initial
    gsub("\\s(.)\\.?\\s", " \\1\\* ", .) %>% # internal initial
    utils::URLencode()
  
  family2 <- trimws(family) %>% utils::URLencode()
  url <- sprintf(query, family2, given2) %>% url("rb")
  on.exit(close(url))
  
  xml <- tryCatch(xml2::read_xml(url), error = function(e) {
    warning("You might not have an internet connection")
    return(list())
  })
  l <- xml2::as_list(xml)
  
  orcids <- sapply(l$search, function(res) {
    res$`orcid-identifier`$path
  }) %>% unlist() %>% unname()
  
  n <- length(orcids)
  if (n == 0) {
    message("No ORCID found for ", given, " ", family)
    return(NULL)
  } else if (n == 1) {
    return(orcids)
  } else if (n > 1) {
    if (length(info_from) == 1 && info_from %in% 1:n) {
      return(orcids[info_from])
    }
    sprintf("Multiple (%d) ORCIDs found for %s %s",
            n, given, family) %>%
      message()
    if (!all(1:n %in% info_from)) {
      isect <- intersect(1:n, info_from)
      if (length(isect) == 0) {
        sprintf("No ORCiD for %s %s with index %d",
                given, family, info_from) %>%
          stop(call. = FALSE)
      }
      orcids <- orcids[isect]
      message("Retrieving info on ORCiDs ",
              paste(isect, collapse = ", "), "...")
    } else {
      message("Retrieving info...")
    }
    
    info <- lapply(orcids, orcid_info)
    return(info)
  }
}

#' Get Info from ORCiD
#'
#' Looks up info on https://pub.orcid.org and returns any info on the author name, location (country only), emails, URLs and keywords. This is useful for disambiguation.
#'
#' @param orcid a 16-character ORCiD in bare or URL format
#'
#' @return a list of info
#' @export
#'
orcid_info <- function(orcid) {
  orcid <- check_orcid(orcid)
  if (isFALSE(orcid)) { stop() }
  
  url <- sprintf("https://pub.orcid.org/v3.0/%s/person", orcid) %>% url("rb")
  on.exit(close(url))
  
  xml <- tryCatch(xml2::read_xml(url), error = function(e) {
    warning("You might not have an internet connection")
    return(list())
  })
  info <- xml2::as_list(xml)
  
  # function to handle duplicate list item names
  pluck_multi <- function(list, name, subname) {
    list[which(names(list) %in% name)] %>%
      sapply(`[[`, subname) %>%
      unlist() %>%
      unname()
  }
  
  # get info
  a <- list(
    surname = info$person$name$`family-name`[[1]],
    given = info$person$name$`given-names`[[1]],
    orcid = orcid,
    email = pluck_multi(info$person$emails, "email", "email"),
    country = pluck_multi(info$person$addresses, "address", "country"),
    url = pluck_multi(info$person$`researcher-urls`, "researcher-url", "url"),
    keywords = pluck_multi(info$person$keywords, "keyword", "content")
  )
  
  # remove NULL items
  a[!sapply(a, is.null)]
}






