library(jsonlite)
library(ggplot2)
library(dplyr)

reportDate <- Sys.Date()

getRanks <- function(pageNo) {
  rank <- readLines(paste0("https://www.kaggle.com/rankings.json?group=competitions&page=", pageNo))
  fromJSON(rank)$list
}

ranksList <- lapply(1:5, getRanks)
ranksDf <- do.call(rbind, ranksList)

getProfile <- function(userUrl) {
  setTxtProgressBar(pb, grep(userUrl, ranksDf$userUrl))
  # cat(substr(userUrl, 2, nchar(userUrl)), "\n")
  fullUrl <- sprintf("https://www.kaggle.com%s/account", userUrl)
  page <- try(readLines(fullUrl, encoding = "UTF-8"))
  json <- gsub("^.*Kaggle\\.State\\.push\\((.*?)\\);performance.*$", "\\1", page[102])
  profile <- fromJSON(json)
  # country <- if (is.null(country)) NA else country
}

pb <- txtProgressBar(max = length(ranksDf$userUrl), style = 3)
profiles <- lapply(ranksDf$userUrl, getProfile)
close(pb)

save(profiles, file = "KaggleProfiles.RData")

getAch <- function(profile) {
  ach <- profile$achievement
  ach <- if (is.null(ach)) NA else ach
}
achievements <- lapply(profiles, getAch)

getOrg <- function(profile) {
  org <- profile$organization
  org <- if (is.null(org)) NA else org
}

getCountry <- function(profile) {
  country <- profile$country
  country <- if (is.null(country)) NA else country
}

organizatoins <- lapply(profiles, getOrg)
countries <- lapply(profiles, getCountry)

ranksDf$orgs <- unlist(organizatoins)
ranksDf$country <- unlist(countries)

ranksDf$country[ranksDf$country %in% c("Россия", "Russian Federation", "RU")] <- "Russia"
ranksDf$country[ranksDf$country %in% c("中国", "CN")] <- "China"
ranksDf$country[ranksDf$country %in% c("台灣")] <- "Taiwan"
ranksDf$country[ranksDf$country %in% c("LT")] <- "Lithuania"
ranksDf$country[ranksDf$country %in% c("GR")] <- "Greece"
ranksDf$country[ranksDf$country %in% c("AT")] <- "Austria"
ranksDf$country[ranksDf$country %in% c("IL")] <- "Israel"
ranksDf$country[ranksDf$country %in% c("JP")] <- "Japan"
ranksDf$country[ranksDf$country %in% c("LV")] <- "Latvia"
ranksDf$country[ranksDf$country %in% c("US", "USA")] <- "United States"

ranksDf$country[ranksDf$orgs %in% c("Brazilian Court of Audit")] <- "Brazil"


write.csv2(ranksDf, "ranksKaggleOrgs.csv", row.names = FALSE)

ranksDfnoNA <- ranksDf[!is.na(ranksDf$country),]
ranksDfOrgnoNA <- ranksDf[!is.na(ranksDf$orgs),]

ranksGr <- ranksDfnoNA %>%
  group_by(country) %>%
  summarise(usersQty = n(), avgRank = mean(currentRanking)) %>%
  arrange(desc(usersQty), avgRank) %>%
  mutate(fct_country = factor(country, levels = country))


ranksGrOrg <- ranksDfOrgnoNA %>%
  group_by(orgs) %>%
  summarise(usersQty = n(), avgRank = mean(currentRanking)) %>%
  arrange(desc(usersQty), avgRank) %>%
  mutate(fct_org = factor(orgs, levels = orgs))


g <- ggplot(data = ranksGr[1:20,]
            , aes(x = fct_country
                  , y = usersQty))
g <- g + geom_bar(stat = "identity", aes(fill = avgRank))
g <- g + labs(title=sprintf("Kaggle Top-100 by Countries on %s (First 20 Countries)"
                            , reportDate)
              , x="Country"
              , y="Number of Users in Top-100")
g <- g + scale_fill_continuous(name = "Average\nRank.\nLess is\nbetter")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)
               , plot.title = element_text(hjust = 0.5))
g
