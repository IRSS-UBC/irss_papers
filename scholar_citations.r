library(scholar); library(data.table); library(tidyverse); library(pbapply)

members = fread("D:/SYNC/Sync/silva21/scholar_citations/IRSS_members - Sheet1.csv", encoding = "UTF-8") %>%
          select(firstname, lastname, left) %>%
          distinct(firstname, lastname, .keep_all = T) %>%
          filter(!firstname %in% c("Zhiyun (George)", "Yu", "Juhan"))
members$left[members$left == ""] <- "02.07.2023"

members = mutate(members, leftyr = as.numeric(str_sub(left, start = -4))) %>% 
          arrange(desc(leftyr)) %>% 
          select(firstname, lastname)

bad.words.journal = c("conference", "abstract", "igarss", "proceedings", "silvilaser", "university", 
                      "institute of technology", "rmit", "csiro")

pbs = get_publications("XvUF8dUAAAAJ") %>% 
      as.data.table() %>% 
      mutate(journal = tolower(journal)) %>%
      filter(!is.na(year),
             !is.na(cid),
             !journal %in% c("", "# placeholder_parent_metadata_value#", "the"),
             str_count(journal, paste0(bad.words.journal, sep = "", collapse = "|")) == 0) %>% 
      distinct(title, .keep_all = T)

journal.counts = pbs %>%
                 # filter(!is.na(year)) %>%
                 group_by(journal) %>% summarize(n.pubs = n(), last.pub = max(year, na.rm = T),
                                                 med.cites = median(cites)) %>%
                 filter(!journal %in% c("", "# placeholder_parent_metadata_value#", "the"),
                        str_count(journal, paste0(bad.words.journal, sep = "", collapse = "|")) == 0,
                        n.pubs > 1)

contact = pbs %>% 
          mutate(irss.count = str_count(pbs$author, paste0(members$lastname, collapse = "|"))) %>% 
          filter(irss.count != 0) %>% 
          group_by(journal) %>%
          filter(year == max(year, na.rm = T)) %>% 
          distinct(journal, .keep_all = T) %>%
          mutate(ath.last = str_extract(author, paste0(members$lastname, collapse = "|"))) %>% 
          as.data.table() %>%
          select(journal, title, author, year, ath.last) %>% 
          left_join(journal.counts, by = "journal")

journal.mets = get_journalrank(journals = contact$journal, max.distance = 0.01) %>% as.data.table() %>% 
               select(journal.name = Journal, SJR, SJR.Best.Quartile, H.index, citations.2yr = Cites...Doc...2years., Categories)

# out.dt = cbind(journal.mets, contact) %>% 
out.dt = contact %>% 
         # select(-c(journal)) %>%
         left_join(members, by = c("ath.last" = "lastname")) %>% 
         unite(col = recent.author, firstname, ath.last, sep = " ") %>%
         na.omit() %>%
         # select(journal:citations.2yr, Categories, median.irss.cites = med.cites, n.irss.pubs = n.pubs, last.irss.pub = year,
                # last.irss.author = recent.author, author.list = author, last.paper.title = title) %>%
         as.data.table()

fwrite(out.dt, "D:/SYNC/Sync/silva21/scholar_citations/journal_list_08_01_25_v3.csv")

dt.t = pbs %>% 
  filter(!is.na(cid),
         !journal %in% c("", "# placeholder_parent_metadata_value#", "the"),
         str_count(journal, "university") == 0) %>% 
  pull(title) %>% 
  str_split(" ", simplify = T) %>% 
  .[,1] %>% 
  as.data.table() %>% 
  setNames("firstWord") %>% 
  group_by(firstWord) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(pct = n/sum(n)*100) %>% 
  arrange(desc(n))

mutate(dt.t, ing = str_sub(firstWord, start = -3)) %>% 
  mutate(ing = case_when(ing == "ing" ~ 1,
                         .default = 0)) %>% 
  mutate(n.ing = n*ing) %>% 
  pull(n.ing) %>% 
  sum()

pbs %>% 
  filter(!is.na(cid),
         !journal %in% c("", "# placeholder_parent_metadata_value#", "the"),
         str_count(journal, "university") == 0) %>% 
  pull(title) %>% 
  str_split(" ") %>% 
  unlist() %>% 
  tolower() %>% 
  data.table(w = .) %>% 
  group_by(w) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  as.data.table() %>% 
  filter(!w %in% c("of", "and", "in", "a", "the", "for", "to", "with", "on", "using", "from", "an")) %>% 
  head(10)

titles = pbs %>% 
         filter(!is.na(cid),
               !journal %in% c("", "# placeholder_parent_metadata_value#", "the"),
               str_count(journal, "university") == 0) %>% 
         pull(title) %>% 
         tolower() %>% 
         str_remove_all("[:digit:]|[:punct:]")

dt.w = titles %>% 
     str_split(" ") %>% 
     unlist() %>% 
     tolower() %>% 
     str_remove_all("[:digit:]|[:punct:]") %>% 
     data.table(w = .) %>% 
     group_by(w) %>% 
     tally() %>% 
     arrange(desc(n)) %>% 
     as.data.table() %>% 
     filter(!w %in% c("of", "and", "in", "a", "the", "for", "to", "with", "on", "using", "from", "an")) %>% 
     head(10)


lapply(tolower(titles) %>% paste0(., " "), function(x){str_count(x, paste0(dt.w$w, " ")) %>% 
                                                       setNames(dt.w$w)}) %>% 
  bind_rows() %>% 
  mutate_all(.funs = function(x){case_when(x >= 1 ~ 1, .default = 0)}) %>% 
  colSums() %>% 
  `/`(length(titles))*100 %>% 
  round(2)









