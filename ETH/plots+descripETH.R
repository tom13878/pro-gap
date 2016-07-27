# Ethiopia exploratory analysis as you go
library(reshape2)
library(dplyr)

x <- left_join(HH13, location)

# basic counts -> match the codebook
count1 <- group_by(location, REGNAME, ZONENAME, type) %>% summarise(n=n()) %>%
  dcast(REGNAME + ZONENAME ~ type, fill=0)
names(count1)[3:4] <- paste(names(count1)[3:4], "HH", sep=" ")
count2 <- select(location, -household_id, -household_id) %>% unique %>%
  group_by(REGNAME, ZONENAME, type) %>% summarise(n=n()) %>%
  dcast(REGNAME + ZONENAME ~ type, fill=0)
names(count2)[3:4] <- paste(names(count2)[3:4], "EA", sep=" ")
count <- left_join(count1, count2)
count <- count[c(1, 2, 5, 3, 6, 4)]

# religion
round(table(x$REGNAME, x$religion)/rowSums(table(x$REGNAME, x$religion))*100,2)
round(table(x$religion, x$REGNAME)/rowSums(table(x$religion, x$REGNAME))*100,2)
round(table(x$religion, x$type)/rowSums(table(x$religion, x$type))*100,2)
round(table(x$religion, x$marital)/rowSums(table(x$religion, x$marital))*100,2)
round(table(x$religion, x$cage)/rowSums(table(x$religion, x$cage))*100,2)
round(table(x$religion, x$literate)/rowSums(table(x$religion, x$literate))*100,2)
round(table(x$religion, x$ed_any)/rowSums(table(x$religion, x$ed_any))*100,2)
group_by(x, religion) %>% summarise(family_size=mean(family_size, na.rm=TRUE))
round(table(x$religion, x$death)/rowSums(table(x$religion, x$death))*100,2)

# check out the ten most grown crops
crtab <- table(oput$crop_code)
crtab <- crtab[order(crtab, decreasing=TRUE)]
pos <- as.integer(dimnames(crtab)[[1]][1:10])
oput2 <- oput[oput$crop_code %in% pos,]
oput2 <- left_join(oput2, location)
round(table(oput2$crop_name, oput2$REGNAME)/rowSums(table(oput2$crop_name, oput2$REGNAME))*100,2)

# and the quantities produced
tot <- group_by(oput2, REGNAME, crop_name) %>% summarise(tot=sum(crop_qty_harv))
round(xtabs(tot ~ crop_name + REGNAME, data=tot)/rowSums(xtabs(tot ~ crop_name + REGNAME, data=tot))*100, 2)

