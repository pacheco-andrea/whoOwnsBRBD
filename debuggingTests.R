# should i be checking if i can actually be using st_difference instead?

# testing whether the st_snap works with a small set of problem polygons


setwd(paste0(wdmain,"/data/processed/processed2/public"))
uc <- st_read("protectedAreas.shp")
# first fix two features which prevented running st_intersection
uc[c(1047,1055),]
uc.p <- uc[which(uc$id == "UC-1047" | uc$id == "UC-1055"),]
uc <- uc[which(uc$id != "UC-1047" & uc$id != "UC-1055"),,] # remove problem polygons from entire dataset
uc.p <- st_simplify(uc.p, preserveTopology = T, dTolerance = 100) # fix the polygons by simplifying
uc <- rbind(uc, uc.p) # add back to the original data

# get the self-overlaps within UCs
plot(uc[c(1047,1055),"geometry"])
uc.overlaps <- st_intersection(uc)


# snap
uc.snapped <- st_snap(uc, uc, tolerance = 0.01)
uc.overlaps <- st_intersection(uc)

# now i'm thinking the test should be for each of the states that the CSR data comes from
# # rural settlements
setwd(paste0(wdmain,"/data/processed/landTenure_IRU-AST-PCT"))
l <- list.files()
d <- lapply(l[grep(".shp", l)], st_read)
unlist(lapply(d, nrow))
# get smallest state
amapa <- d[[4]]
plot(amapa[,"geometry"]) # amapá

int_test <- st_intersection(amapa)

any(is.na(st_dimension(amapa)))
any(is.na(st_is_valid(amapa)))
any(na.omit(st_is_valid(amapa)) == FALSE)

test2 <- amapa %>% st_set_precision(1000) %>% st_intersection() # also doesn't work.

# maybe first running intersects() to identify the problems, and then intersection 
intersects_test <- st_intersects(amapa)
str(intersects_test)
length(intersects_test) # but i don't reaaally understand how to use this output
amapa[intersects_test,]

# THIS WAS THE LATEST TEST ############################################################
test2 <- st_set_precision(amapa, 1000000)
snapping.time <- system.time(test3 <- st_snap(test2, test2, tolerance = 5))
any(is.na(st_dimension(test2)))
any(is.na(st_is_valid(test2)))
any(na.omit(st_is_valid(test2)) == FALSE)
falses <- grep("FALSE", st_is_valid(test3))
test3[falses,]

summary(test3[falses,])

snapped_intersecting.time <- system.time(test4 <- st_intersection(test3[-falses,]))



dAST <- amapa[which(amapa$LTcateg == "AST"),]
int_test <- st_intersection(dAST)
plot(int_test[,"n.overlaps"])

diff_test <- st_difference(dAST)

# i think my continual problem is that i actually WANT the overlapping parts
# the only way that i find to get these is the st_intersection 


ast <- do.call(rbind, ast)
ast
unique(ast$LTcateg)
ast <- ast[which(ast$LTcateg == "AST"),]
ast$id <- paste0("AST-", 1:nrow(ast))
ast <- select(ast, c("LTcateg","id", "geometry"))
setwd(paste0(wdmain,"/data/processed/processed2/public"))
st_write(ast, "ruralSettlements.shp", append = F)
