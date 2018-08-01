#
# prepar the data as in the GAM_Code.docx file


######@>---------------------------------------------------------------
######@> Preparing Spatial Structure - neiborhoods matrix...

#####@> Method 01...

####@> creating a temporary data.frame...
tmp2 <- select(tmp, lat5, lon5)

####@> identifying and removing duplicated cases...
id <- tmp2[!duplicated(tmp2),]

####@> arrange data by lon and lat...
id <- arrange(id, -lat5, lon5)

####@> creating a spatial identification variable in dataset...
id$ind <- 1:nrow(id)

####@> building spatial matrix...
loc <- cbind(id$lon5, id$lat5)
colnames(loc) <- c("x", "y")

####@> neiborhoods matrix...
neibor <- dnearneigh(loc, 0, 5)

###@> visualizing neibour matrix...
plot(neibor, loc)
axis(1, at = seq(-57.5, 52.5, 5), las = 1)
axis(2, at = seq(-57.5, -12.5, 5), las = 1)
box()
text(x = id$lon5, y = id$lat5, labels = id$ind, cex = 0.8)
abline(v = seq(-57.5, 52.5, 5), h = seq(-57.5, -12.5, 5), lty = 2,
       col = "grey")

#####@> Method 02 - Using a Sparse Matrix...
n.viz <- sapply(neibor, function(x) length(x))
i <- rep(1:length(n.viz), n.viz)
j <- unlist(neibor)
g <- sparseMatrix(i = i, j = j, x = 1)

####@> visualizing sparseMatrix...
image(g)
table(colSums(g) == n.viz)

####@> merge spatial index into tmp model matrix...
tmp <- merge(tmp, id, by = c("lat5", "lon5"))

######@>---------------------------------------------------------------
######@> Preparing Temporal Structure...

######@> creating an index to time series (years)...
tmp$t <- as.numeric(as.character(tmp$year)) - 2011

######@> creating an index to month...
tmp$m <- as.numeric(as.character(tmp$mm))

######@> creating an index to seasons...
tmp$b <- as.numeric(tmp$breading)

######@>---------------------------------------------------------------
######@> Implementing INLA Spatio-Temporal Model...

######@> Building the model...
form <- birds ~ 1 +
  f(t, model = "ar1") +
  f(m, model = "ar1") +
  f(flag, model = "iid") +
  daylight + moonphase +
  f(ind, model = "besag", graph = g.03, group = b,
    control.group = list(model = "iid"))

#####@> Fits...
system.time(
  pois <- inla(form, family = "poisson", data = tmp,
               offset = log(hooks),
               control.fixed = list(expand.factor.strategy = "inla"),
               control.predictor = list(compute = TRUE, link = 1),
               control.compute = list(cpo = TRUE, dic = TRUE,
                                      waic = TRUE))
)