
# step 1 - make sure your data is read in correctly and is a data.frame with the numbers treated as factors

# step 2 - write a for loop to grab each pair of columns, 2 by 2

# step 3 - calculate a model matrix for each 1st & 2nd column, then add the two

# step 4 - glom all the matrices together to make your new shit.

#crap  <- read.csv("~/Documents/RadishData/MarkerData/TransSSR.csv", colClasses=rep( "factor", 4 ), header=F)

crap  <- read.csv("~/Dropbox/RadishWeedEvolution/Israel_Spain/JustSSR2014.txt", colClasses=rep( "factor", 4 ), header=T)

str(crap)

output <- list()
n.SSR <- ncol( crap )/2
col.even <- seq( 2, ncol( crap ), 2 )
col.odd <- seq( 1, ncol( crap ), 2 )
dummy <- rnorm( nrow( crap ), 0, 1 )

for (i in seq(n.SSR) ){
	col.A <- crap[,col.odd[i]]
	col.B <- crap[,col.even[i]]
	print(nlevels(col.A))
	print(nlevels(col.B))
	}
	
for (i in seq(n.SSR) ){
	col.A <- crap[,col.odd[i]]
	col.B <- crap[,col.even[i]]
	mod.1 <- lm( dummy ~ 0 + col.A )
	mod.2 <- lm( dummy ~ 0 + col.B )
	output[[i]] <- model.matrix( mod.1 ) + model.matrix( mod.2 )
	names( output )[[i]] <- paste( names(crap)[col.odd[i]])
}

for (i in seq(n.SSR) ){
	dimnames(output[[i]])[[2]] <- sub( "col", names(output)[[i]], dimnames(output[[i]])[[2]] )
}

new.crap <- data.frame( do.call( "cbind", output ))

head(new.crap)

write.csv(new.crap, "~/Dropbox/RadishWeedEvolution/Israel_Spain/JustSSRbiallele2014.txt", row.names=F)