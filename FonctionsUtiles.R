# Quelques fonctions utiles

"H2J" = function(data, stat="max", arrondi.entier = T)
{
	nbcol <- ncol(data)
	if(is.null(nbcol))
		v <- statloc(matrix(data, nrow = 24.), stat=stat)
	else {
		v <- NULL
		for(ic in 1.:nbcol)
			v <- cbind(v, statloc(matrix(data[, ic], nrow = 24.), stat=stat))
	}
	if(arrondi.entier) {
		nbcol <- ncol(v)
		if(is.null(nbcol)) {
			ind.demi <- (v %% 1.) == 0.5
			v[ind.demi & !is.na(v)] <- 1. + (v[ind.demi & !is.na(v)] %/% 1.)
			v[!ind.demi & !is.na(v)] <- round(v[!ind.demi & !is.na(v)])
		}
		else {
			for(ic in 1.:nbcol) {
				ind.demi <- (v[, ic] %% 1.) == 0.5
				v[ind.demi & !is.na(v[, ic]), ic] <- 1. + (v[ind.demi & !is.na(v[, ic]), ic] %/% 1.)
				v[!ind.demi & !is.na(v[, ic]), ic] <- round(v[!ind.demi & !is.na(v[, ic]), ic])
			}
		}
	}
	v
}
 
"statloc" = function(x, p = 0.75, stat = "mean", ...)
{
	if(is.null(dim(x)))
		v <- ifelse(sum(is.na(x)) > ((1. - p) * length(x)), NA, apply(matrix(x, ncol = 1.), 2., stat, na.rm = T, ...))
	else {
		ind <- apply(is.na(x), 2., sum)
		v <- apply(x, 2., stat, na.rm = T, ...)
		v[ind > ((1. - p) * dim(x)[1.])] <- NA
	}
	v
}

