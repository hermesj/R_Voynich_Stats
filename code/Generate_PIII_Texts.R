pIIIciphers = read.table(file="resources/ciphers/ciphers_poly_III_in_order.csv", sep=",", header = TRUE)
pIIIciphers <- as.data.frame(apply(pIIIciphers,2,function(x)gsub('\\s+', '',x)))
summary(pIIIciphers)
nrow(pIIIciphers)
ncol(pIIIciphers)

pIIIciphers[1,1]

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

toEncipher <- "Das soll ein Testtext sein, der j und v und außerdem Ümläute und Großbuchstaben enthält"

toEncipher <- stringi::stri_trans_general(toEncipher, "de-ASCII; Latin-ASCII")
toEncipher <- gsub(pattern="v", replacement = "u", toEncipher)
toEncipher <- gsub(pattern="j", replacement = "i", toEncipher)
toEncipher <- tolower(toEncipher)

tESplits <- strsplit(toEncipher,"")[[1]]


characterMapping <- data.frame(Chars = colnames(pIIIciphers), Numbers = (1:24))

for(i in tESplits){
  print(i)
  number = characterMapping$Numbers[characterMapping$Chars==i]
  
  if(is.integer0(number))
    next
  print(number)
  random = sample(1:24,1)
  print(random)
  cipher <- pIIIciphers[number, random]
  print(cipher)
  encoded <- paste(encoded, cipher)
}

encoded <- trimws(encoded)

