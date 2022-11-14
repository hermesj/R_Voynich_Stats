 
# Read PIII cipher table from file, initialize character mapping
pIIIciphers = read.table(file="resources/ciphers/ciphers_poly_III_in_order.csv", sep=",", header = TRUE)
pIIIciphers <- as.data.frame(apply(pIIIciphers,2,function(x)gsub('\\s+', '',x)))
characterMapping <- data.frame(Chars = colnames(pIIIciphers), Numbers = (1:24))
cat("Letters: ", ncol(pIIIciphers))
cat("Synonyms: ", nrow(pIIIciphers))

# Helper function
is.integer0 <- function(x){
  is.integer(x) && length(x) == 0L
}

# Test text with a lot of characters to replace 
toEncipher <- "This paper demonstrates the existence of a cipher method in the early modern period (Polygraphia III by Johannes Trithemius), which -- applied as a random procedure -- is able to produce a text that can mimic the oblique properties of the so-called Voynich Manuscript (VMS). This result is quite exciting since it brings back into play highly-debated approaches claiming the existence of hidden comprehensible information within the text of the VMS (which is often referred to as Voynichese). The paper briefly outlines some of the most salient and difficult-to-explain statistical properties of Voynichese, shows how Trithemius stepwise developed a cipher system whose application looks like an artificial language, points out how an application of this cipher generates a text that comes very close to the statistical properties of Voynichese and finally discusses possible starting points of cryptoanalytic attacks on a cipher that operates similar to the Polygraphia III encryption. "
toEncipher <- stringi::stri_trans_general(toEncipher, "de-ASCII; Latin-ASCII")
toEncipher <- gsub(pattern="v", replacement = "u", toEncipher)
toEncipher <- gsub(pattern="j", replacement = "i", toEncipher)
toEncipher <- tolower(toEncipher)

# Split of the plain text into its chars,  
tESplits <- strsplit(toEncipher,"")[[1]]
encoded <- ""
# encoding
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

# todo: decoding