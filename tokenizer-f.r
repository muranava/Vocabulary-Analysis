filename = "C:/Users/joshua/Dropbox/text/KafkaFranz-Metamorphosis.txt"
s =readChar(filename, file.info(filename)$size)
base1 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd1.txt", header=FALSE, sep=" ")
base2 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd2.txt", header=FALSE, sep=" ")
base3 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd3.txt", header=FALSE, sep=" ")
base4 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd4.txt", header=FALSE, sep=" ")
base5 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd5.txt", header=FALSE, sep=" ")
base6 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd6.txt", header=FALSE, sep=" ")
base7 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd7.txt", header=FALSE, sep=" ")
base8 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd8.txt", header=FALSE, sep=" ")
base9 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd9.txt", header=FALSE, sep=" ")
base10 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd10.txt", header=FALSE, sep=" ")
base11 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd11.txt", header=FALSE, sep=" ")
base12 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd12.txt", header=FALSE, sep=" ")
base13 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd13.txt", header=FALSE, sep=" ")
base14 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd14.txt", header=FALSE, sep=" ")
base15 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd15.txt", header=FALSE, sep=" ")
base16 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd16.txt", header=FALSE, sep=" ")

#This removes all of the punctuation from the text
s1 = gsub(pattern = "[]$*+.?[^{|(\\#%&~_/<=>'!,:;`\")}@-]", replacement = " ", s)
#This changes all of the whitespace characters to spaces and makes all the words uppercase
s2 = toupper(gsub(pattern = "[ \t\n\r\f\v]", replacement = " ", s1))
#Splits words based on spaces
s3 = unlist(strsplit(s2, split=" "))
#Removes all NULL Strings
s4 <- s3[which(s3!="")] 
#Puts into a data.frame
s5= data.frame(table(s4))
#Sets the column names
colnames(s5) = c("word","count")
sum(as.integer(s5$count))

countwords = function (text,basewds) {

	N = nrow(basewds)
	base1done <- data.frame(cbind(1,1:N,0))
	colnames(base1done) = c("root","word","count")

	#This for loop goes through a baseword list, and generates a newbaseword list with rows (root, word, count of word occurences)
	for (i in 1:N) {
		word = as.character(basewds$V1[i])
		if (substr(word,0,1) != "\t") {
			root = word
			#cat("Root is",root)
		} else {
			word = substr(word,2,nchar(word))
		}
		#If the word is found in the text, put the count into base1done, otherwise put zero
		if (length(text$count[text$word==toupper(word)])!=0) {
			#cat("Word is", word,".  Count is",text$count[text$word==toupper(word)],".\n")
			base1done[i,] = list(root, word, text$count[text$word==toupper(word)])
		} else {
			#cat("Word is", word,".  Not found.\n")
			base1done[i,] = list(root, word, "0")
		}
	}
	return(base1done)
}

base1done = countwords(s5,base1)
base2done = countwords(s5,base2)
base3done = countwords(s5,base3)
base4done = countwords(s5,base4)
base5done = countwords(s5,base5)
base6done = countwords(s5,base6)
base7done = countwords(s5,base7)
base8done = countwords(s5,base8)
base9done = countwords(s5,base9)
base10done = countwords(s5,base10)
base11done = countwords(s5,base11)
base12done = countwords(s5,base12)
base13done = countwords(s5,base13)
base14done = countwords(s5,base14)
base15done = countwords(s5,base15)
base16done = countwords(s5,base16)

b1 = sum(as.integer(base1done$count))
b2 = sum(as.integer(base2done$count))
b3 = sum(as.integer(base3done$count))
b4 = sum(as.integer(base4done$count))
b5 = sum(as.integer(base5done$count))
b6 = sum(as.integer(base6done$count))
b7 = sum(as.integer(base7done$count))
b8 = sum(as.integer(base8done$count))
b9 = sum(as.integer(base9done$count))
b10 = sum(as.integer(base10done$count))
b11 = sum(as.integer(base11done$count))
b12 = sum(as.integer(base12done$count))
b13 = sum(as.integer(base13done$count))
b14 = sum(as.integer(base14done$count))
b15 = sum(as.integer(base15done$count))
b16 = sum(as.integer(base16done$count))

y = sum(as.integer(s5$count))

results = function(part,total,q) {
	cat("text is", part, "of", total, "baseword", q,"list words, which is", part/total*100, "percent.\n")
}

results(b1,y,1)
results(b2,y,2)
results(b3,y,3)
results(b4,y,4)
results(b5,y,5)
results(b6,y,6)
results(b7,y,7)
results(b8,y,8)
results(b9,y,9)
results(b10,y,10)
results(b11,y,11)
results(b12,y,12)
results(b13,y,13)
results(b14,y,14)
results(b15,y,15)
results(b16,y,16)

#"[0-9]" - Digits
#"[a-z]" - Lower-case letters
#"[A-Z]" - Upper-case letters
#"[a-zA-Z]" - Alphabetic characters
#"[^a-zA-Z]" - Non-alphabetic characters
#"[a-zA-Z0-9]" - Alphanumeric characters
#"[ \t\n\r\f\v]" - Space characters
#"[]$*+.?[^{|(\\#%&~_/<=>'!,:;`\")}@-]" - Punctuation Characters
data