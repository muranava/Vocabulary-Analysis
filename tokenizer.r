filename = "C:/Users/joshua/Dropbox/text/KafkaFranz-Metamorphosis.txt"
ss =readChar(filename, file.info(filename)$size)
base1 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd1.txt", header=FALSE, sep=" ")
#base2 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd2.txt", header=FALSE, sep="\t")
#base3 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd3.txt", header=FALSE, sep="\t")
#base4 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd4.txt", header=FALSE, sep="\t")
#base5 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd5.txt", header=FALSE, sep="\t")
#base6 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd6.txt", header=FALSE, sep="\t")
#base7 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd7.txt", header=FALSE, sep="\t")
#base8 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd8.txt", header=FALSE, sep="\t")
#base9 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd9.txt", header=FALSE, sep="\t")
#base10 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd10.txt", header=FALSE, sep="\t")
#base11 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd11.txt", header=FALSE, sep="\t")
#base12 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd12.txt", header=FALSE, sep="\t")
#base13 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd13.txt", header=FALSE, sep="\t")
#base14 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd14.txt", header=FALSE, sep="\t")
#base15 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd15.txt", header=FALSE, sep="\t")
#base16 = read.table(file="C:/Users/joshua/Dropbox/text/range/basewrd16.txt", header=FALSE, sep="\t")

#This removes all of the punctuation from the text
s1 = gsub(pattern = "[]$*+.?[^{|(\\#%&~_/<=>'!,:;`\")}@-]", replacement = " ", s)
#This changes all of the whitespace characters to spaces and makes all the words uppercase
s2 = toupper(gsub(pattern = "[ \t\n\r\f\v]", replacement = " ", s1))
#Splits words based on spaces
s3 = unlist(strsplit(s2, split=" "))
#Removes all NULL Strings
s4 <- s3[which(s3!="")] 
#Puts into a data.fram
s5= data.frame(table(s4))
#Sets the column names
colnames(s5) = c("word","count")
sum(as.integer(s5$count))


N = nrow(base1)
base1done <- data.frame(cbind(1,1:N,0))
colnames(base1done) = c("root","word","count")

countwords = function (text,basewds) {

	N = nrow(base1)
	base1done <- data.frame(cbind(1,1:N,0))
	colnames(base1done) = c("root","word","count")

#This for loop goes through a baseword list, and generates a newbaseword list with rows (root, word, count of word occurences)
	for (i in 1:N) {
		word = as.character(base1$V1[i])
		if (substr(word,0,1) != "\t") {
			root = word
			print("Root is ")
			print(root)
		}
		word = gsub(pattern = "[ \t\n\r\f\v]", replacement = "", word)
		print("Word is")
		print(word)
		print("Count is")
		#If the word is found in the text, put the count into base1done, otherwise put zero
		if (length(s5$count[s5$word==tolower(word)])!=0) {
			print (s5$count[s5$word==tolower(word)])
			base1done[i,] = list(root, word, s5$count[s5$word==tolower(word)])
		} else {
			print ("not found")
			base1done[i,] = list(root, word, "0")
		}
	}
}
b1 = sum(as.integer(base1done$count))
y = sum(as.integer(s5$count))

results = function(part,total) {
	cat("text is", part, "of", total, "baseword 1 list words, which is", part/total*100, "percent.\n")
}

results(b1,y)

#"[0-9]" - Digits
#"[a-z]" - Lower-case letters
#"[A-Z]" - Upper-case letters
#"[a-zA-Z]" - Alphabetic characters
#"[^a-zA-Z]" - Non-alphabetic characters
#"[a-zA-Z0-9]" - Alphanumeric characters
#"[ \t\n\r\f\v]" - Space characters
#"[]$*+.?[^{|(\\#%&~_/<=>'!,:;`\")}@-]" - Punctuation Characters
data