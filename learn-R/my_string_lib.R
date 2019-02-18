#String-Manipulation Functions
#===================================

#Given a list of names
names <- c("Ashish Kumar", "Ayush Singh", "Ashna Kumari", "Mohan Mahto", "Rohit Kunwal", "Asha Thakur", "Akshay Kappor")

#Print all the strings that begin with ‘Ash’.
# 1. substr(x, start=n1, end=n2)
substr(names,1,3)
which(substr(names, 1, 3) == "Ash")
names[which(substr(names, 1, 3) == "Ash")]

# 2. strsplit(x, split)
strsplit(names," ")

# 3. paste(x, sep)
paste(names, " ",date())

# 4. toupper(str)
toupper(names)

# 5. tolower(str)
tolower(names)

# 6. nchar(str)
nchar(names)

# 7. grep(pattern, x, value = FALSE)
grep("Ash", names) # returns position
grep("Ash", names, value = TRUE) # returns value
grepl("Ash", names) # returns boolean

# 8. sub(pattern, replacement, x)
sub(" ", "&", names) # replaces pattern with replacement (once)
gsub("A", "MM", names) # replaces pattern with replacement (global)

# 9. regexpr(pattern, x)
regexpr("As",names)
gregexpr("As",names)

#Reversing the string
rev_string <- paste(rev(strsplit(names[1],'')[[1]]), collapse = '')
