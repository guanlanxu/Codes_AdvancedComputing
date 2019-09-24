
encrypt <- function(string) {
  if (class(string) != "character"){
    stop("Expected character input")
  }
  strToBit <- as.numeric(rawToBits(charToRaw(string)))
  ran_bit <- rbinom(n= length(strToBit),prob = 0.5, size = 1)
  E <- packBits(xor(strToBit,ran_bit))
  
  return (list(encryptedString = rawToChar(E),
               key = packBits(ran_bit)))
}


decrypt <- function(enc){
  if (class(enc) != "list") {
    stop("Expected list")
  }
  if (!("encryptedString" %in% names(enc)) || !("key" %in% names(enc))){
    stop("Need both the encrypted string and the key")   
  }
  originalString <- rawToChar(xor(
    charToRaw(enc$encryptedString),
    enc$key
  ))
  return(originalString)
}

# Test the function
my_message <- "Encryption is neat, but this isn't it."
encrypted <- encrypt(my_message)
decrypted <- decrypt(encrypted)

if (my_message != decrypted){
  stop("Decryption doesn't match")
}
if (encrypted$encryptedString == my_message){
  warning("This code doesn't appear to be doing any actual encrypting...")
}
