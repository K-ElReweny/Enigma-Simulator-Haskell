import AssignmentHelp
import Data.List
import Data.Char

-- Written by Karim ElReweny , ksaelreweny1@shefffield.ac.uk , mea18kse



-- validateCipher : takes cipher message as parameter , returns True if Cipher contains each letter once and once only.
alphabet = ['A'..'Z']
validateCipher :: Cipher -> Bool 
validateCipher cipher = if (sort cipher) == alphabet then True else False  -- check if the sorted cipher is equal to alphapet hence no repeated letters.


--offsetCipher : takes offset and a cipher as parameters, returns the Cipher after it moved n position to the right

offsetCipher :: Cipher -> Int -> Cipher
offsetCipher cipher offset = drop (26 - offset `mod` 26) cipher ++ take (26 - offset `mod` 26) cipher -- mod is used for offsets > 26


--encode : takes a cipher, an offset and a character, and returns the corresponding encoded character.
encode :: Cipher -> Int -> Char -> Char
encode cipher offset char = (offsetCipher cipher offset) !! (alphaPos char) 


--encodeMessage: takes a cipher , an offset , and a message (String) and returns encoded message
encodeMessage :: Cipher -> Int -> String -> String
encodeMessage cipher offset message = [ encode cipher offset char | char <- message]  -- for every char drawn from message , encode is called



{- reverseEncode: takes a cipher, an offset and an encoded character, and returns the plain character
    CREDITS TO DR EMMA NORLING FOR USING HER VERSION OF THIS FUNCTION FOR BEING MORE EFFICIENT
-}

reverseEncode :: Cipher -> Int -> Char -> Char
reverseEncode cipher offset encodedChar = ['A' .. 'Z'] !! (head (elemIndices encodedChar (offsetCipher cipher offset)))

--reverseEncodeMessage: takes a cipher, an offset and an encoded message and return the plain text message

reverseEncodeMessage :: Cipher -> Int -> String -> String
reverseEncodeMessage cipher offset encodedMessage = [ reverseEncode cipher offset encodedChar| encodedChar <- encodedMessage ]

--countLetters : takes message and char as input , return number of instances of that char in message , its used in letterStats
countLetters :: Char -> String -> Int
countLetters char message = length [ c | c <- message , c == char]

-- removeZero : takes a list of tuples and if the second element in tuple is zero it removes the tuple
removeZero :: [(Char , Int)] -> [(Char , Int)]
removeZero tupleList = [ (x , y )| (x , y) <- tupleList , y /= 0  ]

{-

 letterStats: takes a message, returns the percentage of each letter occurring in this message as a list of (Char, Int) tuples

 This function uses list comprehension to do a list of tuples of chars and their percentage in string , then sortOn function 
 is used to sort them asceendingly according to percentage and reverse is used to get list in descending order 

 -}

letterStats :: String -> [(Char , Int)]
letterStats message = removeZero (reverse (sortOn snd [ (char , percent (countLetters char message) (length message) )  | char <- (nub message)] ) )

{-
    replaceLetter: takes 2 parameters , tuple and String and for every charachter in string it searches if its the encoded letter
    in the tuple and if so it replaces it with the lower case of plain letter
-}
replaceLetter :: (Char , Char) -> String -> String
replaceLetter (p , e) [] = []
replaceLetter (p , e) (x:xs) = 
     if x == e                                 -- e is encoded letter (second element in tuple)
     then toLower p : replaceLetter (p , e) xs  --p is plain letter (first element in tuple)
     else  x : replaceLetter (p , e) xs


{-
    partialDecode: takes a List of Tuples and String as parameter , replaces letter if it's in encoded part of any tuple by 
    calling the replaceLetter Function but for every Tuple.
-}

partialDecode :: [(Char , Char)] -> String -> String
partialDecode [] message = message
partialDecode (y:ys) message = replaceLetter y (partialDecode ys message)

{- itseasytobreakasubstitutioncipherprovidedyouhavealongenoughmessagestopletsmakethisonealittlebitlongerstopokitshouldbetherightsortofsizenowstop
maybenotletsincreasethemessagelengthabitmorestopkeepthismessagesecretorshareifyouwantthewholeclasstogetthebonusmarksstop -}

{- my strategy to decrypt message was to find the letters related to STOP and then use letter stats to find frequent letters 
in string and replace with frequent letters in english , also try to find the TH and with alot of trial and error i got my message :))

-}

myGuess = [('S' , 'A') , ('T' , 'J') , ('O' , 'F') , ('P' , 'V') , ('I' , 'Q') , ('H' , 'C') , ('E' , 'W') , ('A' , 'X') , ('M' , 'P') , ('G' , 'M') , ('Y' , 'R') , ('C' , 'L') , ('N' , 'Y'), ('V' , 'K') , ('D' , 'H') ,('R' , 'E') , ('L' , 'N'), ('U' , 'D') , ('K' , 'Z') , ('B' , 'B') , ('F' , 'T') , ('W' , 'S') , ('Z' , 'U')]


------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------ASSIGNMENT 2 START-------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

-- define a type for Rotor , Reflector and set of offsets
type Rotor = Cipher
type Reflector = [( Char , Char )] 
type Offsets = (Int , Int , Int)

--Define Type for Steckerboard
type Steckerboard = [( Char , Char )] 

-- define Enigma 
data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets  | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Steckerboard

-- define Crib &  menu
type Crib = (String , String)
type Menu = [Int] 

--getReflector : takes Enigma of any type , returns corrosponding reflector
getReflector :: Enigma -> Reflector
getReflector (SimpleEnigma rotor1 rotor2 rotor3 ref offsets) = ref
getReflector (SteckeredEnigma rotor1 rotor2 rotor3 ref offsets steckerboard) = ref

steckerTest = [('A' ,'M') , ('S' , 'R')]
offsetTest = (0,0,0)
testEnigma = (SimpleEnigma rotor1 rotor2 rotor3 reflectorB offsetTest)
testEnigma2 = (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB offsetTest steckerTest)

--incrementOffset : takes a tuple and decides which offset to inc and does respectively
incrementOffset :: (Int ,Int, Int) -> (Int , Int , Int)
incrementOffset (offset1 , offset2 , offset3) 
    | offset3 <25 = (offset1 , offset2 , offset3 + 1) 
    | offset2 <25 = (offset1 , offset2 + 1 ,0) 
    | offset1 <25 = (offset1 + 1 , 0 , 0) 
    | otherwise = (0 , 0 , 0)

--incrementEnigma : takes an enigma of any type and returns new Enigma with Incremented chosen offset
incrementEnigma :: Enigma -> Enigma
incrementEnigma (SimpleEnigma rotor1 rotor2 rotor3 ref offsets) = (SimpleEnigma rotor1 rotor2 rotor3 ref (incrementOffset offsets))
incrementEnigma (SteckeredEnigma rotor1 rotor2 rotor3 ref offsets steckerboard) = (SteckeredEnigma rotor1 rotor2 rotor3 ref (incrementOffset offsets) steckerboard)

-- encode' : takes a rotor , an offset and a char , returns char after being shifted by offset then encoded then shifted back again.
encode' :: Rotor -> Int -> Char -> Char
encode' rotor offset char = encode alphabet offset (encode rotor 0 (reverseEncode alphabet offset char))

-- decode : takes a rotor , an offset and a char , returns char after being shifted by offset then reverseEncoded then shifted back again.
decode :: Rotor -> Int-> Char -> Char
decode rotor offset char = encode alphabet offset (reverseEncode rotor 0 (reverseEncode alphabet offset char))

-- rotorsEncode : takes an enigma of any type and a char and returns char after being encoded by 3 different rotors in order.
rotorsEncode :: Enigma -> Char -> Char
rotorsEncode (SimpleEnigma rotor1 rotor2 rotor3 ref (offset1 , offset2 , offset3)) char = encode' rotor1 offset1 (encode' rotor2 offset2 (encode' rotor3 offset3 char))
rotorsEncode (SteckeredEnigma r1 r2 r3 ref (o1 , o2 , o3) steckerboard) char = encode' r1 o1 (encode' r2 o2 (encode' r3 o3 (steckerChar char steckerboard )))

--rotorsDecode : takes an enigma and a char and returns char after being reverse encoded by 3 different rotors in order.
rotorsDecode :: Enigma -> Char -> Char
rotorsDecode (SimpleEnigma rotor1 rotor2 rotor3 ref (offset1 , offset2 , offset3)) char = decode rotor3 offset3 (decode rotor2 offset2 (decode rotor1 offset1 char))
rotorsDecode (SteckeredEnigma r1 r2 r3 ref (o1 , o2 , o3) steckbd) char = steckerChar (decode r3 o3 (decode r2 o2 (decode r1 o1 char))) steckbd

-- reflectChar : takes a char and a reflector as a parameter and returns a char after searching recursively over list of char tuples
reflectChar :: Char -> Reflector -> Char
reflectChar char ((f , s):xs)
            | char == f = s
            | char == s = f
            | otherwise = reflectChar char xs

-- steckerChar : takes a char and a steckerboard as parameters and returns char after recursivly searching through list
steckerChar :: Char -> Steckerboard -> Char 
steckerChar char [] = char
steckerChar char ((f , s):xs)
            | char == f = s
            | char == s = f
            | otherwise = steckerChar char xs

{- enigmaEncode: Given a character to encode, and an enigma, returns the encoded letter. it first increments the enigma 
   then it encodes and reflects and decode char.
-}
enigmaEncode :: Char -> Enigma -> Char
enigmaEncode char enigma = do{let newEnigma = incrementEnigma enigma
                                ; rotorsDecode newEnigma (reflectChar (rotorsEncode newEnigma char) (getReflector enigma))  }

-- enigmaEncode' : does same function of enigmaEncode but without incrementing enigma , used in enigmaEncodeMessage
enigmaEncode' :: Char -> Enigma -> Char 
enigmaEncode' char enigma = rotorsDecode enigma (reflectChar (rotorsEncode enigma char) (getReflector enigma))

-- enigmaEncodeMessage : Given a message to encode and an enigma , it returns encoded letter by calling enigmaEncode' on each letter.
enigmaEncodeMessage :: String -> Enigma -> String
enigmaEncodeMessage [] enigma = []
enigmaEncodeMessage (c:cs) enigma
     = (enigmaEncode' c (incrementEnigma enigma)) : enigmaEncodeMessage cs (incrementEnigma enigma)

testMessage ="INXTHEXENIGMAXMACHINEXEACHXROTORXHADXAXNOTCHSTOPXINXTHEXSPECIFICATIONCOMMAXIXHAVEXASSUMEDXTHATXTHATXNOTCHXISXALWAYSXATXPOSITTIONXTWENTYFIVEXZXXWHEREASXINXREALITYXITXWASXATXAXDIFFERENTXPOSITIONXFORXEACHXROTORSTOPXWHENXAXKEYXWASXPRESSEDCOMMAXTHEXVERYXFIRSTXTHINGXTHATXHAPPENEDXWASXTHATXTHEXROTORSXWEREXADVANCEDSTOPXTHEXRIGHTXROTORXISXROTATEDXBYXONESTOPXIFXITXHADXALREADYXBEENXROTATEDXTWENTYFIVEXTIMESXTHATXISXWASXATXPOSITIONXTWENTYFIVECOMMAXTHEXNOTCHXWOULDXCAUSEXTHEXMIDDLEXROTORXTOXALSOXROTATESTOPXIFXTHATXROTORXHADXALREADYXBEENXROTATEDXTWENTYFIVEXTIMECOMMAXITXINXTURNXWOULDXCAUSEXTHEXLEFTXROTORXTOXROTATESTOPXINXOTHERXWORDSCOMMAXFORXTHEXMIDDLEXROTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYSIXXKEYXPRESSESSTOPXFORXTHEXLEFTXROTORXTOXROTATEXONCECOMMAXTHEREXHADXTOXBEXTWENTYSIXXTIMESXTWENTYSIXXKEYXPRESSESSTOPXTOXGETXALLXTHEXWAYXBACKXTOXZEROCOMMAZEROCOMMAZEROCOMMAXTHEREXHADXTOXBEXTWENTYSIXXTIMESXTWENTYSIXXTIMESXTWENTYSIXXKEYXPRESSEESSTOPTHEXDOWNSIDEXOFXTHEXSIMPLIFICATIONXTHATXIXHAVEXGIVENXISXTHATXTHEXONLINEXSIMULATORSXWILLXNOTXPROGRESSXTHEXSUBSEQUENTXROTORSXATXTHEXSAMEXTIMEXTHATXYOURXSIMULATIONXDOESSTOPXINXACTUALXFACTXROTORXONEXHADXAXNOTCHXATXPOSITIONXQCOMMAXROTORTWOXATXPOSITIONXECOMMAXROTORTHREEXATXPOSITIONXVCOMMAXROTORFOURXATXPOSITIONXJCOMMAXANDXROTORFIVEXATXPOSITIONXZSTOP"
test2 = "HQRFMNYYVUUNBHACFQDZYSABBUEXJJGJPSFQGNTAJNLNZEIEPUSAXSYEKUBAHXLJZEUCGRFLYHUCDKDMKLZRPCQFMAHTGVYSSEYKUTWRXFHFMZRUWNNKCRTBNHSIOUWODBTAZXPRSJALISVOTAFSFXETWMZRVFRLJNYCWYNMKVBGJTUJKDQBZTNBRSXUGDJRRBUWJBKVCAAWMSSFELVIPOHZTDGOXIZDQGHNLADFAXHVFKGQYASKCZEFAWFABPIITZQPUWXJRHDFLLSMKIMVCIWEJCYSULAAWVQLOVHGJOKYFHIWVFBATADWVYARQBFEAWHLCKGDRXDRMSMNNBSKHFYIRSYHLQGCEQKIDQEXGIMHTUGHISMWQBWERWLGLEATJIJPRWZJISCGDIVXJCRWJTCJNOFDEBXBGSRRICMQXUZHDVYQVFTNXQVCLOBCNZGKSQUFTAOZUHXURSKLKZFHBBYPQTDILBLXCOSAMFHNEGJPXXBCGAXVSRIVSWSRSQOWUAGZSYVOAEMQHUOFJYKOGRFAXUQLYCPGCFMCOPIBIYGJJJZAFSJVSLRBAJZVWITZKJMFWSBGKTLVOCSWHTDSYVWYNHYZMNISJHSPLXTJGIQVNJHGYWLOCTXGCGKHAURIKBNSAMKLPJWQVAVZOHYNUBEPNAXILRQWDIQDYYPZVXBHLTLSSFXBJGJVVNHZHMWKLCWENMLOYDITLQCERPYNODYZLAPLYPLCEWOMJCEKSKRSAQKCLUMNBYWWWJAHHVEOYKXHOYYNUREFGGTVMJYMJLYUNQKMMWYJQMZXDFVFSIEKYVFTMMFAJSLBQBCKWBDUGKCJSJLRYHGADWCWMTSTKRGGPYRBOLPGZUVVKPRKCFAEJWWVVPWAHEGHKDAVPMXVHBLPWIVYILHKDSKWCSDWLVHRLOSUHCSKUDTAVIIFRXUFBWFYZLAQWBQJADGJOFDEFWGVXSKEYQCKCFTZWMBIQNWLRAXJOONXTNJQMZCREOIQZYYPIVIQEXFSHAZIOKYXJHJCHIWGWWZSIAYJPJVBKWDFKZUOUBYIGVMLCIZWIFKDELOULELFBUBUEJMUTMGTQUDIGIKZLZKNDGYQUAHODPSHEBEEOSNHUBTNPNAUQKJIZFYXHDQOQBXSCRWICRMGBETZKZBURJCHITCUBFJJHSXOLXUQRGKWGJBPKNNODIBHFOCKYDEVRVZITAMPVZPZLEKFLZKHBVLYTWBFCCUWMXGLSRQALPJQPTISHPWDBQAMBMKSKIZCQCHLGPDUVRWYWW"