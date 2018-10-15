#
#
#            Nim's Runtime Library
#        (c) Copyright 2018 Nim Contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements a base32 encoder and decoder.
##
## Encoding data
## -------------
##
## In order to encode some text simply call the ``encode`` procedure:
##
##   .. code-block::nim
##      import base32
##      let encoded = encode("Hello World")
##      echo(encoded) # JBSWY3DPEBLW64TMMQ======
##
## Apart from strings you can also encode lists of integers or characters:
##
##   .. code-block::nim
##      import base32
##      let encodedInts = encode([1,2,3])
##      echo(encodedInts) # AEBAG===
##      let encodedChars = encode(['h','e','y'])
##      echo(encodedChars) # NBSXS===
##
## The ``encode`` procedure takes an ``openarray`` so both arrays and sequences
## can be passed as parameters.
##
## Decoding data
## -------------
##
## To decode a base32 encoded data string simply call the ``decode``
## procedure:
##
##   .. code-block::nim
##      import base32
##      echo(decode("JBSWY3DPEBLW64TMMQ======")) # Hello World

const
  Padding*: char = '='
  AlphabetDefault*: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
  AlphabetLower*: string = "abcdefghijklmnopqrstuvwxyz234567"
  AlphabetHex*: string = "0123456789ABCDEFGHIJKLMNOPQRSTUV"
  AlphabetHexLower*: string = "0123456789abcdefghijklmnopqrstuv"
  AlphabetZbase32*: string = "ybndrfg8ejkmcpqxotluwisza345h769"
  AlphabetCrockford*: string = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"


type 
  AlphabetError* = object of CatchableError
  # Alphabet* {.pure.} = enum
  #   Default = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567", 
  #   Lower = "abcdefghijklmnopqrstuvwxyz234567",
  #   Hex = "0123456789ABCDEFGHIJKLMNOPQRSTUV",
  #   HexLower = "0123456789abcdefghijklmnopqrstuv",
  #   Zbase32 = "ybndrfg8ejkmcpqxotluwisza345h769",
  #   Crockford = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"


func checkAlphabet*(alphabet: string, padding: char) =
  ## Raises an AlphabetError.
  if alphabet.len != 32:
    raise newException(AlphabetError, "Alphabet must be 32 characters.")

  var abSet: set[char] = {}
  for c in alphabet:
    abSet.incl(c)
  if abSet.card != 32:
    raise newException(AlphabetError, "Alphabet must not contain duplicate characters.")

  if alphabet.contains(padding):
    raise newException(AlphabetError, "Alphabet must not contain the padding character.")


template encodeInternal(src: typed, alphabet: string, padding: char): untyped =
  ## encodes `src` into base32 representation.
  if src.len == 0:
    return ""
  
  # 5 src bytes is 8 result bytes
  result = newStringOfCap( ((src.len + 4) div 5) * 8 )

  var 
    buff: int64 = 0
    i, j: int = 0
  
  while i < src.len:
    # store the next 40 bits from 5 src characters, 
    # zeros if past src.len (which will become alphabet[0] characters)
    buff = 0
    for j in 0..4:
      buff = buff shl 8
      if i + j < src.len:
        buff = buff or int64(src[i + j].uint8)
    
    # add 8 characters to result
    for j in countdown(35,0,5):
      # shift by groups of 5 [35..0], mask to 5 bits, 
      # convert to int8 and lookup in the alphabet
      result &= alphabet[int8(buff shr j and 0b00011111'i64)]

    i += 5

  # padding, if we went past the end of src, 
  # replace ending alphabet[0] characters with the padding character
  if i > src.len:
    # padLen is src chars (mod 5) * 8 bits, +4/5 to get result chars
    let padLen: int = 8 - (((src.len mod 5) * 8 + 4) div 5)
    j = 0
    while j < padLen:
      result[result.len - 1 - j] = padding
      j += 1


func encode*[T:SomeInteger|char](src: openArray[T], alphabet: string = AlphabetDefault, 
            padding: char = Padding): string =
  ## encodes `src` into base32 representation.
  ##
  ## This procedure encodes an openarray (array or sequence) of either integers
  ## or characters. Numbers are converted to uint8, so values over 255 do not
  ## raise IndexErrors.
  encodeInternal(src, alphabet, padding)


func encode*(src: string, alphabet: string = AlphabetDefault, 
            padding: char = Padding): string =
  ## encodes `src` into base32 representation.  
  ##
  ## This procedure encodes a string.
  encodeInternal(src, alphabet, padding)


func normalizeCrockford(c: char): char {.inline.}=
  result = case c:
    of ['0', 'o', 'O']: '0'
    of ['1', 'i', 'I', 'l', 'L']: '1'
    # lower to uppercase, except for: o i l 
    of ['a','b','c','d','e','f','g','h',    'j','k',    'm',
        'n',    'p','q','r','s','t','u','v','w','x','y','z',]: chr(ord(c)-32)
    else: c


func decode*(src: string, alphabet: string = AlphabetDefault, 
             padding: char = Padding, isCrockford: bool = false): string =
  ## decodes a string in base32 representation back into its original form.
  ##
  ## Missing padding is ignored.
  if src.len == 0:
    return ""
    
  # 8 src characters (40 bits) is 5 result bytes
  result = newStringOfCap( ((src.len div 8) + 1) * 5 )

  var 
    buff: int64 = 0
    i, j: int = 0
    c: char = ' '
    index: int = 0
    padCount: int = 0
  
  while i < src.len:
    # store the next 40 bits from 8 src characters
    buff = 0
    for j in 0..7:
      buff = buff shl 5
      
      # in case src is missing padding
      c = padding
      if i + j < src.len:
        c = src[i + j]
      
      if isCrockford:
        c = normalizeCrockford(c)
      
      index = alphabet.find(c)
      if index >= 0:
        buff = buff or int64(index.uint8)
      elif c == padding:
        padCount += 1
    
    # add 5 bytes to result
    for j in countdown(32,0,8):
      # shift by groups of 8 [32..0], and mask to 8 bits
      result &= char(buff shr j and 0b11111111'i64)

    i += 8

  # padding of zeros (nulls) needs to be removed from result
  if padCount > 0:
    let resultPad = (padCount * 5) div 8 + 1
    setLen(result, result.len - resultPad)



when isMainModule:
  # echo encode("foobar"), "foobar".len, encode("foobar").len #,  cmp( encode("f") , "MY======")
  # echo decode("MZXW6YTBOI======"), "MZXW6YTBOI======".len, decode("MZXW6YTBOI======").len 
  
  # echo encode("foobar", $Crockford, Padding)
  # echo decode("CSQPYRK1E8======", $Crockford, Padding, true)
  # echo decode("CSqPYRKiE8======", $Crockford, Padding, true)

  assert(
    decode(encode("foobar", "qwertyuiopasdfghjklzxcvbnm~!@#$%", ')'),
          "qwertyuiopasdfghjklzxcvbnm~!@#$%", ')') == "foobar"
  )

  # encode
  # RFC 4648 examples
  assert (encode "") == ""
  assert (encode "f") == "MY======"
  assert (encode "fo") == "MZXQ===="
  assert (encode "foo") == "MZXW6==="
  assert (encode "foob") == "MZXW6YQ="
  assert (encode "fooba") == "MZXW6YTB"
  assert (encode "foobar") == "MZXW6YTBOI======"
  # Wikipedia examples
  assert (encode "sure.") == "ON2XEZJO"
  assert (encode "sure") == "ON2XEZI="
  assert (encode "sur") == "ON2XE==="
  assert (encode "su") == "ON2Q===="
  assert (encode "leasure.") == "NRSWC43VOJSS4==="
  assert (encode "easure.") == "MVQXG5LSMUXA===="
  assert (encode "asure.") == "MFZXK4TFFY======"
  assert (encode "sure.") == "ON2XEZJO"

  # decode
  # RFC 4648 examples
  assert "" == (decode "")
  assert "f" == (decode "MY======")
  assert "fo" == (decode "MZXQ====")
  assert "foo" == (decode "MZXW6===")
  assert "foob" == (decode "MZXW6YQ=")
  assert "fooba" == (decode "MZXW6YTB")
  assert "foobar" == (decode "MZXW6YTBOI======")
  # Wikipedia examples
  assert "sure." == (decode "ON2XEZJO")
  assert "sure" == (decode "ON2XEZI=")
  assert "sur" == (decode "ON2XE===")
  assert "su" == (decode "ON2Q====")
  assert "leasure." == (decode "NRSWC43VOJSS4===")
  assert "easure." == (decode "MVQXG5LSMUXA====")
  assert "asure." == (decode "MFZXK4TFFY======")
  assert "sure." == (decode "ON2XEZJO")

  # array or seq of char
  assert decode(encode(['a','b','c'])) == ['a','b','c']
  assert decode(encode(@['a','b','c'])) == @['a','b','c']
  # array or seq of int (ints are interpreted as a char|uint8 value)
  assert decode(encode([1.int64,2,3])) == "\x01\x02\x03"
  assert decode(encode(@[1.int64,2,3])) == "\x01\x02\x03"
  # only the first 8 bits of an int are included
  assert decode(encode([255.int32,256,257,258,259,260,261])) == "\xff\x00\x01\x02\x03\x04\x05"

  # check alphabets are valid
  var ab = @[AlphabetDefault,AlphabetLower,AlphabetHex,AlphabetHexLower,AlphabetZbase32,AlphabetCrockford]
  for a in ab:
    checkAlphabet(a, Padding)
  
  # padding removed
  assert "f" == (decode "MY=====")
  assert "fo" == (decode "MZXQ===")
  assert "foo" == (decode "MZXW6=")
  assert "foob" == (decode "MZXW6YQ")
  assert "fooba" == (decode "MZXW6YTB")
  assert "foobar" == (decode "MZXW6YTBOI")