import StrLib


print((StrLib.StrLib_StrLen("hello")))
a = "1234567890"                       # should really find a better method
StrLib.StrLib_StrCopy("hello", a)      # for creating, a, which is also
print(a)                                # compatible with ARRAY OF CHAR
