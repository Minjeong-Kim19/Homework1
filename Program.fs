module CS220.Program

/// Define a function `prob1` that takes in three (int) numbers as input and
/// returns the sum of the squares of the two large numbers. If there is any
/// error in processing the inputs, e.g., integer overflows, then the function
/// should simply return -1 (do not raise an exception).
let prob1 a b c =
  let minInt = -2147483648
  let maxint = 2147483647

  if a<minInt || a>maxInt || b<minInt || b>maxInt || c<minInt || c>maxInt then -1

  else
    let sumtwosquares=
      let maxnum = max a (max b c)
      let secondmaxnum= if maxnum =a then max b c elif maxnum=b then max a c else max a b
      maxnum*maxnum+secondmaxnum*secondmaxnum
    sumtwosquares

/// Define a function `prob2` that takes in a string and returns a new string
/// that ends with a newline character '\n'. The function appends a newline
/// character to the given string only if the string does not already end with a
/// newline character. Note that a string in F# is indeed, an array of
/// characters, and each character in a string can be accessed through an item
/// accessor. For example, str[0] returns the first character of the string str.
/// Also, one can get the length of a string s by calling a function
/// String.length: String.length s returns the length of the string s. Finally,
/// you can append two strings using the + operator.
let prob2 (str: string) =
  let lastChar=str.Length-1
  if lastChar >=0 && str.[lastChar] = '\n' then str
  else str + '\n'


/// Write a function `prob3` that takes in as input three floating point numbers
/// a, b, and c, and returns a root of the quadratic formula $ax^2 + bx + c =
/// 0$. If there are two roots, then the function should return the bigger root.
/// If there is only one root, then the function should return the root. If
/// there are no real roots, then the function should return nan, which is a
/// special floating point number representing "Not a Number".
let prob3 a b c =
  let D = b*b-4*a*c in
  if D>=0.0 then
    let root1 = (-b + sqrt D)/(2.0*a)
    let root2 = (-b - sqrt D)/(2.0*a)
    max root1 root2
  else
    nan

/// Define a function `prob4` that returns the number of days in a month. The
/// function takes in as input an integer representing a month, and outputs the
/// number of days. You can assume that February has 28 days. The function
/// returns -1 for any error cases. For example, if a number big than 12 is
/// given as input, then the function should return -1.
let prob4 month =
  if month = 2 then 28
  elif month =1 || month =3 || month =5 || month = 7 || month = 8 || month = 10 ||month =12 then 31
  elif month =4 || month =6 || month = 9 || month = 11 then 30
  else -1

[<EntryPoint>]
let main _args =
  0
