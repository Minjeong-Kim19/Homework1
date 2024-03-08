module CS220.Program

/// Define a function `prob1` that takes in three (int) numbers as input and
/// returns the sum of the squares of the two large numbers. If there is any
/// error in processing the inputs, e.g., integer overflows, then the function
/// should simply return -1 (do not raise an exception).
let prob1 a b c =
  failwith "TODO" // REMOVE this line when you implement your own code

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
  failwith "TODO" // REMOVE this line when you implement your own code

/// Write a function `prob3` that takes in as input three floating point numbers
/// a, b, and c, and returns a root of the quadratic formula $ax^2 + bx + c =
/// 0$. If there are two roots, then the function should return the bigger root.
/// If there is only one root, then the function should return the root. If
/// there are no real roots, then the function should return nan, which is a
/// special floating point number representing "Not a Number".
let prob3 a b c =
  failwith "TODO" // REMOVE this line when you implement your own code

/// Define a function `prob4` that returns the number of days in a month. The
/// function takes in as input an integer representing a month, and outputs the
/// number of days. You can assume that February has 28 days. The function
/// returns -1 for any error cases. For example, if a number big than 12 is
/// given as input, then the function should return -1.
let prob4 month =
  failwith "TODO" // REMOVE this line when you implement your own code

[<EntryPoint>]
let main _args =
  0
