#include <stdio.h> // this is the I/O library for C
#include <iostream> // this is the I/O library for C++

using namespace std; // ignore this line for now

// <- This allows a single-line comment.

/* <- This allows a 
   multi-line comment
   */

/* Every C(++) program must have a main function in it.
   main can be non returning or it can have type int.
   It can take arguments (int argc, char * argv[]) or
   else they can be left out. main is the entry-point
   for every C(++) program. The commands in main are
   the only commands run, but main can call other
   functions defined outside main (in the same file
   or other files). The lines above are like import
   statements in Java. stdio.h and iostream are
   libraries holding the classes/functions for I/O in
   their respective languages. Each language has its
   own I/O system, but C++ can use C's as well as its
   own.
   */

int main() {

    /* printf (print format) is C's main output function.
       The first argument is a format string. For the moment,
       we'll just use the string "Hello world". Formatting will
       come later.
       */

    printf("Hello world");
    printf("The next printf statement will print a new line");
    printf("\n");

    /* Format strings are strings with "holes" in them that
       can be filled by expressions. The "holes" are denoted
       by different character sequences, depending on the
       type of expression we want to fit in there.
       */

   // First: %d is the format sequence for integers

   printf("%d", 1);

   int i = 42;

   printf("%d", i);

   i *= 3;
   i -= 100;
   
  printf("The current value of i is %d\n");

  // Second: %c is the format sequence for a character

  printf("%c", 'a');

  printf("The last element of the alphabet is %c\n", 'z');

  char j = 'r';

  printf("%c", j);

  // Third: %f is the format sequence for a float

  printf("%f", 3.14159265);

  float e = 2.71;

  printf("%f", e);

  /* Format sequences can be modified to print expressions differently!
     If I want just the first two decimal places of a float, I can use
     %.2f. There are loads of different formatting options! Look them
     up! Lastly, %s is the formatting sequence for a string.
     */

  printf("%s is an example of a string\n", "String");

  char str[] = "This is a string!";

  printf("%s\n");

  // ===============

  /* Now let's have a look at C++'s output system. C++
     uses streams to output things. Streams are sequences
     of different expressions that can all be converted to
     strings for outputting. We don't have to tell the compiler
     how to format each of these in terms of letting it know what
     the type of the expression is. It just works it out by itself.
     Each expression is delimited by << on the left. The output
     function is called cout. Here's an example.
     */

  cout << "This string will be output\n";
  cout << "Now I'll output a number\n";
  cout << 29; 
  cout << "This string will be followed by a number\n" << 56;
  cout << 'c';
  cout << 0.45;
  
  /* As you can see, cout takes floats, strings, characters and integers,
     but you can only have one expression between every pair of << (except
     for the last expression, which is followed by a semi-colon. For example,
     cout << "String" "String"; wouldn't work. Nor would cout << "S" 78;
     So cout << expr1 << expr2 << expr3 << ... << exprn; is the format.
     There are two extra expressions that can be used: flush and endl.
     When C++ writes output, it doesn't do it exactly when the command
     is reached. It waits until the opportune moment and then outputs
     everything that has been stored in one go. flush forces cout to
     output what it has stored. endl is like flush, but it outputs a
     new line at the same time
     */

    }
