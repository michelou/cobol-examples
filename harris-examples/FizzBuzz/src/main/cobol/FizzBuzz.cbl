identification division.
program-id. FizzBuzz.

*> Write a program that prints the numbers from 1 to 100.
*> But for multiples of three print "Fizz" instead of the
*> number and for the multiples of five print "Buzz". For
*> numbers which are multiples of both three and five
*> print "FizzBuzz".

data division.
working-storage section.
01 Counter  pic 999.

procedure division.
Main section.
    perform with test after varying Counter from 1 by 1 until Counter equal to 100
        if function rem(Counter; 15) equal zero then
            display "FizzBuzz"
        else if function rem(Counter; 5) equal zero then
            display "Buzz"
        else if function rem(Counter; 3) equal zero then
            display "Fizz"
        else
            display Counter
        end-if
    end-perform.

stop run.
