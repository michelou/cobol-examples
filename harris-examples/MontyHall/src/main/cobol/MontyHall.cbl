identification division.
program-id. MontyHall.

data division.
working-storage section.
01 CurrentTime.
    02 filler   pic 9(4).
    02 Seed     pic 9(4).

01 DoorThatHasCar pic 9.
01 DoorPlayerChooses pic 9.

01 Round pic 9(4).
01 NumberOfRoundsToPlay constant 1000.
01 WinsWhenNotSwapping pic 9(3) value zero.
01 WinsWhenSwapping pic 9(3) value zero.

procedure division.
Main section.
    accept CurrentTime from time
    compute DoorThatHasCar = function random(Seed)

    perform with test after varying Round from 1 by 1
        until Round equal to NumberOfRoundsToPlay
        compute DoorThatHasCar = (function random * 3) + 1
        compute DoorPlayerChooses = (function random * 3) + 1
        if DoorThatHasCar equal to DoorPlayerChooses then
            add 1 to WinsWhenNotSwapping
        else
            add 1 to WinsWhenSwapping
        end-if
    end-perform

    display "Results"
    display "-------"
    display "After playing " Round " rounds ..."
    display "Times correct door selected after swapping:   " WinsWhenSwapping
    display "Times correct door selected without swapping: " WinsWhenNotSwapping
    display spaces

    evaluate true
        when WinsWhenNotSwapping greater than WinsWhenSwapping
            display
                "Conclusion is that it's better not to swap."
                "  Which doesn't make much sense, but hey!"
            end-display
        when WinsWhenNotSwapping equal to WinsWhenSwapping
            display
                "There's no conclusion; they are equal outcomes."
                "  Which shouldn't really be the case."
            end-display
        when other
            display
                "Conclusion is that it's better to swap."
                "  This makes sense as the probability in the first round of getting"
                "  the correct door is 1/3; whereas in the second, it's 1/2."
            end-display
    end-evaluate

    stop run
    .

end program MontyHall.
