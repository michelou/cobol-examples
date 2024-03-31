identification division.
program-id. 100DoorsTest.

*> 100 doors in a row are all initially closed. You make
*> 100 passes by the doors. The first time through, you
*> visit every door and toggle the door (if the door is
*> closed, you open it; if it is open, you close it).
*> The second time you only visit every 2nd door (door
*> #2, #4, #6, ...). The third time, every 3rd door
*> (door #3, #6, #9, ...), etc, until you only visit
*> the 100th door.
*>
*> Question: What state are the doors in after the last
*> pass? Which are open, which are closed?
*>
*> [Source http://rosettacode.org]

data division.
working-storage section.
01 Doors.
    02 Door occurs 100 times.
        03 DoorState pic x(6) value "Open".
            88 DoorIsOpen value "Open".
            88 DoorIsClosed value "Closed".

01 DoorToStartWith pic 999.
01 NumberOfDoors   pic 999 value 100.
01 CurrentDoor     pic 999.

procedure division.
perform varying DoorToStartWith from 1 by 1 until DoorToStartWith greater than NumberOfDoors
    perform varying CurrentDoor from DoorToStartWith by 1 until CurrentDoor greater than NumberOfDoors
        evaluate true
            when DoorIsOpen(CurrentDoor) set DoorIsClosed(CurrentDoor) to true
            when DoorIsClosed(CurrentDoor) set DoorIsOpen(CurrentDoor) to true
        end-evaluate
    end-perform
end-perform

display "State of doors at end of run: "
display spaces
perform varying CurrentDoor from 1 by 1 until CurrentDoor greater than NumberOfDoors
    display "Door " CurrentDoor " is " DoorState(CurrentDoor)
end-perform

stop run.

end program 100DoorsTest.
