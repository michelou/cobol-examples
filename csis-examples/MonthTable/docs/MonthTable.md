<div style="text-align:center;">
  <img src="T-CobolExercise.gif"/>
  <h2 style="margin-top:0;">Using Tables</h2>
  <span style="margin-top:0;font-size:80%;">READ, pre-defined Tables, Tables.</span>
</div>

***

## Introduction

A program is required which will process the Students File (Students.Dat) and will count the number students born on each month of the year and will display the results to January to December order.

Download <A HREF="ftp://www.csis.ul.ie/cobol/exercises/students.dat">Students.Dat</A> and save it to the WorkArea directory on the drive D:

## Student File Description

The Students File is a sequential file held in **ascending StudentId**
order.

Each record of the students file contains the following items;

| Field        | Type | Length | Value      |
|:-------------|:----:|:------:|:----------:|
| Student Id   |   9  |    7   |  0-9999999 |
| Student Name |      |        |   Group    |
| Surname      |   X  |    8   |     -      |
| Initials     |   X  |    2   |     -      |
| DateOfBirth  |      |        |   Group    |
| Year         |   9  |    4   |  0000-9999 |
| Month        |   9  |    2   |    01-12   |
| Day          |   9  |    2   |    01-31   |
| Course Code  |   X  |    4   |     -      |
| Gender       |   X  |    1   |     M/F    |

## Example Run

<pre style="font-size:80%;"">
 Month    StudCount
January        2
February       2
March          3
April          4
May            1
June           0
July           2
August         1
September      4
October        5
November       3
December       5
</pre>

Use Edited picture clauses and the zero suppression symbol to produce the output as shown.<TT></TT>

## Suggested Approaches

The main problem that we face here is that the results must be displayed in MonthName order but the file is held in StudentId order.

A tables based solution is a good idea here.&nbsp; We can use the month number as a direct index into the month table.

## Sample Solution

When you have written your program and have compiled it and have it working correctly you may wish to compare it with this <A HREF="ftp://www.csis.ul.ie/cobol/exercises/MonthTable.cbl">sample solution</A>.

<p style="text-align:center;color:#FF0000;"><u><b>WARNING</b></u></p>

As always please do not look at the solution until you have finished your own program. At the very least you should make a substantial effort to complete your own attempt at the program before examining the sample solution.

***

<span style="font-size:80%;">*Last updated : February 1999*<br/>
<a href="mailto:michael.coughlan@ul.ie">e-mail : CSISwebeditor@ul.ie</a></span>
