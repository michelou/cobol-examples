# <span id="top">COBOL examples from UL CSIS</span> <span style="font-size:90%;">[⬆](../README.md#top)</span>

<table style="font-family:Helvetica;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:100px;">
    <a href="https://www.mainframestechhelp.com/tutorials/cobol/introduction.htm" rel="external"><img style="border:0;width:100px;" src="../docs/images/cobol.png" width="100" alt="COBOL language"/></a>
  </td>
  <td style="border:0;padding:0;vertical-align:text-top;">
    Directory <strong><code>csis-examples\</code></strong> contains <a href="https://www.mainframestechhelp.com/tutorials/cobol/introduction.htm" rel="external" title="COBOL">COBOL</a> code examples coming from the <a href="https://www.csis.ul.ie/cobol/examples/">CSIS</a> department of the <a href="https://www.ul.ie/">University of Limerick</a>.
  </td>
  </tr>
</table>

## <span id="month_table">`MonthTable` Example</span>

The description of example `MonthTable` is presented in document [`docs\MonthTable.md`](./MonthTable/docs/MonthTable.md).

Example `MonthTable` has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/tree" loc="external">tree</a> /a /f . | <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /v /b [A-Z]</b>
|   <a href="./MonthTable/build.bat">build.bat</a>
|   <a href="./MonthTable/STUDENTS.DAT">STUDENTS.DAT</a>
+---<b>docs</b>
|       <a href="./MonthTable/docs/00download.txt">00download.txt</a>
|       <a href="./MonthTable/docs/MonthTable.md">MonthTable.md</a>
|       <a href="./MonthTable/docs/T-CobolExercise.gif">T-CobolExercise.gif</a>
\---<b>src</b>
    \---<b>main</b>
        \---<b>cobol</b>
                <a href="./MonthTable/src/main/cobol/MonthTable.cbl">MonthTable.cbl</a>
</pre>

Command [`build`](./MonthTable/build.bat)`-verbose clean run` generates and executes the [COBOL] program `target\MonthTable.exe` :

<pre style="font-size:80%;">
<b>&gt; <a href="./MonthTable/build.bat">build</a> -verbose clean run</b>
Compile 1 COBOL source file into directory "target"
Execute program "target\MonthTable.exe"
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

## <span id="ref_mod">`RefMod` Example</span> [**&#x25B4;**](#top)

Example `RefMod` solves a number of string handling tasks such as :
- Extracting a substring from a string given the start position and length of the substring.
- Extracting the first x number of chars from a string.
- Extracting the last x number of chars from a string.
- Removing trailing blanks from a string.
- Removing leading blanks from a string.
- Finding the location of the first occurrence of any of a substring's chars in a string

Example `RefMod` has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/tree" rel="external">tree</a> /a /f . | <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /v /b [A-Z]</b>
|   <a href="./RefMod/build.bat">build.bat</a>
\---src
    \---main
        \---cobol
                <a href="./RefMod/src/main/cobol/RefMod.cbl">RefMod.cbl</a>
</pre>

Command [`build`](./RefMod/build.bat)`-verbose clean run` generates and executes the [COBOL] program `target\RefMod.exe` :

<pre style="font-size:80%;">
<b>&gt; <a href="./RefMod/build.bat">build</a> -verbose clean clean</b>
Delete directory "target"
Compile 1 COBOL source file into directory "target"
Execute program "target\RefMod.exe"
Task1 = is
Task2 =    This
Task3 = source string
Task4 Before =    This is the first source string                <<<<<<
Task4 After =    This is the first source string<<<<<<<
Task4 After =    This is the first source string<<<<<<<
Task5 =This is the first source string
Task6 First occurrence is in char position 22
Task7 First occurrence is in char position 14
The character is e
</pre>


## <span id="seq_write">`SeqWrite` Example</span> [**&#x25B4;**](#top)

Example `SeqWrite` has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="">tree</a> /a /f .| <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /v /b [A-Z]</b>
|   <a href="./SeqWrite/build.bat">build.bat</a>
|   <a href="./SeqWrite/build.sh">build.sh</a>
\---<b>src</b>
    \---<b>main</b>
        \---<b>cobol</b>
                <a href="./SeqWrite/src/main/cobol/SeqWrite.cbl">SeqWrite.cbl</a>
</pre>

Command [`build`](./SeqWrite/build.bat)`-verbose clean run` generates and executes the [COBOL] program `target\SeqWrite.exe` :

<pre style="font-size:80%;">
<b>&gt; <a href="./SeqWrite/build.bat">build</a> -verbose clean run</b>
Compile 1 COBOL source file into directory "target"
Execute program "target\SeqWrite.exe"
Enter student details using template below.  Enter no data to end.
Enter - StudId, Surname, Initials, YOB, MOB, DOB, Course, Gender
NNNNNNNSSSSSSSSIIYYYYMMDDCCCCG
</pre>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/January 2025* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[cobol]: https://en.wikipedia.org/wiki/COBOL
