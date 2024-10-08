# <span id="top">COBOL katas from Mike Harris</span> <span style="size:30%;"><a href="../README.md">⬆</a></span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:100px;"><a href="https://www.mainframestechhelp.com/tutorials/cobol/introduction.htm" rel="external"><img style="border:0;" src="../docs/images/cobol.png" width="100" alt="COBOL language"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">Directory <strong><code>harris-examples\</code></strong> contains <a href="https://github.com/mikebharris/COBOL-katas" alt="COBOL katas">COBOL</a> code examples coming from Mike Harris GitHub repository <a href="https://github.com/mikebharris/COBOL-katas" rel="external">COBOL-katas</a>.</td>
  </tr>
</table>

## <span id="100_doors">`100Doors` Example</span>

Example `100Doors` has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/tree" rel="external">tree</a> /a /f . | <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /b /v [A-Z]</b>
|   <a href="./100Doors/build.bat">build.bat</a>
|   <a href="./100Doors/build.sh">build.sh</a>
\---src
    \---main
        \---cobol
                <a href="./100Doors/src/main/cobol/100Doors.cbl">100Doors.cbl</a>  <span style="font-family:Helvetica,Arial;font-size:9pt;">(from <a href="https://github.com/mikebharris/COBOL-katas/tree/master/COBOL-100-Doors">Harris's solution</a>)</span>
</pre>

Command [`build`](./100Doors/build.bat)`-verbose clean run` generates and executes the COBOL program `target\100Doors.exe`:

<pre style="font-size:80%;">
<b>&gt; <a href="./100Doors/build.bat">build</a> -verbose clean run</b>
Delete directory "target"
Compile 1 COBOL source file into directory "target"
Execute program "target\100Doors.exe"
State of doors at end of run:

Door 001 is Closed
Door 002 is Open
Door 003 is Closed
Door 004 is Open
[...]
Door 097 is Closed
Door 098 is Open
Door 099 is Closed
Door 100 is Open
</pre>

<!--=======================================================================-->

## <span id="fizz_buzz">`FizzBuzz` Example</span> [**&#x25B4;**](#top)

Example `FizzBuzz` has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/tree" rel="external">tree</a> /a /f . | <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /b /v [A-Z]</b>
|   <a href="./FizzBuzz/build.bat">build.bat</a>
|   <a href="./FizzBuzz/build.sh">build.sh</a>
\---src
    \---main
        \---cobol
                <a href="./FizzBuzz/src/main/cobol/FizzBuzz.cbl">FizzBuzz.cbl</a>  <span style="font-family:Helvetica,Arial;font-size:9pt;">(from <a href="https://github.com/mikebharris/COBOL-katas/tree/master/COBOL-FizzBuzz">Harris's solution</a>)</span>
</pre>

Command `sh`[`./build.sh`](./FizzBuzz/build.sh)`-verbose clean run` generates and executes the COBOL program `target\FizzBuzz.exe` :

<pre style="font-size:80%;">
<b>&gt; <a href="https://man7.org/linux/man-pages/man1/sh.1p.html" rel="external">sh</a> <a href="./FizzBuzz/build.sh">./build.sh</a> -verbose clean run</b>
Compile 1 COBOL source file to directory "target"
Execute "target/FizzBuzz.exe"
001
002
Fizz
[...]
097
098
Fizz
Buzz
</pre>

<!--=======================================================================-->

## <span id="monty_hall">`MontyHall` Example</span> [**&#x25B4;**](#top)

Example `MontyHall` has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/tree" rel="external">tree</a> /a /f . | <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /b /v [A-Z]</b>
|   <a href="./MontyHall/build.bat">build.bat</a>
|   <a href="./MontyHall/build.sh">build.sh</a>
\---src
    \---main
        \---cobol
                <a href="./MontyHall/src/main/cobol/MontyHall.cbl">MontyHall.cbl</a>  <span style="font-family:Helvetica,Arial;font-size:9pt;">(from <a href="https://github.com/mikebharris/COBOL-katas/tree/master/COBOL-Monty-Hall">Harris's solution</a>)</span>
</pre>

Command `sh`[`./build.sh`](./MontyHal/build.sh)`-verbose clean run` generates and executes the COBOL program `target\FizzBuzz.exe` :

<pre style="font-size:80%;">
<b>&gt; <a href="https://man7.org/linux/man-pages/man1/sh.1p.html" rel="external">sh</a> ./<a href="./MontyHall/build.sh">build.sh</a> -verbose clean run</b>
Delete directory "target"
Compile 1 COBOL source file to directory "target"
Execute "target/MontyHall.exe"
Results
-------
After playing 1000 rounds ...
Times correct door selected after swapping:   656
Times correct door selected without swapping: 344

Conclusion is that it's better to swap.  This makes sense as the probability in the first round of getting  the correct door is 1/3; whereas in the second, it's 1/2.
</pre>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/October 2024* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[cobc_cmd]: https://gnucobol.sourceforge.io/doc/gnucobol.html
