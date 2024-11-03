# <span id="top">COBOL examples</span> <span style="font-size:90%;">[⬆](../README.md#top)</span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:100px;"><a href="https://www.mainframestechhelp.com/tutorials/cobol/introduction.htm" rel="external"><img style="border:0;" src="../docs/images/cobol.png" width="100" alt="COBOL project"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">Directory <strong><code>examples\</code></strong> contains <a href="https://www.mainframestechhelp.com/tutorials/cobol/introduction.htm" alt="COBOL">COBOL</a> code examples coming from various websites - mostly from the <a href="https://www.mainframestechhelp.com/tutorials/cobol/introduction.htm" rel="external">COBOL project</a>.</td>
  </tr>
</table>

Our build scripts support the following options to specify the COBOL compiler :

| Option  | Compiler    | Version |
|:--------|:------------|:--------|
| `-cobj` | [COBOL 4J](../COBOL_4J.md) | 1.1.3 |
| `-gnu` *(default)*  | [GnuCOBOL](../GNUCOBOL.md) | 3.3-dev.0 |
| `-mf`   | [Visual COBOL](../VISUAL_COBOL.md) | 9.0.0.49 |

## <span id="circles">`Circles` Example</span>

Example `Circles` has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/tree" rel="external">tree</a> /a /f . | <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /b /v [A-Z]</b>
|   <a href="./Circles/00download.txt">00download.txt</a>
|   <a href="./Circles/build.bat">build.bat</a>
|   <a href="./Circles/build.sh">build.sh</a>
|   <a href="./Circles/Makefile">Makefile</a>
\---<b>src</b>
    \---<b>main</b>
        \---<b>cobol</b>
                <a href="./Circles/src/main/cobol/CIRCLES.cbl">Circles.cbl</a>
</pre>

Command [`build.bat`](./Circles/build.bat)` run` generates and executes the program `target\Circles.exe` (use option `-debug` <sup id="anchor_01">[1](#footnote_01)</sup> to print details of the build process) :

<pre style="font-size:80%;">
<b>&gt; <a href="./Circles/build.bat">build</a> -verbose clean run</b>
Delete directory "target"
Compile 1 COBOL source file into directory "target"
Execute program "target\Circles.exe"
Please enter radius of circle:
3
Circle circumference is:   18.84
Circle area is:    28.27
</pre>

<!--=======================================================================-->

## <span id="helloworld">`HelloWorld` Example</span> [**&#x25B4;**](#top)

Example `HelloWorld` has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/tree" rel="external">tree</a> /a /f . | <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /b /v [A-Z]</b>
|   <a href="./HelloWorld/build.bat">build.bat</a>
\---<b>src</b>
    \---<b>main</b>
        +---<b>cobol</b>
        |       <a href="./HelloWorld/src/main/cobol/HelloWorld.cbl">HelloWorld.cbl</a>
        \---<b>cobol-fixed</b>
                <a href="./HelloWorld/src/main/cobol/HelloWorld.cbl">HelloWorld.cbl</a>
</pre>

<!--
[cobc.exe][cobc_cmd]
-->

Command [`build.bat`](./HelloWorld/build.bat)`run` generates and executes the program `target\HelloWorld.exe` (use option `-debug` <sup id="anchor_01">[1](#footnote_01)</sup> to print details of the build process) :

<pre style="font-size:80%;">
<b>&gt; <a href="./HelloWorld/build.bat">build</a> -verbose clean run</b>
Delete directory "target"
Compile 1 COBOL source file into directory "target"
Execute program "target\HelloWorld.exe"
Hello World!
</pre>

> **Note**: With option `-fixed` we select the COBOL source files from directory `src\main\cobol-fixed\` :
> <pre style="font-size:80%;">
> <b>&gt; <a href="">build</a> -debug -fixed clean run</b>
> [build] Options    : _FORMAT=fixed _STANDARD=cobol2014 _TOOLSET=gnu _VERBOSE=0
> [build] Subcommands: _CLEAN=1 _COMPILE=1 _RUN=1
> [build] Variables  : "COB_HOME=C:\opt\GnuCOBOL"
> [build] Variables  : "COBDIR=C:\Program Files (x86)\Micro Focus\Visual COBOL"
> [build] Variables  : "COBJ_HOME=C:\opt\cobj"
> [build] Variables  : "GIT_HOME=C:\opt\Git"
> [build] 00000000000000 Target : "J:\examples\HelloWorld\target\HelloWorld.exe"
> [build] 20240324185858 Sources: "J:\examples\HelloWorld\src\main\cobol-fixed\*.cbl"
> [build] _ACTION_REQUIRED=1
> [build] "C:\opt\GnuCOBOL\\bin\cobc.exe" --info | findstr env:
> [build]   env: COB_CC            : C:\opt\GnuCOBOL\\mingw64\bin\gcc.exe
> [build]   env: COB_CFLAGS        : -I "C:\opt\GnuCOBOL\\include" -pipe
> [build]   env: COB_LIBS          : -L "C:\opt\GnuCOBOL\\lib" -lcob
> [build]   env: COB_CONFIG_DIR    : C:\opt\GnuCOBOL\\config
> [build]   env: COB_COPY_DIR      : C:\opt\GnuCOBOL\\copy
> [build] "C:\opt\GnuCOBOL\\bin\cobc.exe" --debug --verbose -std=cobol2014 -x -o "J:\examples\HelloWorld\target\HelloWorld.exe"  "J:\examples\HelloWorld\src\main\cobol-fixed\HelloWorld.cbl"
> [...]
> [build] Execute program "target\HelloWorld.exe"
> Hello World!
> [build] _EXITCODE=0
> </pre>

<!--=======================================================================-->

## <span id="sort">`Sort` Example</span> [**&#x25B4;**](#top)

Example `Sort` comes from the webpage [Mainframe tips](https://mainframe-tips-and-tricks.blogspot.com/2021/08/sort-table-or-array-using-cobol-sort.html); it has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/tree" rel="external">tree</a> /f /a . | <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /v /b [A-Z]</b>
|   <a href="./Sort/00download.txt">00download.txt</a>
|   <a href="./Sort/build.bat">build.bat</a>
|   <a href="./Sort/build.sh">build.sh</a>
|   <a href="./Sort/Makefile">Makefile</a>
\---<b>src</b>
    \---<b>main</b>
        \---<b>cobol</b>
                <a href="./Sort/src/main/cobol/Sort.cbl">Sort.cbl</a>
</pre>

Command [`build.bat`](./Sort/build.bat)`-verbose clean run` generates and executes the COBOL program `target\Sort.exe` :

<pre style="font-size:80%;">
<b>&gt; <a href="./Sort/build.bat">build</a> -verbose clean run</b>
Delete directory "target"
Compile 1 COBOL source file into directory "target" (GnuCOBOL)
Execute program "target\Sort.exe"
0003
0013
0015
0016
0034
0043
0046
0055
0078
0112
</pre>

<!--=======================================================================-->

## <span id="subprogram">`SubProgram` Example</span> [**&#x25B4;**](#top)

Example `SubProgram` has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/tree" rel="external">tree</a> /f /a . | <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /v /b [A-Z]</b>
|   <a href="./SubProgram/00download.txt">00download.txt</a>
|   <a href="./SubProgram/build.bat">build.bat</a>
|   <a href="./SubProgram/build.sh">build.sh</a>
|   <a href="./SubProgram/Makefile">Makefile</a>
\---<b>src</b>
    \---<b>main</b>
        \---<b>cobol</b>
                <a href="./SubProgram/src/main/cobol/SubProgram.cbl">SubProgram.cbl</a>
</pre>

Command [`build.bat`](./SubProgram/build.bat)`-verbose clean run` generates and executes the COBOL program `target\SubProgram.exe` :

<pre style="font-size:80%;">
<b>&gt; <a href="./SubProgram/build.bat">build</a> -verbose clean run</b>
Delete directory "target"
Compile 1 COBOL source file into directory "target" (GnuCOBOL)
Execute program "target\SubProgram.exe"
Hello World!
functionABC: param1=100 result=+0000000200
Return value from functionABC sub-program: +0000000200
functionABC2: param1=+0000000100 result=+0000000200
Return value from functionABC2 function: +0000000200
</pre>

> **Note**: The Visual COBOL compiler (option `-mf`) generates the following error message :
> <pre style="font-size:80%;">
> <b>&gt; <a href="./SubProgram/build.bat">build</a> -verbose -mf clean run</b>
> Delete directory "target"
> Generate Micro Focus environment file
> Compile 1 COBOL source file into directory "target" (Visual COBOL)
>     COMPUTE returnvalue = functionABC2(someValue).
> *  72-S*******************************                                       **
> **    Invalid FUNCTION name FUNCTIONABC2
> C:\Program Files (x86)\Micro Focus\Visual COBOL\bin64\ccbl64.exe: error(s) in compilation: J:\examples\SubProgram\src\main\cobol\SubProgram.cbl
> Error: Failed to compile 1 COBOL source file into directory "target" (Visual COBOL)
> </pre>
> We get a different error message with the COBOL 4J compiler (option `-cobj`) :
> <pre style="font-size:80%;">
> <b>&gt; <a href="./SubProgram/build.bat">build</a> -verbose -cobj clean run</b>
> Compile 1 COBOL source file into directory "target" (COBOL 4J)
> H:/examples/SubProgram/src/main/cobol/SubProgram.cbl:6: Error: syntax error
> Error: Failed to compile 1 COBOL source file into directory "target" (COBOL 4J)
> </pre>

<!--=======================================================================-->

## <span id="footnotes">Footnotes</span> [**&#x25B4;**](#top)

<span id="footnote_01">[1]</span> ***Option* `-debug`** (GnuCOBOL) [↩](#anchor_01)

<dl><dd>
With option <code>-debug</code> we print each build step :

<pre style="font-size:80%;">
<b>&gt; <a href="./helloworld/build.bat">build</a> -debug compile</b>
[build] Options    : _FORMAT=free _STANDARD=cobol2014 _TOOLSET=gnu _VERBOSE=0
[build] Subcommands: _CLEAN=0 _COMPILE=1 _RUN=0
[build] Variables  : "COBOL_HOME=C:\opt\GnuCOBOL"
[build] Variables  : "COBDIR=C:\Program Files (x86)\Micro Focus\Visual COBOL"
[build] Variables  : "COBJ_HOME=C:\opt\cobj"
[build] Variables  : "GIT_HOME=C:\opt\Git"
[build]   env: COB_CC            : C:\opt\GnuCOBOL\mingw64\bin\gcc.exe
[build]   env: COB_CFLAGS        : -I "C:\opt\GnuCOBOL\include" -pipe
[build]   env: COB_LIBS          : -L c:\opt\GnuCOBOL\lib -lcob
[build]   env: COB_CONFIG_DIR    : C:\opt\GnuCOBOL\config
[build]   env: COB_COPY_DIR      : C:\opt\GnuCOBOL\copy
[build] "C:\opt\GnuCOBOL\\bin\<a href="https://gnucobol.sourceforge.io/doc/gnucobol.html" rel="external">cobc.exe</a>" --debug --verbose -std=cobol2014 --free -x -o "J:\examples\helloworld\target\helloworld.exe"  "J:\examples\helloworld\src\main\cobol\helloworld.cob"
cobc (GnuCOBOL) 3.3-dev.0
Built     Mar 06 2024 10:59:48  Packaged  Mar 06 2024 09:58:52 UTC
C version (MinGW) "13.2.0"
command line:   C:\opt\GnuCOBOL\\bin\cobc.exe --debug --verbose -std=cobol2014 --free -x -o J:\examples\helloworld\target\helloworld.exe J:\examples\helloworld\src\main\cobol\helloworld.cob
preprocessing:  J:\examples\helloworld\src\main\cobol\helloworld.cob -> %LOCALAPPDATA%\Temp\cob20404_0.cob
return status:  0
parsing:        %LOCALAPPDATA%\Temp\cob20404_0.cob (J:\examples\helloworld\src\main\cobol\helloworld.cob)
return status:  0
translating:    %LOCALAPPDATA%\Temp\cob20404_0.cob -> %LOCALAPPDATA%\Temp\cob20404_0.c (J:\examples\helloworld\src\main\cobol\helloworld.cob)
executing:      C:\opt\GnuCOBOL\mingw64\bin\gcc.exe -c -I
                "C:\opt\GnuCOBOL\include" -pipe -Wno-unused -fsigned-char
                -Wno-pointer-sign -o
                "%LOCALAPPDATA%\Temp\cob20404_0.o"
                "%LOCALAPPDATA%\Temp\cob20404_0.c"
return status:  0
executing:      C:\opt\GnuCOBOL\mingw64\bin\gcc.exe -Wl,--export-all-symbols
                -Wl,--enable-auto-import -Wl,--enable-auto-image-base -o
                "J:\examples\helloworld\target\helloworld.exe"
                "%LOCALAPPDATA%\Temp\cob20404_0.o" -L
                c:\opt\GnuCOBOL\lib -lcob
return status:  0
[build] _EXITCODE=0
</pre>
</dd></dl>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/November 2024* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[cobc_cmd]: https://gnucobol.sourceforge.io/doc/gnucobol.html
