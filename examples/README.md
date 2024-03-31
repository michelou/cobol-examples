# <span id="top">COBOL examples</span> <span style="size:30%;"><a href="../README.md">⬆</a></span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:100px;"><a href="https://" rel="external"><img style="border:0;" src="../docs/images/cobol.png" width="100" alt="COBOL project"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">Directory <strong><code>examples\</code></strong> contains <a href="https://" alt="Akka">COBOL</a> code examples coming from various websites - mostly from the <a href="https://" rel="external">COBOL project</a>.
  </td>
  </tr>
</table>

## <span id="circles">`Circles` Example</span>

Example `Circles` has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/tree" rel="external">tree</a> /a /f . | <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /b /v [A-Z]</b>
|   <a href="./Circles/00download.txt">00download.txt</a>
|   <a href="./Circles/build.bat">build.bat</a>
|   <a href="./Circles/build.sh">build.sh</a>
\---src
    \---main
        \---cobol
                <a href="./Circles/src/main/cobol/CIRCLES.cbl">CIRCLES.cbl</a>
</pre>

We run command [`build.bat run`](./Circles/build.bat) <sup id="anchor_01">[1](#footnote_01)</sup> to generate and execute the program `target\Circles.exe` (use option `-debug` <sup id="anchor_02">[2](#footnote_02)</sup> to print details of the build process) :

<pre style="font-size:80%;">
$ build -verbose clean run
Delete directory "target"
Compile 1 COBOL source file into directory "target"
Execute program "target\Circles.exe"
Please enter radius of circle:
3
Circle Circumfrence is:   18.84
Circle area is:    00.00
</pre>

<!--=======================================================================-->

## <span id="helloworld">`HelloWorld` Example</span> [**&#x25B4;**](#top)

Example `HelloWorld` has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/tree" rel="external">tree</a> /a /f . | <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /b /v [A-Z]</b>
|   <a href="./helloworld/build.bat">build.bat</a>
\---src
    \---main
        +---cobol
        |       <a href="./helloworld/src/main/cobol/HelloWorld.cbl">HelloWorld.cbl</a>
        \--cobol-fixed
                <a href="./helloworld/src/main/cobol/HelloWorld.cbl">HelloWorld.cbl</a>
</pre>

<!--
[cobc.exe][cobc_cmd]
-->

We run command [`build.bat run`](./HelloWorld/build.bat) <sup id="anchor_01">[1](#footnote_01)</sup> to generate and execute the program `target\HelloWorld.exe` (use option `-debug` <sup id="anchor_02">[2](#footnote_02)</sup> to print details of the build process) :

<pre style="font-size:80%;">
<b>&gt; <a href="./HelloWorld/build.bat">build</a> -verbose clean run</b>
Delete directory "target"
Compile 1 COBOL source file into directory "target"
Execute program "target\HelloWorld.exe"
HELLO WORLD
</pre>

> **Note**: With option `-fixed` we select the COBOL source files from directory `src\main\cobol-fixed\` :
> <pre style="font-size:80%;">
> <b>&gt; <a href="">build</a> -debug -fixed clean run</b>
> [build] Options    : _FORMAT=fixed _TARGET= _VERBOSE=0
> [build] Subcommands: _CLEAN=1 _COMPILE=1 _RUN=1
> [build] Variables  : "COB_HOME=C:\opt\GnuCOBOL\"
> [build] Variables  : "GIT_HOME=C:\opt\Git"
> [build] 00000000000000 Target : "H:\examples\HelloWorld\target\HelloWorld.exe"
> [build] 20240324185858 Sources: "H:\examples\HelloWorld\src\main\cobol-fixed\*.cbl"
> [build] _ACTION_REQUIRED=1
> [build] "C:\opt\GnuCOBOL\\bin\cobc.exe" --info | findstr env:
>   env: COB_CC            : C:\opt\GnuCOBOL\\mingw64\bin\gcc.exe
>   env: COB_CFLAGS        : -I "C:\opt\GnuCOBOL\\include" -pipe
>   env: COB_LIBS          : -L "C:\opt\GnuCOBOL\\lib" -lcob
>   env: COB_CONFIG_DIR    : C:\opt\GnuCOBOL\\config
>   env: COB_COPY_DIR      : C:\opt\GnuCOBOL\\copy
> [build] "C:\opt\GnuCOBOL\\bin\cobc.exe" --debug --verbose -std=cobol2014 -x -o "H:\examples\HelloWorld\target\HelloWorld.exe"  "H:\examples\HelloWorld\src\main\cobol-fixed\HelloWorld.cbl"
> [...]
> [build] Execute program "target\HelloWorld.exe"
> Hello World!
> [build] _EXITCODE=0
> </pre>

<!--=======================================================================-->

## <span id="footnotes">Footnotes</span> [**&#x25B4;**](#top)

<span id="footnote_01">[1]</span> ***Environment variables*** [↩](#anchor_01)

<dl><dd>
The <a href="https://gnucobol.sourceforge.io/" rel="external">GnuCOBOL</a> compiler configuration occurs in two ways :
<ul>
<li>through command line options (e.g. option <code>--free</code>) and
<li>through the execution environment (see variables below).
</ul>
<pre style="font-size:80%;">
<a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/set_1" rel="external"><b>set</b></a> COB_CC=%COB_HOME%\mingw64\bin\gcc.exe
<b>set</b> COB_CFLAGS=-pipe -I%%COB_HOME%\include -Wno-unused -fsigned-char -Wno-pointer-sign
<b>set</b> COB_LIBS=-L "%COB_HOME%\lib" -lcob
<b>set</b> COB_CONFIG_DIR=c%COB_HOME%\config
<b>set</b> COB_COPY_DIR=%COB_HOME%\copy
</pre>

> **Note**: Environment variable `COB_CONFIG_DIR` must be set for compiler option `-std=cobol2002`.
</dd></dl>

<span id="footnote_02">[2]</span> ***Option* `-debug`** [↩](#anchor_02)

<dl><dd>
With option <code>-debug</code> we print each build step :

<pre style="font-size:80%;">
<b>&gt; <a href="./helloworld/build.bat">build</a> -debug compile</b>
[build] Options    : _FORMAT=free _TARGET= _VERBOSE=0
[build] Subcommands: _CLEAN=0 _COMPILE=1 _RUN=0
[build] Variables  : "COBOL_HOME=C:\opt\GnuCOBOL\"
[build] Variables  : "GIT_HOME=C:\opt\Git"
  env: COB_CC            : C:\opt\GnuCOBOL\mingw64\bin\gcc.exe
  env: COB_CFLAGS        : -I "C:\opt\GnuCOBOL\include" -pipe
  env: COB_LIBS          : -L c:\opt\GnuCOBOL\lib -lcob
  env: COB_CONFIG_DIR    : C:\opt\GnuCOBOL\config
  env: COB_COPY_DIR      : C:\opt\GnuCOBOL\copy
[build] "C:\opt\GnuCOBOL\\bin\<a href="https://gnucobol.sourceforge.io/doc/gnucobol.html" rel="external">cobc.exe</a>" --debug --verbose -std=cobol2002 --free -x -o "H:\examples\helloworld\target\helloworld.exe"  "H:\examples\helloworld\src\main\cobol\helloworld.cob"
cobc (GnuCOBOL) 3.3-dev.0
Built     Mar 06 2024 10:59:48  Packaged  Mar 06 2024 09:58:52 UTC
C version (MinGW) "13.2.0"
command line:   C:\opt\GnuCOBOL\\bin\cobc.exe --debug --verbose -std=cobol2002 --free -x -o H:\examples\helloworld\target\helloworld.exe H:\examples\helloworld\src\main\cobol\helloworld.cob
preprocessing:  H:\examples\helloworld\src\main\cobol\helloworld.cob -> C:\Users\michelou\AppData\Local\Temp\cob20404_0.cob
return status:  0
parsing:        C:\Users\michelou\AppData\Local\Temp\cob20404_0.cob (H:\examples\helloworld\src\main\cobol\helloworld.cob)
return status:  0
translating:    C:\Users\michelou\AppData\Local\Temp\cob20404_0.cob -> C:\Users\michelou\AppData\Local\Temp\cob20404_0.c (H:\examples\helloworld\src\main\cobol\helloworld.cob)
executing:      C:\opt\GnuCOBOL\mingw64\bin\gcc.exe -c -I
                "C:\opt\GnuCOBOL\include" -pipe -Wno-unused -fsigned-char
                -Wno-pointer-sign -o
                "C:\Users\michelou\AppData\Local\Temp\cob20404_0.o"
                "C:\Users\michelou\AppData\Local\Temp\cob20404_0.c"
return status:  0
executing:      C:\opt\GnuCOBOL\mingw64\bin\gcc.exe -Wl,--export-all-symbols
                -Wl,--enable-auto-import -Wl,--enable-auto-image-base -o
                "H:\examples\helloworld\target\helloworld.exe"
                "C:\Users\michelou\AppData\Local\Temp\cob20404_0.o" -L
                c:\opt\GnuCOBOL\lib -lcob
return status:  0
[build] _EXITCODE=0
</pre>
</dd></dl>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/April 2024* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[cobc_cmd]: https://gnucobol.sourceforge.io/doc/gnucobol.html
