# <span id="top">COBOL examples from Jay Moseley</span> <span style="font-size:90%;">[⬆](../README.md#top)</span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:100px;"><a href="https://www.mainframestechhelp.com/tutorials/cobol/introduction.htm" rel="external"><img style="border:0;" src="../docs/images/cobol.png" width="100" alt="COBOL language"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">Directory <strong><code>moseley-examples\</code></strong> contains <a href="hhttps://www.jaymoseley.com/gnucobol/" alt="COBOL examples">COBOL</a> code examples coming from Moseley's webpage <a href="https://www.jaymoseley.com/gnucobol/" rel="external">"GnuCOBOL"</a>.
  </td>
  </tr>
</table>

## <span id="elapsed">`Elapsed` Example</span>

This example has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/tree" rel="external">tree</a> /a /f . | <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /b /v [A-Z]</b>
|   <a href="./Elapsed/build.bat">build.bat</a>
|   <a href="./Elapsed/build.sh">build.sh</a>
\---src
    \---main
        \---cobol
                <a href="./Elapsed/src/main/cobol/ELAPSED.cbl">ELAPSED.cbl</a>
                <a href="./Elapsed/src/main/cobol/Y2K.cpy">Y2K.cpy</a>
</pre>

Command [`build`](./Elapsed/build.bat)`-verbose clean run` generates and executes the COBOL program `target\Elapsed.exe` with parameter `1994` :

<pre style="font-size:80%;">
<b>&gt; <a href="./Elapsed/build.bat">build</a> -verbose clean run</b>
Delete directory "target"
Compile 1 COBOL source file into directory "target"
Execute program "target\Elapsed.exe" 1994
</pre>

<!--=======================================================================-->

## <span id="timeline">`Timeline` Example</span>

This example has the following directory structure :

<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/tree" rel="external">tree</a> /a /f . | <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/findstr" rel="external">findstr</a> /b /v [A-Z]</b>
|   <a href="./Timeline/build.bat">build.bat</a>
|   <a href="./Timeline/build.sh">build.sh</a>
|   TIMELINE.dat
|   timeline.rpt
\---src
    \---main
        \---cobol
                <a href="./Timeline/src/main/cobol/TIMELINE.cbl">TIMELINE.cbl</a>
                <a href="./Timeline/src/main/cobol/Y2K.cpy">Y2K.cpy</a>
                <a href="">Y2KDFMT.cbl</a>
                <a href="">Y2KDOWN.cbl</a>
                <a href="">Y2KGTOA.cbl</a>
                <a href="">Y2KLAGE.cbl</a>
</pre>

Command [`build`](./Timeline/build.bat)`-verbose clean run` generates and execution the COBOL program `target\Timeline.exe` :

<pre style="font-size:80%;">
<b>&gt; <a href="./Timeline/build.bat">build</a> -verbose clean run</b>
Delete directory "target"
Compile 5 COBOL source files into directory "target"
Execute program "target\Timeline.exe"
</pre>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/February 2025* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[cobc_cmd]: https://gnucobol.sourceforge.io/doc/gnucobol.html
