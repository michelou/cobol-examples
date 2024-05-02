# <span id="top">COBOL examples from Jay Moseley</span> <span style="size:30%;"><a href="../README.md">⬆</a></span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:100px;"><a href="https://www.mainframestechhelp.com/tutorials/cobol/introduction.htm" rel="external"><img style="border:0;" src="../docs/images/cobol.png" width="100" alt="COBOL language"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">Directory <strong><code>moseley-examples\</code></strong> contains <a href="hhttps://www.jaymoseley.com/gnucobol/" alt="COBOL examples">COBOL</a> code examples coming from Moseley's webpage <a href="https://www.jaymoseley.com/gnucobol/" rel="external">"GnuCOBOL"</a>.
  </td>
  </tr>
</table>

## <span id="elapsed">`Elapsed` Example</span>

Example `Elapsed` has the following directory structure :

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

<!--=======================================================================-->

## <span id="timeline">`Timeline` Example</span>

Example `Timeline` has the following directory structure :

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

***

*[mics](https://lampwww.epfl.ch/~michelou/)/May 2024* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[cobc_cmd]: https://gnucobol.sourceforge.io/doc/gnucobol.html
