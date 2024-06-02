# <span id="top">GnuCOBOL</span> <span style="font-size:90%;">[↩](./README.md#top)</span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 4px 0 0;min-width:100px;"><a href=" rel="external"><img style="border:0;" src="docs/images/gnucobol.png" width="100" alt="GnuCOBOL"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">This page presents usage information about <a href="https://">GnuCOBOL </a> on a Windows machine.</td>
  </tr>
</table> 


## <span id="env">GnuCOBOL environment</span>

Configuration ot the <a href="https://gnucobol.sourceforge.io/" rel="external">GnuCOBOL</a> compiler occurs in two ways :
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

***

*[mics](https://lampwww.epfl.ch/~michelou/)/June 2024* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->
[gnucobol]: https://
