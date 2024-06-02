# <span id="top">Playing with COBOL on Windows</span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 4px 0 0;min-width:60px;max-width:100px;">
    <a href="https://www.mainframestechhelp.com/tutorials/cobol/introduction.htm" rel="external"><img style="border:0;" src="docs/images/cobol.png" alt="COBOL language"/></a>
  </td>
  <td style="border:0;padding:0;vertical-align:text-top;">
    This repository gathers <a href="https://www.mainframestechhelp.com/tutorials/cobol/introduction.htm" rel="external">COBOL</a> code examples coming from various websites - mostly from the <a href="https://www.mainframestechhelp.com/tutorials/cobol/introduction.htm" rel="external">Mainframestechhelp</a> website - or written by ourself.<br/>
    In particular it includes several build scripts (<a href="https://www.gnu.org/software/bash/manual/bash.html" rel="external">Bash scripts</a>, <a href="https://en.wikibooks.org/wiki/Windows_Batch_Scripting" rel="external">batch files</a>, <a href="https://makefiletutorial.com/" rel="external">Make scripts</a>) for experimenting with the <a href="https://www.mainframestechhelp.com/tutorials/cobol/introduction.htm" rel="external">COBOL</a> language on a Windows machine.
  </td>
  </tr>
</table>

[Ada][ada_examples], [Akka][akka_examples], [C++][cpp_examples], [Deno][deno_examples], [Docker][docker_examples], [Erlang][erlang_examples], [Flix][flix_examples], [Golang][golang_examples], [GraalVM][graalvm_examples], [Haskell][haskell_examples], [Kafka][kafka_examples], [Kotlin][kotlin_examples], [LLVM][llvm_examples], [Modula-2][m2_examples], [Node.js][nodejs_examples], [Rust][rust_examples], [Scala 3][scala3_examples], [Spark][spark_examples], [Spring][spring_examples], [TruffleSqueak][trufflesqueak_examples] and [WiX Toolset][wix_examples] are other topics we are continuously monitoring.

## <span id="proj_deps">Project dependencies</span>

This project depends on the following external software for the **Microsoft Windows** platform:

- [Git 2.45][git_releases] ([*release notes*][git_relnotes])
- [GnuCOBOL 3.2][gnucobol_binaries] ([*release notes*][gnucobol_relnotes], [*news file*][gnucobol_news])
- [MSYS2 2024][msys2_downloads] <sup id="anchor_01">[1](#footnote_01)</sup> ([*changelog*][msys2_changelog])

Optionally one may also install the following software:

- [ConEmu][conemu_downloads] ([*release notes*][conemu_relnotes])
- [opensource COBOL 4J 1.0][cobj_downloads] ([*release notes*][cobj_relnotes])
- [Temurin OpenJDK 17 LTS][temurin_openjdk17] ([*release notes*][temurin_openjdk17_relnotes])
- [Visual COBOL 9.0][visual_cobol_downloads] ([*release notes*][visual_cobol_relnotes])
- [Visual Studio Code 1.89][vscode_downloads] ([*release notes*][vscode_relnotes])

> **&#9755;** ***Installation policy***<br/>
> When possible we install software from a [Zip archive][zip_archive] rather than via a [Windows installer][windows_installer]. In our case we defined **`C:\opt\`** as the installation directory for optional software tools (*in reference to* the [**`/opt/`**][unix_opt] directory on Unix).

For instance our development environment looks as follows (*June 2024*) <sup id="anchor_02">[2](#footnote_02)</sup>:

<pre style="font-size:80%;">
C:\opt\cobj\<sup id="anchor_03"><a href="#footnote_03">3</a></sup>                                     <i>(  10 MB)</i>
C:\opt\ConEmu\                                    <i>(  26 MB)</i>
C:\opt\Git\                                       <i>( 367 MB)</i>
C:\opt\GnuCOBOL\                                  <i>( 548 MB)</i>
C:\opt\jdk-temurin-17.0.11_9\                     <i>( 301 MB)</i>
C:\opt\msys64\                                    <i>(2.83 GB)</i>
C:\opt\VSCode\                                    <i>( 341 MB)</i>
C:\Program Files (x86)\Micro Focus\Visual COBOL\  <i>(1.26 GB)</i>
</pre>

> **:mag_right:** [Git for Windows][git_releases] provides a Bash emulation used to run [**`git.exe`**][git_cli] from the command line (as well as over 250 Unix commands like [**`awk`**][man1_awk], [**`diff`**][man1_diff], [**`file`**][man1_file], [**`grep`**][man1_grep], [**`more`**][man1_more], [**`mv`**][man1_mv], [**`rmdir`**][man1_rmdir], [**`sed`**][man1_sed] and [**`wc`**][man1_wc]).

## <span id="structure">Directory structure</span> [**&#x25B4;**](#top)

This project is organized as follows:

<pre style="font-size:80%;">
bin\
csis-examples\{<a href="./csis-examples/README.md">README.md</a>, <a href="./csis-examples/MonthTable/">MonthTable</a>, etc.}
docs\
dovey-examples\{<a href="./dovey-examples/README.md">README.md</a>, <a href="./dovey-examples/Colors/">Colors</a>, etc.}
examples\{<a href="./examples/README.md">README.md</a>, <a href="examples/helloworld/">helloworld</a>, etc.}
harris-examples\{<a href="./harris-examples/README.md">README.md</a>, <a href="./harris-examples/100Doors/">100Doors</a>, <a href="./harris-examples/FizzBuzz/">FizzBuzz</a>, etc.}
moseley-examples\{<a href="./moseley-examples/README.md">README.md</a>, <a href="./moseley-examples/Elapsed/">Elapsed</a>, etc.}
<a href="COBOL_4J.md">COBOL_4J.md</a>
<a href="GNUCOBOL.md">GNUCOBOL.md</a>
<a href="VISUAL_COBOL.md">VISUAL_COBOL.md</a>
README.md
<a href="RESOURCES.md">RESOURCES.md</a>
<a href="setenv.bat">setenv.bat</a>
</pre>

where

- directory [**`bin\`**](bin/) provides several utility [batch files][windows_batch_file]
- directory [**`csis-examples`**](./csis-examples/) contains several [COBOL] examples from the [University of Limerick](https://www.ul.ie/).
- directory [**`docs\`**](docs/) contains several [COBOL] related papers/articles ([**`docs\README.md`**](docs/README.md)).
- directory [**`examples\`**](examples/) contains [COBOL] examples grabbed from various websites ([**`examples\README.md`**](examples/README.md)).
- directory [**`harris-examples`**](./harris-examples/) contains [COBOL] examples from [Mike Harris GitHub repository](https://github.com/mikebharris/COBOL-katas).
- file [**`COBOL_4J.md`**](COBOL_4J.md) gathers usage information about [COBOL 4J][cobol_4j].
- file [**`GNUCOBOL.md`**](GNUCOBOL.md) gathers usage information about [GnuCOBOL].
- file [**`VISUAL_COBOL.md`**](VISUAL_COBOL.md) gathers usage information about [Visual COBOL][visual_cobol].
- file [**`README.md`**](README.md) is the [Markdown][github_markdown] document for this page.
- file [**`RESOURCES.md`**](RESOURCES.md) is the [Markdown][github_markdown] document presenting external resources.
- file [**`setenv.bat`**](setenv.bat) is the batch command for setting up our environment.

We also define a virtual drive &ndash; e.g. drive **`T:`** &ndash; in our working environment in order to reduce/hide the real path of our project directory (see article ["Windows command prompt limitation"][windows_limitation] from Microsoft Support).

> **:mag_right:** We use the Windows external command [**`subst`**][windows_subst] to create virtual drives; for instance:
>
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/subst">subst</a> T: <a href="https://en.wikipedia.org/wiki/Environment_variable#Default_values">%USERPROFILE%</a>\workspace\dart-examples</b>
> </pre>

In the next section we give a brief description of the batch files present in this project.

## <span id="commands">Batch/Bash commands</span>

### **`setenv.bat`** <sup id="anchor_04">[4](#footnote_04)</sup>

We execute command [**`setenv.bat`**](setenv.bat) once to setup our development environment; it makes external tools such as [**`git.exe`**][git_cli], [**`make.exe`**][make_cli] and [**`sh.exe`**][sh_cli] directly available from the command prompt.

   <pre style="font-size:80%;">
   <b>&gt; <a href="./setenv.bat">setenv</a></b>
   Tool versions:
   cobc 3.3.0, ccbl 9.0.0.49, cobj 1.0.22, java 17.0.11, make 4.4.1,
   git 2.45.1, diff 3.10, bash 5.2.26(1)-release
   &nbsp;
   <b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/where">where</a> git make sh</b>
   C:\opt\Git\bin\git.exe
   C:\opt\Git\mingw64\bin\git.exe
   C:\opt\msys64\usr\bin\make.exe
   C:\opt\Git\bin\sh.exe
   C:\opt\Git\usr\bin\sh.exe
   </pre>

> :mag_right: Command [**`setenv help`**](./setenv.bat) displays the help messsage :

<!--=======================================================================-->

## <span id="footnotes">Footnotes</span> [**&#x25B4;**](#top)

<span id="footnote_01">[1]</span> ***MSYS2 libraries*** [↩](#anchor_01)

<dl><dd>
Some COBOL examples depend on the <a href="https://packages.msys2.org/package/gmp" rel="external">gmp</a> library &ndash; A free library for arbitrary precision arithmetic &ndash; which is part of our local MSYS2 installation.
</dd></dl>

<span id="footnote_02">[2]</span> ***Downloads*** [↩](#anchor_02)

<dl><dd>
In our case we downloaded the following installation files (<a href="#proj_deps">see section 1</a>):
</p>
<pre style="font-size:80%;">
<a href="https://github.com/Maximus5/ConEmu/releases/tag/v23.07.24" rel="external">ConEmuPack.230724.7z</a>                               <i>(  5 MB)</i>
<a href="https://get-superbol.com/software/gnucobol-windows-installer/aio-release/">gnucobol-3.2-aio-20240306-user.msi</a>                 <i>( 92 MB)</i>
<a href="https://repo.msys2.org/distrib/x86_64/">msys2-x86_64-20240113.exe</a>                         <i> ( 83 MB)</i>
<a href="https://adoptium.net/releases.html?variant=openjdk17&jvmVariant=hotspot">OpenJDK17U-jdk_x64_windows_hotspot_17.0.11_9.zip</a>   <i>(188 MB)</i>
<a href="https://github.com/opensourcecobol/opensourcecobol4j/releases" rel="external">opensourcecobol4j-1.0.22.zip</a>                       <i>(  8 MB)</i>
<a href="https://git-scm.com/download/win">PortableGit-2.45.1-64-bit.7z.exe</a>                   <i>( 41 MB)</i>
<a href="https://code.visualstudio.com/Download#" rel="external">VSCode-win32-x64-1.89.1.zip</a>                        <i>(131 MB)</i>
<a href="https://">vcvs2022_90.exe</a> (for Visual Studio 2022)           <i>(820 MB)</i>
</pre>
<p style="background-color:#eeeeee;">
<b>&#9755; <i>GnuCOBOL distribution</i></b><br/>
 We prefer the <a href="https://get-superbol.com/software/gnucobol-windows-installer/aio-release/">All-in-One distribution</a> from <a href="https://get-superbol.com/" rel="external">SuperBOL</a> over the <a href="https://www.arnoldtrembley.com/GnuCOBOL.htm">GnuCOBOL distribution</a> from Arnold Trembley.
</p>
</dd></dl>

<span id="footnote_03">[3]</span> ***COBOL 4J*** [↩](#anchor_03)

<dl><dd>
We built the COBOL 4J distribution from the source archive <code><a href="https://github.com/opensourcecobol/opensourcecobol4j/releases" rel="external">opensourcecobol4j-1.0.22.zip</a></code> and installed it into directory <code>C:\opt\cobj\</code>. The 3 build steps are described in the <a href="https://github.com/opensourcecobol/opensourcecobol4j#install-opensource-cobol-4j">COBOL 4J online documentation</a>:
<pre style="font-size:80%;">
<b>&gt; <a href="https://">sh</a> ./configure --prefix=/c/opt/cobj</b>
<b>&gt; <a href="https://">sh</a> make</b>
<b>&gt; <a href="https://">sh</a> make install</b>
</pre> 
In our case the installation directory <code>C:\opt\cobj\</code> looks as follows :
<pre style="font-size:80%;">
<b>&gt; <a href="">tree</a> /a /f c:\opt\cobj | <a href="">findstr</a> /v /b [A-Z]</b>
+---bin
|       cob-config
|       cobj-api
|       cobj-idx
|       cobj.exe
|       cobjrun.exe
+---include
|       libcobj.h
+---lib
|   \---opensourcecobol4j
|           libcobj.jar
\---share
    \---opensource-cobol-4j-1.0.22
        +---config
        |       bs2000.conf
        |       cobol2002.conf
        |       cobol85.conf
        |       default.conf
        |       ibm.conf
        |       mf.conf
        |       mvs.conf
        |
        \---copy
                screenio.cpy
</pre>
</dd></dl>

<span id="footnote_04">[4]</span> **`setenv.bat` *usage*** [↩](#anchor_04)

<dl><dd>
Batch file <a href=./setenv.bat><code><b>setenv.bat</b></code></a> has specific environment variables set that enable us to use command-line developer tools more easily.
</dd>
<dd>It is similar to the setup scripts described on the page <a href="https://learn.microsoft.com/en-us/visualstudio/ide/reference/command-prompt-powershell" rel="external">"Visual Studio Developer Command Prompt and Developer PowerShell"</a> of the <a href="https://learn.microsoft.com/en-us/visualstudio/windows" rel="external">Visual Studio</a> online documentation.
</dd>
<dd>
For instance we can quickly check that the two scripts <code><b>Launch-VsDevShell.ps1</b></code> and <code><b>VsDevCmd.bat</b></code> are indeed available in our Visual Studio 2019 installation :
<pre style="font-size:80%;">
<b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/where" rel="external">where</a> /r "C:\Program Files (x86)\Microsoft Visual Studio" *vsdev*</b>
C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\Common7\Tools\Launch-VsDevShell.ps1
C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\Common7\Tools\VsDevCmd.bat
C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\Common7\Tools\vsdevcmd\core\vsdevcmd_end.bat
C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\Common7\Tools\vsdevcmd\core\vsdevcmd_start.bat
</pre>
</dd>
<dd>
Concretely, in our GitHub projects which depend on Visual Studio (e.g. <a href="https://github.com/michelou/cpp-examples"><code>michelou/cpp-examples</code></a>), <a href="./setenv.bat"><code><b>setenv.bat</b></code></a> does invoke <code><b>VsDevCmd.bat</b></code> (resp. <code><b>vcvarall.bat</b></code> for older Visual Studio versions) to setup the Visual Studio tools on the command prompt. 
</dd></dl>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/June 2024* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[ada_examples]: https://github.com/michelou/ada-examples#top
[akka_examples]: https://github.com/michelou/akka-examples#top
[cobj_downloads]: https://github.com/opensourcecobol/opensourcecobol4j
[cobj_relnotes]: https://github.com/opensourcecobol/opensourcecobol4j/releases/tag/v1.0.22
[cobol]: https://en.wikipedia.org/wiki/COBOL
[cobol_4j]: https://github.com/opensourcecobol/opensourcecobol4j
[conemu_downloads]: https://github.com/Maximus5/ConEmu/releases
[conemu_relnotes]: https://conemu.github.io/blog/2023/07/24/Build-230724.html
[cpp_examples]: https://github.com/michelou/cpp-examples#top
[deno_examples]: https://github.com/michelou/deno-examples#top
[docker_examples]: https://github.com/michelou/docker-examples#top
[erlang_examples]: https://github.com/michelou/erlang-examples#top
[flix_examples]: https://github.com/michelou/flix-examples#top
[git_cli]: https://git-scm.com/docs/git
[git_releases]: https://git-scm.com/download/win
[git_relnotes]: https://raw.githubusercontent.com/git/git/master/Documentation/RelNotes/2.45.1.txt
[github_markdown]: https://github.github.com/gfm/
[gnucobol]: https://gnucobol.sourceforge.io/
[gnucobol_binaries]: https://get-superbol.com/software/gnucobol-windows-installer/aio-release/
[gnucobol_news]: https://sourceforge.net/p/gnucobol/code/HEAD/tree/tags/gnucobol-3.2/NEWS
[gnucobol_relnotes]: https://gnucobol.sourceforge.io/index.html#Releases
[golang_examples]: https://github.com/michelou/golang-examples#top
[graalvm_examples]: https://github.com/michelou/graalvm-examples#top
[haskell_examples]: https://github.com/michelou/haskell-examples#top
[jmh]: https://openjdk.java.net/projects/code-tools/jmh/
[kafka_examples]: https://github.com/michelou/kafka-examples#top
[kotlin_examples]: https://github.com/michelou/kotlin-examples#top
[llvm_examples]: https://github.com/michelou/llvm-examples#top
[m2_examples]: https://github.com/michelou/m2-examples
[make_cli]: https://www.gnu.org/software/make/manual/html_node/Running.html
[man1_awk]: https://www.linux.org/docs/man1/awk.html
[man1_diff]: https://www.linux.org/docs/man1/diff.html
[man1_file]: https://www.linux.org/docs/man1/file.html
[man1_grep]: https://www.linux.org/docs/man1/grep.html
[man1_more]: https://www.linux.org/docs/man1/more.html
[man1_mv]: https://www.linux.org/docs/man1/mv.html
[man1_rmdir]: https://www.linux.org/docs/man1/rmdir.html
[man1_sed]: https://www.linux.org/docs/man1/sed.html
[man1_wc]: https://www.linux.org/docs/man1/wc.html
[msys2_changelog]: https://github.com/msys2/setup-msys2/blob/main/CHANGELOG.md
[msys2_downloads]: http://repo.msys2.org/distrib/x86_64/
[nodejs_examples]: https://github.com/michelou/nodejs-examples#top
[rust_examples]: https://github.com/michelou/rust-examples#top
[scala3_examples]: https://github.com/michelou/dotty-examples#top
[scala3_metaprogramming]: https://dotty.epfl.ch/docs/reference/metaprogramming/toc.html
[sh_cli]: https://man7.org/linux/man-pages/man1/sh.1p.html
[spark_examples]: https://github.com/michelou/spark-examples#top
[spring_examples]: https://github.com/michelou/spring-examples#top
<!--
17.0.7  -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2023-April/021899.html
17.0.8  -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2023-July/024063.html
17.0.9  -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2023-October/026352.html
17.0.10 -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2024-January/029089.html
17.0.11 -> https://mail.openjdk.org/pipermail/jdk-updates-dev/2024-April/032197.html
-->
[temurin_openjdk17]: https://adoptium.net/releases.html?variant=openjdk17&jvmVariant=hotspot
[temurin_openjdk17_relnotes]: https://mail.openjdk.org/pipermail/jdk-updates-dev/2024-April/032197.html
[trufflesqueak_examples]: https://github.com/michelou/trufflesqueak-examples#top
[unix_opt]: https://tldp.org/LDP/Linux-Filesystem-Hierarchy/html/opt.html
[visual_cobol]: https://www.microfocus.com/en-us/products/visual-cobol/overview
[visual_cobol_downloads]: https://www.microfocus.com/en-us/products/visual-cobol/overview
[visual_cobol_relnotes]: https://portal.microfocus.com/s/article/KM000020023?language=en_US
[vscode_downloads]: https://code.visualstudio.com/#alt-downloads
[vscode_relnotes]: https://code.visualstudio.com/updates/
[windows_batch_file]: https://en.wikibooks.org/wiki/Windows_Batch_Scripting
[windows_installer]: https://docs.microsoft.com/en-us/windows/win32/msi/windows-installer-portal
[windows_limitation]: https://support.microsoft.com/en-gb/help/830473/command-prompt-cmd-exe-command-line-string-limitation
[windows_subst]: https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/subst
[wix_examples]: https://github.com/michelou/wix-examples#top
[zip_archive]: https://www.howtogeek.com/178146/htg-explains-everything-you-need-to-know-about-zipped-files/
