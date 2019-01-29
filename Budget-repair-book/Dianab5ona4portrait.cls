\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{Dianab5portrait}[2015/05/18 A book of the Grattan Institute, Melbourne]

\LoadClass[%
11pt,%
b5paper,
%BCOR=10mm,
titlepage,headings=normal,chapterprefix=false,
%parskip=half-,
numbers=noenddot,twoside,bibliography=totoc,captions=tablesignature,english]{scrbook}
% Permits pagebreaks within pages
%\interlinepenalty=-50
%
\RequirePackage[english]{babel}
\RequirePackage{cmap}
\RequirePackage{scrhack}
\RequirePackage{acronym}
\RequirePackage{makeidx}
  \makeindex
\RequirePackage[cmyk]{xcolor}
\RequirePackage{amsmath,amssymb}
\RequirePackage{tikz}
\RequirePackage[export]{adjustbox}
  \usetikzlibrary{positioning}
  \usetikzlibrary{decorations.text}
  \usetikzlibrary{decorations.pathmorphing}
  \usetikzlibrary{patterns,arrows,decorations.pathreplacing} % for waterfalls
\RequirePackage{float}
\RequirePackage{placeins}
\RequirePackage{afterpage}
\RequirePackage[final]{microtype}
%\frenchspacing
\linespread{1.04}
%% defaults are far too flamboyant. Indeed, zero protrusion with the 
%% Helvetica font appears to be protruding slightly (even though it 
%% is simply flush with the box).
%% encoding = T1 as in \usepackage[T1]{fontenc}
%\SetProtrusion
%   [ name     = T1-phv,      % the name is optional
%     load     = T1-default ] % first load `T1-default` settings
   %{ encoding = T1 }        % use for Helvetica family
   %{
   %  \textendash = {-50, }, \textemdash = {-50, }  % cancel out left protrusion
   %}

%\SetExtraSpacing
%[ unit = space ] % default
%{ font = * / * / * / * / * }
%{
%. = {-500,-500,-500},
%}
%\DisableLigatures{encoding = T1, family = *}
% ------
% Grattan colors
\definecolor{Orange}{HTML/cmyk}{F68B33/0,0.435,0.7927,0.0353}
\definecolor{DarkOrange}{HTML/cmyk}{D4582A/0,0.5849,0.8019,0.1686}
\definecolor{OrangeBackground}{RGB/cmyk}{254,240,222/0,0.06,0.13,0}  % for boxes
\definecolor{Color1}{RGB}{255,224,127}
\definecolor{Color2}{RGB}{255,195,90}
\definecolor{Color3}{RGB}{246,139,51}
\definecolor{Color4}{RGB}{212,88,42}
\definecolor{Color5}{RGB}{160,34,38}
\definecolor{Color6}{RGB}{98,18,20}
\definecolor{theGrey}{HTML}{6A737B}
\definecolor{AuthorPage}{RGB}{160,34,38}
\definecolor{AuthorGrey}{RGB}{174,174,174}

% See http://tex.stackexchange.com/questions/8510/reduce-size-of-bullet-character-in-lists
% Purpose to enlarge and subsequently raise the bullet
% see also \renewcommand{\labelitemi}{\raise .5ex\hbox{\tiny$\bullet$}}
\renewcommand{\labelitemi}{\color{Orange}\parbox[c]{.5em}{\ensuremath{\bullet}}}
\renewcommand{\labelenumi}{\color{Orange}{\bfseries \arabic{enumi}.~}}
% ------
\RequirePackage{colortbl}
\RequirePackage{array}
\RequirePackage{tablefootnote} % for footnotes within tables
\newcommand{\myhline}{\noalign{\global\arrayrulewidth1pt}\hline
                      \noalign{\global\arrayrulewidth1pt}}


\RequirePackage{graphicx}

% No widows
\RequirePackage[all]{nowidow}

% Swiss font
\usepackage{fontspec}
\setmainfont{Swiss}[Path = D:/fonts/, UprightFont = Swiss721Light.ttf, BoldFont = Swiss721MdBT.ttf, ItalicFont= Swiss721Italic.ttf, BoldItalicFont = Swiss721BoldItalicX.ttf, FontFace={k}{n}{Swiss721black.ttf},  SmallCapsFeatures={Letters=SmallCaps}]
\setsansfont{Swiss}[Path = D:/fonts/, UprightFont = Swiss721Light.ttf, BoldFont = Swiss721MdBT.ttf, ItalicFont= Swiss721Italic.ttf, BoldItalicFont = Swiss721-BoldItalic-BT-WGL4.ttf, SmallCapsFeatures={Letters=SmallCaps}]
\hyphenpenalty=700 % default 100
\exhyphenpenalty=750 % default 200
\brokenpenalty=500 % default 50
\linepenalty=2 % default 10
\doublehyphendemerits=20000 % default 10,000
\newenvironment{specialpar}[1]{\par#1}{\par}
\lefthyphenmin=3 % default 2
\righthyphenmin=3 % default 3

\RequirePackage[framemethod=TikZ]{mdframed}
\RequirePackage{newfloat}
\RequirePackage{caption}
% Cross references.
\RequirePackage{varioref}
% \RequirePackage{navigator}
% \newcommand\Chapter[1]{%
%   \chapter[\protect\jumplink{ch\thechapter}{#1}]{#1}%
%   \outline[fit=fitv]{1}[ch\thechapter]{#1}%
% }
\RequirePackage[draft, raiselinks, hidelinks, urlcolor=blue]{hyperref}
\RequirePackage{cleveref}

% Make url formatting the same
\urlstyle{same}

\DeclareCaptionFont{Orange}{\color{Orange}}
\DeclareCaptionJustification{nohyphen}{\hyphenpenalty=10000}
\captionsetup{justification=raggedright, singlelinecheck=false}

% Boxes
\crefname{boxe}{Box}{Boxes}
\Crefname{boxe}{Box}{Boxes}
\DeclareFloatingEnvironment[listname={List of boxes}, name = {Box}]{boxe}
\mdfdefinestyle{GrattanFrameBox}{%
    linecolor=Orange,
    nobreak=true, % prevents page breaking
    outerlinewidth=0.5pt,
    innertopmargin=1\baselineskip,
    innerbottommargin=1.5\baselineskip,
    innerrightmargin=15.5pt,
    innerleftmargin=15.5pt,
    backgroundcolor=OrangeBackground
    }
    
\mdfdefinestyle{GrattanFrameBoxUltra}{%
    linecolor=Orange,
    nobreak=false, % prevents page breaking
    outerlinewidth=0.5pt,
    innertopmargin=0.5\baselineskip,
    innerbottommargin=\baselineskip,
    innerrightmargin=11pt,
    innerleftmargin=11pt,
    backgroundcolor=OrangeBackground
    }

\newenvironment{verysmallbox}[3][htb]{%
\setlength{\currentparskip}{\parskip}% save the value
% centre the float in the page.
\begin{boxe}[#1]
\begin{mdframed}[style=GrattanFrameBox]%
\setlength{\parskip}{\currentparskip}% restore the value
\captionsetup{labelfont={bf,Orange},position=top,skip=1.5ex,font={bf,Orange},format=plain,justification=raggedright,singlelinecheck=false}
\caption{#2}\label{#3}
\vspace{-1.5ex}
}{\end{mdframed}\end{boxe}} 

% The smallbox is a box intended for onecolumn.
\newenvironment{smallbox}[3][p]{%
\setlength{\currentparskip}{\parskip}% save the value
% centre the float in the page.
\begin{boxe}[#1]
\begin{minipage}[c][\textheight]{\linewidth}
\begin{mdframed}[style=GrattanFrameBox]%
\setlength{\parskip}{\currentparskip}% restore the value
\captionsetup{labelfont={bf,Orange},position=top,skip=1.5ex,font={bf,Orange},format=plain,justification=raggedright,singlelinecheck=false}
\caption{#2}\label{#3}
\vspace{-1.5ex}
}{\end{mdframed}\end{minipage}\end{boxe}} 

\newlength{\innerMargin}
\setlength{\innerMargin}{2.30cm}
\newlength{\outerMargin}
\setlength{\outerMargin}{2.05cm}

\newlength{\bigboxwidth}
\setlength{\bigboxwidth}{0.975\paperwidth}
%\addtolength{\bigboxwidth}{\outerMargin}

\mdfdefinestyle{GrattanFrameBoxUltra}{%
    linecolor=Orange,
    userdefinedwidth=\bigboxwidth,
    nobreak=false, % prevents page breaking
    outerlinewidth=0.5pt,
    innertopmargin=0.5\baselineskip,
    innerbottommargin=\baselineskip,f
    innerrightmargin=11pt,
    innerleftmargin=11pt,
    backgroundcolor=OrangeBackground
    }

% Big boxes -- over the whole page
\RequirePackage[strict]{changepage}

\newenvironment{bigboxC*}[2]{%
  \setlength{\currentparskip}{\parskip}
  \begin{boxe}
  \centering
  \begin{adjustwidth*}{-0.95\innerMargin}{-0.95\outerMargin}
  \begin{mdframed}[style=GrattanFrameBoxUltra]
  \setlength{\columnsep}{10mm}
  \begin{multicols}{2}
  \captionsetup{labelfont={bf,Orange}, position=top, skip=0pt, font={bf,Orange}, format=plain, justification=raggedright, singlelinecheck=false}
  \caption{#1}\label{#2}%
  \setlength{\parskip}{\currentparskip}% restore the value
}{%
  \end{multicols}\end{mdframed}\end{adjustwidth*}\end{boxe}
}

\newenvironment{bigboxCbottom*}[3]{%
  \setlength{\currentparskip}{\parskip}
  \begin{boxe}
  \centering
  \begin{adjustwidth*}{-0.95\innerMargin}{-0.95\outerMargin}
  \begin{mdframed}[style=GrattanFrameBoxUltra]
  \setlength{\columnsep}{5mm}
  \captionsetup{labelfont={bf,Orange}, position=top, skip=0pt, font={bf,Orange}, format=plain, justification=raggedright, singlelinecheck=false}
  \caption{#1}\label{#2}%
  \begin{multicols}{2}
  \setlength{\parskip}{\currentparskip}% restore the value
}{%
  \end{multicols}
  %#3
  \end{mdframed}\end{adjustwidth*}\end{boxe}
}


\newenvironment{ultrabox}[2]{\setlength{\currentparskip}{\parskip}
\begin{boxe}
\pagecolor{OrangeBackground}
\begin{mdframed}[style=GrattanFrameBoxUltra]%
\setlength{\columnsep}{10mm}
\begin{minipage}[t][\textheight][t]{\textwidth} % textheight
\begin{multicols}{2}
\setlength{\parskip}{\currentparskip}% restore the value
 \captionsetup{labelfont={bf,Orange}, font={bf,Orange}, format=plain,justification=justified,singlelinecheck=false}
 \caption{#1}\label{#2}%
 } {\end{multicols}\end{minipage}\end{mdframed}\end{boxe}%
 \clearpage\nopagecolor}

\newlength{\twopageboxwidth}
\setlength{\twopageboxwidth}{2\textwidth+2\innerMargin}  %BCOR
\newlength{\twopageboxtextwidth}
\setlength{\twopageboxtextwidth}{\paperwidth-\innerMargin-\outerMargin}
%\setlength{\twopageboxtextwidth}{\paperwidth}

\mdfdefinestyle{GrattanFrameBoxdblpage}{%
    linecolor=Orange,
    userdefinedwidth=\twopageboxwidth,
    nobreak=false, % prevents page breaking
    outerlinewidth=0.5pt,
    innertopmargin=0pt,
    innerbottommargin=15.5pt,
    innerrightmargin=7.5pt,%15.5pt,
    innerleftmargin=7.5pt,%15.5pt,
    backgroundcolor=OrangeBackground
    }

\RequirePackage{dpfloat}
% xelatex
\RequirePackage{pagecolor}

\newenvironment{lultrabox}[2]{\setlength{\currentparskip}{\parskip}
\begin{boxe}[p]
\begin{leftfullpage}
\newpagecolor{OrangeBackground}
\begin{addmargin}[0em]{\textwidth-\twopageboxwidth+30pt}
\captionsetup{labelfont={bf,Orange}, font={bf,Orange}, skip=5pt, format=plain,justification=justified, singlelinecheck=false}
\caption{#1}\label{#2}%
\begin{mdframed}[style=GrattanFrameBoxdblpage]%
\setlength{\columnsep}{2\innerMargin+33pt}
\begin{multicols}{2}
\setlength{\parskip}{\currentparskip}% restore the value
 } {\end{multicols}\end{mdframed}\end{addmargin}\end{leftfullpage}\end{boxe}}

\newenvironment{rultrabox}[2]{\setlength{\currentparskip}{\parskip}
\begin{boxe}[p]
\begin{fullpage}
\thispagestyle{empty}
\begin{addmargin}[-\twopageboxwidth+\textwidth+30pt]{0em}
\captionsetup{labelfont={bf,Orange}, font={bf,Orange}, skip=5pt, format=plain, justification=justified, singlelinecheck=false}
% avoid extra counter
\caption*{#1}\label{#2}%
\begin{mdframed}[style=GrattanFrameBoxdblpage]%
\setlength{\columnsep}{2\innerMargin+33pt}
\begin{multicols}{2}
\setlength{\parskip}{\currentparskip}% restore the value
 } {\end{multicols}\end{mdframed}\end{addmargin}\end{fullpage}\end{boxe}\pagestyle{empty}%
 \clearpage\restorepagecolor}


\newcommand{\lultraboxCom}[3]{
\begin{lultrabox}{#1}{#2}%
#3%
\end{lultrabox}%
}

\newcommand{\rultraboxCom}[3]{
\begin{rultrabox}{#1}{#2:alt}%
#3%
\end{rultrabox}%
}

\newcommand{\twopageboxN}[3]{\lutraboxCom{#1}{#2}{#3}\par\rultraboxCom{#1}{#2}{#3}}

\newcommand{\twopagebox}[5]{%
    \checkoddpage
    \ifoddpage
        \expandafter\@firstofone
    \else
        \expandafter\afterpage
    \fi
    {\afterpage{%
    \if #1t%
        \if #2p%
            \thispagestyle{empty}%%
        \fi
    \fi
    \begin{figure}[#1]
        \if #2p%
            \if #1t%
                \vspace*{-\dimexpr1in+\voffset+\topmargin+\headheight+\headsep\relax}%
            \fi
        \fi
        \makebox[\textwidth][l]{%
        \if #2p\relax
            \let\mywidth\paperwidth
            \hskip-\dimexpr1in+\hoffset+\evensidemargin\relax
        \else
            \let\mywidth\linewidth
        \fi
        \adjustbox{trim=0 0 {.5\twopageboxwidth} 0,clip}{\lultraboxCom{#3}{#4}{#5}}}%
        \if #2p%
            \if #1b%
                \vspace*{-\dimexpr\paperheight-\textheight-1in-\voffset-\topmargin-\headheight-\headsep\relax}%
            \fi
        \fi
    \end{figure}%
    \begin{figure}[#1]
        \if #2p%
            \if #1t%
                \vspace*{-\dimexpr1in+\voffset+\topmargin+\headheight+\headsep\relax}%
            \fi
        \fi
        \makebox[\textwidth][l]{%
        \if #2p%
            \let\mywidth\paperwidth
            \hskip-\dimexpr1in+\hoffset+\oddsidemargin\relax
        \else
            \let\mywidth\linewidth
        \fi
        \adjustbox{trim={.5\twopageboxwidth} 0 0 0,clip}{\rultraboxCom{#3}{#4}{#5}}}%
        \if #2p%
            \if #1b%
                \vspace*{-\dimexpr\paperheight-\textheight-1in-\voffset-\topmargin-\headheight-\headsep\relax}%
            \fi
        \fi
    \end{figure}%
    }
    }%
} 

% Captions in general
\DeclareCaptionFont{theGrey}{\color{theGrey}}
\captionsetup{format=plain,font={small,bf,theGrey},labelfont={small,bf,theGrey},position=top, justification=raggedright}
% suppress hyphenation in caption

\captionsetup[figure]{font={small,bf,sf,theGrey},labelfont={small,bf,theGrey}, justification=raggedright,singlelinecheck=false,position=top,format=plain}
\captionsetup[table]{font={small,bf,theGrey}, labelfont={small,bf,theGrey}, justification=raggedright,singlelinecheck=false,position=top,format=plain}

\newcommand{\captionwithunits}[2]{%
\captionsetup{format=plain,font={small,bf,theGrey},labelfont={small,bf,theGrey}, justification=raggedright,
singlelinecheck=false,position=top,skip=0pt}
\captionabove{{\normalsize\strut}#1}
%\vspace{-11pt}
\captionsetup{font={small,color=theGrey}}
\caption*{#2}
}

\newcommand{\captionoffigurewithunits}[2]{%
\captionsetup{format=plain,font={small,bf,theGrey},labelfont={small,bf,theGrey}, justification=raggedright,
singlelinecheck=false}
\captionof{figure}{#1}
\vspace{-11pt}
\captionsetup{font={small,color=theGrey}}
\caption*{#2}
}

% New user friendly (less typing) \Caption?
\newcommand{\Caption}[3]{\captionwithunits{#1}{#2}\label{#3}}

\newcommand{\source}[1]{\par\scriptsize\textit{Source: #1}}
\newcommand{\notes}[1]{%
\captionsetup{format=plain, font={scriptsize, it}, skip=4pt, justification=justified, singlelinecheck=false, position=below}
\caption*{Notes:\ #1}
}

\newcommand{\longsource}[1]{\scriptsize\begin{tabular}[t]{lp{0.85\linewidth}}\textit{Source:} & \textit{#1}\end{tabular}}
\newcommand{\longnotes}[1]{\scriptsize\begin{tabular}[t]{lp{0.85\linewidth}}\textit{Notes:} & \textit{#1}\end{tabular}}

\newcommand{\dynamiclabel}[1]{\csname label\endcsname{#1}} 
\newcommand{\dynamicref}[2]{\csname hyperref\endcsname[#1]{#2}}

\newcommand\figurenotes{%
    \begingroup
    \addchap{Figure notes}
    \setlength\leftmargini{4.5em}
    \begin{itemize}%
    \small}

    \newcommand*{\printfigurenotes}{%
        \figurenotes\end{itemize}\endgroup}

% Key component:
\newcommand{\fnotes}[2]{%
    \notes{\dynamicref{#1:note}{See \cpageref{#1:note}.}}%
    \g@addto@macro\figurenotes{\item[\phantomsection\dynamicref{#1}{\textbf{Figure\ \ref*{#1}}}] \dynamiclabel{#1:note} #2}\par}


\newcounter{fnotesec}
\setcounter{fnotesec}{0}
\pretocmd{\part}%
{\g@addto@macro\figurenotes{\section*{Part~\stepcounter{fnotesec}\Roman{fnotesec}}}}%
{}%
{}%

\newcommand\tablenotes{%
    \begingroup
    \addchap{Table notes}
    \setlength\leftmargini{4.5em}
    \begin{itemize}%
    \small}

    \newcommand*{\printtablenotes}{%
        \tablenotes\end{itemize}\endgroup}

% Key component:
\newcommand{\tnotes}[2]{%
    \notes{\dynamicref{#1:note}{See \cpageref{#1:note}.}}%
    \g@addto@macro\tablenotes{\item[\phantomsection\dynamicref{#1}{\textbf{Table\ \ref*{#1}}}] \dynamiclabel{#1:note} #2}\par}

\newcounter{tnotesec}
\setcounter{tnotesec}{0}
\pretocmd{\part}%
{\g@addto@macro\tablenotes{\section*{Part~\stepcounter{tnotesec}\Roman{tnotesec}}}}%
{}%
{}%

% Page floats appear 15% of the textheight down the page (portrait)
% *unless* \vfill is called (as it is in smallbox) in which
% case the float can move up (almost all the way to the top, if needed).
\setlength{\@fptop}{0.15\textheight minus 0.15\textheight}
\setlength{\@fpbot}{0pt plus 1fil}

\setkomafont{disposition}{\color{Orange}}
\addtokomafont{chapter}{\bfseries\huge}
\addtokomafont{section}{\normalsize\fontseries{k}\selectfont}
\addtokomafont{subsection}{\normalsize\fontseries{k}\selectfont}

\renewcommand*{\chapterformat}{%
  \mbox{\scalebox{1.61803}{\thechapter\autodot}}}
\renewcommand\chapterlinesformat[3]{%
  \parbox[c]{\textwidth}{#2}\par\medskip
  {\color{black}{\rule{\linewidth}{0.5pt}}}\par\medskip%
  \parbox{\textwidth}{\raggedright #3}\par\medskip
  {\color{black}{\rule{\linewidth}{0.5pt}}}%
}%

\renewcommand\chapterlinesformat[3]{%
  \parbox[c]{\textwidth}{#2}\par\medskip
  {\color{black}{\rule{\linewidth}{0.5pt}}}\par\smallskip%
  \parbox[c]{\textwidth}{\raggedright #3}\par\smallskip
  {\color{black}{\rule{\linewidth}{0.5pt}}}%
}%


% Spacing
\RedeclareSectionCommand[innerskip=0pt,
  beforeskip=0\baselineskip plus 0.0\baselineskip minus 0.0\baselineskip,
  afterskip=3\baselineskip minus 1\baselineskip]{chapter}

\RedeclareSectionCommand[
  beforeskip=0.5\baselineskip plus 3ex minus 1ex,
  afterskip=1sp plus 0pt minus 0pt]{section}
\RedeclareSectionCommand[
  beforeskip=0.5\baselineskip plus 2ex,
  afterskip=1sp]{subsection}
\RedeclareSectionCommand[
  beforeskip=-.5\baselineskip,
  afterskip=1sp]{subsubsection}
\RedeclareSectionCommand[
  beforeskip=.5\baselineskip,
  afterskip=-1em]{paragraph}
\RedeclareSectionCommand[
  beforeskip=-.5\baselineskip,
  afterskip=-1em]{subparagraph}

% parskip
\setparsizes{0em}{0.5\baselineskip plus .25\baselineskip}{1em plus 1fil} 

% Put numbers in margin
%\renewcommand\sectionlinesformat[4]{%
%  \makebox[0pt][r]{#3}#4%
%}

\newcommand*\marginnumber[1]{\makebox[0pt][r]{\makebox[1.5cm][c]{#1}}}
%\renewcommand*\chapterformat{\marginnumber{\thechapter\autodot}}
\renewcommand*\othersectionlevelsformat[3]{\marginnumber{#3\autodot}}

\newcommand{\twopagepicture}[8]{%
    \checkoddpage
    \ifoddpage
        \expandafter\@firstofone
    \else
        \expandafter\afterpage
    \fi
    {\afterpage{%
    \if #1t%
        \if #2p%
            \thispagestyle{empty}%
            \afterpage{\thispagestyle{empty}}%
        \fi
    \fi
    \begin{figure}[#1]
        \if #2p%
            \if #1t%
                \vspace*{-\dimexpr1in+\voffset+\topmargin+\headheight+\headsep\relax}%
            \fi
        \fi
        \if #1t%
            \captionwithunits{#4}{#5}%
        \fi
        \makebox[\textwidth][l]{%
        \if #2p\relax
            \let\mywidth\paperwidth
            \hskip-\dimexpr1in+\hoffset+\evensidemargin\relax
        \else
            \let\mywidth\linewidth
        \fi
        \adjustbox{trim=0 0 {.7\width} 0}{\includegraphics[width=2\mywidth]{#3}}}%
        \if #1b\else
            \caption*{\source{#7}}%
        \fi
        \if #2p%
            \if #1b%
                \vspace*{-\dimexpr\paperheight-\textheight-1in-\voffset-\topmargin-\headheight-\headsep\relax}%
            \fi
        \fi
    \end{figure}%
    \begin{figure}[#1]
        \if #2p%
            \if #1t%
                \vspace*{-\dimexpr1in+\voffset+\topmargin+\headheight+\headsep\relax}%
            \fi
        \fi
        \if #1t%
            \caption*{\phantom{#4}}%
        \fi
        \makebox[\textwidth][l]{%
        \if #2p%
            \let\mywidth\paperwidth
            \hskip-\dimexpr1in+\hoffset+\oddsidemargin\relax
        \else
            \let\mywidth\linewidth
        \fi
        
        % .6 magical for waterfall image
        \adjustbox{trim={.7\width} 0 0 0}{\includegraphics[width=2\mywidth]{#3}}}%
        \if #1b\else
            \caption*{\phantom{#7}}%
        \fi
        \if #2p%
            \if #1b%
                \vspace*{-\dimexpr\paperheight-\textheight-1in-\voffset-\topmargin-\headheight-\headsep\relax}%
            \fi
        \fi
    \end{figure}%
    }}%
}

% N.B. geometry must be after hyperref, and hence cleveref

\RequirePackage[
a4paper,
layout=b5paper,
top=20mm,
bottom=30mm,
bindingoffset=10mm,
% orig Word specify 2.3 and 2.5
inner=\innerMargin,outer=\outerMargin
]{geometry}
%\frenchspacing
  %\addtolength{\columnwidth}{50pt} 
  %\addtolength{\columnwidth}{0.2\columnwidth}

% Ensures hyperlinked chapter headings refer to their referent headings
% not the preceding baseline
\RequirePackage{etoolbox}
% Add extra hyper target for chapter: chapter..\thechapter

 

  
%% ---------------------------------
%  Headers and footers
%% ---------------------------------
\RequirePackage[headsepline=0pt,plainheadsepline,footsepline=0pt,plainfootsepline]{scrlayer-scrpage}

\addtolength{\textheight}{00pt}
%\addtolength{\footskip}{-10pt}

\clearscrheadings
\clearscrplain
\clearscrheadfoot
% -------
% Heads and footers
%
\newcommand{\classification}{}
% Avoid spaces or newlines in the following commands' arguments: they 
% may cause errors.
% Furthermore, optional arguments are required: they appear in Chapter 
% heading pages. Leaving them out will reset the titles on pages with 
% \chapter{}
\pagestyle{scrheadings}
\clearpairofpagestyles
\automark[part]{chapter}
%\lehead[]{\rightmark}
\lehead[]{}
\rehead[]{}
\lohead[]{}
\rohead[]{}
%\rohead[\leftmark]{}
\setkomafont{pageheadfoot}{\normalfont\color{theGrey}}



\ifoot[]{}
\cfoot[\Large \strut]{\Large \strut}
\ofoot[\normalfont\textcolor{theGrey}{\thepage}]{\normalfont\textcolor{theGrey}{\thepage}}

\setkomafont{headsepline}{\color{Orange}}
\setkomafont{footsepline}{\color{Orange}}

% sets part pages to be empty
\renewcommand*\partpagestyle{empty}



\RequirePackage{eso-pic}

\newcommand{\logoskip}{\par\vspace{16pt}}

\patchcmd{\Gin@ii}
  {\begingroup}% <search>
  {\begingroup\renewcommand{\@latex@error}[2]{\noimage}}% <replace>
  {}% <success>
  {}% <failure>

\newcommand\acknowledgements[1]{\renewcommand\@acknowledgements{#1}}
\newcommand\@acknowledgements{}
  
\AtBeginDocument{%
  \let\mytitle\@title
  \let\myauthor\@author

% Table of contents
% down to chapter.
\setcounter{tocdepth}{0}
}

% Designing the overview
% ---
% We define a new environment that take as its contents the text of the 
% overview, placing it with a chapter* heading 'Overview' all on the same
% page. 
\RequirePackage{multicol}
\newlength{\currentparskip}

\newlength{\overviewextra}
\newcounter{grattanOverview}
\setcounter{grattanOverview}{0}

\newenvironment{overview}[1][]%
  {%\@openrightfalse
  %\cleardoubleevenemptypage 
  \addchap{Overview}\enlargethispage{#1}}%
  {\cleardoublepage}

% 'Overview' centered
\newenvironment{overviewC}[1][]%
  {\vtop to 0pt\bgroup\vspace{#1}\chapter*{\centerline{Overview}}\begin{multicols}{2}}%
  {\end{multicols}\vss\egroup\hfill}

\newenvironment{overview*}%
  {\vtop to 0pt\bgroup\begin{multicols}{2}}%
  {\end{multicols}\vss\egroup\hfill}
  
\newenvironment{summary}%
  {\vtop to 0pt\bgroup\vspace{-25pt}\chapter*{Summary}\begin{multicols}{2}}%
  {\end{multicols}\vss\egroup\hfill}
  
\newenvironment{onepager}[1]%  
  {\vtop to 0pt\bgroup\vspace{-25pt}\chapter*{#1}\begin{multicols}{2}}%
  {\end{multicols}\vss\egroup\hfill}

\newenvironment{recommendations}[1][]% for Diana -- need to be full page
  {\vtop to 0pt\bgroup\vspace{-30pt}\addchap{Recommendations}\thispagestyle{empty}\vspace{-2\baselineskip}}%
  {\vss\egroup\hfill}


% Bibliography
\RequirePackage{csquotes}
\RequirePackage[backend=biber, style=authoryear-ibid, ibidtracker=strict, autocite=footnote]{biblatex}
\DeclareLanguageMapping{english}{british-apa}


%% Bibliography formatting
% separating entries
\setlength\bibitemsep{1.5\itemsep}
% format bibliography
\renewcommand*{\bibfont}{\small\raggedright}
\DeclareFieldFormat{url}{\url{#1}}

%% Citation tweaking
\DeclareFieldFormat{titlecase}{#1}
\setlength\bibhang{.5in}
\renewcommand*{\bibnamedash}{%
  \ifdimless{\leftmargin}{0.75em}
    {\mbox{\textemdash\space}}
    {\makebox[\leftmargin][l]{%
       \ifdimless{\leftmargin}{1.25em}
         {\textendash}
         {\rule{0.8\bibhang}{.2pt}}}}}
% Hyperlinks entire citation label for most citaiton commands 
% http://tex.stackexchange.com/questions/15951/hyperlink-name-with-biblatex-authoryear-biblatex-1-4b
\DeclareFieldFormat{citehyperref}{%
  \DeclareFieldAlias{bibhyperref}{noformat}% Avoid nested links
  \bibhyperref{#1}}

\DeclareFieldFormat{textcitehyperref}{%
  \DeclareFieldAlias{bibhyperref}{noformat}% Avoid nested links
  \bibhyperref{%
    #1%
    \ifbool{cbx:parens}
      {\bibcloseparen\global\boolfalse{cbx:parens}}
      {}}}

\savebibmacro{cite}
\savebibmacro{textcite}

\renewbibmacro*{cite}{%
  \printtext[citehyperref]{%
    \restorebibmacro{cite}%
    \usebibmacro{cite}}}

\renewbibmacro*{textcite}{%
  \ifboolexpr{
    ( not test {\iffieldundef{prenote}} and
      test {\ifnumequal{\value{citecount}}{1}} )
    or
    ( not test {\iffieldundef{postnote}} and
      test {\ifnumequal{\value{citecount}}{\value{citetotal}}} )
  }
    {\DeclareFieldAlias{textcitehyperref}{noformat}}
    {}%
  \printtext[textcitehyperref]{%
    \restorebibmacro{textcite}%
    \usebibmacro{textcite}}}

% The following lines give the correct footcite Author (Year) but make the ibid erroneous
% Author (ibid) % use ifciteibid
\DeclareCiteCommand{\footcite}[\mkbibfootnote]
  {\boolfalse{cbx:parens}}
  {\usebibmacro{citeindex}%
  \ifciteibid{Ibid.}{%  % for ibidem
   \iffirstcitekey
     {\setcounter{textcitetotal}{1}}
     {\stepcounter{textcitetotal}%
      \textcitedelim}%
   \usebibmacro{textcite}}%
   }
  {\ifbool{cbx:parens}
     {\bibcloseparen\global\boolfalse{cbx:parens}}
     {}}
  {\usebibmacro{textcite:postnote}}
\newcommand{\gcite}[1]{{\citeauthor{#1} (\citedate{#1})}}

\RequirePackage{tocloft}
\renewcommand{\cftchapfont}{\normalsize}
\renewcommand{\cftchapdotsep}{.}
% space reserved for pages (before dots) in toc
\renewcommand{\@pnumwidth}{2em}

\setlength{\cftfigindent}{0pt}  % remove indentation from figures in lof
\setlength{\cfttabindent}{0pt}  % remove indentation from tables in lot

% List of figures include part 
\newcommand{\addtocentrylistof}[3]{%
  \ifstr{#2}{}{%
    \addcontentsline{lof}{#1}{#3}%
    \addcontentsline{lot}{#1}{#3}%
  }{%
    \addcontentsline{lof}{#1}{\protect\numberline{#2}#3}%
    \addcontentsline{lot}{#1}{\protect\numberline{#2}#3}%
  }%
}

\renewcommand*{\addparttocentry}[2]{%
  \addtocentrydefault{part}{#1}{#2}%
  \addtocentrylistof{part}{#1}{#2}%
}

\AtBeginDocument{\renewcommand{\contentsname}{Table of contents}}
\newcommand{\contentspage}{%
\tableofcontents
}


