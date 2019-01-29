\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{grattanBudgetRepairBook}[2015/05/18 A book of the Grattan Institute, Melbourne]

\LoadClass[a4paper,11pt,titlepage,headings=big,chapterprefix=false,headsepline,
parskip=half-,twocolumn,numbers=noenddot,twoside,bibliography=totoc, captions=tablesignature]{scrbook}
% Permits pagebreaks within pages
\interlinepenalty=-100
%
\RequirePackage{cmap}
\RequirePackage{scrhack}
\RequirePackage{acronym}
\RequirePackage{makeidx}
  \makeindex
\RequirePackage{xcolor}
\RequirePackage{amsmath,amssymb}
\RequirePackage{tikz,adjustbox}
  \usetikzlibrary{positioning}
  \usetikzlibrary{decorations.text}
  \usetikzlibrary{decorations.pathmorphing}
  \usetikzlibrary{patterns,arrows,decorations.pathreplacing} % for waterfalls
\RequirePackage{float}
\RequirePackage{placeins}
\RequirePackage{afterpage}
\RequirePackage[protrusion=true, spacing=false]{microtype}
%% defaults are far too flamboyant. Indeed, zero protrusion with the 
%% Helvetica font appears to be protruding slightly (even though it 
%% is simply flush with the box).
%% encoding = T1 as in \usepackage[T1]{fontenc}
\SetProtrusion
   [ name     = T1-phv,      % the name is optional
     load     = T1-default ] % first load `T1-default` settings
   { encoding = T1,
     family   = phv }        % use for Helvetica family
   {
     \textendash = {-25, }, \textemdash = {-25, }  % cancel out left protrusion
   }
%\DisableLigatures{encoding = T1, family = *}
% ------
% Grattan colors
\definecolor{Orange}{HTML/cmyk}{F68B33/0,0.435,0.7927,0.0353}
\definecolor{DarkOrange}{HTML/cmyk}{D4582A/0,0.5849,0.8019,0.1686}
\definecolor{OrangeBackground}{RGB}{254,240,222}  % for boxes
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

% Arial font
\RequirePackage[utf8]{inputenc}
\RequirePackage[T1]{fontenc}
\RequirePackage{lmodern}
%\RequirePackage{uarial}
\RequirePackage[scaled]{helvet}
  \renewcommand{\familydefault}{\sfdefault}

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
\RequirePackage[hypertexnames=false, raiselinks, hidelinks, urlcolor=blue]{hyperref}
\RequirePackage{cleveref}

% Make url formatting the same
\urlstyle{same}

\DeclareCaptionFont{Orange}{\color{Orange}}
\DeclareCaptionJustification{nohyphen}{\hyphenpenalty=10000}
\captionsetup{justification=nohyphen, singlelinecheck=false}

% Boxes
\crefname{boxe}{Box}{Boxes}
\Crefname{boxe}{Box}{Boxes}
\DeclareFloatingEnvironment[listname={List of boxes}, name = {Box}]{boxe}
\mdfdefinestyle{GrattanFrameBox}{%
    linecolor=Orange,
    nobreak=true, % prevents page breaking
    outerlinewidth=0.5pt,
    innertopmargin=0.5\baselineskip,
    innerbottommargin=\baselineskip,
    innerrightmargin=11pt,
    innerleftmargin=11pt,
    backgroundcolor=OrangeBackground
    }
    
\mdfdefinestyle{GrattanFrameBoxUltra}{%
    linecolor=Orange,
    nobreak=true, % prevents page breaking
    outerlinewidth=0.5pt,
    innertopmargin=0.5\baselineskip,
    innerbottommargin=\baselineskip,
    innerrightmargin=11pt,
    innerleftmargin=11pt,
    backgroundcolor=OrangeBackground
    }
    
% The smallbox is a box intended for onecolumn.
\newenvironment{smallbox}[3][tp]{%
\setlength{\currentparskip}{\parskip}% save the value
% centre the float in the page.
\begin{boxe}[#1]
%\vfill
\begin{mdframed}[style=GrattanFrameBox]%
\setlength{\parskip}{\currentparskip}% restore the value
\captionsetup{labelfont={bf,Orange},position=top,skip=1.5ex,font={bf,Orange},format=plain,justification=raggedright,singlelinecheck=false}
\caption{#2}\label{#3}
\vspace{-1.5ex}
}{\end{mdframed}\vfill\end{boxe}} 



% Big boxes -- over the whole page
% It is recommended to surround this with an \afterpage{% 
\newenvironment{bigbox}[2]{\onecolumn\setlength{\currentparskip}{\parskip}
\begin{boxe}
\begin{mdframed}[style=GrattanFrameBox]%
\setlength{\columnsep}{10mm}
\begin{multicols}{2}
\setlength{\parskip}{\currentparskip}% restore the value
 \captionsetup{labelfont={bf,Orange}, font={bf,Orange}, format=plain,justification=justified,singlelinecheck=false}
 \caption{#1}\label{#2}%
 } {\end{multicols}\end{mdframed}\end{boxe}\twocolumn}

% dbl floats
\newenvironment{bigbox*}[2]{%
  \setlength{\currentparskip}{\parskip}
  \@dblfloat{boxe}%
  \begin{mdframed}[style=GrattanFrameBox]
  \setlength{\columnsep}{10mm}
  \begin{multicols}{2}
  \setlength{\parskip}{\currentparskip}% restore the value
 \captionsetup{labelfont={bf,Orange}, font={bf,Orange}, format=plain,justification=justified,singlelinecheck=false}
 \caption{#1}\label{#2}%
}{%
  \end{multicols}\end{mdframed}
  \end@dblfloat
}

% caption above body text
\newenvironment{bigboxC*}[2]{%
  \setlength{\currentparskip}{\parskip}
  \@dblfloat{boxe}%
  \begin{mdframed}[style=GrattanFrameBox]
  \setlength{\columnsep}{10mm}
  \captionsetup{labelfont={bf,Orange}, position=top, skip=0pt, font={bf,Orange}, format=plain, justification=centering, singlelinecheck=false}
  \caption{#1}\label{#2}%
  \begin{multicols}{2}
  \setlength{\parskip}{\currentparskip}% restore the value
}{%
  \end{multicols}\end{mdframed}
  \end@dblfloat
}

\newenvironment{bigboxCsep*}[3]{%
  \setlength{\currentparskip}{\parskip}
  \@dblfloat{boxe}%
  \begin{mdframed}[style=GrattanFrameBox]
  \setlength{\columnsep}{#3}
  \captionsetup{labelfont={bf,Orange}, position=top, skip=0pt, font={bf,Orange}, format=plain, justification=centering, singlelinecheck=false}
  \caption{#1}\label{#2}%
  \begin{multicols}{2}
  \setlength{\parskip}{\currentparskip}% restore the value
}{%
  \end{multicols}\end{mdframed}
  \end@dblfloat
}
 
\newenvironment{ultrabox}[2]{\onecolumn\setlength{\currentparskip}{\parskip}
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
 \clearpage\twocolumn\nopagecolor}
 

% Captions in general
\DeclareCaptionFont{theGrey}{\color{theGrey}}
\captionsetup{format=plain,font={small,bf,theGrey},labelfont={small,bf,theGrey}}
% suppress hyphenation in caption
\DeclareCaptionJustification{nohyphen}{\hyphenpenalty=10000}
\captionsetup{justification=nohyphen}

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

% Page floats appear 5% of the textheight down the page
% *unless* \vfill is called (as it is in smallbox) in which
% case the float can move up (almost all the way to the top, if needed).
\setlength{\@fptop}{0.05\textheight minus 0.05\textheight}
\setlength{\@fpbot}{0pt plus 1fil}

\setkomafont{disposition}{\color{Orange}}
\addtokomafont{chapter}{\bfseries\Large}
\addtokomafont{section}{\bfseries\normalsize}
\addtokomafont{subsection}{\normalsize}
\addtokomafont{subsubsection}{\normalsize}

% Spacing
\RedeclareSectionCommand[
  beforeskip=15pt,
  afterskip=15pt]{chapter}
\RedeclareSectionCommand[
  beforeskip=0.5\baselineskip,
  afterskip=1sp]{section}
\RedeclareSectionCommand[
  beforeskip=-.75\baselineskip,
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

  
% --------------
%
% Grattan twocolumn
%
%---------------

% N.B. geometry must be after hyperref, and hence cleveref


\RequirePackage[landscape,twocolumn,
% orig Word specify 2.3 and 2.5
left=2.75cm,right=2.75cm
]{geometry}
  %\addtolength{\columnwidth}{50pt}
  \setlength{\columnsep}{15mm}  
  %\addtolength{\columnwidth}{0.2\columnwidth}

% Ensures hyperlinked chapter headings refer to their referent headings
% not the preceding baseline
\RequirePackage{etoolbox}
% Add extra hyper target for chapter: chapter..\thechapter

\renewcommand{\theHchapter}{\thepart.\arabic{chapter}}

\renewcommand*{\chapterformat}{%
  \mbox{\raisebox{25pt}[0pt][0pt]{\hypertarget{\thepart..\thechapter}{}}% Add 
    \chapappifchapterprefix{\nobreakspace}\thechapter\hspace{1em}\autodot\enskip}%
}

% Update \addcontentsline to jump to new hyper target _only_ if \chapter is used
\patchcmd{\addcontentsline}% <cmd>
  {\Hy@writebookmark}% <search>
  {\ifnum\pdfstrcmp{chapter}{#2}=0 % Chapter mark
     \edef\@currentHref{page.\thepage}%
   \fi
   \Hy@writebookmark}% <replace>
  {}{}% <success><failure>

  

 % Requests forcefully that figures refrain from
% appearing in the first column.  This will fail
% should a \clearpage be experienced.
\g@addto@macro\@floatplacement{%
  \if@firstcolumn
  \global\@colnum\z@
  \fi
}
  
%% ---------------------------------
%  Headers and footers
%% ---------------------------------
\RequirePackage[headsepline=1pt,plainheadsepline,footsepline=1pt,plainfootsepline]{scrlayer-scrpage}

\addtolength{\textheight}{40pt}
\addtolength{\footskip}{-10pt}

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
\automark[chapter]{part}
\lehead[]{\rightmark}
\rehead[]{}
\lohead[]{}
\rohead[Budget repair]{\leftmark}
\setkomafont{pageheadfoot}{\normalfont\color{theGrey}}



\ifoot[{\textcolor{theGrey}{\normalfont Grattan Institute \the\year}}]{\textcolor{theGrey}{\normalfont Grattan Institute \the\year}}
\cfoot[\phantom{\Large X}]{\phantom{\Large X}}
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
  {\onecolumn\vtop to 0pt\bgroup\vspace{#1}%
  \addchap{Overview}
  \begin{multicols}{2}}%
  %
  {\end{multicols}\vss\egroup\hfill\twocolumn}

% 'Overview' centered
\newenvironment{overviewC}[1][]%
  {\onecolumn\vtop to 0pt\bgroup\vspace{#1}\chapter*{\centerline{Overview}}\begin{multicols}{2}}%
  {\end{multicols}\vss\egroup\hfill\twocolumn}

\newenvironment{overview*}%
  {\onecolumn\vtop to 0pt\bgroup\begin{multicols}{2}}%
  {\end{multicols}\vss\egroup\hfill\twocolumn}
  
\newenvironment{summary}%
  {\onecolumn\vtop to 0pt\bgroup\vspace{-25pt}\chapter*{Summary}\begin{multicols}{2}}%
  {\end{multicols}\vss\egroup\hfill\twocolumn}
  
\newenvironment{onepager}[1]%  
  {\onecolumn\vtop to 0pt\bgroup\vspace{-25pt}\chapter*{#1}\begin{multicols}{2}}%
  {\end{multicols}\vss\egroup\hfill\twocolumn}

\newenvironment{recommendations}[1][]%
  {\onecolumn\vtop to 0pt\bgroup\vspace{#1}\addchap{Recommendations}\begin{multicols}{2}}%
  {\end{multicols}\vss\egroup\hfill\twocolumn}


% Bibliography
\RequirePackage[english]{babel}
\RequirePackage{csquotes}
\RequirePackage[backend=biber,style=authoryear-ibid,ibidtracker=strict, autocite=footnote]{biblatex}
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
\onecolumn
\begin{multicols}{2}
\phantom{.}
\vfill
\columnbreak
\tableofcontents
\end{multicols}
\clearpage
}


