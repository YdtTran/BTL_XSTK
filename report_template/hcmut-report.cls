\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{hcmut-report}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass[12pt,a4paper]{article}

% Only keep what is needed
\RequirePackage[T5]{fontenc}
\RequirePackage{anyfontsize,amsmath,amssymb}
\RequirePackage{geometry,graphicx,indentfirst,setspace}
\RequirePackage{booktabs}
\RequirePackage[center]{caption}
\RequirePackage{fancyhdr,hyperref}
\RequirePackage[super,square,sort]{natbib}
\RequirePackage[vietnamese]{babel}

% Preload logo
\newcommand{\HCMUTlogoSmall}{\includegraphics[width=20pt]{graphics/hcmut.png}}
\newcommand{\HCMUTlogoBig}{\includegraphics[scale=0.4]{graphics/hcmut.png}}
% Commands
\newcommand{\@upperuniname}{Đại học quốc gia Thành phố Hồ Chí Minh}
\newcommand{\@uniname}{Trường Đại học Bách Khoa Thành phố Hồ Chí Minh}
\newcommand{\@deptname}{Khoa khoa học và kỹ thuật máy tính}
\newcommand{\deptname}[1]{\renewcommand{\@deptname}{#1}}

\newcommand{\@coursename}{Course name}
\newcommand{\coursename}[1]{\renewcommand{\@coursename}{#1}}

\newcommand{\@reporttype}{Assignment Report}
\newcommand{\reporttype}[1]{\renewcommand{\@reporttype}{#1}}

\renewcommand{\@title}{Report title}

\newcommand{\@advisor}{ADVISOR}
\newcommand{\advisor}[1]{\renewcommand{\@advisor}{#1}}

\newcommand{\@stuname}{STUDENT:\ H (xxxxxxx)}
\newcommand{\stuname}[1]{\renewcommand{\@stuname}{#1}}

\newcommand{\@reportplace}{Hồ Chí Minh}
\newcommand{\reportplace}[1]{\renewcommand{\@reportplace}{#1}}

\newcommand{\@reporttime}{%
  \ifcase\month 0/\or Tháng 1/\or Tháng 2/\or Tháng 3/\or Tháng 4/\or Tháng 5/\or Tháng 6/%
  \or Tháng 7/\or Tháng 8/\or Tháng 9/\or Tháng 10/\or Tháng 11/\or Tháng 12/%
  \fi\space\the\year
}
\newcommand{\reporttime}[1]{\renewcommand{\@reporttime}{#1}}

% Geometry
\geometry{
  paper=a4paper,
  left=3cm,
  right=2cm,
  vmargin=2cm,
  includeheadfoot=true,
  headheight=35pt
}

\onehalfspacing
\raggedbottom
\renewcommand{\arraystretch}{1.2}

\setcounter{secnumdepth}{3}
\setcounter{tocdepth}{3}

% Numbering
\AtBeginDocument{\counterwithin{equation}{section}}
\AtBeginDocument{\counterwithin{table}{section}}
\AtBeginDocument{\counterwithin{figure}{section}}
\AtBeginDocument{\renewcommand*\bibname{References}}

% Hyperref
\hypersetup{
  colorlinks=true,
  linkcolor=black,
  citecolor=red,
  urlcolor=blue
}

% fancyhdr
\pagestyle{fancy}
\fancyhf{}
\renewcommand{\headrulewidth}{0.3pt}
\renewcommand{\footrulewidth}{0pt}

\if@twoside
  % Even page (left header)
  \fancyhead[EL]{%
    \makebox[\textwidth][l]{%
      \begin{minipage}{0.75\textwidth}
        \raggedright
        \ttfamily\@uniname\\
        \ttfamily\@deptname
      \end{minipage}
      \hfill
      \HCMUTlogoSmall
    }
  }

  % Odd page (right header)
  \fancyhead[OR]{%
    \makebox[\textwidth][l]{%
      \begin{minipage}{0.75\textwidth}
        \raggedright
        \ttfamily\@uniname\\
        \ttfamily\@deptname
      \end{minipage}
      \hfill
      \HCMUTlogoSmall
    }
  }
\else
  % One-sided document
  \fancyhead[R]{%
    \begin{tabular}{rl}
      \begin{tabular}{l}
        \ttfamily\@uniname \\
        \hspace{6.3em}\ttfamily\@deptname
      \end{tabular}
       & \HCMUTlogoSmall
    \end{tabular}
  }
\fi


\fancyfoot[C]{\thepage}

% Cover page
\newcommand{\coverpage}{
  \pagestyle{empty}
  \vspace*{-3\baselineskip}
  \begin{center}
    \MakeUppercase{\@upperuniname}\\
    \MakeUppercase{\@uniname}\\
    \MakeUppercase{\@deptname}

    \vfill
    \HCMUTlogoBig
    \vfill

    \bfseries
    \begin{tabular}{c}
      \Large\@coursename                                 \\
      {}                                                 \\ \midrule \\ {}\\
      \Large\@reporttype                                 \\
      {}                                                 \\
      \Huge\parbox[c]{0.8\linewidth}{\centering \@title} \\
      {}                                                 \\ \bottomrule
    \end{tabular}
    \vfill

    \mdseries
    GVHD: \@advisor\\
    Mã nhóm: \@stuname\\

    \vfill
    \MakeUppercase{\@reportplace, \@reporttime}
  \end{center}
  \cleardoublepage
  \pagestyle{fancy}
}
