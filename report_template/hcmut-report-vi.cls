\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{hcmut-report-vi}


\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass[12pt,a4paper]{article}

\RequirePackage[utf8]{vntex,inputenc}
\RequirePackage{anyfontsize,amsmath,amssymb}
\RequirePackage[T5]{fontenc}

\RequirePackage{geometry,graphicx,indentfirst,setspace}
\RequirePackage{booktabs}
\RequirePackage[center]{caption}

\RequirePackage{fancyhdr,hyperref}
\RequirePackage[super,square,sort]{natbib}

% Change to '[english,vietnamese]' to use Vietnamese labels
\RequirePackage[vietnamese,english]{babel}

% Custom commands
\newcommand{\Month}{
  \ifcase \the\month
  \or January
  \or February
  \or March
  \or April
  \or May
  \or June
  \or July
  \or August
  \or September
  \or October
  \or November
  \or December
  \fi
}

\AtBeginDocument{
  \renewcommand{\listfigurename}{Danh sách hình vẽ}
  \renewcommand{\listtablename}{Danh sách bảng}
  \renewcommand{\lstlistlistingname}{Danh sách đoạn mã}
  \renewcommand{\contentsname}{Mục lục}
}


\newcommand{\@upperuniname}{Đại học Quốc gia Thành phố Hồ Chí Minh}
\newcommand{\@uniname}{Trường Đại học Bách khoa}

\newcommand{\@deptname}{Khoa Khoa học và Kỹ thuật Máy tính}
\newcommand{\deptname}[1]{\renewcommand{\@deptname}{#1}}

\newcommand{\@coursename}{Course name}
\newcommand{\coursename}[1]{\renewcommand{\@coursename}{#1}}

\newcommand{\@reporttype}{Assignment Report}
\newcommand{\reporttype}[1]{\renewcommand{\@reporttype}{#1}}

\renewcommand{\@title}{Report title}

\newcommand{\@advisor}{Advisor}
\newcommand{\advisor}[1]{\renewcommand{\@advisor}{#1}}

\newcommand{\@stuname}{STUDENT:\ H (xxxxxxx)}
\newcommand{\stuname}[1]{\renewcommand{\@stuname}{#1}}

\newcommand{\@reportplace}{Thành phố Hồ Chí Minh}
\newcommand{\reportplace}[1]{\renewcommand{\@reportplace}{#1}}

\newcommand{\@reporttime}{Tháng \the\month\ năm \the\year}
\newcommand{\reporttime}[1]{\renewcommand{\@reporttime}{#1}}

% Configurations
%\graphicspath{{subdir1/}{subdir2/}{subdir3/}...{subdirn/}}

\geometry{
  paper=a4paper,
  left=2cm,
  right=2cm,
  vmargin=2cm,
  includeheadfoot=true,
  headheight=35pt
}

\onehalfspacing
\raggedbottom
\renewcommand{\arraystretch}{1.2}

% === DI CHUYỂN 4 DÒNG XUỐNG ĐÂY ===
\AtBeginDocument{
  \setlength{\abovedisplayskip}{4pt}
  \setlength{\belowdisplayskip}{4pt}
  \setlength{\abovedisplayshortskip}{0pt}
  \setlength{\belowdisplayshortskip}{4pt}
}
% ===================================

% Set depth of numbering for sections and table of contents
\setcounter{secnumdepth}{3}
\setcounter{tocdepth}{3}

% Set depth of numbering for counters
\AtBeginDocument{\counterwithin{equation}{section}}
\AtBeginDocument{\counterwithin{table}{section}}
\AtBeginDocument{\counterwithin{figure}{section}}

\AtBeginDocument{\renewcommand*\bibname{References}}

\hypersetup{
  colorlinks=true,
  linkcolor=black,
  citecolor=red,
  urlcolor=blue
}

\renewcommand{\headrulewidth}{0.3pt}
\renewcommand{\footrulewidth}{0pt}

\pagestyle{fancy}
\fancyhf{}

%\renewcommand{\subsectionmark}[1]{\markright{\thesubsection.\ #1}}

% The drawing units are interpreted as multiples of \unitlength
% unless a unit is given
%\setlength{\unitlength}{1pt}

\if@twoside
  \fancyhead[EL]{
    \bfseries
    \begin{tabular}{rl}
      \begin{picture}(25pt,15pt)
        \put(0,-8pt){\includegraphics[width=20pt]{graphics/hcmut.png}}
      \end{picture}
      \begin{tabular}{l}
        \ttfamily\@uniname  \\
        \ttfamily\@deptname \\
      \end{tabular}
    \end{tabular}
  }
  \fancyhead[OR]{
    \bfseries
    \begin{tabular}{rl}
      \begin{tabular}{r}
        \ttfamily\@uniname  \\
        \ttfamily\@deptname \\
      \end{tabular}
      \begin{picture}(25pt,15pt)
        \put(0,-8pt){\includegraphics[width=20pt]{graphics/hcmut.png}}
      \end{picture}
    \end{tabular}
  }
\else
  \fancyhead[R]{
    \bfseries
    \begin{tabular}{rl}
      \begin{tabular}{r}
        \ttfamily\@uniname  \\
        \ttfamily\@deptname \\
      \end{tabular}
      \begin{picture}(25pt,15pt)
        \put(0,-8pt){\includegraphics[width=20pt]{graphics/hcmut.png}}
      \end{picture}
    \end{tabular}
  }
\fi

\fancyfoot[C]{\thepage}


% Custom pages and sections
\newcommand{\coverpage}{
  \pagestyle{empty}

  \vspace*{-3\baselineskip}
  \begin{center}
    \MakeUppercase{\@upperuniname}

    \MakeUppercase{\@uniname}

    \MakeUppercase{\@deptname}
    \vfill
    \includegraphics[scale=0.4]{graphics/hcmut.png}
    \vfill
    \bfseries
    \begin{tabular}{c}
      \Large\@coursename                                 \\
      {}                                                 \\
      \midrule                                           \\
      {}                                                 \\
      \Large\@reporttype                                 \\
      {}                                                 \\
      \Huge\parbox[c]{0.8\linewidth}{\centering \@title} \\
      {}                                                 \\
      \bottomrule
    \end{tabular}
    \vfill
    \mdseries
    \begin{tabular}{rlll}
      Giảng viên hướng dẫn: \@advisor \\
    \end{tabular}
    \begin{tabular}{|c|l|l|l|}
      \hline
      STT & Tên                    & MSSV             & Nhóm Lớp     \\
      \hline
      1   & Lê Ngô Khôi Nguyên     & 2412338          & L07          \\
      \hline
      2   & Phan Nguyễn Hoàng Minh & 2412114          & L07          \\
      \hline
      3   & \textbf{Trần Đình Ý}   & \textbf{2414100} & \textbf{L07} \\
      \hline
      4   & Phạm Nghĩa Trung       & 2413713          & L08          \\
      \hline
      6   & Nguyễn Gia Hưng        & 2411356          & L09          \\
      \hline
      7   & Nguyễn Quốc Trung      & 2413707          & L14          \\
      \hline
      8   & Phạm Minh Đức          & 2414106          & L15          \\
      \hline
    \end{tabular}
    \vfill
    \MakeUppercase{\@reportplace, \@reporttime}
  \end{center}

  \cleardoublepage
  \pagestyle{fancy}
}
