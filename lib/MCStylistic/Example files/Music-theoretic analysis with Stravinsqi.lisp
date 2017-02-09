#| Copyright 2008-2014 Tom Collins
   Monday 23 June 2014

Music-theoretic analysis with Stravinsqi

This code demonstrates the use of an algorithm called
Stravinsqi-Jun2014 \citep{collins2014}, which stands
for STaff Representation Analysed VIa Natural language
Query Input. The algorithm parses a symbolic
representation of a piece of music as well as a query
string consisting of a natural language expression,
and identifies where event(s) specified by the query
occur in the music.


References

Tom Collins. Stravinsqi/De Montfort University at the
MediaEval 2014 C@merata task. In Proceedings of the
MediaEval 2014 Workshop, page 6 pages, Barcelona,
Spain, 2014. |#


#| \textbf{Step 1.} Specify a piece. In the last
chapter we saw several methods for importing a piece
of music into the Lisp environment. On this occasion,
we do not need to import the piece explicitly, but
merely provide the Stravinsqi algorithm with its path,
and it will do the rest. The piece will need to be in
kern format though. Let us suppose we have a MusicXML
excerpt of `If ye love me' by Thomas Tallis (as shown
in Fig.~\ref{fig:tallis}). So before we can call
Stravinsqi, the MusicXML file needs converting to a
kern file.

To do this, open up Terminal (separate from Lisp)
and navigate using \href{http://en.wikipedia.org/wiki/Cd_\%28command\%29}{cd}
(change directory) to

\vspace{.25cm}
\noindent MCStylistic/Functions/Third party/humdrum-extras
\vspace{.25cm}

\noindent Then execute
\begin{verbatim}
./xml2hum-mac
 ../../../Example\ files/Example\ data/tallis-love.xml
 > ../../../Example\ files/Example\ data/tallis-love.krn
\end{verbatim}

\noindent This should create a kern file in the same
location as the original MusicXML file. This command
can (and probably should) be all on one line: I have
broken it on to several lines for ease of reading. If
using Linux or Windows, then be sure to replace
xml2hum-mac above with the
operating-system-appropriate function name.

Now, back in Lisp, we can specify the kern file's
location: |#
(setq
 notation-path
 *MCStylistic-MonthYear-example-files-data-path*)
(setq notation-name "tallis-love")
(setq
 notation-path&name
 (merge-pathnames
  (make-pathname
   :name notation-name :type "krn") notation-path))

#| \textbf{Step 2.} Ask some questions. In the same
location as the kern file there is an (ordinary) XML
file called \emph{natural-language-queries.xml}. The
first query it contains is `perfect cadence'. That is,
we want to know the bar and beat numbers of the
perfect cadence(s) in the music of
Figure~\ref{fig:tallis}. We can pass this query to
Stravinsqi by specifying the path of
\emph{natural-language-queries.xml}, and the question
number `001'. Stravinsqi will parse the query,
identify any events that correspond to the query, and
write the corresponding bar and beat numbers to a text
file. By default, it creates a text file called
\emph{dmun01.txt} in the same location as the query
file. Try executing the following code in the Lisp
Listener: |#
(setq
 question-path&name
 (merge-pathnames
  (make-pathname
   :name "natural-language-queries" :type "xml")
  *MCStylistic-MonthYear-example-files-data-path*))
(setq question-number "001")
(Stravinsqi-Jun2014
 question-number question-path&name notation-path
 notation-name)

#| \noindent Checking the text file \emph{dmun01.txt},
it can be confirmed that Stravinsqi's answers to
question 1 are:
\begin{itemize}
\item bar 2 beat 1 to bar 3 beat 2;
\item bar 12 beat 3 to bar 13 beat 4.
\end{itemize}

Question 2 shows that one can use British or American
terminology interchangeably (`authentic cadence'). |#
(setq question-number "002")
(Stravinsqi-Jun2014
 question-number question-path&name notation-path
 notation-name)

#| \noindent Stravinsqi's answers are the same.

Question 3 asks Stravinsqi to identify a
homophonic texture. Its answer is:
\begin{itemize}
\item bar 1 beat 1 to bar 5 beat 4.
\end{itemize} |#
(setq question-number "003")
(Stravinsqi-Jun2014
 question-number question-path&name notation-path
 notation-name)

#| Question 4 shows that Stravinsqi can also answer
more straightforward questions, such as `dotted
crotchet G'. Its answers are:
\begin{itemize}
\item bar 8 quaver beats 5 to 7;
\item bar 12 quaver beats 5 to 7.
\end{itemize} |#
(setq question-number "004")
(Stravinsqi-Jun2014
 question-number question-path&name notation-path
 notation-name)

#| Question 5 demonstrates that Stravinsqi can answer
compound questions, such as `two melodic unisons then
a melodic rising third'. This musical event occurs
set to `and I will pray', in the soprano and alto
voices. Accordingly, Stravinsqi's answers are:
\begin{itemize}
\item bar 5 beat 2 to bar 6 beat 2;
\item bar 7 beat 2 to bar 8 beat 2.
\end{itemize} |#
(setq question-number "005")
(Stravinsqi-Jun2014
 question-number question-path&name notation-path
 notation-name)

#| Question 6 indicates that Stravinsqi can answer
queries about text, articulation marks, ties, and
rests. For example, for the query `word love',
Stravinsqi's answer is:
\begin{itemize}
\item bar 2 beats 1 to 2.
\end{itemize} |#
(setq question-number "006")
(Stravinsqi-Jun2014
 question-number question-path&name notation-path
 notation-name)

#| Users are encouraged to open up the file
\emph{natural-language-queries.xml}, and use
copy-paste to add a few questions of their own. The
divisions value specifies granularity for Stravinsqi:
that is, if you want time windows up to crotchet-beat
granularity, then set divisions to 1; for quaver-beat
granularity, set divisions to 2, etc. If Stravinsqi
cannot answer what seems an entirely reasonable query,
or gets the answer wrong, you are welcome to get in
touch to request an improvement. |#
