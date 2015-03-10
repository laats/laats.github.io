#!/usr/bin/env python
################################################################################
#
# File:         tex2wp.py
# RCS:          $Header: $
# Description:  A modification of Luca Trevisan's latex2wp program
# Author:       Base: Luca Trevisan with Radu Grigore
#               Revision: Staal Vinterbo
# Created:      Wed Mar 28 20:29:59 2012
# Modified:     Thu Mar 29 16:58:53 2012 (Staal Vinterbo) staal@dink
# Language:     Python
# Package:      N/A
# Status:       Experimental
#
# tex2wp.py is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# tex2wp.py is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with tex2wp.py; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# (c) Copyright 2012, Staal Vinterbo, Luca Trevisan, all rights reserved.
#
################################################################################
#
# Revisions:
#
# Wed Mar 28 22:31:34 2012 (Staal Vinterbo) staal@mats
#  Collected both files latex2wpstyle.py and latex2wp.py into one file for ease
#  of distribution. Added processing of verbatim environment, now wrapped in
#  <pre> </pre>, changed standalone HTML processing to use urllib.quote and
#  latex.codecogs.com instead of wordpress.com which did not seem to work
#  anymore.
#  Added wrapping of wordpress $latex ...$ inline equations in a
#  <span style="vertical-align:xxx> </span> element where xxx can be given
#  on the command line. The reason is that many wordpress styles result in
#  bad vertical alignment of the resulting images. Added
#  rendering of \texttt as "code". Added command line option handling. Added
#  printing of a 'wordpress' LaTeX class file, allowing LaTeX documents to
#  use this as \documentclass{wordpress}\begin{document} ... \end{document}
#  instead of the previous preamble copy/paste + \include. Added printing
#  of the example.tex (slightly modified) file using the wordpress.cls.
################################################################################

##############################
# Contents of modified latex2wpstyle.py
"""
 Copyright 2009 Luca Trevisan

 Additional contributors: Radu Grigore

 LaTeX2WP version 0.6.2

 This file is part of LaTeX2WP, a program that converts
 a LaTeX document into a format that is ready to be
 copied and pasted into WordPress.

 You are free to redistribute and/or modify LaTeX2WP under the
 terms of the GNU General Public License (GPL), version 3
 or (at your option) any later version.

 I hope you will find LaTeX2WP useful, but be advised that
 it comes WITHOUT ANY WARRANTY; without even the implied warranty
 of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GPL for more details.

 You should have received a copy of the GNU General Public
 License along with LaTeX2WP.  If you can't find it,
 see <http://www.gnu.org/licenses/>.
"""

# Lines starting with #, like this one, are comments

# align-vertical property for inline latex formulae
alignplacement = "baseline"

# change to HTML = True to produce standard HTML
HTML = False

# color of LaTeX formulas
textcolor = "000000"

# colors that can be used in the text
colors = { "red" : "ff0000" , "green" : "00ff00" , "blue" : "0000ff" }
# list of colors defined above
colorchoice = ["red","green","blue"]


# counters for theorem-like environments
# assign any counter to any environment. Make sure that
# maxcounter is an upper bound to the any counter being used

T = { "theorem" : 0 , "lemma" : 0 , "proposition" : 0, "definition" : 0,
               "corollary" : 0, "remark" : 3 , "example" : 1, "claim" : 4,
               "exercise" : 2  }

# list of theorem-like environments
ThmEnvs = ["theorem","definition","lemma","proposition","corollary","claim",
           "remark","example","exercise"]

# the way \begin{theorem}, \begin{lemma} etc are translated in HTML
# the string _ThmType_ stands for the type of theorem
# the string _ThmNumb_ is the theorem number
beginthm = "\n<blockquote><b>_ThmType_ _ThmNumb_</b> <em>"

# translation of \begin{theorem}[...]. The string
# _ThmName_ stands for the content betwee the
# square brackets
beginnamedthm = "\n<blockquote><b>_ThmType_ _ThmNumb_ (_ThmName_)</b> <em>"

#translation of \end{theorem}, \end{lemma}, etc.
endthm = "</em></blockquote>\n<p>\n"


beginproof = "<em>Proof:</em> "
endproof = "$latex \Box&fg=000000$\n\n"

beginverbatim = "<pre>"
endverbatim = "</pre>"


section = "\n<p>\n<b>_SecNumb_. _SecName_ </b>\n<p>\n"
sectionstar = "\n<p>\n<b> _SecName_ </b>\n<p>\n"
subsection = "\n<p>\n<b>  _SecNumb_._SubSecNumb_. _SecName_ </b>\n<p>\n"
subsectionstar = "\n<p>\n<b> _SecName_ </b>\n<p>\n"

# Font styles. Feel free to add others. The key *must* contain
# an open curly bracket. The value is the namem of a HTML tag.
fontstyle = {
  r'{\em ' : 'em',
  r'{\bf ' : 'b',
  r'{\it ' : 'i',
  r'{\sl ' : 'i',
  r'\textit{' : 'i',
  r'\textsl{' : 'i',
  r'\texttt{' : 'code',    
  r'\emph{' : 'em',
  r'\textbf{' : 'b',
}

# Macro definitions
# It is a sequence of pairs [string1,string2], and
# latex2wp will replace each occurrence of string1 with an
# occurrence of string2. The substitutions are performed
# in the same order as the pairs appear below.
# Feel free to add your own.
# Note that you have to write \\ instead of \
# and \" instead of "

M = [     ["\\to","\\rightarrow"] ,
          ["\\B","\\{ 0,1 \\}" ],
          ["\\E","\mathop{\\mathbb E}"],
          ["\\P","\mathop{\\mathbb P}"],
          ["\\N","{\\mathbb N}"],
          ["\\Z","{\\mathbb Z}"],
          ["\\C","{\\mathbb C}"],
          ["\\R","{\\mathbb R}"],
          ["\\Q","{\\mathbb Q}"],
          ["\\xor","\\oplus"],
          ["\\eps","\\epsilon"]
    ]

########################
# contents of modified latex2wp.py
"""
 Copyright 2009 Luca Trevisan

 Additional contributors: Radu Grigore

 LaTeX2WP version 0.6.2

 This file is part of LaTeX2WP, a program that converts
 a LaTeX document into a format that is ready to be
 copied and pasted into WordPress.

 You are free to redistribute and/or modify LaTeX2WP under the
 terms of the GNU General Public License (GPL), version 3
 or (at your option) any later version.

 I hope you will find LaTeX2WP useful, but be advised that
 it comes WITHOUT ANY WARRANTY; without even the implied warranty
 of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GPL for more details.

 You should have received a copy of the GNU General Public
 License along with LaTeX2WP.  If you can't find it,
 see <http://www.gnu.org/licenses/>.
"""


import re
from sys import argv

from urllib import quote
from string import Template

# prepare variables computed from the info in latex2wpstyle
count = dict()
for thm in ThmEnvs:
  count[T[thm]] = 0
count["section"] = count["subsection"] = count["equation"] = 0

ref={}

def lateximg(tex):
    s = '<img src=\"http://latex.codecogs.com/png.latex?{$tex}\"/>'
    return Template(s).substitute(tex=quote(tex, safe='/\\{}:'))


endlatex = "&fg="+textcolor




inthm = ""

"""
 At the beginning, the commands \$, \% and \& are temporarily
 replaced by placeholders (the second entry in each 4-tuple).
 At the end, The placeholders in text mode are replaced by
 the third entry, and the placeholders in math mode are
 replaced by the fourth entry.
"""

esc = [["\\$","_dollar_","&#36;","\\$"],
       ["\\%","_percent_","&#37;","\\%"],
       ["\\&","_amp_","&amp;","\\&"],
       [">","_greater_",">","&gt;"],
       ["<","_lesser_","<","&lt;"]]

M = M + [ ["\\more","<!--more-->"],
          ["\\newblock","\\\\"],
          ["\\sloppy",""],
          ["\\S","&sect;"]]

Mnomath =[["\\\\","<br/>\n"],
          ["\\ "," "],
          ["\\`a","&agrave;"],
          ["\\'a","&aacute;"],
          ["\\\"a","&auml;"],
          ["\\aa ","&aring;"],
          ["{\\aa}","&aring;"],
          ["\\`e","&egrave;"],
          ["\\'e","&eacute;"],
          ["\\\"e","&euml;"],
          ["\\`i","&igrave;"],
          ["\\'i","&iacute;"],
          ["\\\"i","&iuml;"],
          ["\\`o","&ograve;"],
          ["\\'o","&oacute;"],
          ["\\\"o","&ouml;"],
          ["\\`o","&ograve;"],
          ["\\'o","&oacute;"],
          ["\\\"o","&ouml;"],
          ["\\H o","&ouml;"],
          ["\\`u","&ugrave;"],
          ["\\'u","&uacute;"],
          ["\\\"u","&uuml;"],
          ["\\`u","&ugrave;"],
          ["\\'u","&uacute;"],
          ["\\\"u","&uuml;"],
          ["\\v{C}","&#268;"]]


cb = re.compile("\\{|}")

def extractbody(m) :

    begin = re.compile("\\\\begin\s*")
    m= begin.sub("\\\\begin",m)
    end = re.compile("\\\\end\s*")
    m = end.sub("\\\\end",m)
    
    beginenddoc = re.compile("\\\\begin\\{document}"
                          "|\\\\end\\{document}")
    parse = beginenddoc.split(m)
    if len(parse)== 1 :
       m = parse[0]
    else :
       m = parse[1]

    """
      removes comments, replaces double returns with <p> and
      other returns and multiple spaces by a single space.
    """

    for e in esc :
        m = m.replace(e[0],e[1])

    comments = re.compile("%.*?\n")
    m=comments.sub(" ",m)

        

    multiplereturns = re.compile("\n\n+")
    m= multiplereturns.sub ("<p>",m)
    spaces=re.compile("(\n|[ ])+")
    m=spaces.sub(" ",m)

    """
     removes text between \iffalse ... \fi and
     between \iftex ... \fi keeps text between
     \ifblog ... \fi
    """


    ifcommands = re.compile("\\\\iffalse|\\\\ifblog|\\\\iftex|\\\\fi")
    L=ifcommands.split(m)
    I=ifcommands.findall(m)
    m= L[0]
    for i in range(1,(len(L)+1)/2) :
        if (I[2*i-2]=="\\ifblog") :
            m=m+L[2*i-1]
        m=m+L[2*i]

    """
     changes $$ ... $$ into \[ ... \] and reformats
     eqnarray* environments as regular array environments
    """

    doubledollar = re.compile("\\$\\$")
    L=doubledollar.split(m)
    m=L[0]
    for i in range(1,(len(L)+1)/2) :
        m = m+ "\\[" + L[2*i-1] + "\\]" + L[2*i]

    m=m.replace("\\begin{eqnarray*}","\\[ \\begin{array}{rcl} ")
    m=m.replace("\\end{eqnarray*}","\\end{array} \\]")

    return m

def convertsqb(m) :

    r = re.compile("\\\\item\\s*\\[.*?\\]")

    Litems = r.findall(m)
    Lrest = r.split(m)

    m = Lrest[0]
    for i in range(0,len(Litems)) :
      s= Litems[i]
      s=s.replace("\\item","\\nitem")
      s=s.replace("[","{")
      s=s.replace("]","}")
      m=m+s+Lrest[i+1]

    r = re.compile("\\\\begin\\s*\\{\\w+}\\s*\\[.*?\\]")
    Lthms = r.findall(m)
    Lrest = r.split(m)

    m = Lrest[0]
    for i in range(0,len(Lthms)) :
      s= Lthms[i]
      s=s.replace("\\begin","\\nbegin")
      s=s.replace("[","{")
      s=s.replace("]","}")
      m=m+s+Lrest[i+1]

    return m


def converttables(m) :
        

    retable = re.compile("\\\\begin\s*\\{tabular}.*?\\\\end\s*\\{tabular}"
                         "|\\\\begin\s*\\{btabular}.*?\\\\end\s*\\{btabular}")
    tables = retable.findall(m)
    rest = retable.split(m)


    m = rest[0]
    for i in range(len(tables)) :
        if tables[i].find("{btabular}") != -1 :
            m = m + convertonetable(tables[i],True)
        else :
            m = m + convertonetable(tables[i],False)
        m = m + rest[i+1]


    return m


def convertmacros(m) :


    comm = re.compile("\\\\[a-zA-Z]*")
    commands = comm.findall(m)
    rest = comm.split(m)


    r= rest[0]
    for i in range( len (commands) ) :
      for s1,s2 in M :
        if s1==commands[i] :
          commands[i] = s2
      r=r+commands[i]+rest[i+1]
    return(r)


def convertonetable(m,border) :

    tokens = re.compile("\\\\begin\\{tabular}\s*\\{.*?}"
                        "|\\\\end\\{tabular}"
                        "|\\\\begin\\{btabular}\s*\\{.*?}"
                        "|\\\\end\\{btabular}"
                        "|&|\\\\\\\\")

    align = { "c" : "center", "l" : "left" , "r" : "right" }

    T = tokens.findall(m)
    C = tokens.split(m)


    L = cb.split(T[0])
    format = L[3]

    columns = len(format)
    if border :
        m = "<table border=\"1\" align=center>"
    else :
        m="<table align = center><tr>"
    p=1
    i=0

    
    while T[p-1] != "\\end{tabular}" and T[p-1] != "\\end{btabular}":
        m = m + "<td align="+align[format[i]]+">" + C[p] + "</td>"
        p=p+1
        i=i+1
        if T[p-1]=="\\\\" :
            for i in range (p,columns) :
                m=m+"<td></td>"
            m=m+"</tr><tr>"
            i=0
    m = m+ "</tr></table>"
    return (m)
 

### added to support the verbatim enviroment which is converted to pre
# must be a applied first of all, and restored last of all.


def separateverbatim(m) :
    verbre = re.compile("\\\\begin\\{verbatim}.*?\\\\end\\{verbatim}", re.M | re.S)
    verb = verbre.findall(m)
    verb = map(lambda x: x.replace('\\begin{verbatim}', beginverbatim), verb)
    verb = map(lambda x: x.replace('\\end{verbatim}', endverbatim), verb)    
    text = verbre.split(m)
    s=text[0]
    for i in range(len(verb)) :
      s=s+"__verb"+str(i)+"__"+text[i+1]
    return(s, verb)

def restoreverbatim(s, verb):
  for i in range(len(verb)) :
    s=s.replace("__verb"+str(i)+"__", "\n"+verb[i])
  return (s)
            
###       
    

def separatemath(m) :
    mathre = re.compile("\\$.*?\\$"
                   "|\\\\begin\\{equation}.*?\\\\end\\{equation}"
                   "|\\\\\\[.*?\\\\\\]")
    math = mathre.findall(m)
    text = mathre.split(m)
    return(math,text)


def processmath( M, alignplacement ) :
    R = []
    counteq=0
    global ref

    mathdelim = re.compile("\\$"
                           "|\\\\begin\\{equation}"
                           "|\\\\end\\{equation}"
                           "|\\\\\\[|\\\\\\]")
    label = re.compile("\\\\label\\{.*?}")
    
    for m in M :
        md = mathdelim.findall(m)
        mb = mathdelim.split(m)

        """
          In what follows, md[0] contains the initial delimiter,
          which is either \begin{equation}, or $, or \[, and
          mb[1] contains the actual mathematical equation
        """
        
        if md[0] == "$" :
            if HTML :
                m=m.replace("$","")
                m = lateximg(m)
                # # m=m.replace("+","%2B") 
                # m=m.replace(" ","+")
                # m=m.replace("'","&#39;")
                # m="<img src=\""+wpserver+"latex.php?latex=%7B"+m+"%7D"+endlatex+"\">"
            else :
                m='<span style="vertical-align:'+alignplacement+';">$latex {'+mb[1]+'}'+endlatex+"$</span>"

        else :
            if md[0].find("\\begin") != -1 :
                count["equation"] += 1
                mb[1] = mb[1] + "\\ \\ \\ \\ \\ ("+str(count["equation"])+")"
            if HTML :
                # mb[1]=mb[1].replace("+","%2B")
                # mb[1]=mb[1].replace("&","%26")
                # mb[1]=mb[1].replace(" ","+")
                # mb[1]=mb[1].replace("'","&#39;")
                # m = "<p align=center><img src=\""+wpserver+"latex.php?latex=\displaystyle " + mb[1] +endlatex+"\"></p>\n"
                m = "<p align=center>" + lateximg("\\displaystyle " + mb[1]) + "</p>\n"
            else :
                m = "<p align=center>$latex \displaystyle " + mb[1] +endlatex+"$</p>\n"
            if m.find("\\label") != -1 :
                mnolab = label.split(m)
                mlab = label.findall(m)
                """
                 Now the mathematical equation, which has already
                 been formatted for WordPress, is the union of
                 the strings mnolab[0] and mnolab[1]. The content
                 of the \label{...} command is in mlab[0]
                """
                lab = mlab[0]
                lab=cb.split(lab)[1]
                lab=lab.replace(":","")
                ref[lab]=count["equation"]

                m="<a name=\""+lab+"\">"+mnolab[0]+mnolab[1]+"</a>"

        R= R + [m]
    return R


def convertcolors(m,c) :
    if m.find("begin") != -1 :
        return("<span style=\"color:#"+colors[c]+";\">")
    else :
        return("</span>")


def convertitm(m) :
    if m.find("begin") != -1 :
        return ("\n\n<ul>")
    else :
        return ("\n</ul>\n\n")

def convertenum(m) :
    if m.find("begin") != -1 :
        return ("\n\n<ol>")
    else :
        return ("\n</ol>\n\n")


def convertbeginnamedthm(thname,thm) :
  global inthm

  count[T[thm]] +=1
  inthm = thm
  t = beginnamedthm.replace("_ThmType_",thm.capitalize())
  t = t.replace("_ThmNumb_",str(count[T[thm]]))
  t = t.replace("_ThmName_",thname)
  return(t)

def convertbeginthm(thm) :
  global inthm

  count[T[thm]] +=1
  inthm = thm
  t = beginthm.replace("_ThmType_",thm.capitalize())
  t = t.replace("_ThmNumb_",str(count[T[thm]]))
  return(t)
 
def convertendthm(thm) :
  global inthm

  inthm = ""
  return(endthm)


def convertlab(m) :
    global inthm
    global ref

    
    m=cb.split(m)[1]
    m=m.replace(":","")
    if inthm != "" :
        ref[m]=count[T[inthm]]
    else :
        ref[m]=count["section"]
    return("<a name=\""+m+"\"></a>")
        


def convertproof(m) :
    if m.find("begin") != -1 :
        return(beginproof)
    else :
        return(endproof)

def convertverbatim(m) :
    print m
    if m.find("begin") != -1 :
        return(beginverbatim)
    else :
        return(endverbatim)

      
    

def convertsection (m) :

 
      L=cb.split(m)

      """
        L[0] contains the \\section or \\section* command, and
        L[1] contains the section name
      """

      if L[0].find("*") == -1 :
          t=section
          count["section"] += 1
          count["subsection"]=0

      else :
          t=sectionstar

      t=t.replace("_SecNumb_",str(count["section"]) )
      t=t.replace("_SecName_",L[1])
      return(t)

def convertsubsection (m) :

      
        L=cb.split(m)

        if L[0].find("*") == -1 :
            t=subsection
        else :
            t=subsectionstar
        
        count["subsection"] += 1
        t=t.replace("_SecNumb_",str(count["section"]) )
        t=t.replace("_SubSecNumb_",str(count["subsection"]) )
        t=t.replace("_SecName_",L[1])     
        return(t)


def converturl (m) :
    L = cb.split(m)
    return ("<a href=\""+L[1]+"\">"+L[3]+"</a>")

def converturlnosnap (m) :
    L = cb.split(m)
    return ("<a class=\"snap_noshots\" href=\""+L[1]+"\">"+L[3]+"</a>")


def convertimage (m) :
    L = cb.split (m)
    return ("<p align=center><img "+L[1] + " src=\""+L[3]
         +"\"></p>")

def convertstrike (m) :
    L=cb.split(m)
    return("<s>"+L[1]+"</s>")

def processtext ( t ) :
        p = re.compile("\\\\begin\\{\\w+}"
                   "|\\\\nbegin\\{\\w+}\\s*\\{.*?}"
                   "|\\\\end\\{\\w+}"
                   "|\\\\item"
                   "|\\\\nitem\\s*\\{.*?}"
                   "|\\\\label\\s*\\{.*?}"
                   "|\\\\section\\s*\\{.*?}"
                   "|\\\\section\\*\\s*\\{.*?}"
                   "|\\\\subsection\\s*\\{.*?}"
                   "|\\\\subsection\\*\\s*\\{.*?}"
                   "|\\\\href\\s*\\{.*?}\\s*\\{.*?}"
                   "|\\\\hrefnosnap\\s*\\{.*?}\\s*\\{.*?}"
                   "|\\\\image\\s*\\{.*?}\\s*\\{.*?}\\s*\\{.*?}"
                   "|\\\\sout\\s*\\{.*?}")


 
        
        for s1, s2 in Mnomath :
            t=t.replace(s1,s2)

        
        ttext = p.split(t)
        tcontrol = p.findall(t)

 
        w = ttext[0]

 
        i=0
        while i < len(tcontrol) :
            if tcontrol[i].find("{itemize}") != -1 :
                w=w+convertitm(tcontrol[i])
            elif tcontrol[i].find("{enumerate}") != -1 :
                w= w+convertenum(tcontrol[i])
            elif tcontrol[i][0:5]=="\\item" :
                w=w+"<li>"
            elif tcontrol[i][0:6]=="\\nitem" :
                    lb = tcontrol[i][7:].replace("{","")
                    lb = lb.replace("}","")
                    w=w+"<li>"+lb
            elif tcontrol[i].find("\\hrefnosnap") != -1 :
                w = w+converturlnosnap(tcontrol[i])
            elif tcontrol[i].find("\\href") != -1 :
                w = w+converturl(tcontrol[i])
            elif tcontrol[i].find("{proof}") != -1 :
                w = w+convertproof(tcontrol[i])
            elif tcontrol[i].find("\\subsection") != -1 :
                w = w+convertsubsection(tcontrol[i])
            elif tcontrol[i].find("\\section") != -1 :
                w = w+convertsection(tcontrol[i])
            elif tcontrol[i].find("\\label") != -1 :
                w=w+convertlab(tcontrol[i])
            elif tcontrol[i].find("\\image") != -1 :
                w = w+convertimage(tcontrol[i])
            elif tcontrol[i].find("\\sout") != -1 :
                w = w+convertstrike(tcontrol[i])
            elif tcontrol[i].find("\\begin") !=-1 and tcontrol[i].find("{center}")!= -1 :
                w = w+"<p align=center>"
            elif tcontrol[i].find("\\end")!= -1  and tcontrol[i].find("{center}") != -1 :
                w = w+"</p>"
            else :
              for clr in colorchoice :
                if tcontrol[i].find("{"+clr+"}") != -1:
                    w=w + convertcolors(tcontrol[i],clr)
              for thm in ThmEnvs :
                if tcontrol[i]=="\\end{"+thm+"}" :
                    w=w+convertendthm(thm)
                elif tcontrol[i]=="\\begin{"+thm+"}":
                    w=w+convertbeginthm(thm)
                elif tcontrol[i].find("\\nbegin{"+thm+"}") != -1:
                    L=cb.split(tcontrol[i])
                    thname=L[3]
                    w=w+convertbeginnamedthm(thname,thm)
            w += ttext[i+1]
            i += 1

        return processfontstyle(w)

def processfontstyle(w) :

        close = dict()
        ww = ""
        level = i = 0
        while i < len(w):
          special = False
          for k, v in fontstyle.items():
            l = len(k)
            if w[i:i+l] == k:
              level += 1
              ww += '<' + v + '>'
              close[level] = '</' + v + '>'
              i += l
              special = True
          if not special:
            if w[i] == '{':
              ww += '{'
              level += 1
              close[level] = '}'
            elif w[i] == '}' and level > 0:
              ww += close[level]
              level -= 1
            else:
              ww += w[i]
            i += 1
        return ww
    

def convertref(m) :
    global ref
    
    p=re.compile("\\\\ref\s*\\{.*?}|\\\\eqref\s*\\{.*?}")

    T=p.split(m)
    M=p.findall(m)

    w = T[0]
    for i in range(len(M)) :
        t=M[i]
        lab=cb.split(t)[1]
        lab=lab.replace(":","")
        if t.find("\\eqref") != -1 :
           w=w+"<a href=\"#"+lab+"\">("+str(ref[lab])+")</a>"
        else :
           w=w+"<a href=\"#"+lab+"\">"+str(ref[lab])+"</a>"
        w=w+T[i+1]
    return w

"""
The program makes several passes through the input.

In a first clean-up, all text before \begin{document}
and after \end{document}, if present, is removed,
all double-returns are converted
to <p>, and all remaining returns are converted to
spaces.

The second step implements a few simple macros. The user can
add support for more macros if desired by editing the
convertmacros() procedure.

Then the program separates the mathematical
from the text parts. (It assumes that the document does
not start with a mathematical expression.) 

It makes one pass through the text part, translating
environments such as theorem, lemma, proof, enumerate, itemize,
\em, and \bf. Along the way, it keeps counters for the current
section and subsection and for the current numbered theorem-like
environment, as well as a  flag that tells whether one is
inside a theorem-like environment or not. Every time a \label{xx}
command is encountered, we give ref[xx] the value of the section
in which the command appears, or the number of the theorem-like
environment in which it appears (if applicable). Each appearence
of \label is replace by an html "name" tag, so that later we can
replace \ref commands by clickable html links.

The next step is to make a pass through the mathematical environments.
Displayed equations are numbered and centered, and when a \label{xx}
command is encountered we give ref[xx] the number of the current
equation. 

A final pass replaces \ref{xx} commands by the number in ref[xx],
and a clickable link to the referenced location.
"""
def main(s):

  """
     separate out verbatim environments as we do not want these to be
     processed. They are added back as the last step.
  """
  s, verb = separateverbatim(s)

  """
    extractbody() takes the text between a \begin{document}
    and \end{document}, if present, (otherwise it keeps the
    whole document), normalizes the spacing, and removes comments
  """

  s=extractbody(s)

  # formats tables
  s=converttables(s)

  # reformats optional parameters passed in square brackets
  s=convertsqb(s)


  #implement simple macros
  s=convertmacros(s)


  # extracts the math parts, and replaces the with placeholders
  # processes math and text separately, then puts the processed
  # math equations in place of the placeholders

  (math,text) = separatemath(s) 


  s=text[0]
  for i in range(len(math)) :
      s=s+"__math"+str(i)+"__"+text[i+1]

  s = processtext ( s )
  math = processmath ( math, opt.a )

  # converts escape sequences such as \$ to HTML codes
  # This must be done after formatting the tables or the '&' in
  # the HTML codes will create problems

  for e in esc :
      s=s.replace(e[1],e[2])
      for i in range ( len ( math ) ) :
          math[i] = math[i].replace(e[1],e[3])

  # puts the math equations back into the text


  for i in range(len(math)) :
      s=s.replace("__math"+str(i)+"__",math[i])

  # translating the \ref{} commands
  s=convertref(s)

  s = restoreverbatim(s, verb)

  if HTML :
      s="<head><style>body{max-width:55em;}a:link{color:#4444aa;}a:visited{color:#4444aa;}a:hover{background-color:#aaaaFF;}</style></head><body>"+s+"</body></html>"

  s = s.replace("<p>","\n<p>\n")
  return s


##### wordpress.cls contents
classtext = '''
\\NeedsTeXFormat{LaTeX2e}[1995/12/01] 
\\ProvidesClass{wordpress} [2012/03/28 v0.1 Wordpress LaTeX document class]

\\LoadClass[12pt]{article}

\\RequirePackage[pdftex,pagebackref,letterpaper=true,colorlinks=true,pdfpagemode=none,urlcolor=blue,linkcolor=blue,citecolor=blue,pdfstartview=FitH]{hyperref}

\\RequirePackage{amsmath,amsfonts}
\\RequirePackage{graphicx}
\\RequirePackage{color}

\\setlength{\\oddsidemargin}{0pt}
\\setlength{\\evensidemargin}{0pt}
\\setlength{\\textwidth}{6.0in}
\\setlength{\\topmargin}{0in}
\\setlength{\\textheight}{8.5in}

\\setlength{\\parindent}{0in}
\\setlength{\\parskip}{5px}

%%%%%%%%% For wordpress conversion

\\def\\more{}

\\newif\\ifblog
\\newif\\iftex
\\blogfalse
\\textrue


\\RequirePackage{ulem}
\\def\\em{\\it}
\\def\\emph#1{\\textit{#1}}

\\def\\image#1#2#3{\\begin{center}\\includegraphics[#1pt]{#3}\\end{center}}

\\let\\hrefnosnap=\\href

\\newenvironment{btabular}[1]{\\begin{tabular} {#1}}{\\end{tabular}}

\\newenvironment{red}{\\color{red}}{}
\\newenvironment{green}{\\color{green}}{}
\\newenvironment{blue}{\\color{blue}}{}

%%%%%%%%% Typesetting shortcuts

\\def\\B{\\{0,1\\}}
\\def\\xor{\\oplus}

\\def\\P{{\\mathbb P}}
\\def\\E{{\\mathbb E}}
\\def\\var{{\\bf Var}}

\\def\\N{{\\mathbb N}}
\\def\\Z{{\\mathbb Z}}
\\def\\R{{\\mathbb R}}
\\def\\C{{\\mathbb C}}
\\def\\Q{{\\mathbb Q}}
\\def\\eps{{\\epsilon}}

\\def\\bz{{\\bf z}}

\\def\\true{{\\tt true}}
\\def\\false{{\\tt false}}

%%%%%%%%% Theorems and proofs

\\newtheorem{exercise}{Exercise}
\\newtheorem{theorem}{Theorem}
\\newtheorem{lemma}[theorem]{Lemma}
\\newtheorem{definition}[theorem]{Definition}
\\newtheorem{corollary}[theorem]{Corollary}
\\newtheorem{proposition}[theorem]{Proposition}
\\newtheorem{example}{Example}
\\newtheorem{remark}[theorem]{Remark}
\\newenvironment{proof}{\\noindent {\\sc Proof:}}{$\\Box$ \\medskip} 
'''

######## example.tex contents
example = '''
\\documentclass{wordpress}
\\begin{document}

\\textbf{Addendum:} The following is an addendum to the original example.tex file 
that was distributed together two python files, latex2wp.py and latex2wpstyle.py, and a
file containing latex macros. The former python file
was the driver program and the latter contained configuration settings. The new program
tex2wp.py essentially consists of the concatenation of latex2wpstyle.py and latex2wp.py,
with some additions. They are mainly the addition of handling the \\texttt{verbatim}
environment as preformatted text, implementing $\\backslash$\\texttt{texttt} as
\\texttt{code}, as well as adding support for vertical alignment of inline math.
The latex definitions needed (formerly contained in the preamble of example.tex, and the
file containing macro definitions) are now collected in a class file wordpress.cls.
Both example.tex (this file), and the wordpress.cls file can be produced by
\\texttt{tex2wp.py}. This was done in order to allow a single python file to
contain all that is needed for writing wordpress text in LaTeX.
Assuming a unix type shell, the following commands 
will produce both the example file, as well as the needed class file:
\\begin{verbatim}
$ python tex2wp.py --example > example.tex
$ python tex2wp.py --cls > wordpress.cls
\\end{verbatim}
The example.tex file can then be compiled into LaTeX using pdflatex, or it can be
converted into ``standalone'' html by
\\begin{verbatim}
$ python tex2wp.py --html example.tex > example.html
\\end{verbatim}
or into something that can be pasted into wordpress' HTML editor:
\\begin{verbatim}
$ python tex2wp.py example.tex > example.wpml
\\end{verbatim}
In many wordpress themes, inline LaTeX is misaligned. We try to rectify
this by wrapping inline formulae (i.e., their images) in a $<$span$>$
element that has a vertical-alignment property that can be set. The
default value is ``baseline'' but a different value can be set from
the command line. Issuing
\\begin{verbatim}
$ python tex2wp.py -a 14% example.tex 
\\end{verbatim}
prints sets this to 14\% in the wordpress HTML output.
In general, try
\\begin{verbatim}
$ python tex2wp.py -h
\\end{verbatim}
for brief help on options. What follows is the original example file body.


Look at the document source to see how to \\sout{strike out} text, how
to \\begin{red}use\\end{red} \\begin{green}different\\end{green} \\begin{blue}colors\\end{blue},
and how to \\href{http://www.google.com}{link to URLs with snapshot preview}
and how to \\hrefnosnap{http://www.google.com}{link to URLs without snapshot preview}.


There is a command which is ignored by pdflatex and which 
defines where to cut the post in the version displayed on the
main page\\more

Anything between the conditional declarations {\\em ifblog . . . fi}
is ignored by LaTeX and processed by latex2wp. Anything
between {\\em iftex . . . fi} is processed by LaTex and ignored
by latex2wp.

\\ifblog \\begin{green}This green sentence appears only in WordPress \\end{green} \\fi

\\iftex \\begin{red}This red sentence appears only in the LaTeX preview \\end{red} \\fi

This is useful if one, in desperation, wants to put pure HTML commands
in the {\\em ifblog . . . fi} scope.


\\begin{lemma}[Main] \\label{lm:main}
Let $\\cal F$ be a total ramification of a compactifier, then
\\begin{equation} \\label{eq:lemma} \\forall g \\in {\\cal F}. g^2 = \\eta \\end{equation}
\\end{lemma}

The  (modifiable) numbering scheme is that lemmas, theorems, 
propositions, remarks and corollaries share the same counters,
while exercises and examples have each their own counter.

\\begin{theorem} \\label{th:ad} The ad\\`ele of a number field is never
hyperbolically transfinite.
\\end{theorem}

\\begin{proof} Left as an exercise. \\end{proof}

\\begin{exercise} Find a counterexample to Theorem \\ref{th:ad}.
\\end{exercise}

\\begin{exercise}[Advanced] Prove Lemma \\ref{lm:main}. \\end{exercise}

Note that accented characters are allowed. Unfortunately,
Erd\\H os's name cannot be properly typeset in HTML.
(Note that to get the above approximation, you need to type
backslash-H-space-o, rather than backslash-H-{o}. Both are
good in LaTeX, but only the second is recognized by LaTeX2WP.)

One can correctly type the names of H\\aa stad, Szemer\\'edi,
\\v{C}ech, and so on.

It is possible to have numbered equations

\\begin{equation} \\label{eq:test} \\frac 1 {x^2} \\ge 0 \\end{equation}

and unnumbered equations

$$ t(x) - \\frac 12 > x^{\\frac 13} $$

Unnumbered equations can be created with the double-dollar sign 
command or with the backslash-square bracket command.

\\[ f(x) = \\int_{-\\infty}^{x} \\frac 1 {t^2} dt \\]

It is possible to refer to equations and
theorems via the {\\em ref}, {\\em eqref} and {\\em label} LaTeX
commands, for example to Equation (\\ref{eq:test}),
to Equation \\eqref{eq:lemma},
and to Lemma \\ref{lm:main} above.

eqnarray* is supported, but not eqnarray:


\\begin{eqnarray*}
f(x) & <  & x^2 - y^2\\\\
& = & (x+y) \\cdot (x-y)
\\end{eqnarray*}

{\\em You {\\bf can} nest a {\\bf bold} text inside an emphasized
text or viceversa.}



The theorem-like environments {\\em theorem}, {\\em lemma},
{\\em proposition}, {\\em remark}, {\\em corollary}, {\\em example}
and {\\em exercise} are defined, as is the {\\em proof} environment.

The LaTex commands to type \\$, \\%, and \\&\\ are supported outside
math mode, and \\%\\ and \\&\\ are supported in math mode as well:

\\[  30 \\&  10 \\% \\]

The section symbol \\S\\ is also supported.

WordPress has trouble if a LaTeX expression containing a $<$
symbol, such as $x^2 < x^2 + 1$ is followed by an expression
containing a $>$ symbol, such as $(x+y)^2 > (x+y)^2 - 3$. This
is fixed by converting the inequality symbols into ``HTML 
character codes.'' Always write the symbols $<$ and $>$ in
math mode.

It it is possible to have tabular environments, both with borders
(the border will not be displayed in the LaTeX preview), as in 

\\begin{btabular}{lr}
blog  & quality\\\\
what's new & excellent\\\\
in theory  & poor
\\end{btabular}

and without borders as in

\\begin{tabular}{ccc}
$a$ & $\\rightarrow$ & $b$\\\\
$\\downarrow$  & & $\\uparrow$\\\\
$c$ & $\\rightarrow$ & $d$
\\end{tabular}

(The tabular environments will be centered in WordPress, but
not in the LaTeX preview.)

And it is possible to include a picture so that the pdf file produced
with pdflatex imports it from a local image file (which has to be
pdf, gif, jpeg, or png) and the WordPress post imports it from a URL.

\\begin{verbatim}
\\image{width = 400}{http://imgs.xkcd.com/comics/donald_knuth.png}{knuth.png}
\\end{verbatim}

The {\\em image} command used to generate the above image
has three parameter: a size parameter for either the width or the height,
expressed in pixels (if different from the original resolution, the picture
will be scaled), a URL for the location of the image (this will be used
by WordPress) and a local file name (which will used by pdflatex).

It is possible to have numbered and unnumbered sections and subsections.
References to {\\em label} commands which are not in the scope of
a numbered equation or a numbered theorem-like environment
will refer to the section number, 
such as a reference to Section \\ref{sec} below.

HTML does not have good support for itemized list with
descriptors (what one gets in LaTeX using the {\\em itemize} environment
with optional parameters in square brackets after the {\\em item} commands).
We can only offer the following rather ugly rendering:

\\begin{itemize}
\\item [Case a.] Description of case a
\\item [Case b.] Description of case b
\\end{itemize}

\\section*{Examples of Sections}

\\subsection*{And Subsections}

\\section{A section}
\\label{sec}

\\subsection{And a subsection}

\\section{Changing the style}

The file latex2wpstyle.py contains several definitions that determine
the appearance of the WordPress translation. It should be self-explanatory
to change the way sections, subsections, proofs and theorem-like
environments are typeset, and to change the numbering scheme
for theorem-like environments.

The variable $M$ in latex2wpstyle.py contains a list of pairs of strings.
For every pair, every occurrence of the first string in the document is
replaced by an occurrence of the second before proceeding to the
conversion from LaTeX to WordPress. If you want to use simple macros
(which do not involve parameter-passing) then edit $M$ to add support
for your own LaTeX macros. (You will have to define the macros in
macrosblog.tex as well, otherwise you will not be able to compile
your LaTeX file and preview it.)

Some macros are already defined. For example, backslash-E produces
an expectation symbol:

\\[ \\E_{x \\in X} f(x) := \\sum_{x\\in X} \\P [x] \\cdot f(x) \\]

Some more macros (see the LaTeX source)

\\[ \\B, \\R , \\C, \\Z, \\N , \\Q,  \\eps \\]

\\end{document}
'''

if __name__ == "__main__":
  import sys
  import re     
  from optparse import OptionParser

  parser = OptionParser(usage="%prog [options] (infile|-) [(outfile|-)]"
                        "\n Convert LaTeX to wordpress HTML."
                        "\n if '-' is given then stdin (or stdout) is used."
                        "\n if no outfile argument is given, stdout is used.")
  parser.add_option("-a", "--alignment", dest="a", default=alignplacement,
                    help="set the vertical-alignment property of inline"
                    " latex elements. Default: %default.")
  parser.add_option("--html", dest="html", default=False, action="store_true",
                    help="output standalone HTML.")
  parser.add_option("--cls", dest="cls", default=False, action="store_true",
                    help="print contents of wordpress.cls file to stdout.")
  parser.add_option("--example", dest="example", default=False, action="store_true",
                    help="print example latex file to stdout. This document"
                    " also gives usage examples.")


  (opt, args) = parser.parse_args()

  if opt.cls:
     print(classtext)
     sys.exit(0)

  if opt.example:
      print(example)
      sys.exit(0)

  HTML = opt.html
  if HTML : endproof = lateximg('\\Box') #"<img src=\""+wpserver+"latex.php?latex=\Box&fg=000000\">"
  ifile = sys.stdin
  ofile = sys.stdout
  if len(args) == 0:
    print(parser.get_usage())
    sys.exit(1)
  if len(args) > 0 and args[0] != '-':
    ifile = open(args[0])
  if len(args) > 1 and args[1] != '-':
    ofile = open(args[1], 'w')


  s=ifile.read()

  s = main(s)

  ofile.write(s)
  ofile.write('\n')
  
  # that's all folks
