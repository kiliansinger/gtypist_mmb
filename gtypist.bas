'gtypist port to mmbasic
'Copyright (c) 2026  Kilian Singer
'
'This program is free software; you can redistribute it and/or
'modify it under the terms of the GNU General Public License
'as published by the Free Software Foundation; either version 2
'of the License, or (at your option) any later version.
'
'This program is distributed in the hope that it will be useful,
'but WITHOUT ANY WARRANTY; without even the implied warranty of
'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'GNU General Public License for more details.
'
'You should have received a copy of the GNU General Public License
'along with this program; if not, write to the Free Software
'Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

Open "gtypist_cp858.typ" For input As #1
'Open "mini.typ" For input As #1
'Open "ttde_cp858.typ" For input As #1
'Open "ktes3_cp858.typ" For input As #1
'Open "gtypist.typ" For input As #1

MODE 3
textcol=RGB(255,255,255)
bancol=RGB(0,255,255)
backcol=RGB(0,0,0)
errcol=RGB(255,0,0)
Color textcol,backcol
'MODE 2
'using codepage 858 encoding
'for f in *.typ; do echo "${f}";iconv -f utf8 -t cp858 "${f}" > "$(basename ${f} .typ)"_cp858.typ ;done
'change input file to convert from ansi with:  iconv -f WINDOWS-1252 -t CP858 ttde.typ > ttde_cp850.typ
'change unput file to convert from utf with: iconv -f utf8 -t cp858 ttde.typ > ttde_cp850b.typ
Font 8
GoTo ll

For n=32 To 255
Print n,Chr$(n)," ";
Next
l:
a= Asc(Inkey$)
If a<>0 Then Print a,Chr$(a),KeyDown(7)
GoTo l
ll:
Dim integer maxcy=MM.VRES\MM.Info(fontheight)
Dim integer maxcx=MM.HRES\MM.Info(fontwidth)
Dim menulabel(50) As string
Dim menutxt(50) As string
Dim menushpos(50) As integer
Dim instructions(30) As string
instructionslen=0
Dim lines(50) As string
Dim lineshpos(50) As integer
Function stripdqm$(s As string)
  Local txt$
  txt$=s
  If Left$(s,1)=Chr$(34) Then txt$=Mid$(txt$,2)
  If Right$(s,1)=Chr$(34) Then txt$=Mid$(txt$,1,Len(txt$)-1)
  stripdqm$=txt$
End Function
Function trimr$(s As string)
  For i=Len(s) To 1 Step -1
    If Mid$(s,i,1)<>" " Then Exit For
  Next
  trimr$=Mid$(s,1,i)
End Function
Function triml$(s As string)
  For i=1 To Len(s)
    If Mid$(s,i,1)<>" " Then Exit For
  Next
  triml$=Mid$(s,i)
End Function
Function trimlr$(s As string)
  trimlr$=trimr$(triml$(s))
End Function
Function centertxt$(s As string)
   Local txt$,txt2$
   txt$=trimlr$(s)
   txt2$=""
   If maxcx>Len(txt$) Then
     If ((maxcx-Len(txt$))\2*2+Len(txt$)<maxcx) Then txt2$=" "
     centertxt$=String$((maxcx-Len(txt$))\2," ")+txt$+String$((maxcx-Len(txt$))\2," ")+txt2$
   Else
     centertxt$=txt$
   End If
End Function

Function findlabel%(label$)
  Local a$
  currloc=Loc(#1)
  Seek #1,1
  Do While Not Eof(#1)
    Line Input #1,a$
    'labels: *:LABELNAME
    If Left$(a$,2)<>"*:" Then Continue do
    If trimr$(Mid$(a$,3))=label$ Then
      findlabel%=Loc(#1)
      Seek #1,currloc
      Exit Function
    End If
  Loop
  findlabel%=0
  Seek #1,currloc


'  for i=0 to labelcnt
'    if(labelname$(i)=label$) then
'      findlabel%=i
'      EXIT FUNCTION
'    end if
'  next
'  findlabel%=-1
End Function
'get all jump labels

labelcnt=0
banner$=""
'scan for all labels

'do while not eof(#1)
'  line input #1,a$
'  'labels: *:LABELNAME
'  if left$(a$,2)<>"*:" then continue do
'  label(labelcnt)=loc(#1)
'  labelname$(labelcnt)=trimr$(mid$(a$,3))
'  labelcnt=labelcnt+1 'inc
'  rem print lof(#1)
'  rem print loc(#1)
'loop
'print "done scanning labels"
'seek #1,1

Dim lastlabel(50)
lastlabelcnt=0
lastlabel(lastlabelcnt)=1 ' "" stands for beginning
lastlabelcnt=lastlabelcnt+1
lastpos=1
redrawpos=1' the concept of redrawpos is broken
menupos=0
esccnt=0
yes=0
no=0
errorrate=10
nexterrorrate=errorrate
faillabel$=""
nextfaillabel$=faillabel$

Do While Not Eof(#1)
  lastpos=Loc(#1)
  Line Input #1,a$
  'comment: #....
  If Left$(a$,1)="#" Then Continue do
  'EMPTY LINE
  If trimr$(Left$(a$,1))="" Then Continue do
  'label: *:LABELNAME
  If Left$(a$,2)="*:" Then
    'lastlabel$(lastlabelcnt)=trimr$(mid$(a$,3))
    'lastlabelcnt=lastlabelcnt+1
    ''currentlabel$=trimr$(Mid$(a$,3))
    Continue do 'labels
  End If
  'goto: G:LABELNAME
  If Left$(a$,2)="G:" Or Left$(a$,2)="Y:" Or Left$(a$,2)="N:" Then

    labnum=findlabel%(trimr$(Mid$(a$,3)))

    If labnum=0 Then
      Print "label "Mid$(a$,3)" does not exists"
      Exit
    Else
      'lastlabel$(lastlabelcnt)=trimr$(mid$(a$,3))
      'lastlabelcnt=lastlabelcnt+1
      ''currentlabel$=trimr$(Mid$(a$,3))
      If Left$(a$,2)="G:" Or (yes=1 And Left$(a$,2)="Y:") Or (no=1 And Left$(a$,2)="N:") Then
        Seek #1,labnum
      End If
    End If
    Continue do
  End If
  'errorrate:  E:<value>%  until the next E:...
  '            E:<value>%* for all following drills
   If Left$(a$,2)="E:" Then
     nexterrorrate=Val(Mid$(a$,3,Instr(a$,"%")-3))
     If Right$(a$,1)="*" Then errorrate=nexterrorrate
     Continue do
   End If
   'fail label:  F:<label>
  '              F:<label>* for all following drills
   If Left$(a$,2)="F:" Then

     If Right$(a$,1)="*" Then
       nextfaillabel$=Mid$(a$,3,Len(a$)-3)
       faillabel$=nextfaillabel$
     Else
       nextfaillabel$=Mid$(a$,3,Len(a$)-2)
     End If
     Continue do
   End If
  'banner: B:...
  If Left$(a$,2)="B:" Then
    CLS backcol
    'redrawpos=lastpos
    Color backcol,bancol
    banner$=centertxt$(Mid$(a$,3))
    Print banner$
    Color textcol,backcol
    Continue do
  End If
  'functionkey: K:fkeynumber:label  depracated=>ignored
  If Left$(a$,2)="K:" Then Continue do
  'Tutorial: T:...
  '           :...
  If Left$(a$,2)="T:" Then
    CLS backcol
    Color backcol,bancol

    Print banner$
    Color textcol,backcol
    Print Mid$(a$,3)
    Do While Not Eof(#1)
      lastpos=Loc(#1)
      Line Input #1,a$
      If Left$(a$,2)=" :" Then
        Print Mid$(a$,3)
      Else
        Seek #1,lastpos
        Exit Do
      End If
    Loop
    Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$("Tutorial: Press RETURN or SPACE to continue, ESC to return to the menu")
    Do
      Do
        pressed=Asc(Inkey$)
      Loop Until pressed<>0
      'return space
      If pressed=10 Or pressed=13 Or pressed=32 Then Exit Do
      'esc
      If pressed=27 Then
        If lastlabelcnt>1 Then
          lastlabelcnt=lastlabelcnt-1
          labnum=lastlabel(lastlabelcnt)
          redrawpos=labnum
          'Print lastlabelcnt,lastlabel$(lastlabelcnt)
          Seek #1,labnum
          Exit Do
        Else
          End
        End If
      End If
    Loop
    Continue do
  End If
  If Left$(a$,2)="Q:" Then
    yes=0
    no=0
    Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$(Mid$(a$,3))
    Do
      Do
        pressed=Asc(Inkey$)
      Loop Until pressed<>0
      'return space
      If pressed=Asc("y") Or pressed=Asc("n") Or pressed=Asc("Y") Or pressed=Asc("N") Then
          If pressed=Asc("y") Or pressed=Asc("Y") Then
            yes=1
          Else
            no=1
          End If
          Exit Do

      End If
    Loop
    Continue do
  End If
  'exit: X:
  If Left$(a$,2)="X:" Then
    End
  End If
  'instruction:I:....
  '             :...
  If Left$(a$,2)="I:" Then
    'redrawpos=lastpos
    CLS backcol
    Color backcol,bancol

    Print banner$
    Color textcol,backcol
    instructionslen=0
    instructions(instructionslen)=Mid$(a$,3)
    Print instructions(instructionslen)
    instructionslen=instructionslen+1
    Do While Not Eof(#1)
      lastpos=Loc(#1)
      Line Input #1,a$
      If Left$(a$,2)=" :" Then
        instructions(instructionslen)=Mid$(a$,3)
        Print instructions(instructionslen)
        instructionslen=instructionslen+1
      Else
        Seek #1,lastpos
        Exit Do
      End If
    Loop
    Print
    Continue do
  End If

  If UCase$(Left$(a$,2))="D:" Or UCase$(Left$(a$,2))="S:" Then
    deadkey=0
    redrawpos=lastpos
  'problem B and I needs to be stored also redrawpos dos not work
  'as we have
  'B:..
  'I:...
  'S:...CLS but keep banner and intro
  'S:...CLS but keep banner and intro
    CLS backcol
    Color backcol,bancol
    Print banner$
    Color textcol,backcol
    For i=0 To instructionslen-1
      Print instructions(i)
    Next
    Print

    txtlen=0
    dospeed=0
    dopractise=0
    xpos=1
    errorcnt=0
    Timer =0
    oldtimer=Timer
    If UCase$(Left$(a$,2))="S:" Then
      If Left$(a$,2)="s:" Then
        dopractise=1
      Else
        dopractise=0
      EndIf
      dospeed=1
    Else
      If Left$(a$,2)="d:" Then
        dopractise=1
      Else
        dopractise=0
      EndIf
      dospeed=0
    End If
   linecnt=0
   linetype=0
   lines(linecnt)=Mid$(a$,3)
   txtlen=txtlen+Len(lines(linecnt))


   Print Mid$(a$,3)
   If dospeed=0 Then
      lineshpos(linecnt)=MM.Info(vpos)
    Else
      lineshpos(linecnt)=MM.Info(vpos)-MM.Info(fontheight)
    End If
   linecnt=linecnt+1
   If dospeed=0 Then
     Print
   End If
    Do While Not Eof(#1)
      lastpos=Loc(#1)
      Line Input #1,a$
      If Left$(a$,2)=" :" Then
        lines(linecnt)=Mid$(a$,3)
        txtlen=txtlen+Len(lines(linecnt))

        Print Mid$(a$,3)
        If dospeed=0 Then
          lineshpos(linecnt)=MM.Info(vpos)
        Else
          lineshpos(linecnt)=MM.Info(vpos)-MM.Info(fontheight)
        End If
        linecnt=linecnt+1
        If dospeed=0 Then
          Print
        End If
      Else
        Seek #1,lastpos
        Exit Do
      End If
    Loop
    If dospeed=0 Then
      Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$("Drill")
    Else
      Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$("Speed Test")
    End If
    Print @(0,lineshpos(linetype),0) "";
    blink=0
    Do
      Do
        pressed=Asc(Inkey$)
        If Timer>oldtimer+500 Then
          oldtimer =Timer
          If blink=0 Then
            blink=2
          Else
            blink=0
          End If
          If dospeed=0 Then
            Print @(MM.Info(hpos),MM.Info(vpos),blink) " ";
          Else
            If Mid$(lines(linetype),xpos,1)="" Then
              Print @(MM.Info(hpos),MM.Info(vpos),blink) " ";
            Else
              Print @(MM.Info(hpos),MM.Info(vpos),blink) Mid$(lines(linetype),xpos,1);
            End If
          EndIf
          Print @(MM.Info(hpos)-MM.Info(fontwidth),MM.Info(vpos),blink) "";
        End If
      Loop Until pressed<>0
      oldtimer=Timer

      'return space
      'if pressed=10 or pressed=32 then exit do
      'esc
      If esccnt<2 Then

        If pressed=27 Then
          If esccnt=0 Then
            esccnt=esccnt+1
            Seek #1,redrawpos
            Exit Do
          End If
          If esccnt=1 Then
            Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$("Press R to repeat, N for next exercise or E to exit")
            esccnt=esccnt+1
          End If
        Else
          esccnt=0
          If pressed=8 And dospeed=1 And xpos>1 Then
            Print @(MM.Info(hpos),MM.Info(vpos),0) Mid$(lines(linetype),xpos,1);
            xpos=xpos-1
            Print @(MM.Info(fontwidth)*(xpos-1),lineshpos(linetype),0) "";
            blink=0
            oldtimer=Timer-600
            Continue Do
          End If
          If pressed=10 Or pressed=13) And (xpos<Len(lines(linetype))) Then
            If (xpos<Len(lines(linetype))) Then
              errorcnt=errorcnt+1
              If dospeed=0 Then
                Color errcol
                Print @(MM.Info(hpos),MM.Info(vpos),2) "^";
                Color textcol
              Else
                Color errcol
                Print @(MM.Info(hpos),MM.Info(vpos),2) Mid$(lines(linetype),xpos,1);
                Color textcol
              End If
              xpos=xpos+1
            End If
          End If
          If (pressed=10 Or pressed=13) And (xpos>=Len(lines(linetype))) Then
            If Mid$(lines(linetype),xpos,1)="" Then
                Print @(MM.Info(hpos),MM.Info(vpos),0) " ";
            End If
            linetype=linetype+1
            If linetype>=linecnt Then
              Print @(MM.HRES/5*3,MM.VRES-5*MM.Info(fontheight),2) "Raw speed      =  "Format$(txtlen/5/(Timer/60000),"%.2f")" wpm"
              'word is 5 char
              'to calculate adjusted wpm we subtract errors from written words
              'as each error counts as a wrong word
              adjusted=(txtlen-errorcnt)/5/(Timer/60000)
              If adjusted<0 Then adjusted=0
              Print @(MM.HRES/5*3,MM.VRES-4*MM.Info(fontheight),2) "Adjusted speed =  "Format$(adjusted,"%.2f")" wpm"
              Print @(MM.HRES/5*3,MM.VRES-3*MM.Info(fontheight),2) "with "Format$(errorcnt/txtlen*100,"%.1f")"% errors"

              If practice=0 Then
                If errorcnt/txtlen*100>errorrate Then
                   num$=Format$(nexterrorrate,"%.1f")
                   ac$="Your error-rate is too high. You have to achieve "+num$+"%"
                   Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$(ac$)
                   Do
                     pressed=Asc(Inkey$)
                   Loop Until pressed=27 Or pressed=10 Or pressed=13 Or pressed=32



                    'Your error-rate is too high. You have to achieve 3.0%.
                    'wait for space enter exit
                    'incase nexterrorrate
                    'You failed this test, so you need to go back to LOOP2.
                    If nextfaillabel$<>"" Then
                      ac$="You failed this test, so you need to go back to "+nextfaillabel$
                      Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$(ac$)
                      Do
                        pressed=Asc(Inkey$)
                      Loop Until pressed=27 Or pressed=10 Or pressed=13 Or pressed=32
                      esccnt=0
                      labnum=findlabel%(nextfaillabel$)

                      If labnum=0 Then
                        Print "label "Mid$(a$,3)" does not exists"
                        Seek #1,redrawpos
                      Else
                        Seek #1,labnum
                      End If
                      Exit Do
                    Else
                      esccnt=0
                      Seek #1,redrawpos
                      Exit Do
                    End If
                  End If
              End If

              Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$("Press R to repeat, N for next exercise or E to exit")
              nexterrorrate=errorrate
              esccnt=2
            Else
              Print @(0,lineshpos(linetype),0) "";

              xpos=1
              blink=2
              If dospeed=0 Then
                Print @(MM.Info(hpos),MM.Info(vpos),blink) " ";
                Print @(MM.Info(hpos)-MM.Info(fontwidth),MM.Info(vpos),blink) "";

              Else
              blink=0
                oldtimer=Timer-600
                Print @(MM.Info(hpos),MM.Info(vpos),blink) "";
              EndIf
            End If
          End If
          If (pressed>=32 And pressed<=126) Or (pressed>=200 And pressed<=209) Then
          'german keyboard
            If pressed=209 Then
              pressed=248'circ
            Else If pressed=201 Then
              pressed=225'sz
            Else If pressed=94 Then
              '^
              deadkey=pressed
              pressed=0
            Else If pressed=96 Then
              'accent grave
              deadkey=pressed
              pressed=0
            Else If pressed=202 Then
              pressed=0'accent aigu
              deadkey=239
            Else If pressed=203 Then
              pressed=129'ue
            Else If pressed=204 Then
              pressed=154'UE
            Else If pressed=205 Then
             pressed=148'oe
            Else If pressed=206 Then
              pressed=153'OE
            Else If pressed=207 Then
              pressed=132'ae
            Else If pressed=208 Then
              pressed=142'AE
            Else
              If deadkey=0 Then
                If KeyDown(7)=16 Then
                  If pressed=Asc("e") Then
                    'euro
                    pressed=213
                  Else If pressed=Asc("3") Then
                    'small 3
                    pressed=252
                  Else If pressed=Asc("2") Then
                    'small 2
                    pressed=253
                  Else If pressed=Asc("m") Then
                    'small 2
                    pressed=230
                  End If
                End If
              Else
                'deadkey pressed previously
                If pressed=Asc(" ") Then
                  pressed=deadkey
                Else If pressed=Asc("a") Then
                  If deadkey=94 Then
                    '^
                    pressed=131
                  Else If deadkey=96 Then
                    'accent gravee
                    pressed=133
                  Else If deadkey=239 Then
                    'accent aigu
                    pressed=160
                  End If
                Else If pressed=Asc("A") Then
                  If deadkey=94 Then
                    '^
                    pressed=182
                  Else If deadkey=96 Then
                    'accent gravee
                    pressed=183
                  Else If deadkey=239 Then
                    'accent aigu
                    pressed=181
                  End If

                Else If pressed=Asc("e") Then
                    If deadkey=94 Then
                    '^
                    pressed=136
                  Else If deadkey=96 Then
                    'accent gravee
                    pressed=138
                  Else If deadkey=239 Then
                    'accent aigu
                    pressed=130
                  End If
                Else If pressed=Asc("E") Then
                  If deadkey=94 Then
                    '^
                    pressed=210
                  Else If deadkey=96 Then
                    'accent gravee
                    pressed=212
                  Else If deadkey=239 Then
                    'accent aigu
                    pressed=144
                  End If

                Else If pressed=Asc("i") Then
                  If deadkey=94 Then
                    '^
                    pressed=140
                  Else If deadkey=96 Then
                    'accent gravee
                    pressed=141
                  Else If deadkey=239 Then
                    'accent aigu
                    pressed=161
                  End If
                Else If pressed=Asc("I") Then
                  If deadkey=94 Then
                    '^
                    pressed=215
                  Else If deadkey=96 Then
                    'accent gravee
                    pressed=222
                  Else If deadkey=239 Then
                    'accent aigu
                    pressed=214
                  End If
                Else If pressed=Asc("o") Then
                  If deadkey=94 Then
                    '^
                    pressed=147
                  Else If deadkey=96 Then
                    'accent gravee
                    pressed=149
                  Else If deadkey=239 Then
                    'accent aigu
                    pressed=162
                  End If
                Else If pressed=Asc("O") Then
                  If deadkey=94 Then
                    '^
                    pressed=226
                  Else If deadkey=96 Then
                    'accent gravee
                    pressed=227
                  Else If deadkey=239 Then
                    'accent aigu
                    pressed=224
                  End If
                Else If pressed=Asc("u") Then
                  If deadkey=94 Then
                    '^
                    pressed=150
                  Else If deadkey=96 Then
                    'accent gravee
                    pressed=151
                  Else If deadkey=239 Then
                    'accent aigu
                    pressed=163
                  End If
                Else If pressed=Asc("U") Then
                  If deadkey=94 Then
                    '^
                    pressed=234
                  Else If deadkey=96 Then
                    'accent gravee
                    pressed=235
                  Else If deadkey=239 Then
                    'accent aigu
                    pressed=233
                  End If
                End If
              End If
              deadkey=0
            End If
            If pressed<>0 Then
              If Mid$(lines(linetype),xpos,1)=Chr$(pressed) Then
                Print @(MM.Info(hpos),MM.Info(vpos),0) Chr$(pressed);
              Else
                errorcnt=errorcnt+1
                If dospeed=0 Then
                  Color errcol
                  Print @(MM.Info(hpos),MM.Info(vpos),2) "^";
                  Color textcol
                Else
                  Color errcol
                  Print @(MM.Info(hpos),MM.Info(vpos),2) Chr$(pressed);
                  Color textcol
                End If
              End If
              xpos=xpos+1
              blink=2
              If dospeed=0 Then
                Print @(MM.Info(hpos),MM.Info(vpos),blink) " ";
                Print @(MM.Info(hpos)-MM.Info(fontwidth),MM.Info(vpos),blink) "";
              Else
                blink=0
                oldtimer=Timer-600
                Print @(MM.Info(hpos),MM.Info(vpos),blink) "";
              EndIf
            End If
          End If
        End If
      Else If esccnt=2 Then
        If pressed=Asc("r") Or pressed=Asc("R") Then
          esccnt=0
          Seek #1,redrawpos
          Exit Do
        End If
        If pressed=Asc("n") Or pressed=Asc("N") Then
          esccnt=0
          Exit Do
        End If
        If pressed=Asc("e") Or pressed=Asc("E") Then
          esccnt=0
          lastlabelcnt=lastlabelcnt-1
          labnum=lastlabel(lastlabelcnt)
          redrawpos=labnum
          'Print lastlabelcnt,lastlabel(lastlabelcnt)
          'input x$
          Seek #1,labnum
          Exit Do
        End If
      End If


    Loop
    Continue do
  End If
  'drill:d or D:....  d: practice only D: drill
  '            :...
  'menu: M: [UP=RETURN_LABEL|_EXIT] "title"
  '       :LABEL1 "item1"
  '       :LABEL2 "item2"
  '       ...
  'This will display a convenient menu made from the specified items
  'and let the user to choose from them.  If an item was selected,
  'gtypist will continue script execution from the corresponding
  'label.  If the Escape key was pressed and bUPb label is defined,
  'gtypist will go to the bUPb label likewise, or quit from, if there
  'is b``_EXIT''b in the place of the label.  If the bUPb label is not
  'defined, gtypist will try to return to the previous menu and jump
  'to the last label met in the script before previous bMb command.
  'If there is no such label and some menu was displayed before the
  'current one, gtypist will just go to the beginning of the script.
  'If none of the previous conditions were met, gtypist will just exit
  'from the script.
  'M only keeps the banner
  'I is not restored
  If Left$(a$,2)="M:" Then
    menulastpos=lastpos
    CLS backcol
    txt$=trimlr$(Mid$(a$,3))
    uplabel$=""
    If Left$(txt$,3)="UP=" Then
      uplabel$=trimlr$(Mid$(txt$,4,Instr(txt$,Chr$(34))-4))
    Else If Left$(txt$,3)="_EXIT" Then
      uplabel$="_EXIT"
    End If
    title$=stripdqm$(Mid$(txt$,Instr(txt$,Chr$(34))))
    Print
    Print centertxt$(title$)
    Print

    menucnt=0
    Do While Not Eof(#1)
      lastpos=Loc(#1)
      Line Input #1,a$
      If Left$(a$,2)=" :" Then
        menulabel(menucnt)=trimlr$(Mid$(Mid$(a$,3),1,Instr(Mid$(a$,3)," ")))
        If Instr(a$,Chr$(34))=0 Then Print "!!!"a$
        menutxt(menucnt)=stripdqm$(Mid$(a$,Instr(a$,Chr$(34))))
        scroll=(menupos\(maxcy-6))*(maxcy-6)
        If menucnt-scroll>=0 Then
          If menucnt-scroll<maxcy-6 Then
            If menucnt=menupos Then
              menushpos(menucnt)=MM.Info(vpos)
              Print @(0,,2) menutxt(menucnt)
            Else
              menushpos(menucnt)=MM.Info(vpos)
              Print @(0,,) menutxt(menucnt)
            End If
          Else If menucnt-scroll=maxcy-6 Then
            Print "..."
          End If
        End If
        menucnt=menucnt+1
      Else
        Seek #1,lastpos
        Exit Do
      End If
    Loop
    Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$("up down SPACE or RETURN to select and ESCAPE to go back")
    Do
      Do
        pressed=Asc(Inkey$)
      Loop Until pressed<>0
      'down
      If pressed=129 Then
        Print @(0,menushpos(menupos),0) menutxt(menupos)
        If menupos<menucnt-1 Then menupos=menupos+1
        If scroll=(menupos\(maxcy-6))*(maxcy-6) Then
          Print @(0,menushpos(menupos),2) menutxt(menupos)
          Continue do
        End If
      'up
      Else If pressed=128 Then
         Print @(0,menushpos(menupos),0) menutxt(menupos)
        If menupos>0 Then menupos=menupos-1
        If scroll=(menupos\(maxcy-6))*(maxcy-6) Then
          Print @(0,menushpos(menupos),2) menutxt(menupos)
          Continue do
        End If
      'return
      Else If pressed=10 Or pressed=13 Then
        labnum=findlabel%(menulabel(menupos))
        If labnum=0 Then
          Print "label "menulabel(menupos)" does not exists"
          Exit
        Else
          lastlabel(lastlabelcnt)=menulastpos
          ''currentlabel$=menulabel(menupos)
          lastlabelcnt=lastlabelcnt+1
          menulastpos=labnum
          Seek #1,labnum
          menupos=0
        End If
        Exit Do
      Else If pressed=27 Then
        menupos=0
        If uplabel$<>"" Then
          If uplabel$="_EXIT" Then End
          labnum=findlabel%(uplabel$)
          menulastpos=labnum
          Seek #1,labnum
        Else If lastlabelcnt>1 Then
          lastlabelcnt=lastlabelcnt-1
          'Print lastlabelcnt,lastlabel$(lastlabelcnt)
          'if lastlabel$(lastlabelcnt)="" then end
          labnum=lastlabel(lastlabelcnt)
          'currentlabel$=lastlabel$(lastlabelcnt)
          menulastpos=labnum
          Seek #1,labnum
          'exit do
        Else
          End
        End If
      Else
        Continue do
      End If
      Exit Do
    Loop
    Seek #1,menulastpos

    Continue do
  End If
  'labelname$(labelcnt)=trim$(mid$(a$,3),,R)

  Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$("NOTHANDLED: "+a$);
  Print @(0,MM.Info(fontheight),0) ""

  Rem print lof(#1)
  Input x$
Loop

' XGA_8x14mod858.bas
' Font type    : Full (223 characters)
' Font start   : CHR$(32)
' Font size    : 8x14 pixels
' Memory usage : 3126 bytes
DefineFont #8
  DF200E08
  00000000 00000000 00000000 00000000 F0F0F060 60006060 00000060 66666600
  00000024 00000000 00000000 6CFE6C6C 6CFE6C6C 0000006C C67C1818 067CC0C2
  187CC686 00000018 C6C20000 6630180C 000000C6 6C380000 DC76386C 0076CCCC
  30000000 00303030 00000000 00000000 60300000 C0C0C0C0 003060C0 00000000
  303060C0 60303030 000000C0 00000000 3CFF3C66 00000066 00000000 18180000
  0018187E 00000000 00000000 00000000 C0606060 00000000 00000000 000000FE
  00000000 00000000 00000000 00C0C000 00000000 180C0602 80C06030 00000000
  C67C0000 E6F6DECE 007CC6C6 00000000 18783818 18181818 0000007E C67C0000
  30180C06 00FEC660 00000000 0606C67C C606063C 0000007C 1C0C0000 FECC6C3C
  001E0C0C 00000000 C0C0C0FE C60606FC 0000007C 60380000 C6FCC0C0 007CC6C6
  00000000 0C06C6FE 30303018 00000030 C67C0000 C67CC6C6 007CC6C6 00000000
  C6C6C67C 0C06067E 00000078 C0000000 000000C0 0000C0C0 00000000 00606000
  60600000 000000C0 0C060000 30603018 00060C18 00000000 7E000000 007E0000
  00000000 30600000 0C060C18 00603018 00000000 0CC6C67C 18001818 00000018
  C67C0000 DEDEDEC6 007CC0DC 00000000 C66C3810 C6C6FEC6 000000C6 66FC0000
  667C6666 00FC6666 00000000 C0C2663C 66C2C0C0 0000003C 6CF80000 66666666
  00F86C66 00000000 686266FE 66626878 000000FE 66FE0000 68786862 00F06060
  00000000 C0C2663C 66C6DEC0 0000003A C6C60000 C6FEC6C6 00C6C6C6 00000000
  1818183C 18181818 0000003C 0C1E0000 0C0C0C0C 0078CCCC 00000000 6C6C66E6
  666C6C78 000000E6 60F00000 60606060 00FE6662 00000000 FEFEEEC6 C6C6C6D6
  000000C6 E6C60000 CEDEFEF6 00C6C6C6 00000000 C6C66C38 6CC6C6C6 00000038
  66FC0000 607C6666 00F06060 00000000 C6C6C67C 7CDED6C6 00000E0C 66FC0000
  6C7C6666 00E66666 00000000 60C6C67C C6C60C38 0000007C 7E7E0000 1818185A
  003C1818 00000000 C6C6C6C6 C6C6C6C6 0000007C C6C60000 C6C6C6C6 0010386C
  00000000 C6C6C6C6 7CFED6D6 0000006C C6C60000 3838386C 00C6C66C 00000000
  66666666 1818183C 0000003C C6FE0000 6030188C 00FEC6C2 00000000 3030303C
  30303030 0000003C C0800000 1C3870E0 0002060E 00000000 0C0C0C3C 0C0C0C0C
  0000003C C66C3810 00000000 00000000 00000000 00000000 00000000 00FF0000
  18306000 00000000 00000000 00000000 78000000 CCCC7C0C 00000076 60E00000
  666C7860 007C6666 00000000 7C000000 C6C0C0C6 0000007C 0C1C0000 CC6C3C0C
  0076CCCC 00000000 7C000000 C6C0FEC6 0000007C 361C0000 30783032 00783030
  00000000 76000000 7CCCCCCC 0078CC0C 60E00000 66766C60 00E66666 00000000
  38001818 18181818 0000003C 06060000 06060E00 66660606 0000003C 666060E0
  666C786C 000000E6 18380000 18181818 003C1818 00000000 EC000000 D6D6D6FE
  000000C6 00000000 6666DC00 00666666 00000000 7C000000 C6C6C6C6 0000007C
  00000000 6666DC00 60607C66 000000F0 76000000 7CCCCCCC 001E0C0C 00000000
  6676DC00 00F06060 00000000 7C000000 C61C70C6 0000007C 30100000 3030FC30
  001C3630 00000000 CC000000 CCCCCCCC 00000076 00000000 66666600 00183C66
  00000000 C6000000 FED6D6C6 0000006C 00000000 386CC600 00C66C38 00000000
  C6000000 7EC6C6C6 00F80C06 00000000 18CCFE00 00FE6630 00000000 1818180E
  18181870 0000000E 30300000 30303030 00303030 00000000 18181870 1818180E
  00000070 DC760000 00000000 00000000 00000000 38100000 FEC6C66C 00000000
  663C0000 C2C0C0C2 060C3C66 0000007C CC00CCCC CCCCCCCC 00000076 30180C00
  FEC67C00 007CC6C0 10000000 78006C38 CCCC7C0C 00000076 CCCC0000 7C0C7800
  0076CCCC 60000000 78001830 CCCC7C0C 00000076 386C3800 7C0C7800 0076CCCC
  00000000 663C0000 0C3C6660 00003C06 6C381000 FEC67C00 007CC6C0 00000000
  7C00CCCC C6C0FEC6 0000007C 18306000 FEC67C00 007CC6C0 00000000 38006666
  18181818 0000003C 663C1800 18183800 003C1818 60000000 38001830 18181818
  0000003C 10C6C600 C6C66C38 00C6C6FE 6C380000 6C380038 C6FEC6C6 000000C6
  FE00180C 78786266 00FE6662 00000000 76CC0000 D8D87E36 0000006E 6C3E0000
  CCFECCCC 00CECCCC 10000000 7C006C38 C6C6C6C6 0000007C C6C60000 C6C67C00
  007CC6C6 60000000 7C001830 C6C6C6C6 0000007C CC783000 CCCCCC00 0076CCCC
  60000000 CC001830 CCCCCCCC 00000076 C6C60000 C6C6C600 0C067EC6 C6000078
  C66C38C6 6CC6C6C6 00000038 00C6C600 C6C6C6C6 007CC6C6 00000000 7C000000
  E6F6DECE 0000007C 646C3800 6060F060 00FCE660 00000000 DE4C6E3A EC64F6D6
  000000B8 00000000 18183C66 0000663C 0E000000 1818181B 1818187E 0070D818
  60301800 7C0C7800 0076CCCC 0C000000 38003018 18181818 0000003C 60301800
  C6C67C00 007CC6C6 18000000 CC006030 CCCCCCCC 00000076 DC760000 6666DC00
  00666666 DC760000 F6E6C600 C6CEDEFE 000000C6 6C6C3C00 007E003E 00000000
  38000000 00386C6C 0000007C 00000000 30300000 60303000 007CC6C6 00000000
  00000000 C0C0C0FE 00000000 B2443800 AAB2BAAA 003844AA C0000000 D8CCC6C0
  86DC6030 003E180C C6C0C000 6630D8CC 063E9ECE 00000006 60006060 F0F0F060
  00000060 00000000 6CD86C36 00000036 00000000 6CD80000 00D86C36 00000000
  44114411 44114411 44114411 AA554411 AA55AA55 AA55AA55 AA55AA55 77DD77DD
  77DD77DD 77DD77DD C0C077DD C0C0C0C0 C0C0C0C0 C0C0C0C0 18181818 F8181818
  18181818 C0601818 C66C3810 C6C6FEC6 000000C6 3800C67C FEC6C66C 00C6C6C6
  060C0000 C66C3810 C6C6FEC6 000000C6 92443800 AAA2A2AA 00384492 36360000
  F6363636 3636F606 36363636 D8D8D8D8 D8D8D8D8 D8D8D8D8 0000D8D8 FE000000
  3636F606 36363636 36363636 FE06F636 00000000 18000000 60663C18 1C3C6660
  00000018 3C666600 7E187E18 00181818 00000000 00000000 1818F800 18181818
  C0C0C0C0 F8C0C0C0 00000000 18180000 18181818 0000FF18 00000000 00000000
  FF000000 18181818 C0C01818 C0C0C0C0 C0C0F8C0 C0C0C0C0 00000000 FF000000
  00000000 18180000 18181818 1818FF18 18181818 00DC7600 7C0C7800 0076CCCC
  DC760000 C66C3800 C6C6FEC6 000000C6 D8D8D8D8 FCC0DCD8 00000000 00000000
  FC000000 D8D8DCC0 D8D8D8D8 36363636 FF00F736 00000000 00000000 FF000000
  3636F700 36363636 D8D8D8D8 DCC0DCD8 D8D8D8D8 0000D8D8 FF000000 0000FF00
  00000000 36363636 F700F736 36363636 00003636 7CC60000 7CC6C6C6 000000C6
  70D80000 CC7C0CD8 0078CCCC 00000000 66666CF8 6C6666F6 000000F8 FE006C38
  78786266 00FE6662 C6000000 6266FE00 66627878 000000FE FE001830 78786266
  00FE6662 00000000 663C0000 66F860F8 0000003C 3C00180C 18181818 003C1818
  663C0000 18183C00 18181818 0000003C 3C006600 18181818 003C1818 18180000
  18181818 0000F818 00000000 00000000 F8000000 C0C0C0C0 FFFFC0C0 FFFFFFFF
  FFFFFFFF FFFFFFFF 00000000 FF000000 FFFFFFFF 0000FFFF 18181818 18181800
  00000018 3C00180C 18181818 003C1818 FFFF0000 FFFFFFFF 000000FF 00000000
  38003018 C6C6C66C 00386CC6 00000000 C67C0000 C6C6FCC6 00C0C0FC 38006C38
  C6C6C66C 00386CC6 18300000 C66C3800 6CC6C6C6 00000038 DC760000 C6C67C00
  007CC6C6 DC760000 C66C3800 6CC6C6C6 00000038 00000000 66666666 C060607C
  00000000 7C6060F0 7C666666 00F06060 F0000000 66667C60 00F0607C 30180000
  C6C6C600 C6C6C6C6 0000007C C6006C38 C6C6C6C6 007CC6C6 18300000 C6C6C600
  C6C6C6C6 0000007C 30180000 C6C6C600 0C067EC6 180C00F8 66666600 1818183C
  0000003C 000000FF 00000000 00000000 18000000 00006030 00000000 00000000
  00000000 00FE0000 00000000 00000000 7E181800 00001818 000000FF 00000000
  00000000 00FF0000 600000FF 789C3690 9ECE6630 0006063E DB7F0000 1B7BDBDB
  001B1B1B 3C000000 3C306066 0C3C6666 003C6606 18000000 007E0018 00001818
  00000000 00000000 00000000 007C060C 6C6C3800 00000038 00000000 C6000000
  00000000 00000000 00000000 00000000 C0000000 00000000 30000000 30303070
  00000078 00000000 10D87000 00F09830 00000000 70000000 C86030D8 000000F8
  00000000 00000000 7C7C7C7C 00007C7C 00000000
End DefineFont
