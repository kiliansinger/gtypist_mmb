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
MODE 1
'MODE 2
Font 8
GoTo ll
l:
a= Asc(Inkey$)
If a<>0 Then Print a,Chr$(a)
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
  local txt$
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
   local txt$,txt2$
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
  local a$
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
'Open "mini.typ" For input As #1
Open "ttde.typ" For input As #1
'Open "gtypist.typ" For input As #1
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
  If Left$(a$,2)="G:" or Left$(a$,2)="Y:" or Left$(a$,2)="N:" Then
    
    labnum=findlabel%(trimr$(Mid$(a$,3)))

    If labnum=0 Then
      Print "label "Mid$(a$,3)" does not exists"
      Exit
    Else
      'lastlabel$(lastlabelcnt)=trimr$(mid$(a$,3))
      'lastlabelcnt=lastlabelcnt+1
      ''currentlabel$=trimr$(Mid$(a$,3))
      if Left$(a$,2)="G:" or (yes=1 and Left$(a$,2)="Y:") or (no=1 and Left$(a$,2)="N:") then
        Seek #1,labnum
      end if
    End If
    Continue do
  End If
  'errorrate:  E:<value>%  until the next E:...
  '            E:<value>%* for all following drills
   If Left$(a$,2)="E:" Then
     nexterrorrate=val(mid$(a$,3,instr(a$,"%")-3))
     if right$(a$,1)="*" then errorrate=nexterrorrate
     Continue do
   end if
   'fail label:  F:<label> 
  '              F:<label>* for all following drills
   If Left$(a$,2)="F:" Then
      
     if right$(a$,1)="*" then
       nextfaillabel$=mid$(a$,3,len(a$)-3)
       faillabel$=nextfaillabel$
     else
       nextfaillabel$=mid$(a$,3,len(a$)-2)
     end if
     Continue do
   end if
  'banner: B:...
  If Left$(a$,2)="B:" Then
    CLS
    'redrawpos=lastpos
    Color RGB(0,0,0),RGB(145, 196, 231)
    banner$=centertxt$(Mid$(a$,3))
    Print banner$
    Color RGB(255,255,255),RGB(0,0,0)
    Continue do
  End If
  'functionkey: K:fkeynumber:label  depracated=>ignored
  If Left$(a$,2)="K:" Then Continue do
  'Tutorial: T:...
  '           :...
  If Left$(a$,2)="T:" Then
    CLS
    Color RGB(0,0,0),RGB(145, 196, 231)

    Print banner$
    Color RGB(255,255,255),RGB(0,0,0)
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
    Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$(mid$(a$,3))
    Do
      Do
        pressed=Asc(Inkey$)
      Loop Until pressed<>0
      'return space    
      If pressed=asc("y") or pressed=asc("n") Then
          if pressed=asc("y") then
            yes=1
          else
            no=1
          end if
          exit do
          
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
    CLS
    Color RGB(0,0,0),RGB(145, 196, 231)

    Print banner$
    Color RGB(255,255,255),RGB(0,0,0)
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
    redrawpos=lastpos
  'problem B and I needs to be stored also redrawpos dos not work
  'as we have
  'B:..
  'I:...
  'S:...CLS but keep banner and intro
  'S:...CLS but keep banner and intro
    CLS
    Color RGB(0,0,0),RGB(145, 196, 231)
    Print banner$
    Color RGB(255,255,255),RGB(0,0,0)
    for i=0 to instructionslen-1
      Print instructions(i)
    next
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
   txtlen=txtlen+len(lines(linecnt))


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
        txtlen=txtlen+len(lines(linecnt))

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
          if pressed=10 or pressed=13) and (xpos<len(lines(linetype))) then
            if(xpos<len(lines(linetype))) then
              errorcnt=errorcnt+1
              If dospeed=0 Then
                Print @(MM.Info(hpos),MM.Info(vpos),2) "^";
              Else
                Print @(MM.Info(hpos),MM.Info(vpos),2) mid$(lines(linetype),xpos,1);
              End If
              xpos=xpos+1
            end if
          end if
          If (pressed=10 Or pressed=13) and (xpos>=len(lines(linetype))) Then
            If Mid$(lines(linetype),xpos,1)="" Then
                Print @(MM.Info(hpos),MM.Info(vpos),0) " ";
            End If
            linetype=linetype+1
            if linetype>=linecnt then
              print @(mm.hres/5*3,mm.vres-5*MM.Info(fontheight),2) "Raw speed      =  "format$(txtlen/5/(timer/60000),"%.2f")" wpm"
              'word is 5 char
              'to calculate adjusted wpm we subtract errors from written words
              'as each error counts as a wrong word
              adjusted=(txtlen-errorcnt)/5/(timer/60000)
              if adjusted<0 then adjusted=0
              print @(mm.hres/5*3,mm.vres-4*MM.Info(fontheight),2) "Adjusted speed =  "format$(adjusted,"%.2f")" wpm"
              print @(mm.hres/5*3,mm.vres-3*MM.Info(fontheight),2) "with "format$(errorcnt/txtlen*100,"%.1f")"% errors"
              
              if practice=0 then
                if errorcnt/txtlen*100>errorrate then
                   num$=format$(nexterrorrate,"%.1f")
                   ac$="Your error-rate is too high. You have to achieve "+num$+"%"
                   Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$(ac$)
                   Do
                     pressed=Asc(Inkey$)
                   Loop until pressed=27 or pressed=10 or pressed=13 or pressed=32
                  
            

                    'Your error-rate is too high. You have to achieve 3.0%.       
                    'wait for space enter exit
                    'incase nexterrorrate
                    'You failed this test, so you need to go back to LOOP2.
                    if nextfaillabel$<>"" then
                      ac$="You failed this test, so you need to go back to "+nextfaillabel$
                      Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$(ac$)
                      Do
                        pressed=Asc(Inkey$)
                      Loop until pressed=27 or pressed=10 or pressed=13 or pressed=32
                      esccnt=0
                      labnum=findlabel%(nextfaillabel$)

                      If labnum=0 Then
                        Print "label "Mid$(a$,3)" does not exists"
                        Seek #1,redrawpos
                      else
                        Seek #1,labnum
                      end if
                      Exit Do
                    else 
                      esccnt=0
                      Seek #1,redrawpos
                      Exit Do
                    end if
                  end if
              end if
       
              Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$("Press R to repeat, N for next exercise or E to exit")
              nexterrorrate=errorrate
              esccnt=2
            else
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
            end if
          End If
          If (pressed>=32 And pressed<=126) Or (pressed>=200 And pressed<=209) Then
            'german keyboard
            If pressed=200 Then
              pressed=176'circ
            Else If pressed=201 Then
              pressed=223'sz
            Else If pressed=202 Then
              pressed=180'accent aigu
            Else If pressed=203 Then
              pressed=252'ue
            Else If pressed=204 Then
              pressed=220'UE
            Else If pressed=205 Then
              pressed=246'oe
            Else If pressed=206 Then
              pressed=214'OE
            Else If pressed=207 Then
              pressed=228'ae
            Else If pressed=208 Then
              pressed=196'AE
            End If
            If Mid$(lines(linetype),xpos,1)=Chr$(pressed) Then
              Print @(MM.Info(hpos),MM.Info(vpos),0) Chr$(pressed);
            Else
              errorcnt=errorcnt+1
              If dospeed=0 Then
                Print @(MM.Info(hpos),MM.Info(vpos),2) "^";
              Else
                Print @(MM.Info(hpos),MM.Info(vpos),2) Chr$(pressed);
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
      Else If esccnt=2 Then
        If pressed=Asc("r") Then
          esccnt=0
          Seek #1,redrawpos
          Exit Do
        End If
        If pressed=Asc("n") Then
          esccnt=0
          Exit Do
        End If
        If pressed=Asc("e") Then
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
    CLS
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
          redrawpos=labnum
          Seek #1,labnum
          menupos=0
        End If
        Exit Do
      Else If pressed=27 Then
        menupos=0
        If uplabel$<>"" Then
          If uplabel$="_EXIT" Then End
          labnum=findlabel%(uplabel$)
          redrawpos=labnum
          Seek #1,labnum
        Else If lastlabelcnt>1 Then
          lastlabelcnt=lastlabelcnt-1
          'Print lastlabelcnt,lastlabel$(lastlabelcnt)
          'if lastlabel$(lastlabelcnt)="" then end
          labnum=lastlabel(lastlabelcnt)
          'currentlabel$=lastlabel$(lastlabelcnt)
          redrawpos=labnum
          Seek #1,labnum
          'exit do
        Else
          End
        End If
      End If
      Exit Do
    Loop
    Seek #1,redrawpos

    Continue do
  End If
  'labelname$(labelcnt)=trim$(mid$(a$,3),,R)

  Print @(0,MM.VRES-2*MM.Info(fontheight),2) centertxt$("NOTHANDLED: "+a$);
  Print @(0,MM.Info(fontheight),0) ""

  Rem print lof(#1)
  Input x$
Loop


' ibm2.bas
' Font type    : Full (223 characters)
' Font start   : CHR$(32)
' Font size    : 9x16 pixels
' Memory usage : 4018 bytes
DefineFont #8
  DF201009
  00000000 00000000 00000000 00000000 00000000 C1830706 183060E0 00030600
  00000000 8C193300 000040C2 00000000 00000000 00000000 C7860D00 6CD8B0F1
  800D1B7F 00000000 181F0C18 F00126CC 3143060C C080818F 00000000 260C0000
  30303030 C0903130 00000000 0D0E0000 D9C1C186 3366CCB8 0000C00E 18000000
  0003060C 00000000 00000000 00000000 03030000 C0800103 06183060 00008001
  00000000 C000030C 0C183060 00060606 00000000 00000000 F1300300 00333CFE
  00000000 00000000 00000000 18FC60C0 0000000C 00000000 00000000 00000000
  060C0000 00000303 00000000 00000000 00FC0100 00000000 00000000 00000000
  00000000 06000000 00000003 00000000 20000000 30303030 00103030 00000000
  0C0F0000 6D1B36CC 9961C3B6 00008087 00000000 80070706 183060C0 C00F060C
  00000000 181F0000 606060C0 31606060 0000C09F 00000000 60C0181F 060CF030
  808F3103 00000000 03030000 3163C383 03060CFC 0000C003 00000000 060C983F
  060CF003 808F3103 00000000 0C0E0000 F103060C 3163C68C 0000808F 00000000
  60C0983F 30303030 00060C18 00000000 181F0000 F13166CC 3163C68C 0000808F
  00000000 66CC181F 060CF831 000F0303 00000000 00000000 00C08001 060C0000
  00000000 00000000 80010000 000000C0 0006060C 00000000 00000000 C0C0C0C0
  030C30C0 0000C000 00000000 03000000 7E0000F0 00000000 00000000 0C000000
  30C00003 0C0C0C0C 0000000C 00000000 60CC181F 18306060 00030600 00000000
  0F000000 7933668C 306EDEBC 0000800F 00000000 C6060704 C6FC1933 C0983163
  00000000 0C3F0000 F03163C6 193366CC 0000809F 00000000 26CC0C0F C0800103
  80871961 00000000 0D3E0000 98316386 1B3366CC 0000001F 00000000 23C68C3F
  60D0E041 C09F1931 00000000 8C3F0000 E04123C6 183060D0 0000001E 00000000
  26CC0C0F C6BC0103 40871963 00000000 98310000 F93366CC 3163C68C 0000C098
  00000000 8001030F 183060C0 8007060C 00000000 81070000 3060C080 3366CC18
  0000000F 00000000 63C68C39 6CF0E061 C09C1933 00000000 0C3C0000 80010306
  193160C0 0000C09F 00000000 F7EFDC30 C3866DFB 60D8B061 00000000 9C310000
  79F367CF 3163C69C 0000C098 00000000 66CC181F C68C1933 808F3163 00000000
  0C3F0000 F03163C6 183060C0 0000001E 00000000 66CC181F C68C1933 808F376B
  000070C0 0C3F0000 F03163C6 193366D8 0000C09C 00000000 63CC181F 0618E000
  808F3163 00000000 DB3F0000 60C09069 060C1830 00008007 00000000 66CC9831
  C68C1933 808F3163 00000000 D8300000 0D1B366C 0F33C386 00000003 00000000
  366CD830 DBB60D1B C08C997F 00000000 D8300000 60E06166 30333C30 000060D8
  00000000 336CD830 1830F030 8007060C 00000000 D83F0000 60606068 B0606060
  0000E0DF 00000000 0103060F 3060C080 80070C18 00000000 10000000 C001070C
  01071C70 00004080 00000000 C080010F 0C183060 80070306 00000000 181B1C10
  000000C0 00000000 00000000 00000000 00000000 00000000 00000000 0000F807
  00030C30 00000000 00000000 00000000 00000000 03000000 CCF830C0 C00E3366
  00000000 0C380000 B0C10306 193366CC 0000808F 00000000 03000000 C08019E3
  808F3160 00000000 01070000 B1E1C180 3366CC98 0000C00E 00000000 03000000
  C0FC19E3 808F3160 00000000 0D0E0000 C0034386 183060C0 0000001E 00000000
  03000000 CC9831B3 800F3366 00E061C6 0C380000 D8610306 193366CC 0000C09C
  00000000 01000306 183060C0 8007060C 00000000 80010000 187000C0 0103060C
  3063C680 000000F0 03060C38 78F0B031 C09C1936 00000000 030E0000 60C08001
  060C1830 00008007 00000000 07000000 DBB6FD33 60DBB66D 00000000 00000000
  98E10600 193366CC 0000C08C 00000000 03000000 C68C19E3 808F3163 00000000
  00000000 98E10600 193366CC 0303868F 000000C0 03000000 CC9831B3 800F3366
  007860C0 00000000 D8E10600 183060CC 0000001E 00000000 03000000 38C018E3
  808F3106 00000000 06040000 C0E00703 0D183060 00008083 00000000 06000000
  CC983163 C00E3366 00000000 00000000 0D1B0600 0F33C386 00000003 00000000
  06000000 DB860D1B C0CCBF6D 00000000 00000000 98190600 191E1878 00006098
  00000000 06000000 C68C1933 C08F3163 00E06360 00000000 30F30700 31303030
  0000C09F 00000000 80018303 1830C0C1 C001060C 00000000 03060000 00C08001
  060C1830 00000003 00000000 8001031C 183038C0 000E060C 00000000 9B1D0000
  00000080 00000000 00000000 00000000 01010000 C68CB1C1 00803F63 00000000
  0C0F0000 010326CC 0F33C280 E0638001 6C000000 C3860D1B 0000B061 00000000
  00000000 06060600 19E30300 3160C0FC 0000808F 08000000 03800D0E CCF830C0
  C00E3366 00000000 1F000000 C00063CC 31303030 0000C09F 30000000 0300030C
  CCF830C0 C00E3366 00000000 071B1C00 30C00300 3366CCF8 0000C00E 00000000
  C3030000 66C08031 C300031E 000000C0 0D0E0800 19E30380 3160C0FC 0000808F
  00000000 03008031 C0FC19E3 808F3160 00000000 030C3000 19E30300 3160C0FC
  0000808F 00000000 01008019 183060C0 8007060C 00000000 0C0F0C00 60C001C0
  060C1830 00008007 30000000 0100030C 183060C0 8007060C 00000000 00000000
  01000000 000000FF 00000000 36380000 8303000E FE8C1963 C0983163 00000000
  00181818 8031E30F 193060F8 0000C09F 00000000 03000000 7E36EC70 E00E376C
  00000000 8D0F0000 F963C68C 3366CC98 0000C019 08000000 03800D0E C68C19E3
  808F3163 00000000 00000000 00C08001 060C00FC 00000000 30000000 0300030C
  C68C19E3 808F3163 00000000 191E1800 31630680 3366CC98 0000C00E 30000000
  0600030C CC983163 C00E3366 00000000 80310000 19330600 3163C68C 6160C08F
  000000E0 00000000 367F0000 C3860D1B 6CD8B061 00000000 01000000 FFFFFFFF
  FFFFFFFF 0C00FFFF 36CC0F06 C3800103 0003063F 00000000 0C1B1C00 80810786
  393060C0 0000809F 00000000 C0C3CC30 FF30FCC3 0003060C 00000000 8C197E00
  9811C3C7 193366DE 0000609E 07000000 8001C306 1830F8C1 0D03060C 00008083
  0C0C0C00 30C00300 3366CCF8 0000C00E 06000000 01000606 183060C0 8007060C
  00000000 0C0C0C00 19E30300 3163C68C 0000808F 0C000000 06000C0C CC983163
  C00E3366 00000000 9B1D0000 98E10680 193366CC 0000C08C 6E760000 67CE1800
  CEBCF9B3 C0983163 00000000 0D1B1E00 F801E083 00000000 00000000 1C000000
  86071833 0CF03063 00001E66 00000000 060C0000 C0800100 3163C0C0 0000808F
  00000000 00000000 C080F903 00003060 00000000 00000000 F8030000 0103060C
  00000080 60000000 664C1830 60606060 C0C02667 0000F8C0 18306000 6060664C
  25676660 3060C087 00000000 00000306 3C3060C0 00030F1E 00000000 00000000
  B1B10100 001B6CB0 00000000 00000000 06000000 6C6CB0C1 0000006C 00000000
  0D1B1C00 00008083 00000000 00000000 55550000 55455515 55545551 45551555
  54555155 06363800 E0430606 00000000 00000000 38000000 04030236 0000C0C1
  00000000 00000000 06060600 00000000 00000000 00000000 0C180000 87010306
  18F061C0 0103060C 3060C080 860D1B36 D9B061C3 0D1B36EC B061C386 00006CD8
  00000000 36FC0100 C3860D1B 6CD8B061 00000000 61C00700 060C18F0 C0800103
  1B363060 67C3860D 36EC19B0 C3860D1B 6CD8B061 860D1B36 D8B061C3 0D1B366C
  B061C386 00006CD8 07000000 36EC19F0 C3860D1B 6CD8B061 860D1B36 19B067C3
  000000FC 00000000 1B360000 61C3860D 00FCD9B0 00000000 00000000 03060C18
  61C08701 000000F0 00000000 00000000 00000000 18F00100 0103060C 3060C080
  03060C18 60C08001 0000003F 00000000 0C180000 80010306 00FF61C0 00000000
  00000000 00000000 01000000 060C18FF C0800103 0C183060 80010306 183F60C0
  0103060C 3060C080 02006300 19638303 3163FE8C 0000C098 0C180000 80010306
  18FF61C0 0103060C 3060C080 03060C18 60FC8001 060C183F C0800103 1B363060
  61C3860D 366FD8B0 C3860D1B 6CD8B061 860D1B36 C0BC61C3 0000007F 00000000
  00000000 01000000 366FC0FC C3860D1B 6CD8B061 860D1B36 01BC67C3 000000FF
  00000000 00000000 07000000 36EF01FC C3860D1B 6CD8B061 860D1B36 C0BC61C3
  0D1B366F B061C386 00006CD8 07000000 00FF01FC 00000000 00000000 860D1B36
  01BC67C3 0D1B36EF B061C386 0C186CD8 87010306 00FF01FC 00000000 00000000
  860D1B36 D9B061C3 000000FF 00000000 00000000 07000000 18FF01FC 0103060C
  3060C080 00000000 01000000 0D1B36FF B061C386 1B366CD8 61C3860D 007FD8B0
  00000000 00000000 03060C18 60FC8001 0000003F 00000000 00000000 00000000
  183F60FC 0103060C 3060C080 0F006300 1933668C 3163C68C 0000808F 1B360000
  61C3860D 36FFD9B0 C3860D1B 6CD8B061 03060C18 61FC8701 060C18FF C0800103
  0C183060 80010306 00F061C0 00000000 00000000 00000000 00000000 060C183F
  C0800103 FFFF3060 FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF 18006300 193366CC
  3163C68C 0000808F 78F00000 070F1E3C F0E0C183 0F1E3C78 E0C18307 E1C3870F
  3E7CF8F0 C3870F1F 7CF8F0E1 00001F3E C68C191E C6986163 80993163 00000000
  00000000 71B30300 376CD8B0 0000C00E FFFF0000 FFFFFFFF 0000FEFF 00000000
  00000000 983F0000 010366CC 3060C080 00000018 00000000 E30F0000 6CD8B061
  800D1B36 00000000 00330000 30C00300 3366CCF8 0000C00E 00000000 03000000
  D8B061F3 000E366C 00000000 00000000 98316306 183E66CC 00000C0C 00000000
  66070000 183060E0 0003060C 00000000 0F000000 98E181C1 061E66CC 0000C00F
  00000000 C6060700 C6FC1933 00071B63 00000000 0D0E0000 1833668C 1B366CD8
  0000C01D 00000000 80018607 66CCF860 80871933 00000000 00000000 6DF30300
  003FDBB6 00000000 00000000 63600000 F3B66DF3 0018183F 00000000 06070000
  F0010306 0C3060C0 00008003 00000000 668C0F00 C68C1933 C0983163 00000000
  00000000 0100E00F 3F0000FC 00000080 00000000 80010000 1830F8C1 E01F0000
  00000000 06000000 18608001 00181818 0000C00F 00000000 81810100 18608081
  C00F0006 00000000 83030000 60C0B061 060C1830 C0800103 0C183060 80010306
  D83060C0 000E366C 00000000 80310000 19E30300 3163C68C 0000808F 00000000
  03000000 760070B3 0000006E 00000000 48042211 44201281 04221188 20128148
  00008844 00000000 18300000 00000000 00000000 00000000 00000000 00001800
  00000000 07000000 C0800183 6CD83160 80030F36 00000000 00330000 31630600
  3366CC98 0000C00E 3BDD0000 D3ED4EB7 DDEE74BB ED4EB73B EE74BBD3 03060C18
  60C08001 060C1830 C0800103 00003060
End DefineFont
