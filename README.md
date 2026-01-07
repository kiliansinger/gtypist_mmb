# gtypist port to mmbasic
Copyright (C)  Kilian Singer

Gtypist port to mmbasic

[Gtypist](https://www.gnu.org/software/gtypist/gtypist.html) is a touch type trainer program. This is a port of the program to mmbasic that runs on the maximite, picomite basic computers.

It includes german font and keyboard settings and defaults to load ttde.typ.
Change the code to load other courses such as the english gtypist.typ file.

using codepage858 encoding.
you can convert gtypist files with:
```bash
#change from utf to cp858 with: 
iconv -f utf8 -t cp858 ttde.typ > ttde_cp850b.typ
#change input file ansi(windows) to cp858:  
iconv -f WINDOWS-1252 -t CP858 ttde.typ > ttde_cp858typ
#change all files with:
for f in *.typ; do echo "${f}";iconv -f utf8 -t cp858 "${f}" > "$(basename ${f} .typ)"_cp858.typ ;done
```

This code is licenses under GPL2 see [LICENSE](LICENSE)
