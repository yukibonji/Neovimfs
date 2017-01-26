#Neovimfs
Neovimfs is that Fsi works on Suave server.

###Install
```
$ git clone https://github.com/callmekohei/Neovimfs
```

###What library to need

01. Suave  
02. FSharp.Compiler.Service


###How to use
01. compile `neovim.fsx` file
02. mono neovim.exe
03. access port8080


###QuickRun with Neovim
see also: http://callmekohei00.hatenablog.com/entry/2016/12/13/154438

create `fio.bash`
```bash
urlencoded_str=$(curl -s -w '%{url_effective}\n' --data-urlencode $1 -G '')
urlencoded_str=${urlencoded_str:2}
curl -s 'localhost:8080/evalScript/'${urlencoded_str}
```

create `fio` command
```bash
$ cp fio.bash fio

$ chmod 777 fio

$ sudo mv fio /usr/local/bin
```

setting file of quickrun
```vim
\     , 'fsharp': {
\           'exec'   :  [ '%c %s:p:r.fsx']
\         , 'command':  'fio'
\     }
```

###LICENCE
The MIT License (MIT)
