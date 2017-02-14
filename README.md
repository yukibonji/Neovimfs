#Neovimfs

Neovimfs is that Fsi and intellisense works on Suave server.

<br>

![alt text](./pic/suave.png)

###Install
```
$ git clone https://github.com/callmekohei/Neovimfs
```

###Required

01. Suave
02. FSharp.Compiler.Service


###How to use
01. compile `neovimfs.fsx`
02. mono `neovimfs.exe` `/path/to/fsi.exe`
03. access port8080

###Terminate neovimfs.exe
```
// check suave server pid
$ lsof -i :8080

// terminate suave server
$ kill (pid)
```

<br>
<br>
<br>

---

#Noevimfs with Neovim

###Neovim configuration
init.vim
```vim
autocmd BufRead,BufNewFile *.fsx set filetype=fsharp
```

<br>

###QuickRun with Neovimfs

![alt text](./pic/quickrun2.png)



Required
- vim-quickrun
    - https://github.com/thinca/vim-quickrun

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

quickrun configuration
```vim
let g:quickrun_config = {
\     , 'fsharp': {
\           'exec'   :  [ '%c %s:p:r.fsx']
\         , 'command':  'fio'
\     }
\ }
```

<br>

###Intellisense completion with Neovimfs

![alt text](./pic/deoplete4.png)

Required

- deoplete.vim

    - https://github.com/Shougo/deoplete.nvim

- deoplete-fsharp

    - https://github.com/callmekohei/deoplete-fsharp

deoplete configuration
```vim
let g:deoplete#enable_at_startup = 1
let g:deoplete#max_list = 500
set previewheight=5
```

<br>

###Syntax highlight of FSharp

![alt text](./pic/syntax.png)

FSharp syntax highlight configuration
```vim
# create syntax folder
$ cd $XDG_CONFIG_HOME/nvim
$ mkdir syntax

# download fsharp.vim file
$ cd syntax
$ wget https://raw.githubusercontent.com/fsharp/vim-fsharp/master/syntax/fsharp.vim
```



<br>
<br>


###LICENCE
The MIT License (MIT)
