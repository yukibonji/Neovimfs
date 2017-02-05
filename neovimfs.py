import re
import urllib
import subprocess
from urllib.request import Request, urlopen
from deoplete.source.base import Base
from deoplete.util import getlines, expand

def pid_suave():
    try:
        cmd    = "lsof -i :8080"
        output = subprocess.check_output( cmd.strip().split(' ') )
        output = output.split(b'\n')
        output = [s.decode('utf-8') for s in output]
        output = output[1].split(" ")
        return output[1]
    except Exception:
        return -1


def fsxfile():
    cmd    = "ps"
    output = subprocess.check_output(cmd.strip().split(' '))
    output = output.split(b'\n')
    output = [ s.decode('utf-8') for s in output ]
    output = filter( lambda x: re.findall(r".*fsx",x) ,output )
    return  len( list( output ) )


class Source(Base):

    def __init__(self, vim):
        super().__init__(vim)
        self.name          = 'callmekohei'
        self.mark          = '[kohei]'
        self.filetypes     = ['fsharp']
        self.input_pattern = r'(?:\b[^\W\d]\w*|[\]\)])(?:\.(?:[^\W\d]\w*)?)*\(?'

        # launch suave server
        # if pid_suave() == -1:
        #     cmd = "/usr/local/Cellar/mono/4.6.2.7/bin/mono /Users/kohei/Documents/00_myBin/suave/neovimfs.exe"
        #     subprocess.call( cmd.strip().split(" ") )



    # terminate suave server
    def __del__(self):
        if fsxfile() == 1:
            pid = pid_suave()
            subprocess.call( ["kill", pid] )


    def get_complete_position(self, context):
        m = re.search(r'\w*$', context['input'])
        return m.start() if m else -1


    def gather_candidates(self, context):

        row      = context['position'][1]
        col      = context['complete_position']
        line     = context['input']
        filepath = expand(self.vim.eval("expand('%:p')"))
        source   = '\n'.join(getlines(self.vim))

        info     = str(row) + ',' + str(col) + ',' + line + ',' + filepath + ',' + source
        infoEnc  = urllib.parse.quote_plus(info)

        url      = 'http://localhost:8080/autoComplete/'
        urlQuery = urllib.parse.urljoin( url, infoEnc )

        result   = urlopen(urlQuery).read().split(b'\n')
        return   [s.decode('utf-8') for s in result]
