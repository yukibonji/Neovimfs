import re
import urllib
from urllib.request import Request, urlopen
from deoplete.source.base import Base
from deoplete.util import getlines, expand

class Source(Base):

    def __init__(self, vim):
        super().__init__(vim)
        self.name          = 'callmekohei'
        self.mark          = '[kohei]'
        self.filetypes     = ['fsharp']
        self.input_pattern = r'(?:\b[^\W\d]\w*|[\]\)])(?:\.(?:[^\W\d]\w*)?)*\(?'


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
