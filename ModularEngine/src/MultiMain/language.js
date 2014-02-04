{
  displayName:    'DKAL',
  name:           'dkal',
  mimeTypes:      ['text/x-mdkal'],
  fileExtensions: ['.mdkal'],
  
  lineComments:      '//',   
  blockCommentStart: '/*',
  blockCommentEnd:   '*/',

  keywords: [
    'type', 'relation', 'datasource', 'namespaces', 'with', 'forall', 'said', 'me', 'knows',
    'upon', 'if', 'do', 'send', 'say', 'justification', 'to', 'learn', 'forget', 'asInfon', 'let', 'fresh'
  ],

  typeKeywords: [],

  operators: [
    '=', '>', '<',
    '==', '<=', '>=', '!=', '&&', '||', 
    '+', '-', '*', '/', '%'
  ],

  
  symbols:  '[=><!~?:&|+\\-*\\/\\^%]+',
  escapes:  '\\\\(?:[abfnrtv\\\\""\']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})',
    
  
  tokenizer: {
    root: [
      
      ['[a-z_$][\\w$]*', { cases: { '@typeKeywords': 'keyword',
                                   '@keywords': 'keyword',
                                   '@default': 'identifier' } }],
      ['[A-Z][\\w\\$]*', 'type.identifier' ],  
      
      
      { include: '@whitespace' },
      
      
      ['[{}()\\[\\]]', '@brackets'],
      ['[<>](?!@symbols)', '@brackets'],
      ['@symbols', { cases: { '@operators': 'operator', 
                              '@default'  : '' } } ],

      
      ['\\d*\\.\\d+([eE][\\-+]?\\d+)?[fFdD]?', 'number.float'],
      ['0[xX][0-9a-fA-F_]*[0-9a-fA-F][Ll]?', 'number.hex'],
      ['0[0-7_]*[0-7][Ll]?', 'number.octal'],
      ['0[bB][0-1_]*[0-1][Ll]?', 'number.binary'],
      ['\\d+[lL]?', 'number'],

      
      ['[;,.]', 'delimiter'],
      
      
      ['""',  'string', '@string' ],
      
      
      ['\'[^\\\\\']\'', 'string'],
      ['(\')(@escapes)(\')', ['string','string.escape','string']],
      ['\'', 'string.invalid']
    ],

    whitespace: [
      ['[ \\t\\r\\n]+', 'white'],
      ['\\/\\*',       'comment', '@comment' ],
      ['\\/\\/.*$',    'comment'],
    ],

    comment: [
      ['[^\\/*]+', 'comment' ],
      
      ['\\/\\*',    'comment.invalid' ],
      [""\\*/"",    'comment', '@pop'  ],
      ['[\\/*]',   'comment' ]
    ],  

    string: [
      ['[^\\\\""]+',  'string'],
      ['@escapes', 'string.escape'],
      ['\\\\.',      'string.escape.invalid'],
      ['""',        'string', '@pop' ]
    ],
  },
}