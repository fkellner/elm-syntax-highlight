module SyntaxHighlight.Language.Haskell exposing (..)

{- State machine for language parsing -}
{- type Rules = List State -}


type State = State
  { patterns: List Pattern
  , include:  List State
  , default:  Token
  }

type alias Token = List String

type Pattern
  = CircularPattern
    { token: Token
    , regex: Regex
    }
  | NextPattern
    { token: Token
    , regex: Regex
    , next:  State
    }
  | PushPattern
    { token: Token
    , regex: Regex
    , push: AnonymousState
    }

type alias AnonymousState =
  { patterns: List Pattern
  , include: List State
  , default: Token
  , endToken: Token
  , endRegex: Regex
  }

{- type alias CircularPattern = {
    token: Token,
    regex: Regex
  }

type alias NextPattern = {
    token: Token,
    regex: Regex,
    next: State
  } -}

type alias Regex = String

{- Has to be filled in for every language -}
module_name : State
module_name = State
  { patterns = []
  , include = []
  , default = [""]
  }
module_exports : State
module_exports = State
  { patterns = []
  , include = []
  , default = [""]
  }

type_signature : State
type_signature = State
  { patterns = []
  , include = []
  , default = [""]
  }

pragma : State
pragma = State
  { patterns = []
  , include = []
  , default = [""]
  }

comments : State
comments = State
  { patterns = []
  , include = []
  , default = [""]
  }

infix_op : State
infix_op = State
  { patterns = []
  , include = []
  , default = [""]
  }

start : State
start = State
  { patterns =
      [ CircularPattern
          { token =
              [ "punctuation.definition.entity.haskell"
              , "keyword.operator.function.infix.haskell"
              , "punctuation.definition.entity.haskell"
              ]
          , regex = "(`)([a-zA-Z_']*?)(`)"
            {- comment: "In case this regex seems unusual for an infix operator, note that Haskell allows any ordinary function application (elem 4 [1..10]) to be rewritten as an infix expression (4 `elem` [1..10])."-}
          }
      , CircularPattern
          { token = ["constant.language.unit.haskell"]
          , regex = "\\(\\)"
          }
      , CircularPattern
          { token = ["constant.language.empty-list.haskell"]
          , regex = "\\[\\]"
          }
      , PushPattern
          { token = ["keyword.other.haskell"]
          , regex = "\\b(module|signature)\\b"
          , push =
              { patterns =
                  [ CircularPattern {
                      token = ["invalid"],
                      regex = "[a-z]+"
                    }
                  ]
              , include =
                  [ module_name
                  , module_exports
                  ]
              , default = ["meta.declaration.module.haskell"]
              , endToken = ["keyword.other.haskell"]
              , endRegex = "\\bwhere\\b"
              }
          }
      , PushPattern
          { token = ["keyword.other.haskell"]
          , regex = "\\bclass\\b"
          , push =
              { patterns =
                  [ CircularPattern
                      { token = ["support.class.prelude.haskell"]
                      , regex = "\\b(?:Monad|Functor|Eq|Ord|Read|Show|Num|(?:Frac|Ra)tional|Enum|Bounded|Real(?:Frac|Float)?|Integral|Floating)\\b"
                      }
                  , CircularPattern
                      { token = ["entity.other.inherited-class.haskell"]
                      , regex = "[A-Z][A-Za-z_']*"
                      }
                  , CircularPattern
                      { token = ["variable.other.generic-type.haskell"]
                      , regex = "\\b[a-z][a-zA-Z0-9_']*\\b"
                      }
                  ]
              , default = ["meta.declaration.class.haskell"]
              , endToken = ["keyword.other.haskell"]
              , endRegex = "\\bwhere\\b"
              , include = []
              }
          }
      , PushPattern
          { token = ["keyword.other.haskell"]
          , regex = "\\binstance\\b"
          , push =
              { patterns = []
              , default = ["meta.declaration.instance.haskell"]
              , endToken = ["keyword.other.haskell"]
              , endRegex = "\\bwhere\\b|$"
              , include = [type_signature]
              }
          }
      , PushPattern
          { token = ["keyword.other.haskell"]
          , regex = "import"
          , push =
            { patterns =
              [ CircularPattern
                { token = ["keyword.other.haskell"]
                , regex = "qualified|as|hiding"
                }
              ]
            , default = ["meta.import.haskell"]
            , endToken = ["meta.import.haskell"]
            , endRegex = "$|;|^"
            , include =
              [ module_name
              , module_exports
              ]
            }
          }
      , PushPattern
          { token = [ "keyword.other.haskell", "meta.deriving.haskell" ]
          , regex = "(deriving)(\\s*\\()"
          , push =
            { patterns =
              [ CircularPattern
                  { token = ["entity.other.inherited-class.haskell"]
                  , regex = "\\b[A-Z][a-zA-Z_']*"
                  }
              ]
            , default = ["meta.deriving.haskell"]
            , endToken = ["meta.deriving.haskell"]
            , endRegex = "\\)"
            , include = []
            }
          }
      , CircularPattern
          { token = ["keyword.other.haskell"]
          , regex = "\\b(?:deriving|where|data|type|case|of|let|in|newtype|default)\\b"
          }
      , CircularPattern
          { token = ["keyword.operator.haskell"]
          , regex = "\\binfix[lr]?\\b"
          }
      , CircularPattern
          { token = ["keyword.control.haskell"]
          , regex = "\\b(?:do|if|then|else)\\b"
          }
      , CircularPattern
          { token = ["constant.numeric.float.haskell"]
          , regex = "\\b(?:[0-9]+\\.[0-9]+(?:[eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+)\\b"
            {- comment = "Floats are always decimal"-}
          }
      , CircularPattern
          { token = ["constant.numeric.haskell"]
          ,  regex = "\\b(?:[0-9]+|0(?:[xX][0-9a-fA-F]+|[oO][0-7]+))\\b"
          }
      , CircularPattern
          { token =
             [ "meta.preprocessor.c"
             , "punctuation.definition.preprocessor.c"
             , "meta.preprocessor.c"
             ]
          , regex = "^(\\s*)(#)(\\s*\\w+)"
          {-  comment = "In addition to Haskell's \"native\" syntax, GHC permits the C preprocessor to be run on a source file."-}
          }
      , PushPattern
          { token = ["punctuation.definition.string.begin.haskell"],
            regex = "\"",
            push =
              { patterns =
                  [ CircularPattern
                      { token = ["constant.character.escape.haskell"]
                      , regex = "\\\\(?:NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE|DC1|DC2|DC3|DC4|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS|US|SP|DEL|[abfnrtv\\\\\\\"'\\&])"
                      }
                  , CircularPattern
                      { token = ["constant.character.escape.octal.haskell"]
                      , regex = "\\\\o[0-7]+|\\\\x[0-9A-Fa-f]+|\\\\[0-9]+"
                      }
                  , CircularPattern
                      { token = ["constant.character.escape.control.haskell"]
                      , regex = "\\^[A-Z@\\[\\]\\\\\\^_]"
                      }
                  ]
              , default = ["string.quoted.double.haskell"]
              , endToken = ["punctuation.definition.string.end.haskell"]
              , endRegex = "\""
              , include = []
              }
          }
      , CircularPattern
          { token =
             [ "punctuation.definition.string.begin.haskell"
             , "string.quoted.single.haskell"
             , "constant.character.escape.haskell"
             , "constant.character.escape.octal.haskell"
             , "constant.character.escape.hexadecimal.haskell"
             , "constant.character.escape.control.haskell"
             , "punctuation.definition.string.end.haskell"
             ]
          , regex = "(')(?:([\\ -\\[\\]-~])|(\\\\(?:NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE|DC1|DC2|DC3|DC4|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS|US|SP|DEL|[abfnrtv\\\\\\\"'\\&]))|(\\\\o[0-7]+)|(\\\\x[0-9A-Fa-f]+)|(\\^[A-Z@\\[\\]\\\\\\^_]))(')"
          }
      , PushPattern
          { token =
             [ "meta.function.type-declaration.haskell"
             , "entity.name.function.haskell"
             , "meta.function.type-declaration.haskell"
             , "keyword.other.double-colon.haskell"
             ]
          , regex = "^(\\s*)([a-z_][a-zA-Z0-9_']*|\\([|!%$+\\-.,=</>]+\\))(\\s*)(::)"
          , push =
              { patterns = []
              , default = ["meta.function.type-declaration.haskell"]
              , endToken = ["meta.function.type-declaration.haskell"]
              , endRegex = "$"
              , include = [type_signature]
              }
          }
      , CircularPattern
          { token = ["support.constant.haskell"]
          , regex = "\\b(?:Just|Nothing|Left|Right|True|False|LT|EQ|GT|\\(\\)|\\[\\])\\b" }
      , CircularPattern
          { token = ["constant.other.haskell"]
          , regex = "\\b[A-Z]\\w*\\b"
          }
      , CircularPattern
          { token = ["support.function.prelude.haskell"]
          , regex = "\\b(?:abs|acos|acosh|all|and|any|appendFile|applyM|asTypeOf|asin|asinh|atan|atan2|atanh|break|catch|ceiling|compare|concat|concatMap|const|cos|cosh|curry|cycle|decodeFloat|div|divMod|drop|dropWhile|elem|encodeFloat|enumFrom|enumFromThen|enumFromThenTo|enumFromTo|error|even|exp|exponent|fail|filter|flip|floatDigits|floatRadix|floatRange|floor|fmap|foldl|foldl1|foldr|foldr1|fromEnum|fromInteger|fromIntegral|fromRational|fst|gcd|getChar|getContents|getLine|head|id|init|interact|ioError|isDenormalized|isIEEE|isInfinite|isNaN|isNegativeZero|iterate|last|lcm|length|lex|lines|log|logBase|lookup|map|mapM|mapM_|max|maxBound|maximum|maybe|min|minBound|minimum|mod|negate|not|notElem|null|odd|or|otherwise|pi|pred|print|product|properFraction|putChar|putStr|putStrLn|quot|quotRem|read|readFile|readIO|readList|readLn|readParen|reads|readsPrec|realToFrac|recip|rem|repeat|replicate|return|reverse|round|scaleFloat|scanl|scanl1|scanr|scanr1|seq|sequence|sequence_|show|showChar|showList|showParen|showString|shows|showsPrec|significand|signum|sin|sinh|snd|span|splitAt|sqrt|subtract|succ|sum|tail|take|takeWhile|tan|tanh|toEnum|toInteger|toRational|truncate|uncurry|undefined|unlines|until|unwords|unzip|unzip3|userError|words|writeFile|zip|zip3|zipWith|zipWith3)\\b"
          }
      , CircularPattern
          { token = ["keyword.operator.haskell"]
          , regex = "[|!%$?~+:\\-.=</>\\\\]+"
              {- comment = "In case this regex seems overly general, note that Haskell permits the definition of new operators which can be nearly any string of punctuation characters, such as $%^&*."-}
          }
      , CircularPattern
          { token = ["punctuation.separator.comma.haskell"]
          , regex = ","
          }
      ]
  , include =
      [ pragma
      , comments
      , infix_op
      ]
  , default = [""]
  }

patLen : State -> Int
patLen (State {patterns}) = List.length patterns
