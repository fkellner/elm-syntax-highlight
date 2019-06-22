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
      ]
  , include = []
  , default = [""]
  }

patLen : State -> Int
patLen (State {patterns}) = List.length patterns
