-- Basic Data
import List
import Array as Ar
import Result (..)

-- PLA
import Parser (parse)
import PLA (..)
import Utils (..)

-- Reactive Graphics
import Signal as Sig
import Window
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Html.Lazy (lazy, lazy2)


------------------------------------------------------------------------------
-- User Interface
------------------------------------------------------------------------------
-- Elm Model

type alias Model =
  { query : String
  , lf : Result String Formula
  , envBox : String
  , env : Env
  -- , lex : Lexicon
  , inpBox : String
  , inp : Stack
  , outs : Result String (List Stack)
  }

defModel : Model
defModel =
  { query = "Ex e(x)"
  , lf = Ok <| Exists (Var 'x') <| Pred 'e' (Var 'x')
  , envBox = "\n"
  , env = emptyEnv
  -- , lex = defLex
  , inpBox = "[]"
  , inp = Ar.empty
  , outs = Ok [Ar.fromList [2], Ar.fromList [4]]
  }


-- Update

type Action
  = NoOp
  | UpdateQuery String
  | EditEnv String
  | EditInput String
  | CompileQuery

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

    UpdateQuery s ->
      {model | query <- s}

    EditEnv s ->
      let newEnv = case parseEnv s of
                     Err _ -> model.env
                     Ok env -> env
      in  {model | env <- newEnv
                 , envBox <- s}

    EditInput s ->
      let newInp = case parseInp s of
                     Err _ -> model.inp
                     Ok inp -> inp
      in  {model | inp <- newInp
                 , inpBox <- s}

    CompileQuery ->
      let formula = parseForm model.query
          o = case formula of
                Err msg -> Err "parse"
                Ok lf -> eval lf model.env model.inp
      in  {model | lf <- formula, outs <- o}


-- View

view : Model -> Html
view model =
  div
    [ class "pla-wrapper" ]
    [ section
      [ id "pla-app" ]
      [ lazy queryEntry model.query
      , lazy envEntry model.envBox
      , lazy inpEntry model.inpBox
      , lazy dispFormula model.lf
      , lazy dispOutputs model.outs
      ]
    , infoFooter
    ]

queryEntry : String -> Html
queryEntry query =
  header 
    [ id "header" ]
    [ h1 [] [ text "query" ]
    , input
        [ id "query"
        , placeholder "Enter expression"
        , autofocus True
        , value query
        , name "query"
        , on "input" targetValue (Sig.send updates << UpdateQuery)
        , onEnter (Sig.send updates CompileQuery)
        ]
        []
    ]

envEntry : String -> Html
envEntry env =
  textarea
    [ id "env"
    , placeholder "Assign variables here"
    , value env
    , name "env"
    , on "input" targetValue (Sig.send updates << EditEnv)
    ]
    []

inpEntry : String -> Html
inpEntry inp =
  input
    [ id "inp"
    , placeholder "Input context"
    , value inp
    , name "inp"
    , on "input" targetValue (Sig.send updates << EditInput)
    ]
    []

dispFormula formula = 
  div [ ]
    [ case formula of
        Err msg -> text msg
        Ok  lf  -> text <| showFormula lf ]

dispOutputs outputs = 
  let toSpan xs = span [] [text <| toString xs]
  in  div [ ]
       (case outputs of
         Err msg -> if msg == "parse" then [text ""] else [text msg]
         Ok  xxs -> List.map (toSpan << Ar.toList) xxs)

infoFooter : Html
infoFooter =
  footer
    [ id "info" ]
    [ p [] [ text "This is my foota" ] ]


-- Inputs

-- process the channel reactively (send current text contents through handler)
main : Signal Html
main = Sig.map view model

model : Signal Model
model = Sig.foldp update defModel (Sig.subscribe updates)

updates : Sig.Channel Action
updates = Sig.channel NoOp

