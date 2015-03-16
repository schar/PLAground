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
import Html.Lazy (..)


------------------------------------------------------------------------------
-- User Interface
------------------------------------------------------------------------------
-- Elm Model

type alias Model =
  { query : String
  , lfHist : List Formula
  , envBox : String
  , env : Env
  -- , lex : Lexicon
  , startBox : String
  , startStack : Stack
  , parseFail : Bool
  }

defModel : Model
defModel =
  { query = "Ex e(x)"
  , lfHist = [Exists (Var 'x') <| Pred 'e' (Var 'x')]
  , envBox = "\n"
  , env = emptyEnv
  -- , lex = defLex
  , startBox = "[ ]"
  , startStack = Ar.empty
  , parseFail = False
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
                     Err _ -> model.startStack
                     Ok inp -> inp
      in  {model | startStack <- newInp
                 , startBox <- s}

    CompileQuery ->
      let formula = parseForm model.query
      in  case formula of
            Err _ -> {model | parseFail <- True}
            Ok lf ->
              let c = List.foldl1 Conj model.lfHist
              in  case eval c model.env model.startStack of
                    Err msg -> {model | parseFail <- False}
                    Ok xxs -> {model | lfHist <- model.lfHist ++ [lf]
                                     , parseFail <- False}


-- View

view : Model -> Html
view model =
  div [ class "base-bg base-copy" ]
    [ div [ class "page-wrap" ]
      [ div [ class "column-main" ]
        [ lazy2 dispLFs model.parseFail model.lfHist
        , div [ class "sh" ]
          [ pre [ ] [ code [ ] [ lazy queryEntry model.query ] ] ]
        -- , lazy envEntry model.envBox
        -- , lazy dispFormula model.lfHist
        , lazy3 dispStacks model.lfHist model.env model.startStack
        , lazy inpEntry model.startBox
        -- , lazy dispOutputs model.outs
        ]
      ]
    ]
        
queryEntry : String -> Html
queryEntry query =
  input
    [ id "query"
    , placeholder "Enter expression"
    , autofocus True
    , value query
    , name "query"
    , on "input" targetValue (Sig.send updates << UpdateQuery)
    , onEnter (Sig.send updates CompileQuery)
    ]
    []

dispLFs : Bool -> List Formula -> Html
dispLFs fail lfs =
  if fail
     then div [ class "parse-msg" ] [ text "NOPE" ]
     else div [ class "lfs" ] <|
            List.map (\lf -> div [class "lf"] [text <| showFormula lf]) lfs

evals : List Formula -> Env -> Stack -> Result String (List (List Stack))
evals lfs env s =
  let cs = List.scanl1 Conj lfs
      xxs = List.map (\lf -> eval lf env s) cs
      seq m m' = m `andThen` \xs -> m' `andThen` \yys -> Ok (xs :: yys)
  in  List.foldr seq (Ok [[]]) xxs

dispStacks : List Formula -> Env -> Stack -> Html
dispStacks lfs env start =
  case evals lfs env start of
    Err _ -> text "This is impossible"
    Ok xxs -> 
      div [class "stack-hist"] <|
        List.intersperse (div [class "stack-list"] [text "->"]) <|
          List.map (\sl -> div [class "stack-list"] <| List.map dispStack sl)
            xxs

dispStack : Stack -> Html
dispStack s =
  div [ class "stack" ] <|
    List.map (text << toString) <| List.reverse <| Ar.toList s

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

dispFormula hist = 
  div [ ] <|
    List.map (text << showFormula) hist


-- Inputs

-- process the channel reactively (send current text contents through handler)
main : Signal Html
main = Sig.map view model

model : Signal Model
model = Sig.foldp update defModel (Sig.subscribe updates)

updates : Sig.Channel Action
updates = Sig.channel NoOp

