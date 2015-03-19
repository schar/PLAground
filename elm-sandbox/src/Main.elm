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
-- Application model
------------------------------------------------------------------------------

type alias Model =
  { query : String
  , lfHist : List Formula
  , lfHigh : Maybe Int
  , envBox : String
  , env : Env
  -- , lex : Lexicon
  , startBox : String
  , startStack : Stack
  , parseMsg : Maybe String
  }

defModel : Model
defModel =
  { query = "Ex e(x)"
  , lfHist = [Exists (Var 'x') <| Pred 'e' (Var 'x')]
  , lfHigh = Nothing
  , envBox = "\n"
  , env = emptyEnv
  -- , lex = defLex
  , startBox = ""
  , startStack = Ar.empty
  , parseMsg = Nothing
  }


------------------------------------------------------------------------------
-- Update events
------------------------------------------------------------------------------

type Action
  = NoOp
  | UpdateQuery String
  | EditEnv String
  | EditInput String
  | HighlightFormula Int
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

    HighlightFormula n ->
      if n < 0
         then {model | lfHigh <- Nothing}
         else {model | lfHigh <- Just <| List.length model.lfHist - n - 1}

    CompileQuery ->
      let formula = parseForm model.query
      in  case formula of
            Err _ -> {model | parseMsg <- Just "FAIL"}
            Ok lf ->
              let c = List.foldl1 (flip Conj) <| model.lfHist ++ [lf]
              in  case eval c model.env model.startStack of
                    Err msg -> {model | parseMsg <- Just msg}
                    Ok xxs -> {model | lfHist <- model.lfHist ++ [lf]
                                     , parseMsg <- Nothing}


------------------------------------------------------------------------------
-- Visualize the model
------------------------------------------------------------------------------

view : Model -> Html
view model =
  div [ class "base-bg base-copy" ]
    [ div [ class "page-wrap" ]
      [ div [ class "column-main" ]
        [ lazy3 dispLFs model.parseMsg model.lfHist model.lfHigh
        , div [ class "sh" ]
          [ pre [ ] [ code [ ] [ lazy queryEntry model.query ] ] ]
        , lazy3 dispStacks
            model.lfHist model.env (model.startStack, lazy inpEntry model.startBox)
        ]
      ]
    ]

dispLFs : Maybe String -> List Formula -> Maybe Int -> Html
dispLFs msg hist high =
  case msg of
    Just m -> div [ class "parse-msg" ] [ text m ]
    Nothing ->
      div [ class "lfs" ] <| List.reverse << fst <|
        flip (flip List.foldr ([], (1, 0))) hist <|
          \lf (lfs,(n,ind)) ->
            ( div
                [ class "lf"
                , if Just ind == high
                     then style [("color", "red"), ("font-weight", "700")]
                     else style [("opacity", toString n)]
                ]
                [text <| showFormula lf]
              :: lfs
            , (0.8 * n, ind + 1)
            )

queryEntry : String -> Html
queryEntry query =
  input
    [ id "query"
    , placeholder "Enter expression"
    , value query
    , autofocus True
    , on "input" targetValue (Sig.send updates << UpdateQuery)
    , onEnter (Sig.send updates CompileQuery)
    ]
    []

dispStacks : List Formula -> Env -> (Stack, Html) -> Html
dispStacks lfs env (start, startbox) =
  case evals lfs env start of
    Err _ -> text "This is impossible"
    Ok xxs -> 
      div [id "stack-hist"] <|
        startbox ::
      -- List.intersperse (div [ ] [text "->"]) <|
        (flip List.indexedMap xxs <|
          -- (List.reverse <| List.tail <| List.reverse xxs) <|
          \n sl ->
            div [ class "outputs"
                , onMouseOver (Sig.send updates (HighlightFormula n))
                , onMouseLeave (Sig.send updates (HighlightFormula -1)) ]
              <| case sl of
                   [] -> [text "False"]
                   _  ->
                     List.map (ul [class "stack-list"]) -- List Html
                     <| chunks 10 -- List (List Html)
                     <| List.map dispStack sl) -- List Html

dispStack : Stack -> Html
dispStack s =
  li [ class "stack" ] <|
    List.map (text << toString) <| Ar.toList s

inpEntry : String -> Html
inpEntry inp =
  div [class "inp"]
    [ input
        [ id "inp"
        , placeholder "s"
        , on "input" targetValue (Sig.send updates << EditInput)
        ]
        []
    ]


------------------------------------------------------------------------------
-- Fold update events over time
------------------------------------------------------------------------------

-- process the channel reactively (send current text contents through handler)
main : Signal Html
main = Sig.map view model

model : Signal Model
model = Sig.foldp update defModel (Sig.subscribe updates)

updates : Sig.Channel Action
updates = Sig.channel NoOp
