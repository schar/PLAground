-- Basic Data
import List
import String
import Array as Ar
import Result (..)

-- PLA
import PLA (..)
import Utils (..)
import Instructions

-- Reactive Graphics
import Signal as Sig
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Html.Lazy (..)


------------------------------------------------------------------------------
-- Application model
------------------------------------------------------------------------------

type alias Model =
  { query : String
  , lfHist : List LF
  , envBox : String
  , env : Env
  , domBox : String
  , domSize: Int
  -- , lex : Lexicon
  , startBox : String
  , startStack : Stack
  , parseMsg : Bool
  , refMsg : Maybe String
  , instructions : Maybe Html
  }

type alias LF =
  { formula : Formula
  , active : Bool
  , highlight : Bool
  }

defLF : Formula -> LF
defLF form =
  { formula = form
  , active = True
  , highlight = False
  }

defModel : Model
defModel =
  { query = ""
  , lfHist = []
  , envBox = "\n"
  , env = emptyEnv
  , domBox = ""
  , domSize = 4
  -- , lex = defLex
  , startBox = ""
  , startStack = Ar.empty
  , parseMsg = False
  , refMsg = Nothing
  , instructions = Nothing
  }


------------------------------------------------------------------------------
-- Update events
------------------------------------------------------------------------------

type Action
  = NoOp
  | UpdateQuery String
  | EditEnv String
  | EditInput String
  | EditDomain String
  | AccentFormula Int Bool
  | CompileQuery
  | ToggleFormula Int
  | ToggleInstr

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

    EditDomain s ->
      let newDom = case String.toInt s of
                     Err _ -> model.domSize
                     Ok n -> if n > 0 then n else model.domSize
      in  {model | domSize <- newDom
                 , domBox <- s}       

    AccentFormula ind isInFocus ->
      let newlfs =
        fst <|
        flip2 List.foldl ([], 0) model.lfHist <|
        \lf (lfs, n) -> 
          if lf.active
             then ( lfs ++
                    [if n == ind then {lf | highlight <- isInFocus} else lf]
                  , n + 1
                  )
             else ( lfs ++ [lf], n )
      in  {model | lfHist <- newlfs}

    ToggleFormula ind ->
      let updateLFs n lf = if n == ind
                              then {lf | active <- not lf.active}
                              else lf
          newm = {model | lfHist <- List.indexedMap updateLFs model.lfHist}
          c = List.foldl (flip Conj) (Pred 'e' (Con 2)) <|
              List.map .formula <| List.filter .active newm.lfHist
      in  case eval c newm.env newm.domSize newm.startStack of
            Err msg -> {newm | refMsg <- Just msg}
            Ok _ -> {newm | refMsg <- Nothing}

    ToggleInstr ->
      case model.instructions of
        Nothing -> {model | instructions <- Just Instructions.instructions}
        Just _ -> {model | instructions <- Nothing}

    CompileQuery ->
      let formula = parseForm model.query
      in  case formula of
            Err _ -> {model | parseMsg <- True
                            , refMsg <- Nothing}
            Ok lf ->
              let c = List.foldl1 (flip Conj) <|
                      (List.map .formula <| List.filter .active model.lfHist)
                      ++ [lf]
              in  case eval c model.env model.domSize model.startStack of
                    Err msg -> {model | parseMsg <- False
                                      , refMsg <- Just msg}
                    Ok xxs -> {model | lfHist <- model.lfHist ++ [defLF lf]
                                     , parseMsg <- False
                                     , refMsg <- Nothing}


------------------------------------------------------------------------------
-- Visualize the model
------------------------------------------------------------------------------

view : Model -> Html
view model =
  div [ class "page-wrap" ]
    [ div [ class "column-main" ]
        [ lazy dispInstr model.instructions
        , lazy2 dispLFs model.parseMsg model.lfHist
        , div [ class "sh" ]
            [ lazy queryEntry model.query
            , inpEntry 
            , domEntry 
            ]
        , lazy3 dispStacks
            (model.refMsg, model.lfHist)
            model.domSize
            (model.env, model.startStack)
        , infoFooter
        ]
    ]
    

dispInstr : Maybe Html -> Html
dispInstr instr =
  case instr of
    Nothing -> div [ ] [ ]
    Just instructions ->
      div [ class "instructions" ]
        [ div [ class "instructions-close" ]
            [ a [ href "#"
                , onClick (Sig.send updates ToggleInstr)
                ]
                [ text "Close" ]
            ]
        , div [ class "instructions-content" ]
            [ instructions ]
        ]

dispLFs : Bool -> List LF -> Html
dispLFs msg hist =
  if msg
     then div [ class "parse-msg" ] [ text "*" ]
     else 
       div [ class "lfs" ] 
         <| fst
         <| flip2 List.foldl
            ([], (0.8 ^ (toFloat <| List.length hist - 1), 0))
            hist
         <| \lf (lfdivs, (n,ind)) ->
              ( div [ classList
                        [ ("lf", True)
                        , ("lf-inactive", not lf.active)
                        , ("lf-highlight", lf.highlight)
                        ]
                    , style [ ( "opacity"
                              , toString (if lf.active then n else 1)
                              ) ]
                    ]
                  [ input
                      [ class "lf-toggle"
                      , type' "checkbox"
                      , checked (not lf.active)
                      , onClick (Sig.send updates <| ToggleFormula ind)
                      ]
                      [ ]
                  , text <| showFormula lf.formula
                  ]
                :: lfdivs
              , (1.25 * n, ind + 1)
              )

queryEntry : String -> Html
queryEntry query =
  div [ class "query" ]
  [ input
      [ id "query"
      , placeholder "Enter expression, e.g. Ex e(x)"
      , value query
      , autofocus True
      , on "input" targetValue (Sig.send updates << UpdateQuery)
      , onEnter (Sig.send updates CompileQuery)
      ]
      []
  ]

dispStacks : (Maybe String, List LF) -> Int -> (Env, Stack) -> Html
dispStacks (msg, lfs) dom (e, start) =
  case msg of
    Just m -> div [ class "ref-msg" ] [ text m ]
    Nothing ->
      case List.filter .active lfs of
        [] -> div [id "stack-hist"] [ ]
        _  ->
          case evals (List.map .formula <| List.filter .active lfs)
               e dom start of
            Err m -> div [ class "ref-msg" ] [ text m ]
            Ok xxs -> 
              div [id "stack-hist"] <|
                flip List.indexedMap xxs <|
                \n sl ->
                  div
                    [ class "outputs"
                    , onMouseOver (Sig.send updates <| AccentFormula n True)
                    , onMouseLeave (Sig.send updates <| AccentFormula n False)
                    ] <|
                    case sl of
                      [] -> [text "False"]
                      _  ->
                        List.map (ul [class "stack-list"]) -- List Html
                        <| chunks 10 -- List (List Html)
                        <| List.map dispStack sl -- List Html

dispStack : Stack -> Html
dispStack s =
  li [ class "stack" ] <|
    List.map (text << toString) <| Ar.toList s

inpEntry : Html
inpEntry =
  div [class "inp"]
    [ input
        [ id "inp"
        , placeholder "input"
        , on "input" targetValue (Sig.send updates << EditInput)
        ]
        []
    ]

domEntry : Html
domEntry =
  div [class "dom"]
    [ input
        [ id "dom"
        , placeholder "4"
        , on "input" targetValue (Sig.send updates << EditDomain)
        ]
        []
    ]

infoFooter : Html
infoFooter =
  footer [ id "instr-footer" ]
    [ a [ href "#"
        , onClick (Sig.send updates ToggleInstr)
        ]
        [ text "Confused?" ]
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
