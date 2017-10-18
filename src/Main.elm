port module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import FileReader exposing (..)
import Json.Decode as Json exposing (Decoder, Value)
import Debug exposing (..)
import Native.XML
import Dict exposing (Dict)
import Regex exposing (Regex,regex)
import Bootstrap.Grid as Grid
import Bootstrap.Alert as Alert
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Support exposing (..)
import List.Extra as List
import Array exposing (Array)
import Time exposing (..)
import Process exposing (..)
import Keyboard exposing (..)
import Char exposing (..)
import Bootstrap.Form as F
import Bootstrap.Form.Input as F

-- Main

main = Html.program
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions
       }

-- Model

type alias Model =
    { selectedFile : Maybe NativeFile
    , message : Maybe String
    , rawContent : Maybe FileContentArrayBuffer
    , sha1 : Maybe String
    , zipContent : Maybe Zip
    , book : Maybe Book
    , position : Maybe BookPosition
    , stored : Maybe Int
    , playing : Bool
    , inContext : Int -- contexts are quotes, bracketed expressions, etc.
    -- These three are approximate! Only used for some basic stats.
    , lastWordPresentTime : Maybe Time
    , averageWordPresentTime : Time
    , numberOfWordsPresented : Int
    , settings : Settings
    }

type alias Settings =
    { wpm : Float
    , commonMultiplier : Float
    , delayStart : Float
    , baseFontScale : Float
    , commaMultiplier : Float
    , bracketMultiplier : Float
    , numberMultiplier : Float
    , stopQuoteMultiplier : Float
    , paragraphMultiplier : Float
    , unitMultiplier : Float
    , capitalizedMultiplier : Float
    , allcapsMultiplier : Float
    , imageDelayMultiplier : Float
    , lengthExponent : Float
    , lengthDivisor : Float
    , lengthBound : Float
    , imageStop : Bool
    , numberOfItemsToShow : Int
    }

init : (Model, Cmd Msg)
init = { selectedFile = Nothing
       , message = Nothing
       , rawContent = Nothing
       , sha1 = Nothing
       , zipContent = Nothing
       , book = Nothing
       , position = Nothing
       , stored = Nothing
       , playing = False
       , inContext = 0
       , lastWordPresentTime = Nothing
       , averageWordPresentTime = 0
       , numberOfWordsPresented = 0
       , settings = { wpm = 400
                    , commonMultiplier = 0.75
                    , delayStart = 500
                    , baseFontScale = 2
                    , commaMultiplier = 1.5
                    , bracketMultiplier = 2.5
                    , numberMultiplier = 4
                    , stopQuoteMultiplier = 3
                    , paragraphMultiplier = 3
                    , unitMultiplier = 1
                    , capitalizedMultiplier = 2
                    , allcapsMultiplier = 2
                    , imageDelayMultiplier = 1
                    , imageStop = True
                    , lengthExponent = 1.2
                    , lengthDivisor = 20
                    , lengthBound = 8
                    , numberOfItemsToShow = 1
                    } } ! [ send LoadData ]

type alias Book = { zip : Zip
                  , fileIndex : Dict String ZipEntry
                  , rootFiles : List String
                  , rootPrefix : String
                  , manifest : Manifest
                  , spine : Array String
                  , title : String }

type Item = ItemString String
          | ItemEndParagraph
          | ItemImage String

type alias Manifest = Dict String { filename : String
                                  , mimetype : String }

-- This is an offset into a string
type alias StringIterator = List Char

-- This is a DOM Node. nextSibling can be used to iterate horizontally, parent
-- can be used to pop up, and firstChild can be used to move down.
type alias DOMIterator = { nextNode : Node, nodeEndsParagraph : Bool }

mapMaybe f l = case l of
                   [] -> []
                   (x::l) -> case f x of
                                 Nothing -> mapMaybe f l
                                 Just v -> v :: mapMaybe f l

type alias BookPosition = { spineOffset : Int, items : Array Item, offset : Int }

wpmToDelay : Float -> Float
wpmToDelay wpm = 60000.0 / wpm

isCapitalized : String -> Bool
isCapitalized s = case List.head (String.toList s) of
                      Just x -> isUpper x
                      _ -> False

isAllCaps : String -> Bool
isAllCaps s = case s of
                  "" -> False
                  _ -> List.all isUpper (String.toList s)

isNumberlike : String -> Bool
isNumberlike s = Regex.contains (regex "[1-9]") s

-- Returns nothing if we should stop
itemDelayOrStop : Settings -> Item -> Maybe Float
itemDelayOrStop settings item =
    let f v = wpmToDelay settings.wpm * v
    in case item of
        ItemEndParagraph -> Just (f settings.paragraphMultiplier)
        ItemImage s ->  case settings.imageStop of
                            True -> Nothing
                            False -> Just (f settings.imageDelayMultiplier)
        ItemString s ->
            Just ((let l = String.toList s
                   in if List.all isUpper l then
                          settings.allcapsMultiplier else
                          case List.head l of
                              Just x -> if isUpper x then
                                            settings.capitalizedMultiplier
                                        else 1
                              Nothing -> 1)
                      *
                      (let n = String.length s
                       in if isNumberlike s  then
                              f settings.numberMultiplier
                           else if String.contains s "(){}[]<>" then
                                       f settings.bracketMultiplier
                                   else if String.contains s "?!‑-,‒‚․‸" then
                                            f settings.commaMultiplier
                                   else if String.contains s "?!;\".‑-,:‒‘’‚‛“”„‟․‥…‧′″‴‵‶‷‸‹›※‼‽⁁⁂⁃⁄⁅⁆⁇⁈⁉⁊⁋⁌⁍⁎⁏⁐⁑⁒⁓⁔⁕⁖⁗⁘⁙⁚⁛⁜⁝⁞" then
                                            f settings.stopQuoteMultiplier
                                        else if n == 1 then
                                                 f settings.unitMultiplier
                                             else  wpmToDelay settings.wpm + ((Basics.max 0 ((wpmToDelay settings.wpm / settings.lengthDivisor)
                                                                                                 * (toFloat n - settings.lengthBound))) ^ settings.lengthExponent)))

itemColor : Item -> Maybe String
itemColor item =
    case item of
        ItemEndParagraph -> Nothing
        ItemImage s -> Nothing
        ItemString s ->
            -- soft delays in sentences
            if String.contains s "/;‑-,:‒–—―‖‗‚†‡•‣‰‱‸※‾‿⁀⁏⁐⁒⁓⁔⁕⁖⁘⁙⁚⁛⁜⁝⁞"
            then Just "#3380dE"
            -- hard stops
            else if String.contains s "?!\".‘’‛“”„‟․‥…‧′″‴‵‶‷‼‽⁁⁂⁃⁄⁅⁆⁇⁈⁉⁊⁋⁌⁍⁗"
                 then Just "#DB1A1A"
                 -- start of parentheticals
                 else if String.contains s "({[<‹"
                      then Just "#24a700"
                      -- end of parentheticals
                      else if String.contains s ")}]>›"
                           then Just "#A10199"
                           else Nothing

itemSizeMultiplier : Item -> Float
itemSizeMultiplier item =
    case item of
        -- TODO This doesn't quite correspond to speedread.scm
        ItemEndParagraph -> 1
        ItemImage s -> 1
        ItemString s -> 1
                        -- TODO I want this but it jerks the box around and is very distracting!
        -- ItemString s ->
        --     -- soft delays in sentences
        --     if String.contains s "/;‑-,:‒–—―‖‗‚†‡•‣‰‱‸※‾‿⁀⁏⁐⁒⁓⁔⁕⁖⁘⁙⁚⁛⁜⁝⁞"
        --     then 2
        --     -- hard stops
        --     else if String.contains s "?!\".‘’‛“”„‟․‥…‧′″‴‵‶‷‼‽⁁⁂⁃⁄⁅⁆⁇⁈⁉⁊⁋⁌⁍⁗"
        --          then 2
        --          -- start of parentheticals
        --          else if String.contains s "({[<‹"
        --               then 1.5
        --               -- end of parentheticals
        --               else if String.contains s ")}]>›"
        --                    then 1.5
        --                    else 1

positionAtSection : Book -> Int -> Result String BookPosition
positionAtSection book spineOffset =
    case Array.get spineOffset book.spine of
        Just spineId ->
            case Dict.get spineId book.manifest of
                Nothing -> Err "Book position exists in spine but not in the manifest"
                Just manifestInfo ->
                    case Dict.get (String.append book.rootPrefix manifestInfo.filename) book.fileIndex of
                        Nothing -> Err "Entry exists in manifest but doesn't exist in the zip file"
                        Just fileInfo ->
                            case xmlParse fileInfo.data of
                                Nothing -> Err "Can't parse book source file"
                                Just xml ->
                                    case xmlFind xml "/*:html/*:body" of
                                        [] -> Err "No body element in file"
                                        (body::_) -> Ok ({ spineOffset = spineOffset
                                                         , items = Array.fromList (items book { domPosition = Just { nextNode = wrapNode body
                                                                                                                   , nodeEndsParagraph = True }
                                                                                              , stringPosition = Nothing })
                                                         , offset = 0 })
        Nothing -> Err "Unknown book spine position"


type Next a = NextStop a (Maybe Item)
            | NextEnd (Maybe Item)

type NextString a = NextStopString a String
                  | NextEndString String

nextWordInStringIterator : StringIterator -> NextString StringIterator
nextWordInStringIterator si =
    case si of
        (a::l) -> if isWhitespace a then
                      NextStopString (List.dropWhile isWhitespace si) ""
                  else
                      case splitWord si of
                          (r, [], False) -> NextEndString (String.fromList r)
                          (r, l, _) -> NextStopString l (String.fromList r)
        [] -> NextEndString ""

-- Note that we don't break hypens (-) anymore
-- The two dashes below look very similar but they aren't the same character
isPunctuation : Char -> Bool
isPunctuation c = String.contains (String.fromList [c]) "/(){}[]?!;\".‑,:<>‒–—―‖‗‘’‚‛“”„‟†‡•‣․‥…‧‰‱′″‴‵‶‷‸‹›※‼‽‾‿⁀⁁⁂⁃⁄⁅⁆⁇⁈⁉⁊⁋⁌⁍⁎⁏⁐⁑⁒⁓⁔⁕⁖⁗⁘⁙⁚⁛⁜⁝⁞"

-- Note that we don't break hypens (-) anymore
-- The two dashes below look very similar but they aren't the same character
isPunctuation_ : String -> Bool
isPunctuation_ s = String.contains s "/(){}[]?!;\".‑,:<>‒–—―‖‗‘’‚‛“”„‟†‡•‣․‥…‧‰‱′″‴‵‶‷‸‹›※‼‽‾‿⁀⁁⁂⁃⁄⁅⁆⁇⁈⁉⁊⁋⁌⁍⁎⁏⁐⁑⁒⁓⁔⁕⁖⁗⁘⁙⁚⁛⁜⁝⁞"

isOpenContext_ : String -> Bool
isOpenContext_ s = String.contains s "({[<“„‹"

isCloseContext_ : String -> Bool
isCloseContext_ s = String.contains s ")}]>”‟›"

isOpenOrCloseContext_ : String -> Bool
isOpenOrCloseContext_ s = String.contains s "\""

isWhitespace : Char -> Bool
isWhitespace c = String.contains (String.fromList [c]) " \n\t"

-- Take off the first word or item from the a string (list of characters) and
-- return the word/item, the remaining list, and true if the word is complete
splitWord : List Char -> (List Char, List Char, Bool)
splitWord si =
    let loop r l = case l of
                       [] -> (r, [], False)
                       ('.'::a::l) -> if isWhitespace a then
                                          case r of
                                              [] -> (['.'], a::l, True)
                                              _  -> (r, '.'::a::l, True)
                                      else
                                          loop (List.append r ['.']) (a::l)
                       ('’'::a::l) -> if isWhitespace a || isPunctuation a then
                                          case r of
                                              [] -> (['’'], a::l, True)
                                              _  -> (r, '’'::a::l, True)
                                      else
                                          loop (List.append r ['’']) (a::l)
                       ('s'::'’'::a::l) -> if isWhitespace a || isPunctuation a then
                                          case r of
                                              [] -> (['s', '’'], a::l, True)
                                              _  -> (List.append r ['s','’'], a::l, True)
                                      else
                                          loop (List.append r ['s', '’']) (a::l)
                       (a::l) -> if isWhitespace a then
                                     case r of
                                         [] -> loop r l
                                         _ -> (r, l, True)
                                 else
                                     if isPunctuation a then
                                         case r of
                                             [] -> ([a], l, True)
                                             _ -> (r, a::l, True)
                                     else
                                         loop (List.append r [a]) l
    in loop [] si

xmlStringIterator : Value -> Maybe StringIterator
xmlStringIterator d = case xmlText d of
                          Just t -> Just (String.toList t)
                          Nothing -> Nothing

nodeEndsParagraph node = case xmlName node of
                        Just "p" -> True
-- TODO This sort of does the right thing for now but we should split it apart eventually.
                        Just "br" -> True
                        _ -> False

nextDomPosition : Value -> Maybe DOMIterator
nextDomPosition node = case xmlFirstChild node of
                           Nothing -> case xmlNextSibling node of
                                          Nothing -> case xmlParent node of
                                                         Nothing -> Nothing
                                                         Just p -> skipDomPosition_ p (nodeEndsParagraph node)
                                          Just s -> Just { nextNode = wrapNode s, nodeEndsParagraph = nodeEndsParagraph node }
                           Just c -> Just { nextNode = wrapNode c, nodeEndsParagraph = False }

-- Like next but doesn't recurse into the current element
-- Takes an extra argument, is any node we've seen so far a terminator of words?
skipDomPosition_ : Value -> Bool -> Maybe DOMIterator
skipDomPosition_ node end = case xmlNextSibling node of
                                Nothing -> case xmlParent node of
                                               Nothing -> Nothing
                                               Just p -> skipDomPosition_ p (end || nodeEndsParagraph node)
                                Just s -> Just { nextNode = wrapNode s, nodeEndsParagraph = end || nodeEndsParagraph node }

-- Like next but doesn't recurse into the current element
skipDomPosition : Value -> Maybe DOMIterator
skipDomPosition node = skipDomPosition_ node False

maybeEmptyItem s = case s of
                       "" -> Nothing
                       x -> Just (ItemString x)

-- advance the current position (XML -> Maybe (Position, XML))
--  return a span of the document and the next span.
-- This is used to produce the set of items. Items can span dom notes (both text
-- and non-text elements) and in the case of text elements can be some subset of
-- a text element combined with multiple other nodes. We need to keep an offset
-- in the dom and an offset into a text element to represent this.

type alias ItemPosition = { domPosition : Maybe DOMIterator, stringPosition : Maybe StringIterator }

nextItem_ : Book -> ItemPosition -> Next ItemPosition
nextItem_ book position =
    case position.stringPosition of
        Just si -> case nextWordInStringIterator si of
                       NextStopString si str -> NextStop { position | stringPosition = Just si } (maybeEmptyItem str)
                       NextEndString str ->
                           let nextPosition = { position | stringPosition = Nothing }
                           in case nextItem_ book nextPosition of
                                  -- If the next item is a stright, they might be combined.
                                  -- Think H<small>ello</small>. This should result in Hello.
                                  -- Unless the second part of the string shouldn't be joined to the first,
                                  -- for example when it's punctuation.
                                  NextStop p (Just (ItemString s)) ->
                                      if isPunctuation_ s then
                                          NextStop nextPosition (maybeEmptyItem str)
                                      else
                                          NextStop p (maybeEmptyItem (String.append str s))
                                  NextEnd (Just (ItemString s)) -> NextEnd (maybeEmptyItem (String.append str s))
                                  -- But if the next item is not a string, it
                                  -- cannot be combined with one. We always stop
                                  -- before it with the previous position after
                                  -- consuming the string.
                                  _ -> NextStop nextPosition (maybeEmptyItem str)
        Nothing ->
            case position.domPosition of
                -- At the end of the dom there's nothing to do.
                Nothing -> NextEnd Nothing
                Just dp ->
                    -- Some dom elements, like p, end paragraphs. We want to
                    -- know when we're transitioning out of such a node and only
                    -- the dom walker can tell us this information.
                    case dp.nodeEndsParagraph of
                        True -> NextStop { position | domPosition = Just { nextNode = dp.nextNode, nodeEndsParagraph = False } } (Just ItemEndParagraph)
                        False ->
                            let nextNode = unwrapNode dp.nextNode
                            in case xmlType nextNode of
                                     -- DOM comments, etc.
                                     Nothing -> nextItem_ book { position | domPosition = nextDomPosition nextNode }
                                     -- Unpack raw text into a string iterator since it might contain multiple words.
                                     Just NodeTypeText ->
                                         nextItem_ book { position | domPosition = nextDomPosition nextNode
                                                        , stringPosition = xmlStringIterator nextNode }
                                     -- Some elements need special love
                                     Just NodeTypeElement ->
                                         case xmlName nextNode of
                                             Nothing -> nextItem_ book { position | domPosition = skipDomPosition nextNode }
                                             Just "img" -> NextStop { position | domPosition = skipDomPosition nextNode }
                                                           (case xmlAttribute nextNode "src" of
                                                                Nothing -> Nothing
                                                                Just src ->
                                                                    case Dict.get (String.append book.rootPrefix src) book.fileIndex of
                                                                        Nothing -> Just (ItemImage src)
                                                                        Just fileInfo -> Just (ItemImage fileInfo.data))
                                             -- Skip footnotes (make this an item and skip it later?)
                                             Just "sup" -> nextItem_ book { position | domPosition = skipDomPosition nextNode }
                                             _ -> nextItem_ book { position | domPosition = nextDomPosition nextNode }

nextItem : Book -> ItemPosition -> (Maybe ItemPosition, Maybe Item)
nextItem book position = case nextItem_ book position of
                             -- We're at the end of our data stream, no more items are left
                             NextEnd i -> (Just {position | domPosition = Nothing, stringPosition = Nothing}, i)
                             -- Otherwise skip over empty items
                             NextStop p Nothing -> nextItem book p
                             NextStop p i -> (Just p, i)

items_ : Book -> ItemPosition -> List Item -> List Item
items_ book position l =
    case nextItem book position of
        (Nothing, Nothing) -> l
        (Nothing, Just s) -> s :: l
        (Just p, Nothing) -> case (p.domPosition, p.stringPosition) of
                                 (Nothing, Nothing) -> l
                                 _ -> items_ book p l
        (Just p, Just s) -> items_ book p (s :: l)

items : Book -> ItemPosition -> List Item
items book position = List.reverse (items_ book position [])

nextSection : Book -> BookPosition -> Result String BookPosition
nextSection book position = positionAtSection book (position.spineOffset + 1)

previousSection : Book -> BookPosition -> Result String BookPosition
previousSection book position = positionAtSection book (position.spineOffset - 1)

zipToBook : Zip -> Result String Book
zipToBook zip =
    let fileIndex = Dict.fromList (List.map (\x -> (x.filename, x)) zip)
        rootFiles = case Dict.get "META-INF/container.xml" fileIndex of
                        Nothing -> Err "Can't find 'META-INF/container.xml'. This might not be a book in epub format"
                        Just info -> case xmlParse info.data of
                                         Nothing -> Err "Can't parse 'META-INF/container.xml'"
                                         Just xml -> case xmlFind xml "/*:container/*:rootfiles/*:rootfile" of
                                                         [] -> Err "No root files found"
                                                         rootFiles -> Ok (mapMaybe (\f -> xmlAttribute f "full-path") rootFiles)
        withRootFile fn = case rootFiles of
                              Err s -> Err s
                              Ok [] -> Err "No root file"
                              Ok (rootFile::_) ->
                                  case Dict.get rootFile fileIndex of
                                      Nothing -> Err "Can't find root file"
                                      Just info ->
                                          case xmlParse info.data of
                                              Nothing -> Err "Can't parse root file"
                                              Just xml -> fn xml
        rootPrefix = case rootFiles of
                              Err s -> Err s
                              Ok [] -> Err "No root file"
                              Ok (rootFile::_) -> Ok (String.join "/" (List.reverse (List.drop 1 (List.reverse (String.split "/" rootFile)))))
        manifest = withRootFile (\xml ->
                                     case xmlFind xml "/*:package/*:manifest/*:item" of
                                         [] -> Err "Empty manifest"
                                         manifest ->
                                             let fn i h m = case (i, h, m) of
                                                                (Just ij, Just hj, Just mj) -> Just (ij, { filename = hj, mimetype = mj })
                                                                _ -> Nothing in
                                             Ok (Dict.fromList
                                                     (mapMaybe
                                                          (\f -> fn (xmlAttribute f "id")
                                                               (xmlAttribute f "href")
                                                               (xmlAttribute f "media-type"))
                                                          manifest)))
        spine = withRootFile (\xml ->
                                     case xmlFind xml "/*:package/*:spine/*:itemref" of
                                         [] -> Err "Empty manifest"
                                         spine -> Ok (mapMaybe (\f -> xmlAttribute f "idref") spine))
        title = withRootFile (\xml -> case xmlFind xml "/*:package/*:metadata/*:title" of
                                          [] -> Err "No title found"
                                          (title::_) -> case xmlText title of
                                                            Nothing -> Err "Empty title"
                                                            Just txt -> Ok txt)
    in
        case (rootFiles, manifest, spine, title, rootPrefix) of
            (Err s, _, _, _, _) -> Err s
            (_, Err s, _, _, _) -> Err s
            (_, _, Err s, _, _) -> Err s
            (_, _, _, Err s, _) -> Err s
            (_, _, _, _, Err s) -> Err s
            (Ok rootFiles, Ok manifest, Ok spine, Ok title, Ok rootPrefix) ->
                Ok ({ zip = zip
                    , fileIndex = fileIndex
                    , rootFiles = rootFiles
                    , rootPrefix = if rootPrefix == "" then
                                       "" else
                                       String.append rootPrefix "/"
                    , manifest = manifest
                    , spine = Array.fromList spine
                    , title = title })

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [ jszip_loaded FileZipLoaded
                                , localforage_item_was_get ReceiveStorage
                                , sha1arraybuffer_get SHA1Get
                                , presses Keypress ]

-- Update

type Msg = SelectFiles (List NativeFile)
         | UploadFile
         | FileData (Result Error FileContentArrayBuffer)
         | FileZipLoaded Zip
         | PreviousWord
         | NextWord
         | PreviousParagraph
         | NextParagraph
         | PreviousSection
         | NextSection
         | Start
         | Stop
         | Advance
         | ClearStorage
         | ReceiveStorage (String, Bool, Value)
         | LoadData
         | StorePosition
         | StoreSettings
         | SHA1Get String
         | Keypress Int
         | SettingsUpdate String String
         | WordPresentTime Time

type Direction = Backward | Forward

arrayFindForward o a i = case Array.get o a of
                             Just e -> if e == i then
                                           Just o
                                       else
                                           arrayFindForward (o + 1) a i
                             Nothing -> Nothing

arrayFindBackward o a i = case Array.get o a of
                             Just e -> if e == i then
                                           Just o
                                       else
                                           arrayFindBackward (o - 1) a i
                             Nothing -> Nothing

nextParagraph position good atend missing =
    case position of
        Just p ->
            let startOffset = case (Array.get p.offset p.items, Array.get (p.offset + 1) p.items) of
                                  (Just ItemEndParagraph, _) -> p.offset + 1
                                  (_, Just ItemEndParagraph) -> p.offset + 2
                                  _ -> p.offset
            in case arrayFindForward startOffset p.items ItemEndParagraph of
                   Just offset -> good (Just { p | offset = offset })
                   Nothing -> atend p
        _ -> missing position

previousParagraph position good atend missing =
    case position of
        Just p ->
            let startOffset = case (Array.get (p.offset - 1) p.items, Array.get p.offset p.items) of
                                  (_, Just ItemEndParagraph) -> p.offset - 1
                                  (Just ItemEndParagraph, _) -> p.offset - 2
                                  _ -> p.offset
            in case arrayFindBackward startOffset p.items ItemEndParagraph of
                   Just offset -> good (Just { p | offset = offset })
                   Nothing -> atend p
        _ -> missing position

nextPosition position good atend missing =
    case position of
        Just p -> case itemsToDisplay 1 p of
                      [] -> atend p
                      l -> good (Just { p | offset = p.offset + List.length l })
        _ -> missing position

-- TODO Implement this correctly!
-- Sadly this has to suffer due to variable item chunking. We need to search for
-- which word position would include the previous position. We include some
-- lookback for this purpose.
previousPosition position good atend missing =
    case position of
        Just p -> case Array.get (p.offset - 1) p.items of
                      Just _ -> good (Just { p | offset = p.offset - 1 })
                      _ -> atend p
        _ -> missing position

itemsToDisplay_ items offset r =
    case r of
        [] -> case Array.get (offset+1) items of
                  Just e -> itemsToDisplay_ items (offset+1) [e]
                  Nothing -> []
        ((ItemString h)::t) ->
            -- Some words appear at the beginning of sentences and are often followed by named entities
            if List.member h ["The", "When", "In", "On", "At", "Before", "After", "Why", "Had", "Many", "Few", "All", "Some", "No"
                             ,"Former", "Future", "Current", "Past", "For", "If", "About", "But", "Despite", "With", "Because"
                             , "Without", "Throughout", "Through", "Into", "By", "For", "Against", "As", "OK", "Oksudo", "To"] then
                r
            else if isCapitalized h || List.member h ["de", "von"] then
                     case (Array.get (offset+1) items, Array.get (offset+2) items) of
                         (Just (ItemString s1), Nothing) -> if isCapitalized s1 && s1 /= "I"  && s1 /= "I'll"  && s1 /= "OK" then
                                                                (ItemString s1)::r else
                                                                r
                         (Just (ItemString s1), Just (ItemString s2)) ->
                             case (isCapitalized s1 && s1 /= "I" && s1 /= "I'll" && s1 /= "OK", "." == s1, isCapitalized s2) of
                                 (True, _, _) -> itemsToDisplay_ items (offset+1) (ItemString s1::r)
                                 (False, True, True) -> if String.length h < 3 then
                                                            -- These are often abbreviations
                                                            itemsToDisplay_ items (offset+2) (ItemString s2::ItemString s1::r) else
                                                            r
                                 _ -> r
                         _ -> r
                 else if isNumberlike h then
                          case (Array.get (offset+1) items, Array.get (offset+2) items) of
                              (Just (ItemString "percent"), _) -> (ItemString "percent")::r
                              (Just (ItemString "am"), _) -> (ItemString "am")::r
                              (Just (ItemString "pm"), _) -> (ItemString "pm")::r
                              (Just (ItemString "AM"), _) -> (ItemString "AM")::r
                              (Just (ItemString "PM"), _) -> (ItemString "PM")::r
                              (Just (ItemString ","), Just (ItemString s)) ->
                                  if isNumberlike s then (ItemString s)::(ItemString ",")::r else r
                              (Just (ItemString "."), Just (ItemString s)) ->
                                  if isNumberlike s then (ItemString s)::(ItemString ",")::r else r
                              _ -> r
                      else case (h, Array.get (offset+1) items, Array.get (offset+2) items) of
                               (".", Just (ItemString "."), Just (ItemString ".")) -> (ItemString ".")::(ItemString ".")::r
                               ("\"", Just (ItemString s), Just (ItemString "\"")) -> (ItemString "\"")::(ItemString s)::r
                               ("“", Just (ItemString s), Just (ItemString "”")) -> (ItemString "”")::(ItemString s)::r
                               ("'", Just (ItemString s), Just (ItemString "'")) -> (ItemString "'")::(ItemString s)::r
                               ("‘", Just (ItemString s), Just (ItemString "’")) -> (ItemString "’")::(ItemString s)::r
                               ("[", Just (ItemString s), Just (ItemString "]")) -> (ItemString "]")::(ItemString s)::r
                               ("(", Just (ItemString s), Just (ItemString ")")) -> (ItemString ")")::(ItemString s)::r
                               (a, Just (ItemString b), Just (ItemString c)) ->
                                   -- Many nouns contain repetitions like "Mau Mau".
                                   -- Any repetitions like this are totally lost otherwise.
                                   if a == b && b == c then
                                       (ItemString c)::(ItemString b)::r
                                   else if a == b then
                                       (ItemString b)::r
                                   else
                                    let l = Array.toList (slice offset (offset+3) items)
                                    in case find (\a -> listIsPrefix a l) phrases of
                                           (Just p) -> case List.tail p of
                                                           Nothing -> r
                                                           Just tp -> List.append (List.reverse (List.map ItemString tp)) r
                                           _ -> r
                               _ -> r
        _ -> r

find f l = case l of
               [] -> Nothing
               h::t -> if f h then Just h else find f t

listIsPrefix a l = case (a, l) of
                       ([],_) -> True
                       (ah::at, (ItemString lh)::lt) -> if ah == lh then listIsPrefix at lt else False
                       _ -> False

phrases = [["ad", "hoc"]
          ,["bona", "fide"]
          ,["carpe", "diem"]
          ,["caveat", "emptor"]
          ,["deus", "ex"]
          ,["et", "alia"]
          ,["habeas", "corpus"]
          ,["in", "absentia"]
          ,["in", "camera"]
          ,["in", "situ"]
          ,["in", "vitro"]
          ,["in", "vivo"]
          ,["inter", "alia"]
          ,["mea", "culpa"]
          ,["non", "sequitur"]
          ,["per", "capita"]
          ,["per", "diem"]
          ,["prima", "facie"]
          ,["pro", "forma"]
          ,["sine", "qua", "non"]
          ,["vice", "versa"]]

itemsToDisplay : Int -> BookPosition -> List Item
itemsToDisplay nrItems position = -- List.reverse (itemsToDisplay_ position.items (position.offset-1) [])
    let fn o = List.reverse (itemsToDisplay_ position.items (position.offset-1+o) [])
        justPunctuation l = case l of
                                [ItemString s] -> isPunctuation_ s
                                _ -> False
        loop l a =
            if a > 5 || justPunctuation l then
                l else
                let inext = fn (List.length l)
                    lnext = List.append l inext
                 in if List.length lnext > nrItems
                    then l
                        -- Stop when we hit a punctuation-only entry
                    else if justPunctuation inext
                         then l
                         else loop lnext (a + 1)
    in loop (fn 0) 0

displayOne settings item =
    case item of
        ItemString s ->
            span [scaleFont (itemSizeMultiplier (ItemString s) * settings.baseFontScale)
                 ,style (case itemColor (ItemString s) of
                             Just c -> [("color", c)]
                             Nothing -> [("color", "rgb(76, 76, 76)")])]
            [text s]
        ItemEndParagraph -> text ""
        ItemImage i -> img [ src i ] []

joinMaybe : List (Maybe a) -> List a
joinMaybe xs =
    let f l = case l of
                  Just h::t -> h :: f t
                  Nothing::t -> f t
                  [] -> []
    in f xs

isJust : Maybe a -> Bool
isJust x = case x of
               Nothing -> False
               Just _ -> True

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SelectFiles fnames ->
            if 1 == List.length fnames then
                { model | selectedFile = List.head fnames } ! [ send UploadFile ]
            else
                { model | selectedFile = Nothing
                , message = Just "Exactly one file must be selected" } ! []
        UploadFile ->
            case model.selectedFile of
                Nothing -> { model | selectedFile = Nothing
                           , message = Just "No file selected" } ! []
                Just fname -> model ! [readArrayBuffer fname]
        FileData (Err err) -> { model | message = Just (FileReader.prettyPrint err )} ! []
        FileData (Ok d) -> { model | rawContent = Just d } ! [jszip_load d, localforage_setItem ("book", d)]
        StoreSettings -> model ! (case model.sha1 of
                                      Just s -> [ localforage_setItem ("settings-", Native.XML.coerce model.settings) ]
                                      Nothing -> [])
        StorePosition -> model ! (case (model.sha1, model.position) of
                                      (Just s, Just p) -> [ localforage_setItem (String.append "position-" s
                                                                                , Native.XML.coerce (p.spineOffset, p.offset)) ]
                                      _ -> [])
        LoadData -> model ! [localforage_getItem "book"]
        ReceiveStorage (s,True,v) ->
            if s == "book" then
                { model | rawContent = Just v} ! [jszip_load v]
            else if String.startsWith "settings" s then
                     { model | settings = Native.XML.coerce v} ! []
                 else if String.startsWith "position" s then
                          case model.book of
                              Just book -> case positionAtSection book (Tuple.first (Native.XML.coerce v)) of
                                               Ok p -> let p_ = { p | offset = Tuple.second (Native.XML.coerce v) }
                                                       in { model | position = Just p_ } ! []
                                               Err _ -> { model | position = Nothing } ! []
                              Nothing -> { model | position = Nothing } ! []
                      else { model | message = Just "BUG: Unknown load" } ! []
        ReceiveStorage (s,False,v) -> { model | message = Just (String.append "Can't load " s)} ! []
        FileZipLoaded z ->
            case zipToBook z of
                Err err -> { model | message = Just err } ! []
                Ok book -> case positionAtSection book 0 of
                               Err err -> { model | message = Just err
                                          , zipContent = Just z
                                          , book = Just book
                                          } ! []
                               Ok position -> { model | zipContent = Just z
                                              , book = Just book
                                              , position = Just position
                                              } ! [case model.rawContent of
                                                       Nothing -> Cmd.none
                                                       Just c -> sha1arraybuffer c]
        SHA1Get s -> { model | sha1 = Just s } ! [localforage_getItem (String.append "position-" s)
                                                 ,localforage_getItem "settings-"]
        PreviousWord -> previousPosition model.position
                        (\p -> { model | position = p })
                        (\_ -> { model | message = Just "Already at the beginning of the section" })
                        (\_ -> { model | message = Just "No position" }) ! [send StorePosition]
        NextWord -> nextPosition model.position
                    (\p -> { model | position = p })
                    (\_ -> { model | message = Just "Already at the end of the section" })
                    (\_ -> { model | message = Just "No position" }) ! [send StorePosition]
        PreviousParagraph -> previousParagraph model.position
                        (\p -> { model | position = p })
                        (\_ -> { model | message = Just "Already at the beginning of the section" })
                        (\_ -> { model | message = Just "No position" }) ! [send StorePosition]
        NextParagraph -> nextParagraph model.position
                        (\p -> { model | position = p })
                        (\_ -> { model | message = Just "Already at the end of the section" })
                        (\_ -> { model | message = Just "No position" }) ! [send StorePosition]
        PreviousSection -> (case (model.book, model.position) of
                        (Just book, Just position) -> case previousSection book position of
                                                          Ok p -> { model | position = Just p }
                                                          Err s -> { model | message = Just s }
                        _ -> { model | message = Just "Can't advance to the next section" }) ! [send StorePosition]
        NextSection -> (case (model.book, model.position) of
                        (Just book, Just position) -> case nextSection book position of
                                                          Ok p -> { model | position = Just p }
                                                          Err s -> { model | message = Just s }
                        _ -> { model | message = Just "Can't advance to the next section" }) ! [send StorePosition]
        ClearStorage -> model ! [localforage_clear True]
        Start -> if model.playing then
                     model ! [] else
                     { model | playing = True
                     , lastWordPresentTime = Nothing
                     , averageWordPresentTime = 0
                     , numberOfWordsPresented = 0 } ! [Task.perform (\_ -> Advance) (sleep (model.settings.delayStart * millisecond))]
        Stop -> if model.playing then
                    { model | playing = False, inContext = 0 } ! [send StoreSettings, send StorePosition] else
                    model ! []
        Advance -> if model.playing == True then
                       case (model.book, model.position) of
                           (Just book, Just position) ->
                               case itemsToDisplay model.settings.numberOfItemsToShow position of
                                   [] -> { model | playing = False, message = Just "stopped playing" } ! [send StorePosition]
                                   l -> let newPosition = { position | offset = (position.offset + List.length l) }
                                            newModel = { model | position = Just newPosition }
                                            newItems = itemsToDisplay model.settings.numberOfItemsToShow newPosition
                                        in case List.map (itemDelayOrStop model.settings) newItems of
                                               [] -> { model | playing = False, message = Just "stopped playing" } ! [send StorePosition]
                                               delays -> if List.all isJust delays then
                                                            (case newItems of
                                                                 [ItemString s] ->
                                                                 if isCloseContext_ s then
                                                                     { newModel | inContext = Basics.max 0 (model.inContext - 1) }
                                                                 else if isOpenContext_ s then
                                                                          { newModel | inContext = model.inContext + 1 }
                                                                      else if isOpenOrCloseContext_ s then
                                                                               -- If we're in any context and the context marker is ambiguous (like ") we always close.
                                                                               -- NB: We could do more here and see if we're in a " context instead of just closing.
                                                                          { newModel | inContext =
                                                                                if model.inContext > 0 then
                                                                                    model.inContext - 1
                                                                                else
                                                                                    model.inContext + 1 }
                                                                           else
                                                                               newModel
                                                                 _ -> newModel)
                                                            ! [Task.perform (\_ -> Advance) (sleep (List.sum (joinMaybe delays) * millisecond))
                                                              ,Task.perform WordPresentTime now]
                                                        else
                                                            { newModel | playing = False, message = Just "paused" } ! [send StorePosition]
                           _ -> { model | playing = False, message = Just "can't play" } ! []
                   else
                       model ! []
        Keypress k -> model ! (case log (toString k) k of
                                   -- space would be nice here but I have to figure out how to suppress space elsewhere
                                   115 -> [send (if model.playing then Stop else Start)]
                                   37 -> [send PreviousParagraph]
                                   39 -> [send NextParagraph]
                                   112 -> [send PreviousParagraph]
                                   110 -> [send NextParagraph]
                                   _ -> [])
        SettingsUpdate key val ->
            let s = model.settings
                f v = { model | settings = v }
            in (case (key, String.toFloat val) of
                   ("wpm", Ok v)                   -> f { s | wpm = v }
                   ("commonMultiplier", Ok v)      -> f { s | commonMultiplier = v }
                   ("delayStart", Ok v)            -> f { s | delayStart = v }
                   ("baseFontScale", Ok v)         -> f { s | baseFontScale = v }
                   ("commaMultiplier", Ok v)       -> f { s | commaMultiplier = v }
                   ("bracketMultiplier", Ok v)     -> f { s | bracketMultiplier = v }
                   ("numberMultiplier", Ok v)      -> f { s | numberMultiplier = v }
                   ("stopQuoteMultiplier", Ok v)   -> f { s | stopQuoteMultiplier = v }
                   ("paragraphMultiplier", Ok v)   -> f { s | paragraphMultiplier = v }
                   ("unitMultiplier", Ok v)        -> f { s | unitMultiplier = v }
                   ("capitalizedMultiplier", Ok v) -> f { s | capitalizedMultiplier = v }
                   ("allcapsMultiplier", Ok v)     -> f { s | allcapsMultiplier = v }
                   ("imageDelayMultiplier", Ok v)  -> f { s | imageDelayMultiplier = v }
                   ("lengthExponent", Ok v)        -> f { s | lengthExponent = v }
                   ("lengthDivisor", Ok v)         -> f { s | lengthDivisor = v }
                   ("lengthBound", Ok v)           -> f { s | lengthBound = v }
                   ("numberOfItemsToShow", Ok v)   -> f { s | numberOfItemsToShow = Basics.round v }
                   _                               -> model) ! [send StoreSettings]
        WordPresentTime t ->
            let newModel = { model | lastWordPresentTime = Just t }
            in (case model.lastWordPresentTime of
                   Just tp -> if (t - tp) < second then
                                  { newModel |
                                        averageWordPresentTime = ((t-tp) + model.averageWordPresentTime * (toFloat model.numberOfWordsPresented))
                                        / (toFloat model.numberOfWordsPresented + 1)
                                  , numberOfWordsPresented = model.numberOfWordsPresented + 1 }
                                            else newModel
                   _ -> newModel) ! []

readArrayBuffer : NativeFile -> Cmd Msg
readArrayBuffer fileValue =
    readAsArrayBuffer fileValue.blob
        |> Task.attempt FileData

-- View

debugView model = div [] [ text (case model.position of
                               Nothing -> "nowhere"
                               Just p -> String.join " " (List.map (\x -> case x of
                                                                              ItemString s -> s
                                                                              _ -> toString x)
                                                              (Array.toList p.items))) ]

slice : Int -> Int -> Array a -> Array a
slice from to a =
    let newFrom = Basics.min (Basics.max 0 from) (Array.length a - 1)
        newTo = Basics.min (Basics.max 0 to) (Array.length a - 1)
    in if newFrom > newTo || newFrom == newTo then
           Array.empty
       else Array.slice newFrom newTo a

scaleFont scale = style [("font-size", (String.append (toString (100 * scale)) "%"))]

view : Model -> Html Msg
view model =
    div []
        [ div
          [ class "bd-pageheader" ]
          [ Grid.container []
                [ h1 [ class "title" ]
                      [ div [class "row"]
                            [ div [class "col"]
                                  [text "Speedreader"]
                            , div [ class "col-8" ]
                                [ text (case model.book of
                                            Nothing -> "Open a book below"
                                            Just b -> b.title) ] ] ] ] ]
        , Grid.container []
            [div [ class "card"
                 , class "card-outline-danger"
                 , class "text-center"
                 , class "mb-2"
                 , class "w-100"
                 , class "h-100"
                 , style [("z-index", "50")]]
                [ div [ class "card-block"
                      , style [("background-color", "#fcfcfc")]
                      ]
                      [ p [ class "card-text"]
                            [ div [scaleFont 2
                                  ,style [("display", "inline-block"), ("visibility", "hidden")]]
                                  [text "⁠"]
                            , div [style [("display", "inline-block")
                                         ,("background-color", (String.join "" ["rgba(0,0,0,", toString (0.05 * toFloat model.inContext),")"]))]]
                                  (case (model.book, model.position) of
                                       (Just book, Just position) ->
                                           case List.map (displayOne model.settings)
                                                         (List.intersperse (ItemString " ")
                                                              (itemsToDisplay model.settings.numberOfItemsToShow position)) of
                                               [] -> [text ""]
                                               l -> l
                                       _ -> [text "..."])]]]
            ,div [ style [("z-index", "51"), ("position", "relative")] ]
                [ Button.button [ Button.onClick (if model.playing then Stop else Start)
                                , Button.disabled (case model.book of
                                                       Nothing -> True
                                                       Just _ -> False)
                                , Button.primary
                                , Button.block
                                , Button.large
                                , Button.attrs [ class "mb-1"
                                               , style [("z-index", "51")]] ]
                      [ i [ class (if model.playing then
                                       "fa fa-stop" else
                                       "fa fa-play")
                          , style [("z-index", "51")]] [] ] ]
            ,case (model.book, model.position) of
                 (Just book, Just position) ->
                     div [ class "card mb-1" ]
                         [div [class "card-body"]
                              [p [class "card-text text-center"]
                                   [text "Section "
                                   ,text (String.join
                                              ""
                                              [toString position.spineOffset
                                              ," of "
                                              ,toString (Array.length book.spine)])
                                   ,let progress = String.append
                                                   (toString (Basics.round (100 * (toFloat position.offset
                                                                                       / toFloat (Array.length position.items)))))
                                                   "%"
                                    in div [class "progress"]
                                       [div [class "progress-bar"
                                            ,attribute "role" "progressbar"
                                            ,style [("width", progress)]
                                            ,attribute "aria-valuenow" "25"
                                            ,attribute "aria-valuemin" "0"
                                            ,attribute "aria-valuemax" "100"]
                                            [text progress
                                            ,text " "
                                            ,text (toString (Array.length position.items))]]
                                   ,text "Last read at "
                                   ,text (if model.averageWordPresentTime == 0 then
                                              "" else
                                              toString (model.settings.numberOfItemsToShow
                                                            * Basics.round (60 / (model.averageWordPresentTime / second))))
                                   ,text " wpm"]]]
                 _ -> div [] [text "Select a book below"]
            ,div [ class "dimmer"
              , style [("opacity", if model.playing then "100%" else "0%")
                      ,("background-color", if model.playing then "rgba(0,0,0,1)" else "rgba(0,0,0,0)")
                      ,("z-index", if model.playing then "10" else "-10")]] [text " "]
            ,div [ class "text-center mx-auto center-block mb-1" ]
                [ Button.button [ Button.onClick PreviousSection
                                , Button.disabled (case model.book of
                                                       Nothing -> True
                                                       Just _ -> False)
                                , Button.primary ]
                      [ i [ class "fa fa-fast-backward" ] []
                      , text " section" ]
                , Button.button [ Button.onClick PreviousParagraph
                                , Button.disabled (case model.book of
                                                       Nothing -> True
                                                       Just _ -> False)
                                , Button.primary
                                , Button.attrs [ class "ml-1" ] ]
                    [ i [ class "fa fa-backward" ] []
                      , text " paragraph" ]
                , Button.button [ Button.onClick PreviousWord
                                , Button.disabled (case model.book of
                                                       Nothing -> True
                                                       Just _ -> False)
                                , Button.primary
                                , Button.attrs [ class "ml-1" ]]
                    [ i [ class "fa fa-step-backward" ] []
                      , text " word" ]
                , Button.button [ Button.onClick NextWord
                                , Button.disabled (case model.book of
                                                       Nothing -> True
                                                       Just _ -> False)
                                , Button.primary
                                , Button.attrs [ class "ml-1" ]]
                    [ i [ class "fa fa-step-forward" ] []
                      , text " word" ]
                , Button.button [ Button.onClick NextParagraph
                                , Button.disabled (case model.book of
                                                       Nothing -> True
                                                       Just _ -> False)
                                , Button.primary
                                , Button.attrs [ class "ml-1" ]]
                    [ i [ class "fa fa-forward" ] []
                      , text " paragraph" ]
                , Button.button [ Button.onClick NextSection
                                , Button.disabled (case model.book of
                                                       Nothing -> True
                                                       Just _ -> False)
                                , Button.primary
                                , Button.attrs [ class "ml-1" ]]
                    [ i [ class "fa fa-fast-forward" ] []
                      , text " section" ]]
            ,div [ class "text-center mx-auto center-block mb-1" ]
                [ label [ class "btn"
                        , class (case model.book of
                                     Nothing -> "btn-warning"
                                     Just _ -> "btn-primary") ]
                      [ text "Open book (.epub)"
                      , input
                            [ type_ "file"
                            , onFileChange SelectFiles
                            , class "btn"
                            , hidden True
                            ]
                            [] ]
                ]
            ,div [ class "card"
                 , class "card-outline-danger"
                 , class "w-100"
                 , class "text-display-card mb-3" ]
                [ div [ class "card-block" ]
                      [ p [ class "card-text" ]
                      (case model.position of
                          Nothing -> []
                          Just p ->
                              let display x = case x of
                                                  ItemString s -> if isPunctuation_ s then
                                                                      text s else
                                                                      text (String.append " " s)
                                                  ItemImage i -> img [ src i ] []
                                                  ItemEndParagraph -> div [ class "mb-1 para-end" ] []
                              in if not model.playing then
                                  List.concat
                                  [List.map display (Array.toList (slice (previousParagraph
                                                                              (Just { p | offset = p.offset - 150 })
                                                                              (\r -> case r of
                                                                                         Just r -> r.offset + 1
                                                                                         Nothing -> p.offset - 150)
                                                                              (\p -> p.offset)
                                                                              (\_ -> p.offset - 150))
                                                                              -- (p.offset - 150)
                                                                         p.offset
                                                                         p.items))
                                          , case Array.get p.offset p.items of
                                                -- This is special because if we put the </br> tag in the span we don't get a newline for some reason.
                                                Just ItemEndParagraph -> [span [class "badge badge-danger"] [text " "], display ItemEndParagraph]
                                                Just e -> [span [class "badge badge-danger"] [display e]]
                                                Nothing -> []
                                          ,List.map display (Array.toList (slice
                                                                               (p.offset + 1)
                                                                               (nextParagraph
                                                                                    (Just { p | offset = p.offset + 301 })
                                                                                    (\r -> case r of
                                                                                               Just r -> r.offset
                                                                                               Nothing -> p.offset + 301)
                                                                                    (\p -> p.offset)
                                                                                    (\_ -> p.offset + 301))
                                                                               p.items))]
                                  else [])] ]
            ,div [ class "text-center mx-auto center-block mb-2 row" ]
                (if not model.playing then
                     (let f k v t = F.group [] [F.label [] [text t]
                                         ,F.number [F.onInput (SettingsUpdate v)
                                                   ,F.value (toString k)]]
                in (List.map (\(k,v,t) -> f k v t)
                        [(model.settings.wpm, "wpm", "base wpm")
                        ,(model.settings.commonMultiplier, "commonMultiplier", "common word multiplier")
                        ,(model.settings.delayStart, "delayStart", "start delay ms")
                        ,(model.settings.baseFontScale, "baseFontScale", "base font scale")
                        ,(model.settings.commaMultiplier, "commaMultiplier", "comma multiplier")
                        ,(model.settings.bracketMultiplier, "bracketMultiplier", "bracket multiplier")
                        ,(model.settings.numberMultiplier, "numberMultiplier", "number multiplier")
                        ,(model.settings.stopQuoteMultiplier, "stopQuoteMultiplier", "stopQuote multiplier")
                        ,(model.settings.paragraphMultiplier, "paragraphMultiplier", "paragraph multiplier")
                        ,(model.settings.unitMultiplier, "unitMultiplier", "unit multiplier")
                        ,(model.settings.capitalizedMultiplier, "capitalizedMultiplier", "capitalized multiplier")
                        ,(model.settings.allcapsMultiplier, "allcapsMultiplier", "allcaps multiplier")
                        ,(model.settings.imageDelayMultiplier, "imageDelayMultiplier", "image delay multiplier")
                        ,(model.settings.lengthExponent, "lengthExponent", "length exponent")
                        ,(model.settings.lengthDivisor, "lengthDivisor", "length divisor")
                        ,(model.settings.lengthBound, "lengthBound", "length bound")
                        ,(toFloat model.settings.numberOfItemsToShow, "numberOfItemsToShow", "items to display")
                        ]))
                     else [])
            ] ]
