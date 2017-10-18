port module Support exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import FileReader exposing (..)
import Json.Decode as Json exposing (Decoder, Value)
import Debug exposing (..)
import Native.XML
import Dict exposing (Dict)
import Regex exposing (Regex)
import Bootstrap.Grid as Grid

-- XML stuff

xmlParse : String -> Maybe Value
xmlParse = Native.XML.parse

xmlFirstChild : Value -> Maybe Value
xmlFirstChild = Native.XML.firstChild

xmlNextSibling : Value -> Maybe Value
xmlNextSibling = Native.XML.nextSibling

xmlParent : Value -> Maybe Value
xmlParent = Native.XML.parent

xmlText : Value -> Maybe String
xmlText = Native.XML.text

xmlAttribute : Value -> String -> Maybe String
xmlAttribute = Native.XML.attribute

xmlName : Value -> Maybe String
xmlName = Native.XML.name

type NodeType = NodeTypeElement | NodeTypeText

xmlType : Value -> Maybe NodeType
xmlType xml = case Native.XML.nodeType xml of
                  Just 1 -> Just NodeTypeElement
                  Just 3 -> Just NodeTypeText
                  _ -> Nothing

xmlFind : Value -> String -> List Value
xmlFind = Native.XML.find

xmlSerialize : Value -> String
xmlSerialize = Native.XML.serialize

type Node = Node Value

wrapNode : Value -> Node
wrapNode v = Node (Native.XML.wrapNode v)

unwrapNode : Node -> Value
unwrapNode (Node n) = Native.XML.unwrapNode n

-- Zip stuff

type alias ZipEntry = { filename : String
                      , data : String
                      , datatype : String }
type alias Zip = List ZipEntry

-- Call & return for js
port jszip_load : FileContentArrayBuffer -> Cmd msg
port jszip_loaded : (Zip -> msg) -> Sub msg

port localforage_setItem : (String, Value) -> Cmd msg
port localforage_getItem : String -> Cmd msg
port localforage_removeItem : String -> Cmd msg
-- This bool does nothing, but elm isn't hpapy without it
port localforage_clear : Bool -> Cmd msg

port localforage_item_was_set : ((String, Bool) -> msg) -> Sub msg
port localforage_item_was_get : ((String, Bool, Value) -> msg) -> Sub msg
port localforage_item_was_removed : (String -> msg) -> Sub msg
port localforage_item_was_cleared : (Bool -> msg) -> Sub msg

-- Hash

port sha1arraybuffer : FileContentArrayBuffer -> Cmd msg
port sha1arraybuffer_get : (String -> msg) -> Sub msg

-- Misc functions

send : msg -> Cmd msg
send msg =
  Task.succeed msg
  |> Task.perform identity
