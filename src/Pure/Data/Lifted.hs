{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface, JavaScriptFFI, BangPatterns, ViewPatterns, FlexibleContexts #-}
module Pure.Data.Lifted where

-- | This module exports functions for interacting with the DOM that are
-- conditionally compiled to no-ops on GHC.

-- from base
import Control.Monad (when,join)
import Data.Coerce (Coercible())
import Data.Int
import Data.IORef (newIORef,readIORef,writeIORef)
import Prelude hiding (head)

-- pure-core
import Pure.Data.View (JSV,toJSV,IsNode(..),Win(..),Doc(..),Body(..),Head(..),Element(..),Text(..),Node(..),Frag(..),History(..),Options(..),Head(..),Evt(..))
import Pure.Data.Txt (Txt)

-- from ghcjs-base
#ifdef __GHCJS__
import GHCJS.Foreign.Callback (Callback,syncCallback1,OnBlocked(..),releaseCallback)
import GHCJS.Marshal.Pure (PFromJSVal(..))
import GHCJS.Marshal (FromJSVal(..))
import GHCJS.Types hiding (isNull)
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = document" document :: Doc

foreign import javascript unsafe
  "$r = document.head" head :: Head

foreign import javascript unsafe
  "$r = window" window :: Win

foreign import javascript unsafe
  "$r = window.history" history :: History

foreign import javascript unsafe
  "$r = document.body" body :: Body

-- note this is only for objects
foreign import javascript unsafe
  "$r = $1 === $2" reference_equality_js :: JSV -> JSV -> Bool

foreign import javascript unsafe
  "$1.appendChild($2)" append_child_js :: Node -> Node -> IO ()

foreign import javascript unsafe
  "$1.insertBefore($2,$3)" insert_before_js :: Element -> Node -> Node -> IO ()

foreign import javascript unsafe
  "$1.insertBefore($2, $1.children[$3]);" insert_at_js :: Element -> Node -> Int -> IO ()

foreign import javascript unsafe
  "$1.innerHTML = $2" set_inner_html_js :: Element -> Txt -> IO ()

-- This is a quick fix for a Component issue where content has yet to be rendered,
-- but a setState changes the view.  We should track rendered state and queue calls
-- to setState to be run after the first render.  For now, just check if a parentNode
-- exists. This fix should only affect code that was previously broken.
foreign import javascript unsafe
  "if ($1.parentNode) { $1.parentNode.replaceChild($2,$1); }" replace_node_js :: Node -> Node -> IO ()

foreign import javascript unsafe
  "$1.textContent=$2" replace_text_js :: Text -> Txt -> IO ()

foreign import javascript unsafe
  "$1.innerHTML = ''" clear_node_js :: Node -> IO ()

foreign import javascript unsafe
  "$1[$2] = $3" set_property_js :: Element -> Txt -> Txt -> IO ()

foreign import javascript unsafe
  "$1.removeAttribute($2)" remove_attribute_js :: Element -> Txt -> IO ()

foreign import javascript unsafe
  "$1.removeAttributeNS($2,$3)" remove_attribute_ns_js :: Element -> Txt -> Txt -> IO ()

foreign import javascript unsafe
  "$1.style[$2] = null" remove_style_js :: Element -> Txt -> IO ()

foreign import javascript unsafe
  "$1[$2] = null" remove_property_js :: Element -> Txt -> IO ()

foreign import javascript unsafe
  "$r = document.createElement($1)" create_element_js :: Txt -> IO Element

foreign import javascript unsafe
  "$r = document.createElementNS($1,$2)" create_element_ns_js :: Txt -> Txt -> IO Element

foreign import javascript unsafe
  "$r = document.createTextNode($1)" create_text_js :: Txt -> IO Text

foreign import javascript unsafe
  "$r = document.createDocumentFragment()" create_frag_js :: IO Frag

foreign import javascript unsafe
  "$r = document.getElementById($1)" get_element_by_id_js :: Txt -> IO Element

foreign import javascript unsafe
  "$r = $1 === null" is_null_js :: JSV -> Bool

foreign import javascript unsafe
  "$r = document.getElementsByTagName($1)[0]" get_first_element_by_tag_name_js :: Txt -> IO Element

foreign import javascript unsafe
  "$1.parentNode.removeChild($1)" remove_js :: Node -> IO ()

-- The difference between childNodes and children:
-- https://stackoverflow.com/questions/7935689/what-is-the-difference-between-children-and-childnodes-in-javascript
foreign import javascript unsafe
  "$r = $1.childNodes[$2]" get_child_js :: Node -> Int -> IO Node

foreign import javascript unsafe
  "$r = $1.textContent" text_content_js :: Node -> IO Txt

foreign import javascript unsafe
  "$1.setAttribute($2,$3)" set_attribute_js :: Element -> Txt -> Txt -> IO ()

foreign import javascript unsafe
  "$1.style[$2] = $3" set_style_js :: Element -> Txt -> Txt -> IO ()

foreign import javascript unsafe
  "$1.preventDefault()" prevent_default_js :: JSV -> IO ()

foreign import javascript unsafe
  "$1.stopPropagation()" stop_propagation_js :: JSV -> IO ()

foreign import javascript unsafe
  "$1.addEventListener($2,$3,{passive:$4})" add_event_listener_js :: JSV -> Txt -> Callback (JSV -> IO ()) -> Bool -> IO ()

foreign import javascript unsafe
  "$1.removeEventListener($2,$3)" remove_event_listener_js :: JSV -> Txt -> Callback (JSV -> IO ()) -> IO ()

foreign import javascript unsafe
  "$r = window.requestAnimationFrame($1)" request_animation_frame_js :: Callback (JSV -> IO ()) -> IO Int

foreign import javascript unsafe
  "window.cancelAnimationFrame($1)" cancel_animation_frame_js :: Int -> IO ()

foreign import javascript unsafe
  "history.pushState({},'',$1)" push_state_js :: Txt -> IO ()

foreign import javascript unsafe
  "$1.setAttributeNS($2,$3,$4)" set_attribute_ns_js :: Element -> Txt -> Txt -> Txt -> IO ()

foreign import javascript unsafe
  "dispatchEvent(new PopStateEvent('popstate',{}))" pop_state_js :: IO ()

foreign import javascript unsafe
  "window.scrollTo(0,0)" scroll_to_top_js :: IO ()

foreign import javascript unsafe
  "$1.click()" click_js :: Node -> IO ()

foreign import javascript unsafe
  "$1.blur()" blur_js :: Node -> IO ()

foreign import javascript unsafe
  "$1.focus()" focus_js :: Node -> IO ()

foreign import javascript unsafe
  "while ($1.firstChild) $1.removeChild($1.firstChild)" clear_js :: Node -> IO ()

foreign import javascript unsafe
  "$r = location.pathname" pathname_js :: IO Txt

foreign import javascript unsafe
  "$r = location.search" search_js :: IO Txt

foreign import javascript unsafe
  "$r = $1[$2]" get_prop_unsafe_js :: JSV -> Txt -> IO JSV

foreign import javascript unsafe
  "$r = $1[$2]" get_prop_unsafe_js_pure :: JSV -> Txt -> JSV

foreign import javascript unsafe
  "$1.preventDefault" prev_def_js :: JSV -> IO ()

foreign import javascript unsafe
  "$1.stopPropagation" prev_prop_js :: JSV -> IO ()

{-# INLINABLE prevDef #-}
prevDef :: Evt -> IO ()
prevDef ev = prev_def_js (evtObj ev)

{-# INLINABLE prevProp #-}
prevProp :: Evt -> IO ()
prevProp ev = prev_prop_js (evtObj ev)

{-# INLINABLE onRaw #-}
onRaw :: Node -> Txt -> Options -> (IO () -> JSV -> IO ()) -> IO (IO ())
onRaw n nm os f = do
  stopper <- newIORef undefined
  cb <- syncCallback1 ContinueAsync $ \ev -> do
    when (preventDef os) (preventDefault ev)
    when (stopProp os) (stopPropagation ev)
    f (join $ readIORef stopper) ev
  writeIORef stopper $ do
    removeEventListener n nm cb
    releaseCallback cb
  addEventListener n nm cb (passive os)
  return (join $ readIORef stopper)

{-# INLINABLE (.#) #-}
(.#) :: PFromJSVal a => JSV -> Txt -> Maybe a
(.#) jsv t =
  let v = get_prop_unsafe_js_pure jsv t
  in if isNull v || isUndefined v then Nothing else Just (pFromJSVal v)

{-# INLINABLE (..#) #-}
(..#) :: FromJSVal a => JSV -> Txt -> IO (Maybe a)
(..#) jsv t = do
  v <- get_prop_unsafe_js jsv t
  if isNull v || isUndefined v then return Nothing else fromJSVal v

{-# INLINABLE create #-}
create :: Txt -> IO Element
create tag = create_element_js tag

{-# INLINABLE createNS #-}
createNS :: Txt -> Txt -> IO Element
createNS ns tag = create_element_ns_js ns tag

{-# INLINABLE createText #-}
createText :: Txt -> IO Text
createText txt = create_text_js txt

{-# INLINABLE createFrag #-}
createFrag :: IO Frag
createFrag = create_frag_js

{-# INLINABLE append #-}
append :: (IsNode child) => Node -> child -> IO ()
append parent (toNode -> child) = append_child_js parent child

{-# INLINABLE insertBefore #-}
insertBefore :: Element -> Node -> Node -> IO ()
insertBefore parent new ref = insert_before_js parent new ref

{-# INLINABLE insertAt #-}
insertAt :: Element -> Node -> Int -> IO ()
insertAt parent new idx = insert_at_js parent new idx

{-# INLINABLE setInnerHTML #-}
setInnerHTML :: Element -> Txt -> IO ()
setInnerHTML e t = set_inner_html_js e t

{-# INLINABLE replaceNode #-}
replaceNode :: Node -> Node -> IO ()
replaceNode n1 n2 = replace_node_js n1 n2

{-# INLINABLE replaceText #-}
replaceText :: Text -> Txt -> IO ()
replaceText n t = replace_text_js n t

{-# INLINABLE findByTag #-}
findByTag :: Txt -> IO (Maybe Element)
findByTag tag = do
  e <- get_first_element_by_tag_name_js tag
  if isNull e
    then return Nothing
    else return (Just e)

{-# INLINABLE findById #-}
findById :: Txt -> IO (Maybe Element)
findById i = do
  e <- get_element_by_id_js i
  if isNull e
    then return Nothing
    else return (Just e)

{-# INLINABLE removeNode #-}
removeNode :: IsNode n => n -> IO ()
removeNode (toNode -> n) = remove_js n

{-# INLINABLE same #-}
same :: (Coercible j1 JSV, Coercible j2 JSV) => j1 -> j2 -> Bool
same (toJSV -> j1) (toJSV -> j2) = reference_equality_js j1 j2

{-# INLINABLE isNull #-}
isNull :: (Coercible j JSV) => j -> Bool
isNull (toJSV -> j) = is_null_js j

{-# INLINABLE getChild #-}
getChild :: IsNode n => n -> Int -> IO (Maybe Node)
getChild (toNode -> n) i = do
  mn <- get_child_js n i
  if isNull mn
    then return Nothing
    else return (Just mn)

{-# INLINABLE textContent #-}
textContent :: IsNode n => n -> IO Txt
textContent (toNode -> n) = text_content_js n

{-# INLINABLE setAttribute #-}
setAttribute :: Element -> Txt -> Txt -> IO ()
setAttribute e k v = set_attribute_js e k v

{-# INLINABLE setProperty #-}
setProperty :: Element -> Txt -> Txt -> IO ()
setProperty e k v = set_property_js e k v

{-# INLINABLE setStyle #-}
setStyle :: Element -> Txt -> Txt -> IO ()
setStyle e k v = set_style_js e k v

{-# INLINABLE removeAttribute #-}
removeAttribute :: Element -> Txt -> IO ()
removeAttribute e k = remove_attribute_js e k

{-# INLINABLE removeAttributeNS #-}
removeAttributeNS :: Element -> Txt -> Txt -> IO ()
removeAttributeNS e k v = remove_attribute_ns_js e k v

{-# INLINABLE removeProperty #-}
removeProperty :: Element -> Txt -> IO ()
removeProperty e p = remove_property_js e p

{-# INLINABLE removeStyle #-}
removeStyle :: Element -> Txt -> IO ()
removeStyle e s = remove_style_js e s

{-# INLINABLE preventDefault #-}
preventDefault :: JSV -> IO ()
preventDefault e = prevent_default_js e

{-# INLINABLE stopPropagation #-}
stopPropagation :: JSV -> IO ()
stopPropagation e = stop_propagation_js e

{-# INLINABLE addEventListener #-}
addEventListener :: Coercible target JSV => target -> Txt -> Callback (JSV -> IO ()) -> Bool -> IO ()
addEventListener (toJSV -> target) e cb p = add_event_listener_js target e cb p

{-# INLINABLE cancelAnimationFrame #-}
cancelAnimationFrame :: Int -> IO ()
cancelAnimationFrame af = cancel_animation_frame_js af

{-# INLINABLE removeEventListener #-}
removeEventListener :: Coercible target JSV => target -> Txt -> Callback (JSV -> IO ()) -> IO ()
removeEventListener (toJSV -> target) e cb = remove_event_listener_js target e cb

{-# INLINABLE requestAnimationFrame #-}
requestAnimationFrame :: Callback (JSV -> IO ()) -> IO Int
requestAnimationFrame cb = request_animation_frame_js cb

{-# INLINABLE getWindow #-}
getWindow :: IO Win
getWindow = return window

{-# INLINABLE getBody #-}
getBody :: IO Body
getBody = return body

{-# INLINABLE getDocument #-}
getDocument :: IO Doc
getDocument = return document

{-# INLINABLE getHead #-}
getHead :: IO Head
getHead = return head

{-# INLINABLE getHistory #-}
getHistory :: IO History
getHistory = return history

{-# INLINABLE pushState #-}
pushState :: Txt -> IO ()
pushState h = push_state_js h

{-# INLINABLE setAttributeNS #-}
setAttributeNS :: Element -> Txt -> Txt -> Txt -> IO ()
setAttributeNS e ns k v = set_attribute_ns_js e ns k v

{-# INLINABLE popState #-}
popState :: IO ()
popState = pop_state_js

{-# INLINABLE scrollToTop #-}
scrollToTop :: IO ()
scrollToTop = scroll_to_top_js

{-# INLINABLE clickNode #-}
clickNode :: Node -> IO ()
clickNode n = click_js n

{-# INLINABLE blurNode #-}
blurNode :: Node -> IO ()
blurNode n = blur_js n

{-# INLINABLE focusNode #-}
focusNode :: Node -> IO ()
focusNode n = focus_js n

{-# INLINABLE clear #-}
clear :: Node -> IO ()
clear n = clear_js n

{-# INLINABLE getPathname #-}
getPathname :: IO Txt
getPathname = pathname_js

{-# INLINABLE getSearch #-}
getSearch :: IO Txt
getSearch = search_js
#else

----------------------------------------
-- GHC
--
-- We can get away with mostly noops for
-- diffing, creation, append, etc....
--
-- Not sure which noops will be needed,
-- though, so I'm implementing them all
-- just to be safe.

prevDef :: Evt -> IO ()
prevDef _ = return ()

prevProp :: Evt -> IO ()
prevProp _ = return ()

onRaw :: Node -> Txt -> Options -> (IO () -> JSV -> IO ()) -> IO (IO ())
onRaw n nm os f = return (return ())

(.#) :: JSV -> Txt -> Maybe a
(.#) _ _ = Nothing

(..#) :: JSV -> Txt -> IO (Maybe a)
(..#) _ _ = return Nothing

create :: Txt -> IO Element
create _ = return (Element ())

createNS :: Txt -> Txt -> IO Element
createNS _ _ = return (Element ())

createText :: Txt -> IO Text
createText _ = return (Text ())

createFrag :: IO Frag
createFrag = return (Frag ())

append :: (IsNode child) => Node -> child -> IO ()
append _ _ = return ()

insertBefore :: Element -> Node -> Node -> IO ()
insertBefore _ _ _ = return ()

insertAt :: Element -> Node -> Int -> IO ()
insertAt _ _ _ = return ()

setInnerHTML :: Element -> Txt -> IO ()
setInnerHTML _ _ = return ()

replaceNode :: Node -> Node -> IO ()
replaceNode _ _ = return ()

replaceText :: Text -> Txt -> IO ()
replaceText _ _ = return ()

findByTag :: Txt -> IO (Maybe Element)
findByTag _ = return $ Just $ Element ()

findById :: Txt -> IO (Maybe Element)
findById _ = return $ Just $ Element ()

removeNode :: IsNode n => n -> IO ()
removeNode _ = return ()

same :: (Coercible j1 JSV, Coercible j2 JSV) => j1 -> j2 -> Bool
same _ _ = True

isNull :: (Coercible j JSV) => j -> Bool
isNull _ = False

getChild :: IsNode n => n -> Int -> IO (Maybe Node)
getChild _ _ = return (Just $ Node ())

setAttribute :: Element -> Txt -> Txt -> IO ()
setAttribute _ _ _ = return ()

setProperty :: Element -> Txt -> Txt -> IO ()
setProperty _ _ _ = return ()

setStyle :: Element -> Txt -> Txt -> IO ()
setStyle _ _ _ = return ()

removeAttribute :: Element -> Txt -> IO ()
removeAttribute _ _ = return ()

removeAttributeNS :: Element -> Txt -> Txt -> IO ()
removeAttributeNS _ _ _ = return ()

removeProperty :: Element -> Txt -> IO ()
removeProperty _ _ = return ()

removeStyle :: Element -> Txt -> IO ()
removeStyle _ _ = return ()

preventDefault :: JSV -> IO ()
preventDefault _ = return ()

stopPropagation :: JSV -> IO ()
stopPropagation _ = return ()

addEventListener :: Coercible target  JSV=> target -> Txt -> (JSV -> IO ()) -> Bool -> IO ()
addEventListener _ _ _ _ = return ()

removeEventListener :: Coercible target  JSV=> target -> Txt -> (JSV -> IO ()) -> IO ()
removeEventListener _ _ _ = return ()

requestAnimationFrame :: (JSV -> IO ()) -> IO Int64
requestAnimationFrame _ = return 0

cancelAnimationFrame :: Int64 -> IO ()
cancelAnimationFrame _ = return ()

window :: Win
window = Win ()

body :: Body
body = Body ()

getWindow :: IO Win
getWindow = return $ Win ()

document :: Doc
document = Doc ()

getDocument :: IO Doc
getDocument = return $ Doc ()

head :: Head
head = Head ()

getHead :: IO Head
getHead = return $ Head ()

getBody :: IO Body
getBody = return $ Body ()

history :: History
history = History ()

getHistory :: IO History
getHistory = return $ History ()

pushState :: Txt -> IO ()
pushState _ = return ()

popState :: IO ()
popState = return ()

setAttributeNS :: Element -> Txt -> Txt -> Txt -> IO ()
setAttributeNS _ _ _ _ = return ()

scrollToTop :: IO ()
scrollToTop = return ()

clickNode :: Node -> IO ()
clickNode _ = return ()

blurNode :: Node -> IO ()
blurNode _ = return ()

focusNode :: Node -> IO ()
focusNode _ = return ()

clear :: Node -> IO ()
clear _ = return ()

getPathname :: IO Txt
getPathname = return ""

getSearch :: IO Txt
getSearch = return ""
#endif

