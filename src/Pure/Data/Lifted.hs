{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface, JavaScriptFFI, BangPatterns, ViewPatterns, FlexibleContexts, DefaultSignatures #-}
module Pure.Data.Lifted
    ( module Pure.Data.Lifted
#ifdef __GHCJS__
    , module Export
#endif
    ) where

-- from base
import Control.Monad (when,join)
import Data.Coerce (Coercible(),coerce)
import Data.Int
import Data.IORef (newIORef,readIORef,writeIORef)
import Prelude hiding (head)

-- from pure-default
import Pure.Data.Default (Default(..))

-- from pure-txt
import Pure.Data.Txt (Txt)

-- from ghcjs-base
#ifdef __GHCJS__
import GHCJS.Foreign.Callback as Export 
import GHCJS.Marshal.Pure (PFromJSVal(..))
import GHCJS.Marshal (FromJSVal(..))
import GHCJS.Types hiding (isNull)
import qualified JavaScript.Web.AnimationFrame as GHCJS
import qualified GHCJS.Concurrent as GHCJS
#endif

-- from ghcjs-base
#ifdef __GHCJS__
import GHCJS.Types (JSVal(..))

type JSV = JSVal

instance Default JSVal where
  def = nullJSV
#else
type JSV = ()
#endif

newtype Win     = Win JSV
newtype Doc     = Doc JSV
newtype Head    = Head JSV
newtype Body    = Body JSV
newtype Element = Element JSV
newtype Text    = Text JSV
newtype Node    = Node JSV
newtype Frag    = Frag JSV

newtype History = History JSV
newtype Loc     = Loc JSV

{-# INLINE toJSV #-}
toJSV :: Coercible a JSV => a -> JSV
toJSV = coerce

class IsNode e where
  toNode :: e -> Node
  default toNode :: Coercible e Node => e -> Node
  {-# INLINE toNode #-}
  toNode = coerce
instance IsNode Node where
  {-# INLINE toNode #-}
  toNode = id
instance IsNode Body
instance IsNode Head
instance IsNode Element
instance IsNode Text
instance IsNode Frag
instance IsNode Doc
instance IsNode Win

type RAFHandle =
#ifdef __GHCJS__
  GHCJS.AnimationFrameHandle
#else
  JSV
#endif

data Evt = Evt
  { evtObj            :: JSV
  , evtTarget         :: JSV
  , evtRemoveListener :: (IO ())
  }

data Options = Options
  { preventDef :: Bool
  , stopProp   :: Bool
  , passive    :: Bool
  } deriving (Eq,Show)
instance Default Options where
  def = Options False False True

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
  "$2.parentNode.insertBefore($1,$2)" insert_before_js :: Node -> Node -> IO ()

foreign import javascript unsafe
  "$1.insertBefore($2, $1.children[$3]);" insert_at_js :: Element -> Node -> Int -> IO ()

foreign import javascript unsafe
  "Array.prototype.indexOf.call($1.parentNode,$1);" node_index_js :: Node -> IO Int

foreign import javascript unsafe
  "$1.nextSibling" next_sibling_js :: Node -> IO Node

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
  "$1.textContent = ''" clear_node_js :: Node -> IO ()

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
  "$r = null" null_jsv_js :: JSV

foreign import javascript unsafe
  "$r = document.getElementsByTagName($1)[0]" get_first_element_by_tag_name_js :: Txt -> IO Element

foreign import javascript unsafe
  "$1.parentNode.removeChild($1)" remove_js :: Node -> IO ()

foreign import javascript unsafe
  "if ($1.parentNode) { $1.parentNode.removeChild($1); };" remove_maybe_js :: Node -> IO ()

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
  "$r = window.requestIdleCallback($1)" request_idle_callback_js :: Callback (JSV -> IO ()) -> IO Int64

foreign import javascript unsafe
  "window.cancelAnimationFrame($1)" cancel_animation_frame_js :: Int64 -> IO ()

foreign import javascript unsafe
  "window.cancelIdleCallback($1)" cancel_idle_callback_js :: Int64 -> IO ()

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

{-# INLINE prevDef #-}
prevDef :: Evt -> IO ()
prevDef ev = prev_def_js (evtObj ev)

{-# INLINE prevProp #-}
prevProp :: Evt -> IO ()
prevProp ev = prev_prop_js (evtObj ev)

{-# INLINE onRaw #-}
onRaw :: Coercible a JSV => a -> Txt -> Options -> (IO () -> JSV -> IO ()) -> IO (IO ())
onRaw (toJSV -> n) nm os f = do
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

{-# INLINE (.#) #-}
(.#) :: PFromJSVal a => JSV -> Txt -> Maybe a
(.#) jsv t =
  let v = get_prop_unsafe_js_pure jsv t
  in if isNull v || isUndefined v then Nothing else Just (pFromJSVal v)

{-# INLINE (..#) #-}
(..#) :: FromJSVal a => JSV -> Txt -> IO (Maybe a)
(..#) jsv t = do
  v <- get_prop_unsafe_js jsv t
  if isNull v || isUndefined v then return Nothing else fromJSVal v

{-# INLINE nullJSV #-}
nullJSV :: JSV
nullJSV = null_jsv_js

{-# INLINE create #-}
create :: Txt -> IO Element
create = create_element_js

{-# INLINE createNS #-}
createNS :: Txt -> Txt -> IO Element
createNS = create_element_ns_js

{-# INLINE createText #-}
createText :: Txt -> IO Text
createText = create_text_js

{-# INLINE createFrag #-}
createFrag :: IO Frag
createFrag = create_frag_js

{-# INLINE clearNode #-}
clearNode :: IsNode node => node -> IO ()
clearNode = clear_node_js . toNode

{-# INLINE append #-}
append :: IsNode node => Node -> node -> IO ()
append parent child = append_child_js parent (toNode child)

{-# INLINE insertBefore #-}
insertBefore :: Node -> Node -> IO ()
insertBefore = insert_before_js

{-# INLINE insertAt #-}
insertAt :: Element -> Node -> Int -> IO ()
insertAt = insert_at_js

{-# INLINE nodeIndex #-}
nodeIndex :: IsNode node => node -> IO Int
nodeIndex = node_index_js . toNode

{-# INLINE nextSibling#-}
nextSibling :: IsNode node => node -> IO Node
nextSibling = next_sibling_js . toNode

{-# INLINE setInnerHTML #-}
setInnerHTML :: Element -> Txt -> IO ()
setInnerHTML e t = set_inner_html_js e t

{-# INLINE replaceNode #-}
replaceNode :: Node -> Node -> IO ()
replaceNode n1 n2 = replace_node_js n1 n2

{-# INLINE replaceText #-}
replaceText :: Text -> Txt -> IO ()
replaceText n t = replace_text_js n t

{-# INLINE findByTag #-}
findByTag :: Txt -> IO (Maybe Element)
findByTag tag = do
  e <- get_first_element_by_tag_name_js tag
  if isNull e
    then return Nothing
    else return (Just e)

{-# INLINE findById #-}
findById :: Txt -> IO (Maybe Element)
findById i = do
  e <- get_element_by_id_js i
  if isNull e
    then return Nothing
    else return (Just e)

{-# INLINE removeNode #-}
removeNode :: IsNode n => n -> IO ()
removeNode (toNode -> n) = remove_js n

{-# INLINE removeNodeMaybe #-}
removeNodeMaybe :: IsNode n => n -> IO ()
removeNodeMaybe = remove_maybe_js . toNode

{-# INLINE same #-}
same :: (Coercible j1 JSV, Coercible j2 JSV) => j1 -> j2 -> Bool
same (toJSV -> j1) (toJSV -> j2) = reference_equality_js j1 j2

{-# INLINE isNull #-}
isNull :: (Coercible j JSV) => j -> Bool
isNull (toJSV -> j) = is_null_js j

{-# INLINE getChild #-}
getChild :: IsNode n => n -> Int -> IO (Maybe Node)
getChild (toNode -> n) i = do
  mn <- get_child_js n i
  if isNull mn
    then return Nothing
    else return (Just mn)

{-# INLINE textContent #-}
textContent :: IsNode n => n -> IO Txt
textContent (toNode -> n) = text_content_js n

{-# INLINE setAttribute #-}
setAttribute :: Element -> Txt -> Txt -> IO ()
setAttribute e k v = set_attribute_js e k v

{-# INLINE setProperty #-}
setProperty :: Element -> Txt -> Txt -> IO ()
setProperty e k v = set_property_js e k v

{-# INLINE setStyle #-}
setStyle :: Element -> Txt -> Txt -> IO ()
setStyle e k v = set_style_js e k v

{-# INLINE removeAttribute #-}
removeAttribute :: Element -> Txt -> IO ()
removeAttribute e k = remove_attribute_js e k

{-# INLINE removeAttributeNS #-}
removeAttributeNS :: Element -> Txt -> Txt -> IO ()
removeAttributeNS e k v = remove_attribute_ns_js e k v

{-# INLINE removeProperty #-}
removeProperty :: Element -> Txt -> IO ()
removeProperty e p = remove_property_js e p

{-# INLINE removeStyle #-}
removeStyle :: Element -> Txt -> IO ()
removeStyle e s = remove_style_js e s

{-# INLINE preventDefault #-}
preventDefault :: JSV -> IO ()
preventDefault e = prevent_default_js e

{-# INLINE stopPropagation #-}
stopPropagation :: JSV -> IO ()
stopPropagation e = stop_propagation_js e

{-# INLINE addEventListener #-}
addEventListener :: Coercible target JSV => target -> Txt -> Callback (JSV -> IO ()) -> Bool -> IO ()
addEventListener (toJSV -> target) e cb p = add_event_listener_js target e cb p

{-# INLINE cancelAnimationFrame #-}
cancelAnimationFrame :: RAFHandle -> IO ()
cancelAnimationFrame = cancelAnimationFrame

{-# INLINE cancelIdleCallback #-}
cancelIdleCallback :: Int64 -> IO ()
cancelIdleCallback cb = cancel_idle_callback_js cb

{-# INLINE removeEventListener #-}
removeEventListener :: Coercible target JSV => target -> Txt -> Callback (JSV -> IO ()) -> IO ()
removeEventListener (toJSV -> target) e cb = remove_event_listener_js target e cb

{-# INLINE requestAnimationFrame #-}
requestAnimationFrame :: (Double -> IO ()) -> IO RAFHandle
requestAnimationFrame f = GHCJS.inAnimationFrame ContinueAsync (GHCJS.withoutPreemption . f)

{-# INLINE requestIdleCallback #-}
requestIdleCallback :: Callback (JSV -> IO ()) -> IO Int64
requestIdleCallback cb = request_idle_callback_js cb

{-# INLINE getWindow #-}
getWindow :: IO Win
getWindow = return window

{-# INLINE getBody #-}
getBody :: IO Body
getBody = return body

{-# INLINE getDocument #-}
getDocument :: IO Doc
getDocument = return document

{-# INLINE getHead #-}
getHead :: IO Head
getHead = return head

{-# INLINE getHistory #-}
getHistory :: IO History
getHistory = return history

{-# INLINE pushState #-}
pushState :: Txt -> IO ()
pushState h = push_state_js h

{-# INLINE setAttributeNS #-}
setAttributeNS :: Element -> Txt -> Txt -> Txt -> IO ()
setAttributeNS e ns k v = set_attribute_ns_js e ns k v

{-# INLINE popState #-}
popState :: IO ()
popState = pop_state_js

{-# INLINE scrollToTop #-}
scrollToTop :: IO ()
scrollToTop = scroll_to_top_js

{-# INLINE clickNode #-}
clickNode :: Node -> IO ()
clickNode n = click_js n

{-# INLINE blurNode #-}
blurNode :: Node -> IO ()
blurNode n = blur_js n

{-# INLINE focusNode #-}
focusNode :: Node -> IO ()
focusNode n = focus_js n

{-# INLINE clear #-}
clear :: Node -> IO ()
clear n = clear_js n

{-# INLINE getPathname #-}
getPathname :: IO Txt
getPathname = pathname_js

{-# INLINE getSearch #-}
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

onRaw :: Coercible a JSV => a -> Txt -> Options -> (IO () -> JSV -> IO ()) -> IO (IO ())
onRaw n nm os f = return (return ())

(.#) :: JSV -> Txt -> Maybe a
(.#) _ _ = Nothing

(..#) :: JSV -> Txt -> IO (Maybe a)
(..#) _ _ = return Nothing

nullJSV :: JSV
nullJSV = ()

create :: Txt -> IO Element
create _ = return (Element ())

createNS :: Txt -> Txt -> IO Element
createNS _ _ = return (Element ())

createText :: Txt -> IO Text
createText _ = return (Text ())

createFrag :: IO Frag
createFrag = return (Frag ())

clearNode :: (IsNode node) => node -> IO ()
clearNode _ = return ()

append :: IsNode node => Node -> node -> IO ()
append _ _ = return ()

insertBefore :: Node -> Node -> IO ()
insertBefore _ _ = return ()

insertAt :: Element -> Node -> Int -> IO ()
insertAt _ _ _ = return ()

setInnerHTML :: Element -> Txt -> IO ()
setInnerHTML _ _ = return ()

nodeIndex :: IsNode node => node -> IO Int
nodeIndex _ = return 0

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

removeNodeMaybe :: IsNode n => n -> IO ()
removeNodeMaybe _ = return ()

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

requestAnimationFrame :: (Double -> IO ()) -> IO RAFHandle
requestAnimationFrame f = f 0

requestIdleCallback :: (JSV -> IO ()) -> IO Int64
requestIdleCallback f = f () >> return 0

cancelAnimationFrame :: RAFHandle -> IO ()
cancelAnimationFrame _ = return ()

cancelIdleCallback :: Int64 -> IO ()
cancelIdleCallback _ = return ()

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

