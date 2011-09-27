{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | CSS generation.

module Language.CSS.Properties where

import Language.CSS.Types   (CSS,Rule,Property(..))

import Control.Monad.Writer (MonadWriter,tell)
import Data.Text.Lazy       (Text)

-- | Make a CSS property.
prop :: Text -> Text -> CSS (Either Property Rule)
prop name value = tell [Left $ Property name value]

-- Microsoft

-- | CSS property: -ms-filter
msFilter :: Text -> CSS (Either Property Rule)
msFilter = prop "-ms-filter"

-- Additional ones from CSS3

-- | CSS property: opacity
opacity :: Text -> CSS (Either Property Rule)
opacity = prop "opacity"

-- | CSS property: border-radius
borderRadius :: Text -> CSS (Either Property Rule)
borderRadius = prop "border-radius"

-- | CSS property: text-shadow
textShadow :: Text -> CSS (Either Property Rule)
textShadow = prop "text-shadow"

-- | CSS property: box-shadow
boxShadow :: Text -> CSS (Either Property Rule)
boxShadow = prop "box-shadow"

-- | CSS property: border-top-leftradius
borderTopLeftRadius :: Text -> CSS (Either Property Rule)
borderTopLeftRadius = prop "border-top-left-radius"

-- | CSS property: border-bottom-leftradius
borderBottomLeftRadius :: Text -> CSS (Either Property Rule)
borderBottomLeftRadius = prop "border-bottom-left-radius"

-- | CSS property: border-top-rightradius
borderTopRightRadius :: Text -> CSS (Either Property Rule)
borderTopRightRadius = prop "border-top-right-radius"

-- | CSS property: border-bottom-rightradius
borderBottomRightRadius :: Text -> CSS (Either Property Rule)
borderBottomRightRadius = prop "border-bottom-right-radius"

-- Generated from http://www.w3.org/TR/CSS21/propidx.html

-- | CSS property: azimuth
azimuth :: Text -> CSS (Either Property Rule)
azimuth = prop "azimuth"

-- | CSS property: background-color
backgroundColor :: Text -> CSS (Either Property Rule)
backgroundColor = prop "background-color"

-- | CSS property: background-image
backgroundImage :: Text -> CSS (Either Property Rule)
backgroundImage = prop "background-image"

-- | CSS property: background-position
backgroundPosition :: Text -> CSS (Either Property Rule)
backgroundPosition = prop "background-position"

-- | CSS property: background-repeat
backgroundRepeat :: Text -> CSS (Either Property Rule)
backgroundRepeat = prop "background-repeat"

-- | CSS property: background
background :: Text -> CSS (Either Property Rule)
background = prop "background"

-- | CSS property: border-collapse
borderCollapse :: Text -> CSS (Either Property Rule)
borderCollapse = prop "border-collapse"

-- | CSS property: border-color
borderColor :: Text -> CSS (Either Property Rule)
borderColor = prop "border-color"

-- | CSS property: border-spacing
borderSpacing :: Text -> CSS (Either Property Rule)
borderSpacing = prop "border-spacing"

-- | CSS property: border-style
borderStyle :: Text -> CSS (Either Property Rule)
borderStyle = prop "border-style"

-- | CSS property: border-top
borderTop :: Text -> CSS (Either Property Rule)
borderTop = prop "border-top"

-- | CSS property: border-bottom
borderBottom :: Text -> CSS (Either Property Rule)
borderBottom = prop "border-bottom"

-- | CSS property: border-top-color
borderTopColor :: Text -> CSS (Either Property Rule)
borderTopColor = prop "border-top-color"

-- | CSS property: border-top-style
borderTopStyle :: Text -> CSS (Either Property Rule)
borderTopStyle = prop "border-top-style"

-- | CSS property: border-top-width
borderTopWidth :: Text -> CSS (Either Property Rule)
borderTopWidth = prop "border-top-width"

-- | CSS property: border-width
borderWidth :: Text -> CSS (Either Property Rule)
borderWidth = prop "border-width"

-- | CSS property: border
border :: Text -> CSS (Either Property Rule)
border = prop "border"

-- | CSS property: border-right
borderRight :: Text -> CSS (Either Property Rule)
borderRight = prop "border-right"

-- | CSS property: border-left
borderLeft :: Text -> CSS (Either Property Rule)
borderLeft = prop "border-left"

-- | CSS property: bottom
bottom :: Text -> CSS (Either Property Rule)
bottom = prop "bottom"

-- | CSS property: caption-side
captionSide :: Text -> CSS (Either Property Rule)
captionSide = prop "caption-side"

-- | CSS property: clear
clear :: Text -> CSS (Either Property Rule)
clear = prop "clear"

-- | CSS property: clip
clip :: Text -> CSS (Either Property Rule)
clip = prop "clip"

-- | CSS property: color
color :: Text -> CSS (Either Property Rule)
color = prop "color"

-- | CSS property: content
content :: Text -> CSS (Either Property Rule)
content = prop "content"

-- | CSS property: counter-increment
counterIncrement :: Text -> CSS (Either Property Rule)
counterIncrement = prop "counter-increment"

-- | CSS property: counter-reset
counterReset :: Text -> CSS (Either Property Rule)
counterReset = prop "counter-reset"

-- | CSS property: cue-after
cueAfter :: Text -> CSS (Either Property Rule)
cueAfter = prop "cue-after"

-- | CSS property: cue-before
cueBefore :: Text -> CSS (Either Property Rule)
cueBefore = prop "cue-before"

-- | CSS property: cue
cue :: Text -> CSS (Either Property Rule)
cue = prop "cue"

-- | CSS property: cursor
cursor :: Text -> CSS (Either Property Rule)
cursor = prop "cursor"

-- | CSS property: direction
direction :: Text -> CSS (Either Property Rule)
direction = prop "direction"

-- | CSS property: display
display :: Text -> CSS (Either Property Rule)
display = prop "display"

-- | CSS property: elevation
elevation :: Text -> CSS (Either Property Rule)
elevation = prop "elevation"

-- | CSS property: empty-cells
emptyCells :: Text -> CSS (Either Property Rule)
emptyCells = prop "empty-cells"

-- | CSS property: float
float :: Text -> CSS (Either Property Rule)
float = prop "float"

-- | CSS property: font-family
fontFamily :: Text -> CSS (Either Property Rule)
fontFamily = prop "font-family"

-- | CSS property: font-size
fontSize :: Text -> CSS (Either Property Rule)
fontSize = prop "font-size"

-- | CSS property: font-style
fontStyle :: Text -> CSS (Either Property Rule)
fontStyle = prop "font-style"

-- | CSS property: font-variant
fontVariant :: Text -> CSS (Either Property Rule)
fontVariant = prop "font-variant"

-- | CSS property: font-weight
fontWeight :: Text -> CSS (Either Property Rule)
fontWeight = prop "font-weight"

-- | CSS property: font
font :: Text -> CSS (Either Property Rule)
font = prop "font"

-- | CSS property: height
height :: Text -> CSS (Either Property Rule)
height = prop "height"

-- | CSS property: left
left :: Text -> CSS (Either Property Rule)
left = prop "left"

-- | CSS property: letter-spacing
letterSpacing :: Text -> CSS (Either Property Rule)
letterSpacing = prop "letter-spacing"

-- | CSS property: line-height
lineHeight :: Text -> CSS (Either Property Rule)
lineHeight = prop "line-height"

-- | CSS property: list-style-image
listStyleImage :: Text -> CSS (Either Property Rule)
listStyleImage = prop "list-style-image"

-- | CSS property: list-style-position
listStylePosition :: Text -> CSS (Either Property Rule)
listStylePosition = prop "list-style-position"

-- | CSS property: list-style-type
listStyleType :: Text -> CSS (Either Property Rule)
listStyleType = prop "list-style-type"

-- | CSS property: list-style
listStyle :: Text -> CSS (Either Property Rule)
listStyle = prop "list-style"

-- | CSS property: margin-right
marginRight :: Text -> CSS (Either Property Rule)
marginRight = prop "margin-right"

-- | CSS property: margin-left
marginLeft :: Text -> CSS (Either Property Rule)
marginLeft = prop "margin-left"

-- | CSS property: margin-top
marginTop :: Text -> CSS (Either Property Rule)
marginTop = prop "margin-top"

-- | CSS property: margin-bottom
marginBottom :: Text -> CSS (Either Property Rule)
marginBottom = prop "margin-bottom"

-- | CSS property: margin
margin :: Text -> CSS (Either Property Rule)
margin = prop "margin"

-- | CSS property: max-height
maxHeight :: Text -> CSS (Either Property Rule)
maxHeight = prop "max-height"

-- | CSS property: max-width
maxWidth :: Text -> CSS (Either Property Rule)
maxWidth = prop "max-width"

-- | CSS property: min-height
minHeight :: Text -> CSS (Either Property Rule)
minHeight = prop "min-height"

-- | CSS property: min-width
minWidth :: Text -> CSS (Either Property Rule)
minWidth = prop "min-width"

-- | CSS property: orphans
orphans :: Text -> CSS (Either Property Rule)
orphans = prop "orphans"

-- | CSS property: outline-color
outlineColor :: Text -> CSS (Either Property Rule)
outlineColor = prop "outline-color"

-- | CSS property: outline-style
outlineStyle :: Text -> CSS (Either Property Rule)
outlineStyle = prop "outline-style"

-- | CSS property: outline-width
outlineWidth :: Text -> CSS (Either Property Rule)
outlineWidth = prop "outline-width"

-- | CSS property: outline
outline :: Text -> CSS (Either Property Rule)
outline = prop "outline"

-- | CSS property: overflow
overflow :: Text -> CSS (Either Property Rule)
overflow = prop "overflow"

-- | CSS property: padding-top
paddingTop :: Text -> CSS (Either Property Rule)
paddingTop = prop "padding-top"

-- | CSS property: padding-bottom
paddingBottom :: Text -> CSS (Either Property Rule)
paddingBottom = prop "padding-bottom"

-- | CSS property: padding-left
paddingLeft :: Text -> CSS (Either Property Rule)
paddingLeft = prop "padding-left"

-- | CSS property: padding-right
paddingRight :: Text -> CSS (Either Property Rule)
paddingRight = prop "padding-right"

-- | CSS property: padding
padding :: Text -> CSS (Either Property Rule)
padding = prop "padding"

-- | CSS property: page-break-after
pageBreakAfter :: Text -> CSS (Either Property Rule)
pageBreakAfter = prop "page-break-after"

-- | CSS property: page-break-before
pageBreakBefore :: Text -> CSS (Either Property Rule)
pageBreakBefore = prop "page-break-before"

-- | CSS property: page-break-inside
pageBreakInside :: Text -> CSS (Either Property Rule)
pageBreakInside = prop "page-break-inside"

-- | CSS property: pause-after
pauseAfter :: Text -> CSS (Either Property Rule)
pauseAfter = prop "pause-after"

-- | CSS property: pause-before
pauseBefore :: Text -> CSS (Either Property Rule)
pauseBefore = prop "pause-before"

-- | CSS property: pause
pause :: Text -> CSS (Either Property Rule)
pause = prop "pause"

-- | CSS property: pitch-range
pitchRange :: Text -> CSS (Either Property Rule)
pitchRange = prop "pitch-range"

-- | CSS property: pitch
pitch :: Text -> CSS (Either Property Rule)
pitch = prop "pitch"

-- | CSS property: play-during
playDuring :: Text -> CSS (Either Property Rule)
playDuring = prop "play-during"

-- | CSS property: position
position :: Text -> CSS (Either Property Rule)
position = prop "position"

-- | CSS property: quotes
quotes :: Text -> CSS (Either Property Rule)
quotes = prop "quotes"

-- | CSS property: richness
richness :: Text -> CSS (Either Property Rule)
richness = prop "richness"

-- | CSS property: right
right :: Text -> CSS (Either Property Rule)
right = prop "right"

-- | CSS property: speak-header
speakHeader :: Text -> CSS (Either Property Rule)
speakHeader = prop "speak-header"

-- | CSS property: speak-numeral
speakNumeral :: Text -> CSS (Either Property Rule)
speakNumeral = prop "speak-numeral"

-- | CSS property: speak-punctuation
speakPunctuation :: Text -> CSS (Either Property Rule)
speakPunctuation = prop "speak-punctuation"

-- | CSS property: speak
speak :: Text -> CSS (Either Property Rule)
speak = prop "speak"

-- | CSS property: speech-rate
speechRate :: Text -> CSS (Either Property Rule)
speechRate = prop "speech-rate"

-- | CSS property: stress
stress :: Text -> CSS (Either Property Rule)
stress = prop "stress"

-- | CSS property: table-layout
tableLayout :: Text -> CSS (Either Property Rule)
tableLayout = prop "table-layout"

-- | CSS property: text-align
textAlign :: Text -> CSS (Either Property Rule)
textAlign = prop "text-align"

-- | CSS property: text-decoration
textDecoration :: Text -> CSS (Either Property Rule)
textDecoration = prop "text-decoration"

-- | CSS property: text-indent
textIndent :: Text -> CSS (Either Property Rule)
textIndent = prop "text-indent"

-- | CSS property: text-transform
textTransform :: Text -> CSS (Either Property Rule)
textTransform = prop "text-transform"

-- | CSS property: top
top :: Text -> CSS (Either Property Rule)
top = prop "top"

-- | CSS property: unicode-bidi
unicodeBidi :: Text -> CSS (Either Property Rule)
unicodeBidi = prop "unicode-bidi"

-- | CSS property: vertical-align
verticalAlign :: Text -> CSS (Either Property Rule)
verticalAlign = prop "vertical-align"

-- | CSS property: visibility
visibility :: Text -> CSS (Either Property Rule)
visibility = prop "visibility"

-- | CSS property: voice-family
voiceFamily :: Text -> CSS (Either Property Rule)
voiceFamily = prop "voice-family"

-- | CSS property: volume
volume :: Text -> CSS (Either Property Rule)
volume = prop "volume"

-- | CSS property: white-space
whiteSpace :: Text -> CSS (Either Property Rule)
whiteSpace = prop "white-space"

-- | CSS property: widows
widows :: Text -> CSS (Either Property Rule)
widows = prop "widows"

-- | CSS property: width
width :: Text -> CSS (Either Property Rule)
width = prop "width"

-- | CSS property: word-spacing
wordSpacing :: Text -> CSS (Either Property Rule)
wordSpacing = prop "word-spacing"

-- | CSS property: z-index
zIndex :: Text -> CSS (Either Property Rule)
zIndex = prop "z-index"
