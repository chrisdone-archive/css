# A very simple Haskell DSL for CSS.

## Example:

```haskell
-- | Input style.
inputs :: CSS Rule
inputs =
  rule "form p label" $ do
    rule "textarea" $ do
      width "100%"
      height "20em"
      clear "both"
      margin "1em 0 0 0"

    rule "textarea, input.text" $ do
      border "2px solid #ddd"
      borderRadius "4px"
    rule "textarea:focus, input.text:focus" $ do
      background "#eee"

    rule "span" $ do
      float "left"
      width "7em"
      display "block"
```

## Output:

    λ> renderCSS $ runCSS inputs
    "form p label
    textarea{width:100%;height:20em;clear:both;margin:1em 0 0 0}form p
    label textarea, input.text{border:2px solid
    #ddd;border-radius:4px}form p label textarea:focus,
    input.text:focus{background:#eee}form p label
    span{float:left;width:7em;display:block}

## Pretty printed:

    λ> T.putStrLn $ renderPrettyCSS $ runCSS inputs

```css
form p label{

}
form p label textarea{
    width: 100%;
    height: 20em;
    clear: both;
    margin: 1em 0 0 0
}
form p label textarea, input.text{
    border: 2px solid #ddd;
    border-radius: 4px
}
form p label textarea:focus, input.text:focus{
    background: #eee
}
form p label span{
    float: left;
    width: 7em;
    display: block
}
```