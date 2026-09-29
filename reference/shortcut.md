# A keyboard shortcut hint

Writes a shortcut once, for every platform: keys joined by `"+"`, with
`"Mod"` for Command on a Mac and Ctrl elsewhere. The hint carries both
forms and blockr.ui shows the one that applies (design system, "Keyboard
shortcuts"): `"Mod+Shift+S"` reads ⌘⇧S on a Mac and Ctrl+Shift+S
elsewhere, and `"Mod+Enter"` reads ⌘↵ or Ctrl+↵. Put it in a menu row's
meta slot
([`menu_item()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/action_menu.md))
or after a button's label.

## Usage

``` r
shortcut(keys)
```

## Arguments

- keys:

  The keys, such as `"Mod+S"`. Named keys: `Mod`, `Shift`, `Alt`,
  `Ctrl`, `Enter` and `Esc`; any other single character is shown in
  upper case.

## Value

A `<span>`, with
[`controls_dep()`](https://bristolmyerssquibb.github.io/blockr.ui/reference/controls_dep.md)
attached.

## Examples

``` r
shortcut("Mod+Shift+S")
#> <span class="blockr-shortcut">
#>   <span class="blockr-shortcut__mac">⌘⇧S</span>
#>   <span class="blockr-shortcut__other">Ctrl+Shift+S</span>
#> </span>
```
