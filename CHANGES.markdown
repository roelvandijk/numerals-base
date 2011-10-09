Changelog for numerals-base
===========================


0.4
---

- Added inflection (case, gender and number).
- Renamed `Text.Numeral.Exp` to `Text.Numeral.Exp.Reified`.
- Renamed `Text.Numeral.Exp.Classes` to `Text.Numeral.Exp`.
- Fast integral logarithm (if GHC ≥ 7.2.1).
- Render combining functions now have access to the expressions being
  combined.
- BigNum postfix names are now a function of their context.


0.3
---

*Thu Sep 15 18:30:40 UTC 2011*

- Split from [numerals-0.1](https://github.com/roelvandijk/numerals/tree/0.1).
- Removed all language definitions.
- Removed `Text.Numeral.Roman` module. It already has a nice home in the
  [roman-numerals](https://github.com/roelvandijk/roman-numerals) package.
- Removed `Text.Numeral.Positional` module. It belongs in a
  [separate package](https://github.com/roelvandijk/positional-numerals).
- Wrote some documentation.
- Complete redesign of the numeral algorithm (multiple times).
- Extended the expression language.
  + Language defind using type classes. Reified as a simple data type.
  + Added `Subtract`.
