- Add pl->pl round trip tests will all of the cases
  - for 3 kinds of params
  - any other non-character/ligature test cases? Maybe check defaults?
- Then read in characters
- Fully implement the write PL function - check code in p218 of tfToPl
- Then finish the TFM -> PL converter (i.e., TFM deserialize). Run this over all of the test data and ensure it matches
- Implement the parse PL function
- Then finish the PL -> TFM converter

- change ast::Node -> ast::Elem and ast::Tree to ast::List
