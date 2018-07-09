module Generator.Style exposing (..)

pageStyle : String
pageStyle = """
body {
  max-width: 700px;
  margin: auto;
  font-family: 'Open Sans';
}
pre {
  font-size: 1rem;
  font-family: 'Roboto Mono';
}
code {
  font-family: 'Roboto Mono';
  padding: 2px 6px;
  border-radius: 5px;
}
.function-signature {
  font-family: 'Roboto Mono';
}
.alias-signature {
  font-family: 'Roboto Mono';
}
.union-signature {
  font-family: 'Roboto Mono';
}
.function-definition {
  padding: 12px;
  background-color: rgb(240, 240, 240);
  border-radius: 5px;
}
pre.prettyprint {
  border-radius: 5px;
  padding: 12px;
  font-family: 'Roboto Mono';
  font-size: 0.75rem;
  line-height: 20px;
}
"""

githubStyle : String
githubStyle = """
.prettyprint {
  background: #2d2d2d;
  font-family: Menlo, "Bitstream Vera Sans Mono", "DejaVu Sans Mono", Monaco, Consolas, monospace;
  border: 0 !important;
}
.pln {
  color: #cccccc;
}
ol.linenums {
  margin-top: 0;
  margin-bottom: 0;
  color: #999999;
}
li.L0, li.L1, li.L2, li.L3, li.L4, li.L5, li.L6, li.L7, li.L8, li.L9 {
  padding-left: 1em;
  background-color: #2d2d2d;
  list-style-type: decimal;
}
@media screen {
  .str { color: #99cc99; }
  .kwd { color: #cc99cc; }
  .com { color: #999999; }
  .typ { color: #6699cc; }
  .lit { color: #f99157; }
  .pun { color: #cccccc; }
  .opn { color: #cccccc; }
  .clo { color: #cccccc; }
  .tag { color: #f2777a; }
  .atn { color: #f99157; }
  .atv { color: #66cccc; }
  .dec { color: #f99157; }
  .var { color: #f2777a; }
  .fun { color: #6699cc; }
}
"""
