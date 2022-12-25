module Css exposing (style)


style : String
style =
    """

.app {
    margin: 2em auto;
    width: 40em;
    box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.5);
}

.header {
    background: black; /*#8c2022; #4aa52e; #e76e55; #f7d51d;*/
    padding: 1.25em 1.5em 0.5em 1.5em;
    margin: -1.3em -1.8em 2.5em -1.8em; 
    color: white;
} 

.logo {
    position: absolute;
    right: 1em;
    top: 1em;
    background: white;
    border-radius: 0.5em;
    padding: 0.5em;
}

table {
    width: 100%;
    border: 1px solid black;
}

table td {
    border: 1px solid black;
    margin: 0px;
}

table.nes-table {
    table-layout: unset !important;
}

html, body, pre, code, kbd, samp {
    font-family: Press Start 2P;
}

td.small {
    width: 3em;
}

.wide {
    width: 100%;
}

.bad-bet {
    background: #8c2022;
    color: white;
}

"""
