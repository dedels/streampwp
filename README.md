# streampwp 
An SWI-Prolog library to compile PWP html files

See SWI-Prologs documentation on PWP:
http://www.swi-prolog.org/pldoc/man?section=pwp

This library takes HTML files containing PWP markup and compiles them to callable Prolog clauses that return string documents.  This library supports all features of PWP as well as some small extensions that gives template developers more control over the output.  

The llama library associated with this should probably be remove.  This was a half-baked idea of mine many years ago so that Prolog could do some more interesting destructuring of objects a-la ML style languages.  This actually worked, but the surrounding code was brittle as it relied on strange hooks in SWI-Prolog's loader.  If anyone is seriously interested in using this library, I could be convinced to remove this dependency and just use record.  
