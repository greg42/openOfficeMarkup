## Markup -> OpenOffice
From time to time, people ask me to send them documents they can edit. That
wouldn't be a problem, if those people would speak LaTeX. Unfortunately however,
this is rarely the case. As I can't really get used to office software, I
decided to implement a solution that enables me to write documents using my
favourite text editor.

There is already quite an amount of markup languages that one could use and
there are excellent tools for them. You should for sure have a look at
[Pandoc](http://johnmacfarlane.net/pandoc/). However, I wanted something more
custom, more special-purpose. So I decided to build some tiny markup language.
Stealing^WBorrowing some ideas from other languages like
[markdown](http://daringfireball.net/projects/markdown/), the language has a
pretty easy syntax:

~~~ 
# This is some headline
Some text in this section.
Linebreaks are not taken into account.

Unless you insert two of them, which creates a new paragraph.

## A second level headline
* Containing
  * A nested
* List

1.) Enumerated
    1.1.) Lists
    1.2.) are also
2.) possible

Besides these simple elements, the language supports some
[b]extended[/b] commands. Section [ref label=commands] provides
more details.

Simple tables:
[table]
foo | bar
bla | bla
[/table]

More complex tables:
[table]
foo  | bar
--------------
here | *comes
     | * a
     | *list
[/table]

# Commands
[label name=commands]
* [b]: Bold text
* [label name=foo]: Set a label named foo
* [ref label=foo]: Reference the label named foo
* [image path=/path/to/image.png, caption="Image caption", label=img]: Insert an
  image with a caption and a reference label
* [imgref label=img] references an image
* [tableOfContents]: Insert a table of contents
* Inline {source} code
* [source] and [/source] for blocks of source code
* [pb] inserts a page break
* [br] inserts a line break
* [table] and [/table] insert a table. The columns are separated by the |
  character. Tables can have a label and a caption.
* [tblref label=tbl] references a table (the referenced table obviously
  has to have a caption and a label).
* [flavor name=BlockQuotes] enables the BlockQuotes parsing extension
  (check the source code for additional extensions)
~~~

This language is first parsed by small parser built with
[Parsec](http://www.haskell.org/haskellwiki/Parsec). The parser generates a
representation of the parsed document and outputs this representation in
a JSON-encoded form. This representation is then processed by the rendering
backend, written in Python. And here things start to get ugly. The rendering
code makes use of the UNO bridge in order to interface with OpenOffice. That is,
the rendering code starts an OpenOffice instance and makes use of its scripting
features in order to insert the text into a document. As ugly as this might
sound, this enables you to use existing OpenOffice templates as they are. By
changing the renderer code you can easily adopt the whole tool to your needs.

### Building
The tool consists of two parts: the parser and the OpenOffice interface.
As you might already have figured, the parser is written in Haskell. If you
haven't done so already, you should install the 
[Haskell platform](http://haskell.org/platform) on your system. 

The parser is shipped as a cabal package, therefore building should be as
easy as:
<pre>
cd markupParser
cabal build
cabal install
</pre>

The OpenOffice part is written in Python. There's no need to "build" it.
However, in order to use it, you should make sure that some requirements are
met.
<ul>
   <li> You should have OpenOffice (or LibreOffice) installed
   <li> If your OpenOffice comes with a built-in Python interpreter,
        then use this one in the subsequent steps.
   <li> Otherwise, make sure you have set up the environment variables
        `URE_BOOTSTRAP` and `UNO_PATH` properly. On my system, I have
        `URE_BOOTSTRAP=vnd.sun.star.pathname:/usr/local/openoffice-3.4.1/openoffice.org3/program/fundamentalrc` and 
        `UNO_PATH=/usr/local/openoffice-3.4.1/openoffice.org3/program`
</ul>

You can give the whole thing a try:
<pre>
cat markupParser/test.udoc | parseUdoc > /tmp/test.json
python OpenOfficeIntegration/toTemplate.py /tmp/test.json\
       OpenOfficeIntegration/template.ott /tmp/test.pdf
xpdf /tmp/test.pdf
</pre>
