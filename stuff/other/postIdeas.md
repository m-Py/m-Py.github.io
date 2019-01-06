- r markdown tricks 
    + tables
    + pandoc commands (e.g. remove automatic figure annotation)
    + formatting: pdf vs. html (think about your output file)
      (http://daringfireball.net/projects/markdown/syntax)
    + markdown is not for formatting! (bad for pdf)

- questions on r markdown
    + how to center images (not from r-chunk images)
    + how to make a header 
    + how to manipulate page numbers (for example show max pages)
        
- r markdown - editors that support Syntax highlighting

- the different priors in Bayes statistics
    + prior probability of hypothesis
    + prior of parameter distribution
    + how does this relate to Bayes factors ("prior odds")

- The likelihood - what is fixed and what varies?

- Vertikales top-alignment for beamer: \documentclass[t]{$documentclass$}

    (

    \documentclass[t,$if(fontsize)$$fontsize$,$endif$$if(lang)$$babel-lang$,$endif$$if(handout)$handout,$endif$$if(beamer)$ignorenonframetext,$endif$$for(classoption)$$classoption$$sep$,$endfor$]{$documentclass$}

    )

- Short title (auch short author und institute) (in template; wo herbekommen?)

    $if(title)$
    \title[$shorttitle$]{$title$}
    $endif$

- {.allowframebreaks} for references

- add whitespace: &nbsp;

- whitespace in YAML headers potentially detrimental 
    + (Beispiel: Leerzeile nach erster Zeile; nocite - was auch immer da passiert)

- tables using pander; using ---|||--- (cell width, table width)

### Upcoming 

- Change the default language in R Markdown output 
- Adding a short title for Beamer slides
- Embedding pandoc commands in a R Markdown file
- Suppress automatic numbering of figures
- Add whitespace to R Markdown pdf documents
- Which editors support R Markdown syntax highlighting?
- Detrimental whitespace in YAML headers
