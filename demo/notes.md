# The overblock family

A **block** is a rendering that sits over the text it came from.  The
text is untouched: it is still there, and the buffer still saves as
what it always was.

## What is in the family

| package | what it renders |
|---------|-----------------|
| `overblock-md` | markdown, line by line |
| `overblock-pydoc` | the doc strings of Python |
| `overblock-pycell` | the output of a notebook cell |
| `overblock-rmd` | the chunks of an R Markdown file |

Math is rendered where it is written: the golden ratio is
$\varphi = \tfrac{1 + \sqrt 5}{2}$, and a sum reads

$$\sum_{k=1}^{n} k = \frac{n(n+1)}{2}$$

Click a line to see its source again:

- a list keeps its bullet
- `code` keeps its face
- [a link](https://example.com) is a link

```python
def hello(name):
    return f"hello, {name}"
```
