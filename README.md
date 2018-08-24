# DBN

This project is a translation of this [article](https://medium.com/@kosamari/how-to-be-a-compiler-make-a-compiler-with-javascript-4a8a13d473b4) about how to build a compiler with javascript into
haskell. 

In the article, the author demonstrates how to build a simple compiler for the visual language [design by numbers](https://mitpress.mit.edu/books/design-numbers) which takes some written commands and compiles them into an svg image.


For example this:

```
Paper 0
Pen 100
Line 0 50 100 50
```

Compiles into this:

```
<svg width="100" height="100" viewBox="0 0 100 100" version="1.1" xmlns="http://www.w3.org/2000/svg">
  <rect x="0" y="0" width="100" height="100" fill="rgb(255%, 255%, 255%)"></rect>
  <line x1="0" y1="50" x2="100" y2="50" style="stroke:rgb(0,0,0)"></line>
</svg>
```
