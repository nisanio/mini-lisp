# Mini-Lisp: A Gentle Introduction

Mini-Lisp is a tiny interpreter written in C.
It is not meant to be a full Common Lisp or Scheme implementation, but a minimal and extensible core for learning how Lisps work — while still being powerful enough to run small, real programs.

This tutorial assumes you already know how to program (e.g., in C, Python, or Java) but have never used Lisp before.

---

## 1. Running Mini-Lisp

You can run Mini-Lisp in two ways:

### Interactive REPL

```bash
./minilisp
```

You will see:

```
Mini-Lisp REPL. Ctrl-D to exit.
>
```

Here you can type Lisp expressions interactively.

### Running files

You can also load and run Lisp files:

```bash
./minilisp stdlib.lisp myprog.lisp
```

* `stdlib.lisp`: the Mini-Lisp standard library (lists, vectors, strings, hash tables, I/O helpers, etc.).
* `myprog.lisp`: your own program.

---

## 2. Basic Syntax

Lisp code is made of **expressions** (also called *forms*), written in parentheses:

```lisp
(+ 1 2 3)     ; adds numbers
(* 2 5)       ; multiplication
```

* The first element is always the function/operator (`+`, `*`, etc.).
* The rest are arguments.

There are no infix operators. `(1 + 2)` is invalid. Always prefix.

---

## 3. Values

Mini-Lisp supports:

* **Numbers**: `42`, `-7`, `3.14`
* **Booleans**: `#t` (true), `#f` (false)
* **Nil**: `nil` (represents “nothing” or the empty list)
* **Strings**: `"hello"`
* **Symbols**: names like `x`, `counter`, `my-func`
* **Pairs / Lists**: `(1 2 3)` (built with `cons`)
* **Vectors**: `(vector "a" "b" "c")`
* **Hash tables**: `(make-hash)`

---

## 4. Variables and Functions

### Defining variables

```lisp
(defun x 10)          ; define a variable x = 10
```

### Defining functions

```lisp
(defun square
  (lambda (n) (* n n)))

(square 5)   ; => 25
```

Mini-Lisp uses `lambda` for anonymous functions and `defun` to bind them to names.

---

## 5. Conditionals

```lisp
(if #t 111 222)   ; => 111
(if #f 111 222)   ; => 222

(if (> 5 3)
    "yes"
    "no")         ; => "yes"
```

## 6. Local Bindings with `let` and `let*`

In Lisp, a **binding** means associating a name with a value inside a specific scope.
Think of it as a temporary variable definition that only exists while evaluating a block of code.

Mini-Lisp provides two special forms for creating local bindings: `let` and `let*`.

---

### `let` (parallel bindings)

```lisp
(let ((a 1)
      (b 2))
  (+ a b))     ; => 3
```

How it works:

1. All expressions on the right-hand side are evaluated **first**.
2. Then the results are bound to the variable names **at the same time**.
3. The body of the `let` is executed with those bindings in place.

Example showing independence of bindings:

```lisp
(let ((a 10)
      (b (+ a 5)))   ; here "a" is not yet bound!
  (list a b))
; => (10 15)
```

Why? Because `(+ a 5)` is evaluated **before** `a = 10` takes effect.
So the outer `a` (if defined) is used, not the inner one.

---

### `let*` (sequential bindings)

```lisp
(let* ((a 10)
       (b (+ a 5)))   ; here "a" is already bound to 10
  (list a b))
; => (10 15)
```

How it works:

* Each binding is created **immediately** after its value is evaluated.
* Later bindings can see and use earlier ones.

---

### Summary

* **`let`**: evaluates all values first, then installs bindings simultaneously.
  → Useful when bindings should not depend on each other.
* **`let*`**: installs bindings one by one, in order.
  → Useful when later variables need the values of earlier ones.

---

### Analogy

If you come from a C-like language:

* `let` is like declaring all variables at once, then assigning values.
* `let*` is like declaring and assigning in order, so each variable can depend on the previous one.


## 7. Lists

Lists are central in Lisp:

```lisp
(cons 1 (cons 2 (cons 3 nil)))   ; => (1 2 3)

(car '(10 20 30))  ; => 10
(cdr '(10 20 30))  ; => (20 30)
```

---

## 8. Vectors

```lisp
(setq v (vector "a" "b" "c"))
(vector-ref v 1)    ; => "b"
(vector-set! v 1 "x")
v                   ; => ("a" "x" "c")
```

---

## 9. Hash Tables

```lisp
(setq h (make-hash))
(hash-set! h "name" "Alice")
(hash-ref h "name" "Unknown")    ; => "Alice"
(hash-has-key? h "name")         ; => #t
(hash-remove! h "name")          ; removes the key
```

---

## 10. File I/O

Mini-Lisp provides simple file utilities in the standard library:

```lisp
(file-exists? "data.txt")        ; => #t or #f
(write-file "out.txt" "hello")   ; create/overwrite file
(read-file "out.txt")            ; => "hello"
(read-lines "out.txt")           ; => ("hello" "")
(append-file "out.txt" "world")  ; append
```

---

## 11. Standard Library

Mini-Lisp ships with a small standard library (`stdlib.lisp`) including:

* List helpers (`map`, `filter`, `fold`)
* String helpers (`string-append`, `substring`)
* Vector helpers
* Hash helpers
* File I/O helpers
* Example algorithms (e.g., Fibonacci with memoization)

Load it before your own program:

```bash
./minilisp stdlib.lisp myprog.lisp
```

---

## 12. Example Program

File: `myprog.lisp`

```lisp
; Count word frequencies in a file

(defun count-words (filename)
  (let* ((text (read-file filename))
         (words (vector "hello" "world" "hello"))
         (tab (make-hash)))
    (begin
      (for-each
        (lambda (w)
          (let* ((k w)
                 (v (hash-ref tab k 0)))
            (hash-set! tab k (+ v 1))))
        words)
      tab)))

(print (count-words "dummy.txt"))
```

Run it:

```bash
./minilisp stdlib.lisp myprog.lisp
```

---

## 13. Key Takeaways

* **Code is data**: Lisp code is just lists. `(f x y)` is literally a list with elements `f`, `x`, `y`.
* **Uniform syntax**: everything is prefix notation.
* **Extensible**: Mini-Lisp is small, but you can extend it with C builtins or Lisp functions.

---

## 14. Next Steps

* Explore the standard library (`stdlib.lisp`).
* Write small programs (factorials, sorting, file parsing).
* Study the interpreter itself: `reader.c`, `eval.c`, `env.c` to see how Lisps work internally.

Mini-Lisp is both a **learning tool** and a **playground** for experimenting with language design.

