###
### fmt.janet
###
### Janet code formatter.
###
### From the spork library (https://github.com/janet-lang/spork)
###
### Copyright (c) 2022 Calvin Rose and contributors
###
### Permission is hereby granted, free of charge, to any person obtaining a copy of
### this software and associated documentation files (the "Software"), to deal in
### the Software without restriction, including without limitation the rights to
### use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
### of the Software, and to permit persons to whom the Software is furnished to do
### so, subject to the following conditions:
###
### The above copyright notice and this permission notice shall be included in all
### copies or substantial portions of the Software.
###
### THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
### IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
### FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
### AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
### LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
### OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
### SOFTWARE.
###

(defn- pnode
  "Make a capture function for a node."
  [tag]
  (fn [x] [tag x]))

(def- parse-peg
  "Peg to parse Janet with extra information, namely comments."
  (peg/compile
    ~{:ws (+ (set " \t\r\f\0\v") '"\n")
      :readermac (set "';~,|")
      :symchars (+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_"))
      :token (some :symchars)
      :hex (range "09" "af" "AF")
      :escape (* "\\" (+ (set "ntrzfev0ab'?\"\\")
                         (* "x" :hex :hex)
                         (* "u" :hex :hex :hex :hex)
                         (* "U" :hex :hex :hex :hex :hex :hex)
                         (error (constant "bad hex escape"))))
      :comment (/ (* "#" '(any (if-not (+ "\n" -1) 1)) (+ "\n" -1)) ,(pnode :comment))
      :span (/ ':token ,(pnode :span))
      :bytes '(* "\"" (any (+ :escape (if-not "\"" 1))) "\"")
      :string (/ :bytes ,(pnode :string))
      :buffer (/ (* "@" :bytes) ,(pnode :buffer))
      :long-bytes '{:delim (some "`")
                    :open (capture :delim :n)
                    :close (cmt (* (not (> -1 "`")) (-> :n) ':delim) ,=)
                    :main (drop (* :open (any (if-not :close 1)) :close))}
      :long-string (/ :long-bytes ,(pnode :string))
      :long-buffer (/ (* "@" :long-bytes) ,(pnode :buffer))
      :ptuple (/ (group (* "(" (any :input) (+ ")" (error)))) ,(pnode :ptuple))
      :btuple (/ (group (* "[" (any :input) (+ "]" (error)))) ,(pnode :btuple))
      :struct (/ (group (* "{" (any :input) (+ "}" (error)))) ,(pnode :struct))
      :parray (/ (group (* "@(" (any :input) (+ ")" (error)))) ,(pnode :array))
      :barray (/ (group (* "@[" (any :input) (+ "]" (error)))) ,(pnode :array))
      :table (/ (group (* "@{" (any :input) (+ "}" (error)))) ,(pnode :table))
      :rmform (/ (group (* ':readermac
                           (group (any :non-form))
                           :form))
                 ,(pnode :rmform))
      :form (choice :rmform
                    :parray :barray :ptuple :btuple :table :struct
                    :buffer :string :long-buffer :long-string
                    :span)
      :non-form (choice :ws :comment)
      :input (choice :non-form :form)
      :main (* (any :input) (+ -1 (error)))}))

(defn- make-tree
  "Turn a string of source code into a tree that will be printed"
  [source]
  [:top (peg/match parse-peg source)])

(defn- remove-extra-newlines
  "Remove leading and trailing newlines. Also remove
   some extra consecutive newlines."
  [node]
  (match node
    [tag (xs (array? xs))]
    (do
      (while (= "\n" (array/peek xs)) (array/pop xs)) # remove trailing newlines
      # Replace a trailing :comment node with :comment-last node
      (when-let [last-node (array/peek xs)]
        (when (= :comment (first last-node))
          (array/pop xs)
          (array/push xs [:comment-last (get last-node 1)])))
      (when-let [index (find-index |(not= "\n" $) xs)]
        (array/remove xs 0 index)) # remove leading newlines
      # remove too many consecutive newlines
      (def max-consec (if (= tag :top) 3 2))
      (var i 0)
      (var consec-count 0)
      (while (< i (length xs))
        (if (= (in xs i) "\n")
          (if (= consec-count max-consec) (array/remove xs i) (do (++ i) (++ consec-count)))
          (do (set consec-count 0) (++ i))))
      node)
    node))

(defdyn *user-indent-2-forms*
  "A user list of forms that are control forms and should be indented two spaces.")

(defn- user-indent-2-forms [] (invert (or (dyn *user-indent-2-forms*) [])))

(def- indent-2-forms
  "A list of forms that are control forms and should be indented two spaces."
  (invert ["fn" "match" "with" "with-dyns" "def" "def-" "var" "var-" "defn" "defn-"
           "varfn" "defmacro" "defmacro-" "defer" "edefer" "loop" "seq" "tabseq" "catseq" "generate" "coro"
           "for" "each" "eachp" "eachk" "case" "cond" "do" "defglobal" "varglobal"
           "if" "when" "when-let" "when-with" "while" "with-syms" "with-vars"
           "if-let" "if-not" "if-with" "let" "short-fn" "try" "unless" "default" "forever" "upscope"
           "repeat" "forv" "compwhen" "compif" "ev/spawn" "ev/do-thread" "ev/spawn-thread" "ev/with-deadline"
           "label" "prompt" "forever"]))

(def- indent-2-peg
  "Peg to use to fuzzy match certain forms."
  (peg/compile ~(+ "with-" "def" "if-" "when-")))

(defn- check-indent-2
  "Check if a tuple needs a 2 space indent or not"
  [items &opt offset]
  (default offset 0)
  (if-let [[tag body] (get items offset)]
    (cond
      (= tag :comment) (break (check-indent-2 (+ 1 offset)))
      (= "\n" (get items (+ 1 offset))) true
      (not= tag :span) nil
      (in indent-2-forms body) true
      (peg/match indent-2-peg body) true
      (in (user-indent-2-forms) body) true)))

(defn- fmt
  "Emit formatted."
  [tree]

  (var col 0)
  (def ident-stack @[])
  (var ident "")
  (def white @"")

  (defn emit [& xs] (each x xs (+= col (length x)) (prin x)))
  (defn indent [&opt delta]
    (array/push ident-stack ident)
    (set ident (string/repeat " " (+ col (or delta 0)))))
  (defn dedent [] (set ident (array/pop ident-stack)))
  (defn flushwhite [] (emit white) (buffer/clear white))
  (defn dropwhite [] (buffer/clear white))
  (defn addwhite [] (buffer/push-string white " "))
  (defn newline [] (dropwhite) (print) (buffer/push-string white ident) (set col 0))

  # Mutual recursion
  (var fmt-1-recur nil)

  (defn emit-body
    [open xs close &opt delta]
    (emit open)
    (indent delta)
    (each x xs (fmt-1-recur x))
    (dropwhite)
    (dedent)
    (emit close)
    (addwhite))

  (defn emit-funcall
    [xs]
    (emit "(")
    (def len (length xs))
    (when (pos? len)
      (fmt-1-recur (xs 0))
      (indent 1)
      (for i 1 len (fmt-1-recur (xs i)))
      (dropwhite)
      (dedent))
    (emit ")")
    (addwhite))

  (defn emit-string
    [x]
    (def parts (interpose "\n" (string/split "\n" x)))
    (each p parts (if (= p "\n") (do (newline) (dropwhite)) (emit p))))

  (defn emit-rmform
    [rm nfs form]
    (emit rm)
    (each nf nfs
      (fmt-1-recur nf))
    (fmt-1-recur form))

  (defn fmt-1
    [node]
    (remove-extra-newlines node)
    (unless (= node "\n") (flushwhite))
    (match node
      "\n" (newline)
      [:comment x] (do (emit "#" x) (newline))
      [:comment-last x] (do (emit "#" x) (newline) (flushwhite))
      [:span x] (do (emit x) (addwhite))
      [:string x] (do (emit-string x) (addwhite))
      [:buffer x] (do (emit "@") (emit-string x) (addwhite))
      [:array xs] (emit-body "@[" xs "]")
      [:btuple xs] (emit-body "[" xs "]")
      [:ptuple xs] (if (check-indent-2 xs)
                     (emit-body "(" xs ")" 1)
                     (emit-funcall xs))
      [:struct xs] (emit-body "{" xs "}")
      [:table xs] (emit-body "@{" xs "}")
      [:rmform [rm nfs form]] (emit-rmform rm nfs form)
      [:top xs] (emit-body "" xs "")))

  (set fmt-1-recur fmt-1)
  (fmt-1 tree)
  (newline)
  (flush))

#
# Public API
#

(defn format-print
  "Format a string of source code and print the result."
  [source]
  (-> source make-tree fmt))

(defn format
  "Format a string of source code to a buffer."
  [source]
  (def out @"")
  (with-dyns [:out out]
    (format-print source))
  out)

(defn format-file
  "Format a file"
  [file]
  (def source (slurp file))
  (def out (format source))
  (spit file out))
