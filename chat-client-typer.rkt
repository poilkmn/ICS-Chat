#lang racket

(require racket/base)
(require 2htdp/universe)
(require picturing-programs)
(require test-engine/racket-tests)

; chat-struct: text(string) -> struct
; contains a string to be used to draw the chat box

(struct chat-struct (text sent) #:transparent)
(define chat (chat-struct "" ""))
(define char-letter-list (list "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "="
                               "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\"
                               "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'"
                               "z" "x" "c" "v" "b" "n" "m" "," "." "/" " "
                               "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+"
                               "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" "|"
                               "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "\""
                               "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?")) 

; key-condition: string(key) string(character) -> boolean
; tells whether the key and the character are equal

(define (key-condition key character)
  (key=? key character))

(check-expect (key-condition "a" "a") true)
(check-expect (key-condition "z" "f") false)
(check-expect (key-condition "s" "d") false)

; key-append: string(character) struct(struct) -> struct
; takes in the character and appends in to the end of the string
; inside of the struct

(define (key-append character struct)
  (struct-copy chat-struct struct 
               [text (string-append (chat-struct-text struct) character)]))

(check-expect (key-append "a" chat) (chat-struct "a" ""))
(check-expect (key-append "b" chat) (chat-struct "b" ""))

; key-backspace: struct(struct) -> struct
; deletes the last letter of the text string in the struct

(define (key-backspace struct)
  (struct-copy chat-struct struct [text 
                                   (if (= (string-length (chat-struct-text struct)) 0)
                                       ""
                                       (substring (chat-struct-text struct) 0 
                                                  (- (string-length (chat-struct-text struct)) 1)))]))

; key-enter: struct(struct) -> struct
; transfers the string on text to sent

(define (key-enter struct)
  (struct-copy chat-struct struct [text ""]
               [sent (chat-struct-text struct)]))

; keystroke: struct(struct) string(key) list(list-of-strings)
; combines both key-condition and key-append and uses a list of strings
; as the different characters

(define (keystroke struct key list-of-strings)
  (cond [(empty? list-of-strings) struct]
        [(key-condition key "\b")
         (key-backspace struct)]
        [(key-condition key "\r")
         (key-enter struct)]
        [(cond [(key-condition key (first list-of-strings))
                (key-append (first list-of-strings) struct)]
               [else (keystroke struct key (rest list-of-strings))])]
        [else struct]))

; key-handler: struct(struct) string(key) -> struct
; inputs the function keystroke to the key handler

(define (key-handler struct key)
  (keystroke struct key char-letter-list))

; text-input-box: struct(struct) -> image
; takes in the information from text in struct
; displays it as keys are pressed

(define (text-input-box struct)
  (overlay/align "left" "middle"
                              (beside/align "middle" (text (chat-struct-text struct) 20 "black")
                                            (rectangle 2 20 "solid" "black"))
                              (rectangle 800 30 "solid" "gray")))

(define (draw-handler struct)
  (above/align "left"
               (rectangle 800 600 "solid" "white")
               (text-input-box struct)))

(test)

(big-bang chat
          (on-draw draw-handler)
          (on-key key-handler))

; NOTES:
; need to work on the chatbox: must be able to output received world states