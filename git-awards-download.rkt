#lang racket
(require net/url
         html-parsing
         sxml/sxpath)

(define (page-get url)
  (call/input-url (string->url url)
                  get-pure-port
                  html->xexp))

(define (parse-user-name ul)
  (match (last ul)
    [(cons _ (cons _ (cons c _))) c]
    [_ '()]))

(define (filter-values re)
  (match re
    [(cons a (cons b (cons c (cons d res))))
     (cons (string-join (list (parse-user-name b) (last c) (first (rest d))) ",") (filter-values res))]
    [_ '()]))

(define (list-of-values url)
  (let* ([res (page-get url)]
         [ret ((sxpath '(// tbody tr td)) res)]
         [val (filter-values ret)])
    val))

(define (last-page-number url)
  (let* ([res (page-get url)]
         [li (last ((sxpath '(// li a @ href)) res))])
    (match (regexp-match #rx"[0-9]+" (last li))
      [(cons a '()) a]
      [_ (error "some thing went wrong")])))
      
(define url "http://git-awards.com/users?city=san+francisco")         
(define res (list-of-values "http://git-awards.com/users?utf8=%E2%9C%93&type=city&language=swift&city=Paris"))
(define h (list-of-values "http://git-awards.com/users"))
(define t (page-get "http://git-awards.com/users?city=san+francisco"))
