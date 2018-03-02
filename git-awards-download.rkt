#lang racket
(require net/url
         html-parsing
         sxml/sxpath)

;; It fetches the content of page. It can be used to fetch any page
;; (page-get "https://cs.brown.edu/courses/cs173/2012/book/")
;; (page-get "http://git-awards.com/users?city=san+francisco")
(define (page-get url)
  (call/input-url (string->url url)
                  get-pure-port
                  html->xexp))

;; Start of parser for git-awards page
(define (parse-user-name ul)
  (match (last ul)
    [(cons _ (cons _ (cons c _))) c]
    [_ '()]))


(define (filter-values re)
  (match re
    [(cons a (cons b (cons c (cons d res))))
     (cons (string-join (list (parse-user-name b) (last c) (first (rest d))) ",") (filter-values res))]
    [_ '()]))

;; This function parses the list of users and their rank
;; (list-of-values "http://git-awards.com/users")
;; (list-of-values "http://git-awards.com/users?utf8=%E2%9C%93&type=country&language=haskell&country=Australia")
;; (list-of-values "http://git-awards.com/users?language=haskell")
;; (list-of-values "http://git-awards.com/users?language=racket")
(define (list-of-values url)
  (let* ([res (page-get url)]
         [ret ((sxpath '(// tbody tr td)) res)]
         [val (filter-values ret)])
    val))

;; This function returns the last page number
;; and it can be used if you want to continue fetching the
;; data by constructing page number url
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

;; End of git-award page parser

