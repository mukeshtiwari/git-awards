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
      
;(define url "http://git-awards.com/users?city=san+francisco")         
;(define res (list-of-values "http://git-awards.com/users?utf8=%E2%9C%93&type=city&language=swift&city=Paris"))
;(define h (list-of-values "http://git-awards.com/users"))
;(define t (page-get "http://git-awards.com/users?city=san+francisco"))
;; End of git-award page parser

;; Start of parsing data from github
;; Removing "\n" and "       " 
(define (remove-empty-and-newline ls)
  (filter (λ(x) (if (string? x) (non-empty-string? (string-trim x)) #t)) ls))

;; if div list contains (span (@ (class "mr-3") (itemprop "programmingLanguage")) "\n" "          Racfrom list ket\n" "        ")
(define (contains-span-class-mr-3 ls)
  (match ls
    [(cons (quasiquote div)
           (cons _ (cons _ (cons (cons (quasiquote span) rest) _)))) (string-trim (caddr rest))]
    [_ ""]))

;; This function takes user repo url and returns the list of repository
;; (github-repo-list "https://github.com/racket?tab=repositories")
(define (github-repo-list user-repo-url)
  (let* ([repo (page-get user-repo-url)]
         [li ((sxpath "//div[contains(@class, 'd-inline-block mb-1')]//h3//a") repo)]
         [lan ((sxpath "//div[contains(@class, 'f6 text-gray mt-2')]") repo)]
         [repo-list (map (λ(x) (string-trim (last x))) li)]
         [lang-list (map (λ(x) (contains-span-class-mr-3 (remove-empty-and-newline x))) lan)])
    (map cons repo-list lang-list)))
   


;(define repo (github-repo-list "https://github.com/racket?tab=repositories"))
;(define own-repo (github-repo-list "https://github.com/mukeshtiwari?tab=repositories"))
;(define star-repo (github-repo-list "https://github.com/mukeshtiwari?tab=stars"))

;; Fetch the followers and their github id
(define (follower-list follower-url)
  (let* ([follower (page-get follower-url)]
         [flist ((sxpath "//span[contains(@class, 'f4 link-gray-dark')]/text()") follower)]
         [github-id ((sxpath "//span[contains(@class, 'link-gray pl-1')]/text()") follower)])
         (map cons flist github-id)))

(define follower (follower-list  "https://github.com/mukeshtiwari?tab=followers"))
(define following (follower-list "https://github.com/mukeshtiwari?tab=following"))


;; Count the commits each day in one year
(define (count-commit-day ls)
  (match ls
    [(cons _ (list* (list* _ _ _ _ _ _ _ (cons (cons _ (cons n _)) (cons (cons _ (cons d _)) _))) _)) (cons d n)]
    [_ '()]))

(define (count-commits-year year-url)
  (let* ([commits (page-get year-url)]
         [commit-list ((sxpath "//rect[contains(@class, 'day')]") commits)])
    (map count-commit-day commit-list)))
         
(define count (count-commits-year "https://github.com/mukeshtiwari?tab=overview&from=2011-12-01&to=2011-12-31"))
