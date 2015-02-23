#lang racket
(require xml)
(require net/url)

#|
The start of the recipe library
Overall Goal: Pull any item from a given api if given proper tags, api key, base url
Main Goal: Pull recipes from big oven given their id name, api key, and possibly just the name of the item itself
|#

;;Example link http://api.bigoven.com/recipe/47725?api_key=dvxKRgLN3XInu21MPS4Dia2PFj3jMX6q

#|
THINGS We need to pull for a given recipe
- Title
- Cuisine
- Category
- ImageUrl
- Ingredients

We can have any number of ingredients 
From Ingredients we need to pull
- Name
- MetricQuantitiy
- MetricUnit

|#

#|
Recipe ID's
Spanish Chicken - 291260
Fondue - 714740

Maybe come up with a way to search for recipes
given some keyword
|#

;;Base URL
(define BASE "http://api.bigoven.com/recipe")
(define KEY "dvxKRgLN3XInu21MPS4Dia2PFj3jMX6q")
(define OUTERTAGS (list 'Title 'Cuisine 'Category 'ImageURL))
(define INGREDSINNER (list 'Name 'MetricQuantity 'MetricUnit))

;;(grab-ingredients "http://api.bigoven.com/recipe" "47725" "dvxKRgLN3XInu21MPS4Dia2PFj3jMX6q")
;;Main function : Reads in the xml and removes white space
(define (read/clean-data baseUrl id key)
(grab-recipe-info
  (remove-white 
   (xml->xexpr 
    (document-element (read-xml (get-pure-port 
                                 (string->url (string-append baseUrl "/" id "?api_key=" key)))))))))
;;grabs outer and inner tags from the recipe
(define (grab-recipe-info data)
  (append  (map (lambda (x1) (if (and (list? x1) (>  (length x1) 2))
                                 (third x1)
                                 "MISSING")) (pull-tags data OUTERTAGS))
        (list (map clean-ingred (map  (lambda (x)  (pull-tags x INGREDSINNER)) 
              (pull-tag (first (pull-tag data 'Ingredients)) 'Ingredient))))))

;;removes whitespace from a given list
;;TODO : make more generic
(define (remove-white ls)
  (cond
    [(empty? ls) empty]
    [(list? (first ls)) (cons (remove-white (first ls)) (remove-white (rest ls)))]
    [(and (string? (first ls)) (regexp-match? "\r" (first ls))) (remove-white (rest ls))]
    [else (cons (first ls) (remove-white (rest ls)))]))

;;pulls a given tag from a list
(define (pull-tag l tag)
   (filter (lambda (x) (and (and (cons? x) (list? x)) (equal? (first x) tag))) l))

;;pulls all given tags from a list
(define (pull-tags l listOfTags)
  (filter (lambda (x) (and (and (cons? x) (list? x)) (relevant? x listOfTags))) l))

(define (relevant? x listOfTags)
  (cond
    [(empty? listOfTags) false]
    [else (or (member (first listOfTags) x) (relevant? x (rest listOfTags)))])) 

(define (clean-ingred x)
  (map (lambda (x1) (if (and (list? x) (> (length x1) 2))
                       (third x1)
                       "MISSING")) x))

(define oneIngred '((Name () "Salmon steaks")
  (MetricQuantity () "8")
  (MetricUnit ())))

(define sl '("Salmon on the Grill with Lemon Butter"
  "American"
  "Main Dish"
  "http://redirect.bigoven.com/pics/rs/640/salmon-on-the-grill-with-lemon-butt-2.jpg"
  (("Salmon steaks"
    "8"
    "MISSING")
   ("olive oil" "0" "MISSING")
   ("Butter" "59" "ml")
   ("Lemon juice" "30" "ml")
   ("Chopped parsley"
    "30"
    "ml")
   ("rosemary, dried"
    "1.23223039580426"
    "ml")
   ("Marjoram" "0" "MISSING")
   ("Salt"
    "1.23223039580426"
    "ml")
   ("Coarsely ground pepper"
    "0.616115197902132"
    "ml"))))