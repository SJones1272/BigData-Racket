;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname bigOvenRacketXml) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/batch-io)
#|
The start of the recipe library
Overall Goal: Pull any item from a given api if given proper tags, api key, base url
Main Goal: Pull recipes from big oven given their id name, api key, and possibly just the name of the item itself
|#

;;Example link http://api.bigoven.com/recipe/47725?api_key=dvxKRgLN3XInu21MPS4Dia2PFj3jMX6q

(read-xexpr/web "http://api.bigoven.com/recipe/47725?api_key=dvxKRgLN3XInu21MPS4Dia2PFj3jMX6q")

#|
THINGS We need to pull for a given recipe
- Title
- Cuisine
- Category
- WebURL
- ImageUrl
- Ingredients

We can have any number of ingredients 
From Ingredients we need to pull
- Name
- Metric Quantitiy
- Metric Unit





|#