#lang racket
(require xml)
(require net/url)
(require srfi/13)

#|
The start of the recipe library
Overall Goal: Pull any item from a given api if given proper tags, api key, base url
Main Goal: Pull recipes from big oven given their id name, api key, and possibly just the name of the item itself
|#

;;Example link http://api.bigoven.com/recipe/47725?api_key=dvxKRgLN3XInu21MPS4Dia2PFj3jMX6q
;Example call
;(read-xexpr/web "http://api.bigoven.com/recipe/47725?api_key=dvxKRgLN3XInu21MPS4Dia2PFj3jMX6q")

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
  (cons (pull-tags data OUTERTAGS)
        (map  (lambda (x) (pull-tags x INGREDSINNER)) 
              (pull-tag (first (pull-tag data 'Ingredients)) 'Ingredient))))


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
#|  
(cond
   [(empty? ls) empty]
   [(and (list? ls) (equal? (first (first ls) 'Ingredients)) (cons (first ls) (grab-relevant-info (rest ls)))]
   [(equal? (first ls) 'Title) (cons (first ls) (grab-relevant-info (rest ls)))]
   [(equal? (first ls) 'Cuisine) (cons (first ls) (grab-relevant-info (rest ls)))]
   [(equal? (first ls) 'ImageUrl) (cons (first ls) (grab-relevant-info (rest ls)))]
   [else (grab-relevant-info (rest ls))]))
  |# 

(define ingred '(Ingredients
  ()
  (Ingredient
   ()
   (IngredientID () "462601")
   (DisplayIndex () "0")
   (IsHeading () "false")
   (Name () "Salmon steaks")
   (HTMLName
    ()
    "<"
    "a href=\"http://www.bigoven.com/glossary/salmon\" class=\"glosslink\""
    ">"
    "Salmon"
    "<"
    "/a"
    ">"
    " steaks")
   (Quantity () "8")
   (DisplayQuantity () "8")
   (MetricQuantity () "8")
   (MetricDisplayQuantity
    ()
    "8")
   (MetricUnit ())
   (IngredientInfo
    ()
    (Name () "Salmon steaks")
    (Department () "Meats"))
   (IsLinked () "true"))
  (Ingredient
   ()
   (IngredientID () "462602")
   (DisplayIndex () "1")
   (IsHeading () "false")
   (Name () "olive oil")
   (HTMLName
    ()
    "<"
    "a href=\"http://www.bigoven.com/glossary/olive%20oil\" class=\"glosslink\""
    ">"
    "olive oil"
    "<"
    "/a"
    ">")
   (Quantity () "1")
   (MetricQuantity () "0")
   (MetricDisplayQuantity ())
   (MetricUnit ())
   (IngredientInfo
    ()
    (Name () "olive oil")
    (Department () "Oils"))
   (IsLinked () "true"))
  (Ingredient
   ()
   (IngredientID () "462603")
   (DisplayIndex () "2")
   (IsHeading () "false")
   (Name () "Butter")
   (HTMLName
    ()
    "<"
    "a href=\"http://www.bigoven.com/glossary/butter\" class=\"glosslink\""
    ">"
    "Butter"
    "<"
    "/a"
    ">")
   (Quantity () "0.25")
   (DisplayQuantity () "1/4")
   (Unit () "c")
   (MetricQuantity () "59")
   (MetricDisplayQuantity
    ()
    "59")
   (MetricUnit () "ml")
   (PreparationNotes
    ()
    "melted")
   (IngredientInfo
    ()
    (Name () "Butter")
    (Department () "Dairy"))
   (IsLinked () "true"))
  (Ingredient
   ()
   (IngredientID () "462604")
   (DisplayIndex () "3")
   (IsHeading () "false")
   (Name () "Lemon juice")
   (HTMLName
    ()
    "<"
    "a href=\"http://www.bigoven.com/glossary/lemon\" class=\"glosslink\""
    ">"
    "Lemon"
    "<"
    "/a"
    ">"
    " juice")
   (Quantity () "2")
   (DisplayQuantity () "2")
   (Unit () "tb")
   (MetricQuantity () "30")
   (MetricDisplayQuantity
    ()
    "30")
   (MetricUnit () "ml")
   (IngredientInfo
    ()
    (Name () "Lemon juice")
    (Department () "Produce"))
   (IsLinked () "true"))
  (Ingredient
   ()
   (IngredientID () "462605")
   (DisplayIndex () "4")
   (IsHeading () "false")
   (Name () "Chopped parsley")
   (HTMLName
    ()
    "Chopped "
    "<"
    "a href=\"http://www.bigoven.com/glossary/parsley\" class=\"glosslink\""
    ">"
    "parsley"
    "<"
    "/a"
    ">")
   (Quantity () "2")
   (DisplayQuantity () "2")
   (Unit () "tb")
   (MetricQuantity () "30")
   (MetricDisplayQuantity
    ()
    "30")
   (MetricUnit () "ml")
   (IngredientInfo
    ()
    (Name () "Chopped parsley")
    (Department () "Produce"))
   (IsLinked () "true"))
  (Ingredient
   ()
   (IngredientID () "462606")
   (DisplayIndex () "5")
   (IsHeading () "false")
   (Name () "rosemary, dried")
   (HTMLName
    ()
    "<"
    "a href=\"http://www.bigoven.com/glossary/rosemary\" class=\"glosslink\""
    ">"
    "rosemary"
    "<"
    "/a"
    ">"
    ", dried")
   (Quantity () "0.25")
   (DisplayQuantity () "1/4")
   (Unit () "ts")
   (MetricQuantity
    ()
    "1.23223039580426")
   (MetricDisplayQuantity
    ()
    "1")
   (MetricUnit () "ml")
   (PreparationNotes
    ()
    "Crushed")
   (IngredientInfo
    ()
    (Name () "rosemary, dried")
    (Department () "Spices"))
   (IsLinked () "true"))
  (Ingredient
   ()
   (IngredientID () "462607")
   (DisplayIndex () "6")
   (IsHeading () "false")
   (Name () "Marjoram")
   (HTMLName
    ()
    "<"
    "a href=\"http://www.bigoven.com/glossary/marjoram\" class=\"glosslink\""
    ">"
    "Marjoram"
    "<"
    "/a"
    ">")
   (Quantity () "1")
   (MetricQuantity () "0")
   (MetricDisplayQuantity ())
   (MetricUnit ())
   (IngredientInfo
    ()
    (Name () "Marjoram")
    (Department () "Spices"))
   (IsLinked () "true"))
  (Ingredient
   ()
   (IngredientID () "462608")
   (DisplayIndex () "7")
   (IsHeading () "false")
   (Name () "Salt")
   (HTMLName
    ()
    "<"
    "a href=\"http://www.bigoven.com/glossary/salt\" class=\"glosslink\""
    ">"
    "Salt"
    "<"
    "/a"
    ">")
   (Quantity () "0.25")
   (DisplayQuantity () "1/4")
   (Unit () "ts")
   (MetricQuantity
    ()
    "1.23223039580426")
   (MetricDisplayQuantity
    ()
    "1")
   (MetricUnit () "ml")
   (IngredientInfo
    ()
    (Name () "Salt")
    (Department () "Baking"))
   (IsLinked () "true"))
  (Ingredient
   ()
   (IngredientID () "462609")
   (DisplayIndex () "8")
   (IsHeading () "false")
   (Name
    ()
    "Coarsely ground pepper")
   (HTMLName
    ()
    "Coarsely "
    "<"
    "a href=\"http://www.bigoven.com/glossary/ground%20pepper\" class=\"glosslink\""
    ">"
    "ground pepper"
    "<"
    "/a"
    ">")
   (Quantity () "0.125")
   (DisplayQuantity () "1/8")
   (Unit () "ts")
   (MetricQuantity
    ()
    "0.616115197902132")
   (MetricDisplayQuantity
    ()
    "0.62")
   (MetricUnit () "ml")
   (IngredientInfo
    ()
    (Name
     ()
     "Coarsely ground pepper")
    (Department () "Spices"))
   (IsLinked () "true"))))



(define test 
'(Recipe
  ((xmlns:xsd "http://www.w3.org/2001/XMLSchema") (xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance"))
  "\r\n  "
  (RecipeID () "47725")
  "\r\n  "
  (Title () "Salmon on the Grill with Lemon Butter")
  "\r\n  "
  (Description ())
  "\r\n  "
  (Cuisine () "American")
  "\r\n  "
  (Category () "Main Dish")
  "\r\n  "
  (Subcategory () "Fish and Shellfish")
  "\r\n  "
  (PrimaryIngredient () "Fish")
  "\r\n  "
  (StarRating () "4.28571428571429")
  "\r\n  "
  (WebURL () "http://www.bigoven.com/recipe/salmon-on-the-grill-with-lemon-butter/47725")
  "\r\n  "
  (ImageURL () "http://redirect.bigoven.com/pics/rs/640/salmon-on-the-grill-with-lemon-butt-2.jpg")
  "\r\n  "
  (ReviewCount () "16")
  "\r\n  "
  (MedalCount () "0")
  "\r\n  "
  (FavoriteCount () "2275")
  "\r\n  "
  (Poster
   ()
   "\r\n    "
   (UserID () "0")
   "\r\n    "
   (ImageURL48 () "http://images.bigoven.com/image/upload/t_recipe-48,d_avatar-default.png/avatar-default.png")
   "\r\n    "
   (IsPremium () "false")
   "\r\n    "
   (IsKitchenHelper () "false")
   "\r\n    "
   (PremiumExpiryDate ((xsi:nil "true")))
   "\r\n    "
   (MemberSince ((xsi:nil "true")))
   "\r\n    "
   (IsUsingRecurly () "false")
   "\r\n  ")
  "\r\n  "
  (Ingredients
   ()
   "\r\n    "
   (Ingredient
    ()
    "\r\n      "
    (IngredientID () "462601")
    "\r\n      "
    (DisplayIndex () "0")
    "\r\n      "
    (IsHeading () "false")
    "\r\n      "
    (Name () "Salmon steaks")
    "\r\n      "
    (HTMLName
     ()
     "<"
     "a href=\"http://www.bigoven.com/glossary/salmon\" class=\"glosslink\""
     ">"
     "Salmon"
     "<"
     "/a"
     ">"
     " steaks")
    "\r\n      "
    (Quantity () "8")
    "\r\n      "
    (DisplayQuantity () "8")
    "\r\n      "
    (MetricQuantity () "8")
    "\r\n      "
    (MetricDisplayQuantity () "8")
    "\r\n      "
    (MetricUnit ())
    "\r\n      "
    (IngredientInfo () "\r\n        " (Name () "Salmon steaks") "\r\n        " (Department () "Meats") "\r\n      ")
    "\r\n      "
    (IsLinked () "true")
    "\r\n    ")
   "\r\n    "
   (Ingredient
    ()
    "\r\n      "
    (IngredientID () "462602")
    "\r\n      "
    (DisplayIndex () "1")
    "\r\n      "
    (IsHeading () "false")
    "\r\n      "
    (Name () "olive oil")
    "\r\n      "
    (HTMLName
     ()
     "<"
     "a href=\"http://www.bigoven.com/glossary/olive%20oil\" class=\"glosslink\""
     ">"
     "olive oil"
     "<"
     "/a"
     ">")
    "\r\n      "
    (Quantity () "1")
    "\r\n      "
    (MetricQuantity () "0")
    "\r\n      "
    (MetricDisplayQuantity ())
    "\r\n      "
    (MetricUnit ())
    "\r\n      "
    (IngredientInfo () "\r\n        " (Name () "olive oil") "\r\n        " (Department () "Oils") "\r\n      ")
    "\r\n      "
    (IsLinked () "true")
    "\r\n    ")
   "\r\n    "
   (Ingredient
    ()
    "\r\n      "
    (IngredientID () "462603")
    "\r\n      "
    (DisplayIndex () "2")
    "\r\n      "
    (IsHeading () "false")
    "\r\n      "
    (Name () "Butter")
    "\r\n      "
    (HTMLName () "<" "a href=\"http://www.bigoven.com/glossary/butter\" class=\"glosslink\"" ">" "Butter" "<" "/a" ">")
    "\r\n      "
    (Quantity () "0.25")
    "\r\n      "
    (DisplayQuantity () "1/4")
    "\r\n      "
    (Unit () "c")
    "\r\n      "
    (MetricQuantity () "59")
    "\r\n      "
    (MetricDisplayQuantity () "59")
    "\r\n      "
    (MetricUnit () "ml")
    "\r\n      "
    (PreparationNotes () "melted")
    "\r\n      "
    (IngredientInfo () "\r\n        " (Name () "Butter") "\r\n        " (Department () "Dairy") "\r\n      ")
    "\r\n      "
    (IsLinked () "true")
    "\r\n    ")
   "\r\n    "
   (Ingredient
    ()
    "\r\n      "
    (IngredientID () "462604")
    "\r\n      "
    (DisplayIndex () "3")
    "\r\n      "
    (IsHeading () "false")
    "\r\n      "
    (Name () "Lemon juice")
    "\r\n      "
    (HTMLName
     ()
     "<"
     "a href=\"http://www.bigoven.com/glossary/lemon\" class=\"glosslink\""
     ">"
     "Lemon"
     "<"
     "/a"
     ">"
     " juice")
    "\r\n      "
    (Quantity () "2")
    "\r\n      "
    (DisplayQuantity () "2")
    "\r\n      "
    (Unit () "tb")
    "\r\n      "
    (MetricQuantity () "30")
    "\r\n      "
    (MetricDisplayQuantity () "30")
    "\r\n      "
    (MetricUnit () "ml")
    "\r\n      "
    (IngredientInfo () "\r\n        " (Name () "Lemon juice") "\r\n        " (Department () "Produce") "\r\n      ")
    "\r\n      "
    (IsLinked () "true")
    "\r\n    ")
   "\r\n    "
   (Ingredient
    ()
    "\r\n      "
    (IngredientID () "462605")
    "\r\n      "
    (DisplayIndex () "4")
    "\r\n      "
    (IsHeading () "false")
    "\r\n      "
    (Name () "Chopped parsley")
    "\r\n      "
    (HTMLName
     ()
     "Chopped "
     "<"
     "a href=\"http://www.bigoven.com/glossary/parsley\" class=\"glosslink\""
     ">"
     "parsley"
     "<"
     "/a"
     ">")
    "\r\n      "
    (Quantity () "2")
    "\r\n      "
    (DisplayQuantity () "2")
    "\r\n      "
    (Unit () "tb")
    "\r\n      "
    (MetricQuantity () "30")
    "\r\n      "
    (MetricDisplayQuantity () "30")
    "\r\n      "
    (MetricUnit () "ml")
    "\r\n      "
    (IngredientInfo
     ()
     "\r\n        "
     (Name () "Chopped parsley")
     "\r\n        "
     (Department () "Produce")
     "\r\n      ")
    "\r\n      "
    (IsLinked () "true")
    "\r\n    ")
   "\r\n    "
   (Ingredient
    ()
    "\r\n      "
    (IngredientID () "462606")
    "\r\n      "
    (DisplayIndex () "5")
    "\r\n      "
    (IsHeading () "false")
    "\r\n      "
    (Name () "rosemary, dried")
    "\r\n      "
    (HTMLName
     ()
     "<"
     "a href=\"http://www.bigoven.com/glossary/rosemary\" class=\"glosslink\""
     ">"
     "rosemary"
     "<"
     "/a"
     ">"
     ", dried")
    "\r\n      "
    (Quantity () "0.25")
    "\r\n      "
    (DisplayQuantity () "1/4")
    "\r\n      "
    (Unit () "ts")
    "\r\n      "
    (MetricQuantity () "1.23223039580426")
    "\r\n      "
    (MetricDisplayQuantity () "1")
    "\r\n      "
    (MetricUnit () "ml")
    "\r\n      "
    (PreparationNotes () "Crushed")
    "\r\n      "
    (IngredientInfo () "\r\n        " (Name () "rosemary, dried") "\r\n        " (Department () "Spices") "\r\n      ")
    "\r\n      "
    (IsLinked () "true")
    "\r\n    ")
   "\r\n    "
   (Ingredient
    ()
    "\r\n      "
    (IngredientID () "462607")
    "\r\n      "
    (DisplayIndex () "6")
    "\r\n      "
    (IsHeading () "false")
    "\r\n      "
    (Name () "Marjoram")
    "\r\n      "
    (HTMLName
     ()
     "<"
     "a href=\"http://www.bigoven.com/glossary/marjoram\" class=\"glosslink\""
     ">"
     "Marjoram"
     "<"
     "/a"
     ">")
    "\r\n      "
    (Quantity () "1")
    "\r\n      "
    (MetricQuantity () "0")
    "\r\n      "
    (MetricDisplayQuantity ())
    "\r\n      "
    (MetricUnit ())
    "\r\n      "
    (IngredientInfo () "\r\n        " (Name () "Marjoram") "\r\n        " (Department () "Spices") "\r\n      ")
    "\r\n      "
    (IsLinked () "true")
    "\r\n    ")
   "\r\n    "
   (Ingredient
    ()
    "\r\n      "
    (IngredientID () "462608")
    "\r\n      "
    (DisplayIndex () "7")
    "\r\n      "
    (IsHeading () "false")
    "\r\n      "
    (Name () "Salt")
    "\r\n      "
    (HTMLName () "<" "a href=\"http://www.bigoven.com/glossary/salt\" class=\"glosslink\"" ">" "Salt" "<" "/a" ">")
    "\r\n      "
    (Quantity () "0.25")
    "\r\n      "
    (DisplayQuantity () "1/4")
    "\r\n      "
    (Unit () "ts")
    "\r\n      "
    (MetricQuantity () "1.23223039580426")
    "\r\n      "
    (MetricDisplayQuantity () "1")
    "\r\n      "
    (MetricUnit () "ml")
    "\r\n      "
    (IngredientInfo () "\r\n        " (Name () "Salt") "\r\n        " (Department () "Baking") "\r\n      ")
    "\r\n      "
    (IsLinked () "true")
    "\r\n    ")
   "\r\n    "
   (Ingredient
    ()
    "\r\n      "
    (IngredientID () "462609")
    "\r\n      "
    (DisplayIndex () "8")
    "\r\n      "
    (IsHeading () "false")
    "\r\n      "
    (Name () "Coarsely ground pepper")
    "\r\n      "
    (HTMLName
     ()
     "Coarsely "
     "<"
     "a href=\"http://www.bigoven.com/glossary/ground%20pepper\" class=\"glosslink\""
     ">"
     "ground pepper"
     "<"
     "/a"
     ">")
    "\r\n      "
    (Quantity () "0.125")
    "\r\n      "
    (DisplayQuantity () "1/8")
    "\r\n      "
    (Unit () "ts")
    "\r\n      "
    (MetricQuantity () "0.616115197902132")
    "\r\n      "
    (MetricDisplayQuantity () "0.62")
    "\r\n      "
    (MetricUnit () "ml")
    "\r\n      "
    (IngredientInfo
     ()
     "\r\n        "
     (Name () "Coarsely ground pepper")
     "\r\n        "
     (Department () "Spices")
     "\r\n      ")
    "\r\n      "
    (IsLinked () "true")
    "\r\n    ")
   "\r\n  ")
  "\r\n  "
  (Instructions
   ()
   "Brush fish lightly with oil. In bowl combine remaining ingredients.  Place salmon on well-oiled, hinged grill. Baste salmon with lemon butter mixture "
   "&"
   " cook 5 minutes, then turn "
   "&"
   " continue cooking 5 minutes. Baste with lemon butter several times during cooking.    ")
  "\r\n  "
  (YieldNumber () "8")
  "\r\n  "
  (YieldUnit () "Servings")
  "\r\n  "
  (TotalMinutes () "0")
  "\r\n  "
  (ActiveMinutes () "0")
  "\r\n  "
  (NutritionInfo
   ()
   "\r\n    "
   (SingularYieldUnit () "Paid API plan required for nutrition.")
   "\r\n    "
   (TotalCalories () "0")
   "\r\n    "
   (TotalFat () "0")
   "\r\n    "
   (CaloriesFromFat () "0")
   "\r\n    "
   (TotalFatPct () "0")
   "\r\n    "
   (SatFat () "0")
   "\r\n    "
   (SatFatPct () "0")
   "\r\n    "
   (MonoFat () "0")
   "\r\n    "
   (PolyFat () "0")
   "\r\n    "
   (TransFat () "0")
   "\r\n    "
   (Cholesterol () "0")
   "\r\n    "
   (CholesterolPct () "0")
   "\r\n    "
   (Sodium () "0")
   "\r\n    "
   (SodiumPct () "0")
   "\r\n    "
   (Potassium () "0")
   "\r\n    "
   (PotassiumPct () "0")
   "\r\n    "
   (TotalCarbs () "0")
   "\r\n    "
   (TotalCarbsPct () "0")
   "\r\n    "
   (DietaryFiber () "0")
   "\r\n    "
   (DietaryFiberPct () "0")
   "\r\n    "
   (Sugar () "0")
   "\r\n    "
   (Protein () "0")
   "\r\n    "
   (ProteinPct () "0")
   "\r\n  ")
  "\r\n  "
  (IsPrivate ((xsi:nil "true")))
  "\r\n  "
  (CreationDate () "2004-01-01T06:00:00Z")
  "\r\n  "
  (LastModified () "2014-10-13T20:16:03.877Z")
  "\r\n  "
  (IsBookmark () "false")
  "\r\n  "
  (BookmarkSiteLogo ())
  "\r\n  "
  (IsRecipeScan ((xsi:nil "true")))
  "\r\n  "
  (MenuCount () "28")
  "\r\n  "
  (NotesCount () "4")
  "\r\n  "
  (AllCategoriesText
   ()
   "collxgf|collxgrill|seafood|butter|olive oil|parsley|salmon|steak|lemon|grill|american|fish|collsxgrillm")
  "\r\n  "
  (IsSponsored () "false")
  "\r\n  "
  (VariantOfRecipeID ((xsi:nil "true")))
  "\r\n  "
  (Collection () "collsxgrillm")
  "\r\n  "
  (AdminBoost () "300")
  "\r\n  "
  (VerifiedDateTime () "2012-05-22T02:46:52Z")
  "\r\n  "
  (MaxImageSquare () "700")
  "\r\n  "
  (ImageSquares
   ()
   "\r\n    "
   (int () "700")
   "\r\n    "
   (int () "640")
   "\r\n    "
   (int () "512")
   "\r\n    "
   (int () "480")
   "\r\n    "
   (int () "320")
   "\r\n    "
   (int () "256")
   "\r\n    "
   (int () "200")
   "\r\n    "
   (int () "128")
   "\r\n    "
   (int () "120")
   "\r\n    "
   (int () "64")
   "\r\n    "
   (int () "48")
   "\r\n    "
   (int () "36")
   "\r\n  ")
  "\r\n  "
  (HeroPhotoUrl () "http://images.bigoven.com/image/upload/salmon-on-the-grill-with-lemon-butt-2.jpg")
  "\r\n  "
  (VerifiedByClass () "editor")
  "\r\n"))