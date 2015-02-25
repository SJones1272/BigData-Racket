#lang racket

(require xml)
(require net/url)
(require lang/htdp-advanced)

(define recipeID "167286")
(define apiKey "dvxbm87QjDgkfurO8s9Yx3UN315f4Djx")

(define R (xml->xexpr 
     (document-element 
      (read-xml (get-pure-port (string->url (string-append 
                                             "http://api.bigoven.com/recipe/" 
                                             recipeID
                                             "?api_key="
                                             apiKey)))))))

;;A Recipe is...
#|

'(Recipe
  (XML stuff)
  (RecipeID () String)
  (Title () String
  (Description () String)
  (Cuisine () String) ;;This is the type of the food
  (Category () String) ;;entre, appetizer, desert, etc.
  (Subcategory () String)
  (PrimaryIngredient () String)
  (StarRating () String)
  (WebURL () URL)
  (ImageURL () URL)
  (ReviewCount () String) ;; A number represented as a string....
  (MedalCount () String) ;;Not sure what this one is for
  (FavoriteCount () String) ;;Number of favorites a recipe received.
  (Poster () Poster) ;;Info about the submitter of the recipe, don't think we care. !!!
  (Ingredients () (...
                   (Ingredient () ...)
                   (Ingredient () ...)
                   etc .....))
  )
  (Instructions () String)           ;; A loooong string with no new lines that is the instructions
  (YieldNumber () Number-as-String)  ;; How many/much of the recipe makes
  (YieldUnit () String)              ;; unit of yeild (i.e. servings, cookies, etc..
  (Total Minutes () Number-as-String);; Approx. time in minutes it takes to make this recipe
  (Active Minutes () Number-as-String) ;; Approx. prep/plating time
  (Nutrition info () ...)            ;; Nutrition info for the recipe, requires paid API plan
  (IsPrivate ((....)))               ;; If the page is public or private
  (CreationDate () Date)             ;; A date recipe page was created (format YYYY-MM-DDTHr:Min:SecZ)
  (LastModified () Date)
  (IsBookMark () Boolean-as-String)  ;; If it's a bookmark?
  (...
    .
    .
    .
  (HeroPhotoUrl () URL)
)

|#

;;An Ingredient is...
#|
(Ingredient
    ()
    (IngredientID () Number-As-String ) ;; the ID of an ingredient

    (DisplayIndex () Number-As-String)  ;; the Indexing of which ingredient shows up where.
    (IsHeading () Boolean)              ;; Wether or not the ingredient is a Heading
    (Name () String)                    ;; name of the ingredient
    (HTMLName () ...)                   ;; HTML file on the ingredient
    (Quantity () Number-as-string)      ;; the inexact quantity of the ingredient needed
    (DisplayQuantity () " 1/3 ")        ;; the exact quantity of the ingredient needed
    (Unit () "cup")                     ;; the unit of measurement for the quantity
    (MetricQuantity () "79")            ;; the metric inexact quantity for the quantity
    (MetricDisplayQuantity () "79")     ;; the metric exact quantity for the quantity
    (MetricUnit () "ml")                ;; the metric unit of measurement
    (IngredientInfo ()                  ;; Extra info about an ingredient
     (Name () "mirin ")                 ;; Name of the ingredient again
     (Department () "Asian")            ;; Department of the store the ingredient might be found !!!
    )
    (IsLinked () "true")                ;; A boolean for if it is linked or not
|#

