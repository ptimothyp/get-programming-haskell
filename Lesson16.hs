type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddleName FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  deriving (Show)

data Creator = AuthorCreator Author | ArtistCreator Artist deriving (Show)

-- data Author = Author Name
data Author = Author Name deriving (Show)

data Artist = Person Name | Band String deriving (Show)

hpLoveCraft =
  AuthorCreator
    ( Author
        ( TwoInitialsWithLast
            'H'
            'P'
            "Lovecraft"
        )
    )

data Book = Book
  { author :: Creator,
    isbn :: String,
    bookTitle :: String,
    bookYear :: String,
    bookPrice :: Double
  }
  deriving (Show)

data VinylRecord = VinylRecord
  { artist :: Creator,
    recordTitle :: String,
    recordYear :: String,
    recordPrice :: Double
  }
  deriving (Show)

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy deriving (Show)

data CollectibleToy = CollectibleToy
  { name :: String,
    description :: String,
    toyPrice :: Double
  }
  deriving (Show)

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy

-- madeBy :: StoreItem -> STg
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "Unknown"

book1 :: StoreItem
book1 =
  BookItem
    ( Book
        { author =
            ( AuthorCreator
                ( Author
                    ( TwoInitialsWithLast
                        'H'
                        'P'
                        "Lovecraft"
                    )
                )
            ),
          isbn = "B1234",
          bookTitle = "Great expectations",
          bookYear = "2009",
          bookPrice = 20
        }
    )
