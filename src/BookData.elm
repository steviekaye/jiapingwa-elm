module BookData exposing (Book, allbooks)

type alias Book =
  {
      bookID : Int
    , title : String
    , year : Int
    , cover : String
    , titleCN : String
    , titlePinyin : String
    , synopsis : String
    , price : Int
  }

type alias BookList =
    {
    books : List Book
    }

a = Book 1 "Happy Dreams" 2007 "/images/happy_dreams.jpg" "高兴" "Gaoxing" "todo" 35
b = Book 2 "Ruined City" 1993 "/images/ruined_city.jpg" "废都" "Fei Du" "todo" 35

c = Book 3 "The Earthen Gate" 1996 "/images/the_earthen_gate.jpg" "土门" "Tu Men" "todo" 40

d = Book 4 "The Lantern Bearer" 2013 "/images/the_lantern_bearer.jpg" "带灯" "Dai Deng" "todo" 30

e = Book 5 "Shaanxi Opera" 2005 "/images/shaanxi_opera.jpg" "秦腔" "Qin Qiang" "todo" 50

f = Book 6 "Turbulence" 1987 "/images/turbulence.jpg" "浮躁" "Fuzao" "todo" 25

g = Book 7 "Old Kiln Village" 2011 "/images/old_kiln_village.jpg" "古炉" "Gu Lu" "todo" 45

allbooks = BookList [a, b, c, d, e, f, g]
