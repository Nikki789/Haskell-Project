--
-- MATHFUN
-- Haskell assignment program
-- UP899244
--
import  Data.List
import  Text.Printf
import  Data.Foldable (minimumBy)
import  Data.Ord (comparing)
import  System.IO 
import  Data.Char
import  System.Exit


-- Types 
data Place =  Place { name :: String, longitude :: Float, latitude :: Float, rainfallFigures :: [Float]} deriving (Eq,Ord,Show,Read)


testData :: [Place]
testData = 
    [Place "London"      51.5  (-0.1)   [0, 0, 5, 8, 8, 0, 0],
    Place "Cardiff"      51.5  (-3.2)   [12, 8, 15, 0, 0, 0, 2],
    Place "Norwich"      52.6   1.3     [0, 6, 5, 0, 0, 0, 3],
    Place "Birmingham"   52.5  (-1.9)   [0, 2, 10, 7, 8, 2, 2],
    Place "Liverpool"    53.4  (-3.0)   [8, 16, 20, 3, 4, 9, 2],
    Place "Hull"         53.8  (-0.3)   [0, 6, 5, 0, 0, 0, 4],
    Place "Newcastle"    55.0  (-1.6)   [0, 0, 8, 3, 6, 7, 5],
    Place "Belfast"      54.6  (-5.9)   [10, 18, 14, 0, 6, 5, 2],
    Place "Glasgow"      55.9  (-4.3)   [7, 5, 3, 0, 6, 5, 0],
    Place "Plymouth"     50.4  (-4.1)   [4, 9, 0, 0, 0, 6, 5],
    Place "Aberdeen"     57.1  (-2.1)   [0, 0, 6, 5, 8, 2, 0],
    Place "Stornoway"    58.2  (-6.4)   [15, 6, 15, 0, 0, 4, 2],
    Place "Lerwick"      60.2  (-1.1)   [8, 10, 5, 5, 0, 0, 3],
    Place "St Helier"    49.2  (-2.1)   [0, 0, 0, 0, 6, 10, 0]]

--
--  Your functional code goes here
--

--1 demo

returnPlaceNames :: [Place] -> [String]
returnPlaceNames placesData = [name i | i <- placesData]

--2 demo

roundFunction :: Float -> Int -> Float
roundFunction number decimalPlaces = fromInteger (round (number * 10^decimalPlaces)) / 10^decimalPlaces


returnPlaceName :: String -> [Place] -> Place
returnPlaceName location (n:ps)
    | location == (name n) = n
    | otherwise = returnPlaceName location ps
    
calculateAverageRainfall :: String -> [Place] -> Float
calculateAverageRainfall name placesData = roundFunction(sum (rainfallFigures i)/7) 2
    where i = returnPlaceName name placesData

--3 demo

--These functions define the left and right padding and take the desired length (l), padding character (c),
--the string to pad (s) and return a padded string of the correct length.
--Replicate - creates a list of length given by the first argument and the items having value of the second one

leftPaddingWithChar, rightPaddingWithChar :: Int -> Char -> String -> String
leftPaddingWithChar l c s = replicate (l - length s) c ++ s
rightPaddingWithChar l c s = s ++ replicate (l - length s) c


--Formats a single place, doesn't bind the second and the third element of each "Place" as its not used
--Intercalate function - inserts "," in between the rainfallFigures
--Show - converts the place to a string

convertPlaceToString :: Place -> String
convertPlaceToString (Place name _ _ rainfallFigures) = rightPaddingWithChar 11 ' ' name
  ++ intercalate "," (map (leftPaddingWithChar 6 ' ' . show) rainfallFigures)
  
  
--Map function - applies the convertPlaceToString function to each element of the list
--Intersperse function - inserts a new line between the elements of the list

convertPlacesToString :: [Place] -> String
convertPlacesToString = concat . intersperse "\n" . map convertPlaceToString

    
--4 demo

--The showDays function takes the rainfall figures and returns the element of a list of the given index, equal to 0.
returnDays :: Int -> Place -> Bool
returnDays x (Place _ _ _ rainfallFigures) = rainfallFigures!!(x-1) == 0

--Filter function - returns all elements of the placesData, which apply to the condition of the showDays function 
returnPlaces :: Int -> [Place] -> [Place]
returnPlaces x placesData = filter(\p -> returnDays x p) placesData 

listOfDryPlaces :: Int -> [Place] -> [String]
listOfDryPlaces x placesData = returnPlaceNames(returnPlaces x placesData)



--5 demo
--zipWith - uses a function to add two lists together
--init - accepts the list and returns the whole list without the last item
updatePlace :: [Place] -> [Float] -> [Place]
updatePlace = zipWith (\p n -> p {rainfallFigures = n : init (rainfallFigures p)})
    

--6 demo
replacePlace :: Place -> Place -> [Place] -> [Place]
replacePlace _ _ [] = []
replacePlace old new (x:xs)
  | old == x  = new:xs
  | otherwise = x:replacePlace old new xs


--7 demo
--fst - returns the first item in a tuple
--snd - returns the second item in a tuple
-- $ - parentheses
--minimumBy - returns the least element 

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((y1 - y2)^2 + (x1 - x2)^2)

distanceToAPlace :: Float -> Float -> Place -> Float
distanceToAPlace lat long place = distance lat long (latitude place) (longitude place)

pairPlacesWithDistances :: Float -> Float -> [Place] -> [(Place, Float)]
pairPlacesWithDistances lat lon = map (\place -> (place, distanceToAPlace lat lon place))

closestDryPlace :: Float -> Float -> [Place] -> Place
closestDryPlace lat lon places = fst $ minimumBy (comparing snd) (pairPlacesWithDistances lat lon places) 
    

--
--  Demo
--

demo :: Int -> IO ()

-- displays the names of all the places
demo 1 = putStrLn (show (returnPlaceNames testData))


-- displays, to two decimal places, the average rainfall in Cardiff
demo 2 = putStrLn (show (calculateAverageRainfall "Cardiff" testData))


--returns all place names and their 7-day rainfall figures as a single string
demo 3 = putStrLn $ convertPlacesToString testData


-- displays the names of all places that were dry two days ago
demo 4 = putStrLn (show (listOfDryPlaces 2 testData))


-- updates the data with most recent rainfall [0,8,0,0,5,0,0,3,4,2,0,8,0,0] (and removes oldest rainfall figures)
demo 5 = putStrLn (show (updatePlace testData [0,8,0,0,5,0,0,3,4,2,0,8,0,0]))


-- replaces "Plymouth" with "Portsmouth" which has location 50.8 (N), -1.1 (E) and rainfall 0, 0, 3, 2, 5, 2, 1
demo 6 = putStrLn (show (replacePlace (Place "Plymouth" 50.4 (-4.1) [4,9,0,0,0,6,5]) (Place "Portsmouth" 50.8 (-1.1) [0, 0, 3, 2, 5, 2, 1]) testData)) 


-- displays the name of the place closest to 50.9 (N), -1.3 (E) that was dry yesterday
demo 7 = putStrLn (show (closestDryPlace 50.9 (-1.3) (returnPlaces 2 testData)))


--demo 8 = -- display the rainfall map


--
-- Screen Utilities (use these to do the rainfall map - note that these do 
-- not work in WinGHCi on Windows, so use GHCi.)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your rainfall map code goes here
--



--
-- Your user interface (and loading/saving) code goes here
--  

main :: IO()
main = do
    putStrLn ("\nThis is the content of the places.txt file:\n")
    loadPlaces <- readFile "places.txt"
    putStrLn loadPlaces
    putStrLn("Data is loaded.\n")
    putStrLn("This is a list with all the place names:\n")
    demo 1
    putStrLn("\nPlease choose one of the given options by typing in the option number.\n")
    answer

answer :: IO ()
answer = do
      putStrLn . unlines $ map concatNums menu
      choice <- getLine
      case validate choice of
         Just n  -> execute . read $ choice
         Nothing -> putStrLn "Please try again"

      answer
   where concatNums (i, (s, _)) = show i ++ ". " ++ s

validate :: String -> Maybe Int
validate s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
               | outOfBounds n = Nothing
               | otherwise     = Just n
         outOfBounds n = (n < 1) || (n > length menu)

menu :: [(Int, (String, IO ()))]
menu = zip [1.. ] [
   ("Display the names of all places", demo 1),
   ("Return the average rainfall for a place based on its name", demo 2),
   ("Display place names and their rainfall figures", demo 3),
   ("Show dry places a given number of days ago", demo 4),
   ("Update Rainfall", demo 5),
   ("Replace a place", demo 6),
   ("Display closest dry place yesterday", demo 7),
   ("Save and exit the program", exit)
 ]

execute :: Int -> IO ()
execute n = doExec $ filter (\(i, _) -> i == n) menu
   where doExec ((_, (_,f)):_) = f



exit = do
    h <- openFile "places.txt" WriteMode
    hPutStrLn h "London       51.5  -0.1   0, 0, 5, 8, 8, 0, 0 \nCardiff      51.5  -3.2   12, 8, 15, 0, 0, 0, 2 \nNorwich      52.6   1.3   0, 6, 5, 0, 0, 0, 3 \nBirmingham   52.5  -1.9   0, 2, 10, 7, 8, 2, 2 \nLiverpool    53.4  -3.0   8, 16, 20, 3, 4, 9, 2 \nHull         53.8  -0.3   0, 6, 5, 0, 0, 0, 4 \nNewcastle    55.0  -1.6   0, 0, 8, 3, 6, 7, 5 \nBelfast      54.6  -5.9   10, 18, 14, 0, 6, 5, 2 \nGlasgow      55.9  -4.3   7, 5, 3, 0, 6, 5, 0 \nPlymouth     50.4  -4.1   4, 9, 0, 0, 0, 6, 5 \nAberdeen     57.1  -2.1   0, 0, 6, 5, 8, 2, 0 \nStornoway    58.2  -6.4   15, 6, 15, 0, 0, 4, 2 \nLerwick      60.2  -1.1   8, 10, 5, 5, 0, 0, 3 \nSt Helier    49.2  -2.1   0, 0, 0, 0, 6, 10, 0 \nThe file has been rewritten"
    hClose h
    exitSuccess :: IO a
    


 

