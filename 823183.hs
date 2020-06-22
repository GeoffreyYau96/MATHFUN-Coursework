-- 
-- MATHFUN
-- Haskell Assignment
-- 823183
--
import Data.List
import Data.Char
--
-- Types
--
type Title = String
type Director = String
type Year = Int
type User = String
type UserWhoLikeTheFilm = [User]
type UserWhoDislikeTheFilm = [User]
-- Define Film type here 
type Film = (Title, Director, Year, UserWhoLikeTheFilm, UserWhoDislikeTheFilm)

-- testdatabase
testDatabase :: [Film]
testDatabase = [("Blade Runner", "Ridley Scott", 1982, ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"], ["Sam", "Olga", "Tim"]), ("The Fly", "David Cronenberg", 1986, ["Garry", "Dave", "Zoe"], ["Kevin", "Emma", "Heidi", "Jo", "Kate"]), ("Body Of Lies", "Ridley Scott", 2008, ["Garry", "Dave"], ["Bill", "Olga", "Tim", "Zoe", "Paula"]), ("Avatar", "James Cameron", 2009, ["Dave", "Amy", "Liz"], ["Olga", "Tim", "Zoe", "Paula"]), ("Titanic", "James Cameron", 1997, ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"], ["Sam", "Wally", "Kate"]), ("The Departed", "Martin Scorsese", 2006, ["Wally", "Liz", "Kevin", "Tim", "Emma"], ["Olga", "Dave", "Kate", "Zoe"]), ("Aliens", "Ridley Scott", 1986, ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"], ["Tim", "Emma", "Jo", "Olga"]), ("Kingdom Of Heaven", "Ridley Scott", 2005, ["Jo", "Wally", "Emma"], ["Tim", "Garry", "Ian", "Neal"]), ("Alien: Covenant", "Ridley Scott", 2017, ["Kevin", "Tim"], ["Emma", "Jo", "Liz"]), ("E.T. The Extra-Terrestrial", "Steven Spielberg", 1982, ["Dave", "Amy", "Garry", "Ian", "Neal"], ["Jenny", "Kate", "Emma", "Olga"]), ("Bridge of Spies", "Steven Spielberg", 2015, ["Wally", "Sam", "Dave", "Neal"], ["Bill", "Garry", "Ian", "Kate"]), ("Jaws", "Steven Spielberg", 1975, ["Jenny", "Emma", "Bill", "Neal"], ["Sam", "Ian", "Kate"]), ("The Martian", "Ridley Scott", 2015, ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"], ["Ian", "Neal", "Tim", "Liz"]), ("The BFG", "Steven Spielberg", 2016, ["Sam", "Wally", "Dave", "Jo", "Kate"], ["Neal"]), ("The Shawshank Redemption", "Frank Darabont", 1994, ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe", "Heidi"], ["Jo"]), ("Gladiator", "Ridley Scott", 2000, ["Olga", "Neal", "Kate", "Garry"], ["Heidi", "Bill", "Sam", "Zoe"]), ("The Green Mile", "Frank Darabont", 1999, ["Kevin", "Tim", "Emma", "Heidi"], ["Kate", "Jenny", "Zoe"]), ("True Lies", "James Cameron", 1994, ["Sam", "Dave"], ["Emma", "Olga", "Jenny", "Zoe"]), ("Super 8", "J J Abrams", 2011, ["Kevin", "Tim", "Emma", "Olga", "Heidi"], ["Wally", "Dave", "Jenny", "Zoe"]), ("Minority Report", "Steven Spielberg", 2002, ["Kevin", "Kate", "Tim", "Emma", "Jenny", "Zoe"], ["Olga", "Heidi"]), ("War Horse", "Steven Spielberg", 2011, ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"], ["Heidi", "Jenny", "Sam"]), ("Silence", "Martin Scorsese", 2016, ["Wally", "Emma", "Tim", "Heidi", "Bill", "Jo"], ["Dave", "Olga"]), ("The Terminal", "Steven Spielberg", 2004, ["Kate", "Dave", "Jo", "Wally", "Emma"], ["Heidi"]), ("Star Wars: The Force Awakens", "J J Abrams", 2015, ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz"], ["Olga", "Jo", "Neal"]), ("Hugo", "Martin Scorsese", 2011, ["Wally", "Sam"], ["Kate", "Bill", "Dave"])]
--
--  Your functional code goes here
--
--
-- Removes all symbols / non letters from a string
removeSymbols :: String -> String
removeSymbols = filter isLetter

-- Check if director exists
checkDirectorExists :: String -> [Film] -> Bool
checkDirectorExists inputDirector ((_, director, year, _, _ ):xs)
 | director == inputDirector = True
 | xs == [] = False
 | otherwise = checkDirectorExists inputDirector xs

 --it caculates the average of a list
average xs = sum xs / genericLength xs

--i.add a new film to the database
-- add new film with film title, director and film year
addFilm :: String -> String -> Int -> [Film] -> [Film]
addFilm title director year film = film ++ [(title, director, year, [], [])]

--ii.give all films in the database
-- get all films from the testDatabase
getAllFilms :: [Film]
getAllFilms = testDatabase

-- return films as formated string
filmsAsString :: [Film] -> String
filmsAsString [] = ""
filmsAsString ((title, director, year, userWhoLikeTheFilm, userWhoDislikeTheFilm):xs) = if length(userWhoLikeTheFilm) == 0 || length (userWhoDislikeTheFilm) == 0
  then "\nTitle: " ++ title ++ "\nDirector: " ++ director  ++ "\nYear: " ++ show ( year ) ++ "\nLike" ++ show (userWhoLikeTheFilm) ++ "\nDislike"++ show (userWhoDislikeTheFilm)++ "\n" ++ filmsAsString xs
  else "\nTitle: " ++ title ++ "\nDirector: " ++ director  ++ "\nYear: " ++ show ( year ) ++ "\nWeb Rating: " ++ show( (length (userWhoLikeTheFilm) * 100) `div` ((length(userWhoDislikeTheFilm)) + (length(userWhoLikeTheFilm)))) ++ "\n" ++ filmsAsString xs

--iii.give all the films by a given director
-- return all the films by the inputDirector
filmsGivenDirector :: String -> [Film] -> [Film]
filmsGivenDirector inputDirector film = [ (title, director, year, userWhoLikeTheFilm, userWhoDislikeTheFilm) | (title, director, year, userWhoLikeTheFilm, userWhoDislikeTheFilm ) <- film, inputDirector == director  ]

--iv.give all films that have a website rating of 75% or higher
-- return all films with a web rating of 75% or higher
filmsWebRatingHigher ::[Film] -> [Film]
filmsWebRatingHigher film = [ (title, director, year, userWhoLikeTheFilm, userWhoDislikeTheFilm) | (title, director, year, userWhoLikeTheFilm, userWhoDislikeTheFilm ) <- film, ((length (userWhoLikeTheFilm) * 100) `div` ((length(userWhoDislikeTheFilm)) + (length(userWhoLikeTheFilm)))) >= 75  ]
--v.give the average website rating for the films of a given director
-- get the webrating of a film
getWebRating :: [Film] -> Float
getWebRating ((title, director, year, userWhoLikeTheFilm, userWhoDislikeTheFilm):xs) = fromIntegral ( length userWhoLikeTheFilm) * 100 / fromIntegral( length userWhoLikeTheFilm + length userWhoDislikeTheFilm)

--giveAverageRating :: Director -> [Film] -> Float
--giveAverageRating inputDirector film = read (printf "%3.1f" (average (map getWebRating (filmsGivenDirector inputDirector film)))) :: Float

--vi.give the titles of all films that a particular user has rated, along with how they have been rated ('like' or 'dislike') by that user
-- display the all the films that the user has rated
displayUserFilm :: String -> [Film] -> [Film]
displayUserFilm user film = filter(userasRated user) film

-- go through all the films and pick up the ones that the user has rated
userasRated :: String -> Film -> Bool
userasRated user (title, director, year, userWhoLikeTheFilm, userWhoDislikeTheFilm)
  | elem user userWhoLikeTheFilm = True
  | elem user userWhoDislikeTheFilm = True
  | otherwise = False

-- return all the films that the user has rated in formated string
filmLikeDislike :: [Film] -> String
filmLikeDislike [] = ""
filmLikeDislike ((title, _, _, userWhoLikeTheFilm, userWhoDislikeTheFilm):xs) = "\nTitle: " ++ title ++ "\nLike" ++ show (userWhoLikeTheFilm) ++ "\nDislike"++ show (userWhoDislikeTheFilm) ++ "\n" ++ filmLikeDislike xs

--vii.allow a user to say they like or dislike a partular film

--viii.give all the films released between two given years (inclusive), sorted in descending order of website rating
-- return all the films released between two given years
filmsReleasedBeforeAfterYear :: Int -> Int -> [Film] -> [Film]
filmsReleasedBeforeAfterYear year1 year2 film = [ (title, director, year, userWhoLikeTheFilm, userWhoDislikeTheFilm) | (title, director, year, userWhoLikeTheFilm, userWhoDislikeTheFilm ) <- film, year1 <= year && year <= year2  ]

--Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).
demo :: Int -> IO ()
demo 1  = putStrLn $ filmsAsString(addFilm "Sherlock Gnomes" "John Stevenson" 2018 testDatabase)

demo 2  = putStrLn $ (filmsAsString testDatabase)

demo 3  = putStrLn $ filmsAsString (filmsGivenDirector "Ridley Scott" testDatabase) 

demo 4  = putStrLn $ filmsAsString (filmsWebRatingHigher testDatabase)

demo 5  = putStrLn $ "This function is work in progress"

demo 6  = putStrLn $ filmLikeDislike(displayUserFilm "Emma" testDatabase)

demo 7  = putStrLn $ "This function is work in progress"

demo 71 = putStrLn $ "This function is work in progress"

demo 72 = putStrLn $ "This function is work in progress"

demo 8  = putStrLn $ filmsAsString (filmsReleasedBeforeAfterYear 2000 2006 testDatabase) 

--
--
-- Your user interface code goes here
--
--
main :: IO ()
main = do 
 putStrLn("================================================================================================================")
 putStrLn("Film Database")
 loadedFile <- readFile "filmDb.txt"
 let filmsDatabase = read loadedFile
 putStrLn("\nSuccesfully loaded "++ show(length filmsDatabase) ++" films!")
 putStrLn("================================================================================================================\n")
 putStr("Please input your name: ")
 name <- getLine
 putStrLn ("\n")
 mainMenu name filmsDatabase

 -- Returns an error message for incorrect input
sendErrorInput :: String -> String
sendErrorInput "int" = "\n[Error]: The value entered was not the expected value (Number / Int)\n[Error]: Option is being reset, please try again.\n"
sendErrorInput "string" = "\n[Error]: The value entered was not the expected value (Word / String)\n[Error]: Option is being reset, please try again.\n"
sendErrorInput "input" = "\n[Error]: There was no input given!\n[Error]: Option is being reset, please try again.\n"
sendErrorInput "year" = "\n[Error]: The year is not valid, enter a value after year 1900 - 2100.!\n[Error]: Option is being reset, please try again.\n"
sendErrorInput "option" = "\n[Error]: An incorrect action was given, please try again!\n"
sendErrorInput _ = ""

-- Main Menu Options
mainMenu :: String -> [Film] -> IO()
mainMenu user filmDB = do 
 putStrLn("================================================================================================================")
 putStrLn("Welcome to the Film Database, " ++ user ++".\nEnter the number for the required option.")
 putStrLn(" 1 | Add a new film to the database")
 putStrLn(" 2 | Display all films within the database")
 putStrLn(" 3 | Display all films by a given director")
 putStrLn(" 4 | Display all films that have a website rating of 75% or higher")
 putStrLn(" 5 | Display the average website rating for the films of a given director")
 putStrLn(" 6 | Display the titles of all films that you have been rated )")
 putStrLn(" 7 | Insert a rating for a particular film")
 putStrLn(" 8 | Display all the films relased between two given years, sorted in decending order of website rating")
 putStrLn(" 0 | Save and Exit")
 putStrLn("================================================================================================================\n")
 putStr("Please insert your option: ")
 option <- getLine
 putStrLn("\n")
 action option user filmDB

-- Actions for the system
action :: String -> String -> [Film] -> IO ()
-- Save the database and exit the UI
action "0" user filmDB = saveAndExit user filmDB
-- Add a new film to the database
action "1" user filmDB = addNewFilm user filmDB
-- Display all the films within the database
action "2" user filmDB = displayAllFilms user filmDB
-- Display all films by a given director
action "3" user filmDB = displayFilmsDirector user filmDB
-- Display all films that have a website rating of 75% or higher
action "4" user filmDB = displayAllFilmsHigher user filmDB
-- Display the average website rating for the films of a given director
action "5" user filmDB = workInProgress user filmDB
-- Display the titles of all films that a particular user has rated, along with how they have been rated ('like' or 'dislike') by that user
action "6" user filmDB = displayTitleWithLikeDislike user filmDB
-- Insert a rating for a particular film
action "7" user filmDB = workInProgress user filmDB
-- Display all the films relased between two given years, sorted in decending order of website rating
action "8" user filmDB = displayFilmsBeforeAfter user filmDB
--Display errors if the input is invalid!
action _ user filmDB = do
 putStrLn (sendErrorInput "option" )
 mainMenu user filmDB

 -- UI Function to save and exit the UI
saveAndExit :: String -> [Film] -> IO()
saveAndExit user filmDB =
 do
  putStrLn("================================================================================================================")
  putStrLn("Saving the Database")
  -- Avoids the issue with Parse Error, Force writing ALL of the filmDB before continuing.
  length filmDB `seq` writeFile "filmDb.txt" (show filmDB)
  putStrLn("Saved the Database successfully")
  putStrLn("Exiting the system")
  putStrLn("================================================================================================================\n")

 -- UI function to add a new film to the film database
addNewFilm :: String -> [Film] -> IO()
addNewFilm user filmDB = 
 do 
  putStrLn("================================================================================================================")
  putStrLn("Add new film to database\n")
  
  putStr("Enter the film title: ")
  filmTitl <- getLine
  let filmTitle = removeSymbols filmTitl
  
  putStr("Enter the film director: ")
  filmDire <- getLine
  let filmDirector = removeSymbols filmDire
  
  putStr("Enter the film release date: ")
  filmYear <- getLine

  if filmTitle == "" && filmDirector == "" && filmYear == ""
  then do
   putStrLn ("Returning you to the main menu")
   mainMenu user filmDB
  else do
   if filmTitle == "" || filmDirector == "" || filmYear == ""
   then do
    putStrLn(sendErrorInput "input")
    addNewFilm user filmDB
   else do
    -- To validate the films; 
    -- reads the filmYear input, converts to an Integer & String
    -- if the film exists with nothing inside the String, it's a valid input / number! otherwise just catch and display an error.
    case reads filmYear :: [(Integer, String)] of
     [(n, "")] -> do
      let year = (read filmYear :: Int)
      if year >= 1990 && year <= 2100
      then do
       putStrLn("Adding your film to the database")
       let filmDBnew = addFilm filmTitle filmDirector year filmDB
       putStrLn("================================================================================================================\n")
       mainMenu user filmDBnew
      else do
       putStrLn(sendErrorInput "year")
       addNewFilm user filmDB
       
     _ -> do
      putStrLn(sendErrorInput "int" )
      addNewFilm user filmDB

-- UI Function to return all the films as a string
displayAllFilms :: String -> [Film] -> IO()
displayAllFilms user filmDB =
 do
  putStrLn("================================================================================================================")
  putStrLn("View all the films within the film database")
  putStrLn( filmsAsString filmDB )
  putStrLn("================================================================================================================\n")
  mainMenu user filmDB

-- UI Function to return all the films of a specific director
displayFilmsDirector :: String -> [Film] -> IO()
displayFilmsDirector user filmDB =
 do
  putStrLn("================================================================================================================")
  putStrLn("View all the films of a particular director.")
  putStr("Director Name: ")
  inputDirector <- getLine

  if inputDirector == ""
  then do
   putStrLn(sendErrorInput "input")
   displayFilmsDirector user filmDB
  else do
   if checkDirectorExists inputDirector filmDB == False
   then do
   putStrLn("[Error] The director does not exist in our database!")
    else do
    putStrLn( filmsAsString (filmsGivenDirector inputDirector filmDB) )
 
  putStrLn("================================================================================================================\n")
  mainMenu user filmDB

  -- UI Function to return all the films with a web rating 75% or higher 
displayAllFilmsHigher :: String -> [Film] -> IO()
displayAllFilmsHigher user filmDB =
 do
  putStrLn("================================================================================================================")
  putStrLn("View all the films within the film database with a web rating 75% or higher")
  putStrLn( filmsAsString (filmsWebRatingHigher filmDB ))
  putStrLn("================================================================================================================\n")
  mainMenu user filmDB


-- UI Function to return all the films titles that the user like or dislike
displayTitleWithLikeDislike :: String -> [Film] -> IO()
displayTitleWithLikeDislike user filmDB =
 do
  putStrLn("================================================================================================================")
  putStrLn("View all the films within the film database that you have liked or disliked")
  putStrLn( filmLikeDislike(displayUserFilm user filmDB ))
  putStrLn("================================================================================================================\n")
  mainMenu user filmDB

-- UI Function to return all the films titles before a certain year and afer a certain year
displayFilmsBeforeAfter :: String -> [Film] -> IO()
displayFilmsBeforeAfter user filmDB =
  do
  putStrLn("================================================================================================================")
  putStrLn("View all the films within the film database before a certain year and after certain a year")
  putStr("Before: ")
  year1 <- getLine
  putStr("After: ")
  year2 <- getLine
  putStrLn("\n")
  if year1 == "" || year2 == ""
  then do
   putStrLn(sendErrorInput "input")
   displayFilmsBeforeAfter user filmDB
  else do
     let filmYearBefore = read year1 :: Int
     let filmYearAfter = read year2 :: Int 
     do
      putStr  (filmsAsString (filmsReleasedBeforeAfterYear filmYearBefore filmYearAfter filmDB) )
      putStrLn("================================================================================================================\n")
      mainMenu user filmDB

workInProgress :: String -> [Film] -> IO()
workInProgress user filmDB =
  do 
    putStrLn("================================================================================================================")
    putStrLn("This function is work in progress")
    putStrLn("================================================================================================================\n")
    mainMenu user filmDB