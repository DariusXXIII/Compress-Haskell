{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : Estéban DARTUS
-}
module LZ.LZW(compress, uncompress) where
import LZ.Dictionaries(ascii)
import Data.Maybe
import Data.List
import Debug.Trace (trace)

type Dictionary = [String]

-- | LZW compress method
compress :: String -> [Int]
compress [] = [] -- Si le texte est vide, on revoie un tableau vide (Optimisation)
compress texte = compressHelper texte [] ascii []            -- Le texte qu'on veut compresser, le morceau de texte à trouver, la table ascii et un accumulateur de récursion.

compressHelper :: String -> String -> Dictionary -> [Int] -> [Int]
compressHelper [] [] _ acc = acc -- Ne devrait jamais être utilisé mais on ne sait jamais. Sortie de secours en quelque sorte
--Condition de sortie standard. A mettre avant la boucle récursive sinon problème de liste vide
compressHelper [] morceau dico acc -- Si on a atteint la fin du texte à compresser . . .
  | morceau == [] = acc -- . . . Si le morceau à rechercher est vide, on retourne l'accumulateur
  | otherwise = (acc ++ [fromJust preIndex]) -- . . . Sinon on concatène l'index du dernier morceau à l'accumulateur
  where
    preIndex = findIndex (\x -> x == morceau) dico --Calcul de l'index du dernier morceau

-- Boucle récursive principale
compressHelper texte morceau dico acc -- Si on n'a pas encore compressé tout le texte
  | isNothing index = compressHelper texte [] (dico ++ [nouvMorceau]) (acc ++ [fromJust preIndex]) -- Si le morceau recherché n'est pas dans le dictionnaire, alors on le rajoute et on récupère l'index du morceau moins son dernier caractère
  | otherwise = compressHelper (tail texte) nouvMorceau dico acc -- Sinon si le morceau recherché existe, on concatène le caractère suivant et on recherche le tout
  where
    index = findIndex (\x -> x == nouvMorceau) dico
    nouvMorceau = trace ("texte: "++ show texte ++ "  morceau: " ++ show morceau ++ "  acc: " ++ show acc) $ morceau ++ [head texte]
    temp = findIndex (\x -> x == morceau) dico -- recherche de l'index d'un morceau
    preIndex = if isNothing temp then Just (length ascii) else temp


-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress [] = Nothing -- Si le code donné est vide, on retourne rien (Optimisation)
uncompress code = uncompressHelper code 0 ascii (Just []) -- Code à décompresser, XXXX, Dictionnaire de décompression et l'accumulateur de récursion

--Fonction de décompression dans le cas où la liste d'entier n'est pas vide
uncompressHelper :: [Int] -> Int -> Dictionary -> Maybe String -> Maybe String
uncompressHelper [] _ _ acc = acc
uncompressHelper (index:code) preIndex dico acc
  | index < 0 || length dico < index || isNothing acc || (length dico == index && length (fromJust acc) == 0) = Nothing --Gestion des cas où la décompression n'est plus possible
  | otherwise = uncompressHelper code index nouvDico (Just (maybeAcc ++ char)) 
  where
    maybeAcc = fromJust acc
    char = nouvDico !! index
    preChar = dico !! preIndex
    nouvDico = if length maybeAcc == 0
               then dico
               else if index < (length dico)
                    then dico ++ [(preChar ++ [head char])]
                    else dico ++ [(preChar ++ [head preChar])] 
