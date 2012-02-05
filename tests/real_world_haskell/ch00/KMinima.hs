-- Obtém os k menores elementos de xs
module KMinima where
	import Data.List
	minima k xs = take k (sort xs)
