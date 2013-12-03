import Lab2

stud = stud' programming discrete_mathematics discrete_mathematics

stud' (x:xs) [] z = (x : stud' xs z z)
stud' [] _ _ = [] 
stud' (x:xs) (y:ys) z  
	|(x == y) = stud' xs (y:ys) z
	|otherwise = stud' (x:xs) ys z

main = do
putStrLn "Список имён студентов:"
mapM_ putStrLn $ map (\(pos, st) -> show pos ++ "). " ++ fst(st) ++ " " ++ snd(st)) $ zip [1..] stud
