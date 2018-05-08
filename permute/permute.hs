main=do
    print (permuteList [1,2,3,4,5])

permuteList::Eq a=>[a] -> [[a]]
permuteList [] = []
permuteList (x:[]) = [[x]]

permuteList lst = lst >>= (\x -> (fmap (\m -> x:m) (permuteList (filter (\n -> n /= x) lst))))
