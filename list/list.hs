--Simple list Implementation for Practice--
main=do
        let list1 = (Cons 5 (Cons 4 None))
        let list2 = (Cons 6 (Cons 7 None))
        print list1
        print (listtail list1)
        print (listhead list1)
        print (fmap (*2) list1)
        print (list1 == Cons 1 list1)
        print (concatList list1 list2)
        print (join (Cons list1 (Cons list2 (Cons list2 None))))
        print (fmap (+) list1 <*> list2)
        print (list1 >>= \n ->return n)
        print (foldr (-) 0 list1)

data MyList a= None | Cons a (MyList a) deriving (Show, Eq)
listhead::MyList a-> a
listhead (Cons n rest)= n

listtail::MyList a->MyList a
listtail (Cons n rest)=rest

concatList::(MyList a) -> (MyList a)-> (MyList a)
concatList None (Cons n rest) = (Cons n rest)
concatList (Cons n rest)  None = (Cons n rest)
concatList None None = None
concatList (Cons n rest1) (Cons m rest2) = (Cons n (concatList rest1 (Cons m rest2)))

join::(MyList (MyList a)) -> MyList a
join (Cons list None)= list
join (Cons list rest)= (join (Cons (concatList list (listhead rest)) (listtail rest)))


instance Functor MyList where
    fmap f (Cons n rest) = Cons (f n) (fmap f rest)
    fmap f None = None

instance Applicative MyList where
    pure n = Cons n None
    None <*> (Cons m rest2) = None
    (Cons f rest1) <*> (Cons m rest2) =  concatList (fmap f (Cons m rest2)) (rest1 <*> (Cons m rest2))
    _ <*> _= None

instance Monad MyList where
    return x = (Cons x None)
    list  >>= f = join (fmap f list)

instance Foldable MyList where
    foldr f acc None = acc
    foldr f acc (Cons n rest) = f n (foldr f acc rest)
