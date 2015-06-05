
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Error
import qualified Data.Sequence as Seq
import qualified Data.Set as DS
import Data.Char
import Data.Either
import Test.QuickCheck 
import Control.Monad.Identity

newtype NFANode = Node Int 
                deriving (Eq,Ord)

instance Show NFANode where
   show (Node i) = "q" ++ show i
   
   
data Transition = Move   { from, to :: NFANode, sym :: Char }
                 | Lambda { from, to :: NFANode }
                 deriving (Eq,Ord)

instance Show Transition where
   show (Move f t i) = show f ++ 
                       " -" ++ show i ++ "-> " ++
                       show t
   show (Lambda f t) = show f ++ " ---> " ++ show t
   
   
   
   
   
   
data NFA = NFA { 
                   sigma   :: (DS.Set Char),
                   states  :: (DS.Set NFANode),
                   moves   :: (DS.Set Transition),
                   initial :: NFANode,
                   final   :: (DS.Set NFANode)
                }
          deriving (Eq)   
          
       
       
nfa0 = NFA {
              sigma  = DS.fromList "ab",
              states = DS.fromList $ fmap Node [0..3],
              moves  = DS.fromList [
                Move { from = Node 0, to = Node 0, sym = 'a' },
                Move { from = Node 0, to = Node 0, sym = 'a' },
                Move { from = Node 0, to = Node 1, sym = 'a' },
                Move { from = Node 1, to = Node 2, sym = 'b' },
                Move { from = Node 2, to = Node 3, sym = 'b' }
              ],
              initial = Node 0,
              final = DS.fromList [ Node 3 ]
            }       
       
       
-- Funciones a desarrollar ARBITRARY

instance Arbitrary NFANode where
   arbitrary = suchThat (liftM Node arbitrary) funcheck
	    where funcheck (Node i) = 0 < i
     

       
instance Show NFA where
   show (NFA sigma states moves initial final)  = "sigma = "++ show sigma ++ ",\n" ++
               "states = "++ show states ++ ",\n " ++ 
		"moves = "++ show moves ++ ",\n " ++
		"initial =" ++ show initial++ ",\n " ++
		"final= " ++ show final ++ "\n " 
   
    
instance Arbitrary NFA where
  arbitrary = do
	      sigma1  <- listOf1 $ (elements ['a'..'z'])
              states1 <- listOf1 $ (arbitrary :: Gen NFANode )
	      movimientos <- listOf1 $ funcMoves sigma1 (Node 0:states1)
	      nodeFinals <- listOf $ elements $ (Node 0:states1)
              return $ NFA {
		  sigma  = DS.fromList $ sigma1,
		  states = DS.fromList $ Node 0:states1,
		  moves  = DS.fromList $ movimientos,
		  initial = Node 0,
		  final = DS.fromList $ nodeFinals
	      }
	where funcMoves sig estados = do
		simbolo <- elements sig
		stateOrig <- elements estados
		stateDest <- elements estados
		frequency [
		    (5,return Move { from = stateOrig, 
			      to = stateDest , 
			      sym = simbolo }  ),
		    (1, return Lambda {from = stateOrig, 
					  to = stateDest })]
				  
	      

	      
ejem = NFA {
   sigma = DS.fromList "cfopr",
   states = DS.fromList [Node 0,Node 18,Node 55,Node 65,Node 114,Node 140,Node 184,Node 209,Node 249,Node 250,Node 432,Node 460],
   moves = DS.fromList [
	   Move { from = Node 0, to = Node 0, sym = 'r' },
	   Move { from = Node 0, to = Node 184, sym = 'o' },
	   Move { from = Node 0, to = Node 460, sym = 'c' },
	   Move { from = Node 55, to = Node 65, sym = 'f' },
	   Move { from = Node 114, to = Node 460, sym = 'p' },
	   Move { from = Node 140, to = Node 184, sym = 'c' },
	   Move { from = Node 184, to = Node 249, sym = 'c' },
	   Move { from = Node 184, to = Node 249, sym = 'o' },
	   Lambda { from = Node 184, to = Node 65},
	   Lambda { from = Node 65, to = Node 140},
	   Move { from = Node 209, to = Node 65, sym = 'o' }, 
	   Move { from = Node 209, to = Node 249, sym = 'f' },
	   Lambda { from = Node 249, to = Node 55},
	   Lambda { from = Node 65, to = Node 249},
	   Lambda { from = Node 460, to = Node 65}
	     ],
 initial = Node 0,
 final= DS.fromList [Node 0,Node 140,Node 184]

}   

ejem1 = NFA {
   sigma = DS.fromList "cfopr",
   states = DS.fromList [Node 0,Node 18,Node 55,Node 65,Node 114,Node 140,Node 184,Node 209,Node 249,Node 250,Node 432,Node 460],
   moves = DS.fromList [
	   Move { from = Node 0, to = Node 1, sym = 'c' },
	   Lambda { from = Node 1, to = Node 2},
	   Lambda { from = Node 0, to = Node 3},
	   Lambda { from = Node 3, to = Node 1},
	   Lambda { from = Node 0, to = Node 4}
	   --Lambda { from = Node 2, to = Node 0}
	   ],
 initial = Node 0,
 final= DS.fromList [Node 1,Node 140,Node 184]

}  

yaContraejem = NFA {
    sigma = DS.fromList "p",
    states = DS.fromList [Node 0,Node 2],
    moves = DS.fromList [Lambda { from = Node 0, to = Node 2}],
    initial =Node 0,
    final= DS.fromList [Node 2]
}

contraejem = NFA {
  sigma = DS.fromList "i",
  states = DS.fromList [Node 0,Node 2],
  moves = DS.fromList [Move { from = Node 0, to = Node 0, sym = 'i' }],
  initial =Node 0,
  final= DS.fromList [Node 0]
}

isMove, isLambda :: Transition -> Bool
isMove (Move _ _ _)  = True
isMove (Lambda _ _)  = False
isLambda  = not.isMove


       
lambdaMoves :: NFA -> NFANode -> DS.Set NFANode
lambdaMoves nfa node = DS.fromList $ filter (filNodes) $ node:map (extracMoves node) (DS.toList (moves nfa))
  where extracMoves node (Lambda f t) = if  node == f then t
					else Node (-1) 
	extracMoves node (Move _ _ _) = Node (-1)
	filNodes (Node i) = i >=0 
	
					  
-- ------------------------ OJO AQUI----------------------------
normalMoves :: NFA -> Char -> NFANode -> DS.Set NFANode
normalMoves nfa char node = DS.fromList $ filter (filNodes) $ map (extracMoves nfa char node) (DS.toList (moves nfa))
  where extracMoves nfa char node (Move f t i) = if  node == f && char == i then t
					else Node (-1)
	extracMoves nfa char node (Lambda f t) = Node (-1)	  
	filNodes (Node i) = i >=0 


	

destinations :: NFA -> Char -> NFANode -> DS.Set NFANode
destinations nfa char node = DS.unions
			( map (fixSet (lambdaMoves nfa))
			(DS.toList (DS.map (normalMoves nfa char)
			     (fixSet (lambdaMoves nfa) (DS.fromList [node])))))

{-
destinations :: NFA -> Char -> NFANode -> DS.Set NFANode
destinations nfa char node= DS.fromList $ filter (filNodes) $ concatMap (extracMoves nfa char node) (DS.toList (moves nfa))
	where extracMoves nfa char node (Move f t i) = if  node == f && char == i then t:(DS.toList (lambdaMoves nfa t))
					else [Node (-1)]
	      extracMoves nfa char node (Lambda f t) = if  node == f then (DS.toList (destinations nfa char t))
					else [Node (-1)] 	  
	      filNodes (Node i) = i >=0 -}
	
	

fixSet :: Ord a => (a -> DS.Set a) -> DS.Set a -> DS.Set a
fixSet f s = if s ==  DS.unions (map f (DS.toList s))
		then s
	     else fixSet f $  DS.unions (map f (DS.toList s))
		

-- ---------------- CONSTRUCCION DEL MONAD ----------------------------------------


data NFAReject = Stuck (DS.Set NFANode) String
                | Reject (DS.Set NFANode)
                deriving (Show)

instance Error NFAReject

data NFARun = NFARun { w :: String , qs :: DS.Set NFANode }
      deriving ( Show , Eq )
      

type NFATypeT a = ErrorT NFAReject (RWST NFA (Seq.Seq (DS.Set NFANode)) NFARun Identity) a

initialState :: String -> NFARun
initialState word = NFARun { w = word  , qs = DS.fromList[Node 0] }

accepting :: NFA -> DS.Set NFANode -> Bool
accepting nfa nodos = not $ DS.null (DS.filter (flip DS.member (final nfa)) nodos)	




runNFA :: NFA -> [Char] -> IO ()
runNFA nfa word = funEither $ runIdentity 
	  $ evalRWST (runErrorT (start >> flow )) nfa (initialState word)
  where funEither (Right _ , s) = putStrLn $ show s
	funEither (Left l , _) = putStrLn $ show l

start :: NFATypeT ()
start = do
       nfa <- ask 
       word <- get
       put $ word { qs = fixSet (lambdaMoves nfa) (DS.fromList [Node 0])}
       tell $ Seq.singleton $ (fixSet (lambdaMoves nfa) (DS.fromList [Node 0]))
       
       return()
       
flow :: NFATypeT ()      
flow = do 
      nfa <- ask
      word <- get
      if (null (w word)) then
	if (accepting nfa (qs word)) then
	  put $ word { w = "",qs = DS.empty}
	else 
	  throwError $ Reject (qs word)
      else	 
	if DS.null (extracStates nfa (head (w word)) (qs word) DS.empty) then
	    throwError $ Stuck (qs word) (w word)
	else
	    do
	      put $ word { w = tail (w word), 
		qs = extracStates nfa (head (w word)) (qs word) DS.empty}
	      tell $ Seq.singleton 
		  $ extracStates nfa (head (w word)) (qs word) DS.empty
	      flow	   
	  
  where extracStates n p e s = if (DS.null e) then s
	else extracStates n p (DS.fromList (drop 1 (DS.toList e))) 
	    (DS.union s (destinations n p ((head (DS.toList e)))))




	    
prop_acceptsemptyword :: NFA -> Property
prop_acceptsemptyword nfa = 
  (DS.empty /= (DS.intersection (fixSet (lambdaMoves nfa) (DS.fromList [Node 0])) (final nfa))) ==> 
    runNFAEmptyWord nfa ""
  where runNFAEmptyWord nfa word = funEither $ runIdentity 
	  $ evalRWST (runErrorT (start >> flow )) nfa (initialState word)
	funEither (Right _ , _) = True
	funEither (Left _ , _) = False
	    
	    
prop_acceptancelength :: NFA -> String -> Property
prop_acceptancelength nfa w = let (x,tam) = runNFAWord nfa w
			      in x ==> length w +1 == tam
  where runNFAWord nfa word = funEither1 $ runIdentity 
	  $ evalRWST (runErrorT (start >> flow )) nfa (initialState word)
	funEither1 (Right _ , l) = (True,Seq.length l)
	funEither1 (Left _ , _) = (False, 0)
--  quickCheckWith (stdArgs { maxDiscardRatio = 500}) prop_acceptancelenght

-- -----------------------------CREACION DE UN MONAD -----------------------



data Otro a = Otro ((a -> Beta) -> Beta)

data Beta = Chamba (IO Beta)
           | Convive Beta Beta
           | Quieto          

           
           
instance Show Beta where
    show (Chamba x)    = " chamba "
    show (Convive x y) = " convive(" ++ show x 
                                     ++ "," 
                                     ++ show y ++ ") "
    show Quieto        = " quieto "           
           

hacer :: Otro a -> Beta
hacer (Otro f) = Convive (f (\_ -> Quieto)) Quieto

quieto :: Otro a
quieto = Otro (\_ -> Quieto)

chambea :: IO a -> Otro a
chambea a = Otro (\f -> Chamba (fmap f a))

--Chequear
convive :: Otro a -> Otro ()
convive a = Otro (\_ -> Convive (hacer a) Quieto)

pana :: Otro a -> Otro a -> Otro a
pana (Otro x) (Otro y) = Otro(\f -> Convive (x f) (y (\_ -> x f)))

vaca :: [Beta] -> IO ()
vaca []               = return ()
vaca (Chamba x   :xs) = do 
                          x' <- x
                          vaca (x':xs)
vaca (Convive x y:xs) = vaca (xs++[x,y])
vaca (Quieto     :xs) = vaca xs

instance Monad Otro where
  return x = Otro (\f -> f x)
  (Otro f) >>= g = Otro (\_ -> f (hacer.g))



cartel :: Otro ()
cartel = pana (dale (clavo 42)) 
              (pana (dale (clavo 69))
                    (pana (dale (clavo 17)) 
                          (dale (clavo 23) >> chambea (putStrLn ""))))

quedo :: Otro a -> IO ()
quedo x = vaca [hacer x]

clavo :: Int -> String
clavo 17 = "/nlmce"
clavo 23 = "/y./p6"
clavo 42 = "htptuc2"
clavo 69 = "t:irofr"
 
dale :: String -> Otro ()
dale xs = mapM_ (chambea . putChar) xs



