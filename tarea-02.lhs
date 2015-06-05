\documentclass[11pt,fleqn]{article}

\usepackage{tikz}
\usepackage{multicol}
\usepackage{latexsym}
\usepackage{array}
\usepackage[english,spanish]{babel}
\usepackage{lmodern}
\usepackage{listings}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}
\usepackage{xcolor}

\usepackage{algorithmic}
\usepackage{algorithm}

\usetikzlibrary{positioning,shapes,folding,positioning,shapes,trees}

\hypersetup{
  colorlinks=true,
  linkcolor=blue,
  urlcolor=blue
}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}



\lstset{
   language=Haskell,
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey}
}

\long\def\ignore#1{}

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 2}

\author{Erick Marrero\\
09-10981\\
\href{mailto:emhn@usb.ve}{<09-10981@usb.ve>}}

\date{Mayo 22, 2015}

\maketitle

\pagebreak

\section*{Autómatas Finitos No Determinísticos}

\begin{lstlisting}

> import Control.Monad
> import Control.Monad.RWS
> import Control.Monad.Error
> import qualified Data.Sequence as Seq
> import qualified Data.Set as DS
> import Data.Char
> import Data.Either
> import Test.QuickCheck 
> import Control.Monad.Identity

\end{lstlisting}

\noindent
Considere el siguiente conjunto de Tipos Abstractos de Datos
diseñados para representar Autómatas Finitos No-Determinísticos
con $\lambda-$Transiciones ($\lambda-$NFA).
\\


\noindent
Los estados de un $\lambda-$NFA serán representados con
un \texttt{newtype}. Esto tiene el doble propósito de tener
enteros diferenciados, para un desempeño aceptable, y poder
disponer de una instancia \texttt{Show} separada con la cual
mantener la tradición de que los estados sean denotados
\texttt{q0}, \texttt{q1}, \ldots

\begin{lstlisting}

> newtype NFANode = Node Int 
>                 deriving (Eq,Ord)
>
> instance Show NFANode where
>    show (Node i) = "q" ++ show i

\end{lstlisting}

\noindent
Luego, representaremos las transiciones de un $\lambda-$NFA con un
tipo completo que incluye alternativas para transiciones que
consumen un símbolo de entrada y para $\lambda-$transiciones.
Con el tipo completo, además de la conveniencia de funciones
de acceso a los campos provista por la notación de registros,
también podemos tener una instancia \texttt{Show} separada que
muestre las transiciones de manera más atractiva.

\begin{lstlisting}

> data Transition = Move   { from, to :: NFANode, sym :: Char }
>                 | Lambda { from, to :: NFANode }
>                 deriving (Eq,Ord)
>
> instance Show Transition where
>   show (Move f t i) = show f ++ 
>                       " -" ++ show i ++ "-> " ++
>                       show t
>   show (Lambda f t) = show f ++ " ---> " ++ show t
>

\end{lstlisting}

\noindent
Finalmente, aprovechando la librería \texttt{Data.Set} para
representar conjuntos, podemos representar un $\lambda-$NFA
de manera directa. 

\begin{lstlisting}

> data NFA = NFA { 
>                   sigma   :: (DS.Set Char),
>                   states  :: (DS.Set NFANode),
>                   moves   :: (DS.Set Transition),
>                   initial :: NFANode,
>                   final   :: (DS.Set NFANode)
>                }
>          deriving (Eq,Show)

\end{lstlisting}

\noindent
En esta definición:

\begin{itemize}
\item
  \texttt{sigma} es el conjunto no vacío de caracteres del alfabeto.
\item 
  \texttt{states} es el conjunto no vacío de estados; siempre debe
  incluir al menos al estado inicial.
\item
  \texttt{moves} es el conjunto de transiciones.
\item
  \texttt{initial} es el estado final que \emph{siempre} sera
  \texttt{q0} (o sea \texttt{(Node 0)}).
\item
  \texttt{final} es el conjunto de estados finales.
\end{itemize}

\noindent
Con estas definiciones podemos construir expresiones que denoten
$\lambda-$NFA

\begin{lstlisting}

> nfa0 = NFA {
>              sigma  = DS.fromList "ab",
>              states = DS.fromList $ fmap Node [0..3],
>              moves  = DS.fromList [
>                Move { from = Node 0, to = Node 0, sym = 'a' },
>                Move { from = Node 0, to = Node 0, sym = 'a' },
>                Move { from = Node 0, to = Node 1, sym = 'a' },
>                Move { from = Node 1, to = Node 2, sym = 'b' },
>                Move { from = Node 2, to = Node 3, sym = 'b' }
>              ],
>              initial = Node 0,
>              final = DS.fromList [ Node 3 ]
>            }

\end{lstlisting}

\pagebreak

\section*{Generando $\lambda-$NFAs}

\noindent

En primera instancia, se necesita crear nodos aleatorios y que 
estos no sean negativos, para ello se usa suchThat para chequear
que sean positivos y crearlos de manera arbitraria.

\begin{lstlisting}

> instance Arbitrary NFANode where
>   arbitrary = suchThat (liftM Node arbitrary) funcheck
> 	    where funcheck (Node i) = 0 < i

\end{lstlisting}

\noindent
En el caso de \texttt{NFA} queremos que el generador sólo produzca
$\lambda-$NFA con estructura consistente. En este sentido, la
instancia debe \emph{garantizar}:

\begin{itemize}
\item
  Que el alfabeto sea no vacío sobre letras minúsculas.
  Para ello se usa \emph{listOf1} para asegurar que no
  sea vacía y \emph{elements} para seleccionar letras del 
  abcedario.
\item
  Que el conjunto de estados sea de tamaño arbitrario pero
  que \emph{siempre} incluya a \texttt{(Node 0)}. De manera 
  análoga para este caso, solo que el \texttt{(Node 0)} se 
  introduce de manera manual
\item
  Que tenga una cantidad arbitraria de transiciones. Todas las
  transiciones tienen que tener sentido: entre estados que están
  en el conjunto de estados y con un símbolo del alfabeto. Se
  desea que haya una $\lambda-$transición por cada cinco
  transiciones convencionales.
  Para esto se seleccionan del conjunto de estados y de símbolos
  para generar los movimientos correctos usando la funcion
  \emph{elements}, y se usa \emph{frequency} para que haya una
  $\lambda-$transición por cada cinco transiciones convencionales.
\item
  El estado inicial siempre debe ser \texttt{(Node 0)}.
\item
  El estado final debe ser un subconjunto del conjunto de estados,
  posiblemente vacío.
  En este caso como puede ser vacío entonces se usa \emph{listOf}
\end{itemize}

\begin{lstlisting}

> instance Arbitrary NFA where
>  arbitrary = do
>	    sigma1  <- listOf1 $ (elements ['a'..'z'])
>           states1 <- listOf1 $ (arbitrary :: Gen NFANode )
>	    movimientos <- 
>		listOf1 $ funcMoves sigma1 (Node 0:states1)
>	    nodeFinals <- listOf $ elements $ (Node 0:states1)
>           return $ NFA {
>		  sigma  = DS.fromList $ sigma1,
>		  states = DS.fromList $ Node 0:states1,
>		  moves  = DS.fromList $ movimientos,
>		  initial = Node 0,
>		  final = DS.fromList $ nodeFinals
>	    }
>	where funcMoves sig estados = do
>		simbolo <- elements sig
>		stateOrig <- elements estados
>		stateDest <- elements estados
>		frequency [
>		    (5,return Move { from = stateOrig, 
>			      to = stateDest , 
>			      sym = simbolo }  ),
>		    (1, return Lambda {from = stateOrig, 
>					  to = stateDest })]

\end{lstlisting}

\section*{Simulador de $\lambda-$NFA}

\noindent
Recordará de CI-3725 que los $\lambda-$NFA son equivalentes a
los Autómatas Determinísticos, en virtud del algoritmo que simula
el paso \emph{simultáneo} por todos los estados válidos para cada
transición. Le sugiero buscar el Sudkamp y sus notas de clase para
reforzar el tema, aún cuando iremos desarrollando la solución paso
a paso.
\\

\noindent
En primer lugar, es necesario identificar si un movimiento es
convencional o es una $\lambda-$transición, así que debe proveer
las funciones


\begin{lstlisting}

> isMove, isLambda :: Transition -> Bool
> isMove (Move _ _ _)  = True
> isMove (Lambda _ _)  = False
> isLambda  = not.isMove
>

\end{lstlisting}

\noindent
En el algoritmo de simulación, la manera de desplazarse a partir
de un estado particular consiste en considerar la $\lambda-$clausura
del estado. Luego, para cada uno de los estados, determinar a su vez
aquellos estados a los cuales es posible moverse consumiendo
exactamente un símbolo de entrada. Finalmente, considerar la
$\lambda-$clausura de esos estados, obteniéndose así los estados
destino.
\\

\noindent
Para resolver ese problema, Ud. deberá implantar varias funciones
auxiliares:

\begin{itemize}
\item 
  Dado un $\lambda-$NFA y un estado, calcular el conjunto
  de estados a los cuales se puede alcanzar con \emph{una}
  $\lambda-$transición.
  Para esta función se busca en todos los movimientos posibles
  las $\lambda-$transición a partir de un estado, y se obtienen
  los nuevos estados con movimientos gratis, y para los que no 
  son $\lambda-$transición, se les pone una marca de \texttt{(Node -1)}
  para luego eliminarla.  

  \begin{lstlisting}

> lambdaMoves :: NFA -> NFANode -> DS.Set NFANode
> lambdaMoves nfa node = 
>	DS.fromList $ filter (filNodes) $ 
>		node:map (extracMoves node) 
>			(DS.toList (moves nfa))
>  where extracMoves node (Lambda f t) = if  node == f then t
>					else Node (-1) 
>        extracMoves node (Move _ _ _) = Node (-1)
>	 filNodes (Node i) = i >=0 

  \end{lstlisting}

\item
  Dado un $\lambda-$NFA, un caracter del alfabeto y un estado,
  calcular el conjunto de estados a los cuales se puede alcanzar
  con una transición que consuma el caracter de entrada.
  De manera análoga con esta función, solo que se obtendrán los estados
  a los que se llega consumiendo la entrada.

  \begin{lstlisting}

> normalMoves :: NFA -> Char -> NFANode -> DS.Set NFANode
> normalMoves nfa char node = 
>	DS.fromList $ filter (filNodes) $ 
>		map (extracMoves nfa char node) 
>		    (DS.toList (moves nfa))
>  where extracMoves nfa char node (Move f t i) = 
>			if  node == f && char == i then t
>					else Node (-1)
>        extracMoves nfa char node (Lambda f t) = Node (-1)	  
>	 filNodes (Node i) = i >=0 

  \end{lstlisting}

\item
  Dado un $\lambda-$NFA, un caracter del alfabeto y un estado,
  calcular el conjunto de estados a los cuales se puede alcanzar
  consumiendo el caracter de entrada. Esta es la función que
  debe calcular la $\lambda-$clausura del estado inicial, los
  desplazamientos desde ese conjunto ahora consumiendo la entrada,
  y luego la $\lambda-$clausura final.
   

  \begin{lstlisting}
  
>
>
> destinations :: NFA -> Char -> NFANode -> DS.Set NFANode
> destinations nfa char node = 
>	DS.unions
>		( map (fixSet (lambdaMoves nfa))
>		(DS.toList (DS.map (normalMoves nfa char)
>			   (fixSet (lambdaMoves nfa) 
>				   (DS.fromList [node])))))

  \end{lstlisting}

\item
  El cálculo de la $\lambda-$clausura es un algoritmo
  de la familia de Algoritmos de Punto Fijo. Para calcular el punto 
  fijo, se construye un conjunto xs' resultante de aplicar f a 
  cada elemento de xs. Si xs' es igual a xs, entonces se encontro
  el punto fijo. Si no son iguales, se calcula fixSet f xs'.

  \begin{lstlisting}

> fixSet :: Ord a => (a -> DS.Set a) -> DS.Set a -> DS.Set a
> fixSet f s = if s ==  DS.unions (map f (DS.toList s))
>		then s
>	     else fixSet f $ DS.unions (map f (DS.toList s))

  \end{lstlisting}


\noindent
Una vez implantadas estas funciones, estará en posición de implantar
el simulador monádico de $\lambda-$NFA poniendo en práctica monads
y transformadores.
\\

\noindent
La función principal de este simulador será

\begin{lstlisting}

> runNFA :: NFA -> [Char] -> IO ()
> runNFA nfa word = funEither $ runIdentity 
>	  $ evalRWST (runErrorT (start >> flow )) 
>				nfa (initialState word)
>  where funEither (Right _ , s) = putStrLn $ show s
>        funEither (Left l , _)  = putStrLn $ show l

\end{lstlisting}

\noindent
que para un \texttt{nfa} particular simulará el procesamiento de la
palabra de entrada \texttt{word}. El comportamiento de la función
dependerá de lo que ocurra en la simulación. 
Para esta función, se usan los  monads \texttt{Error}, \texttt{Reader}, 
\texttt{Writer}, \texttt{State} e \texttt{Identity}. 
Se uso el transformador \texttt{RWST}, y encima de este se coloca el monad
\texttt{Error} para poder atrapar las excepciones que se explican más adelante.

El monad \texttt{Reader} se usará para llevar
el NFA que se está simulando, el monad \texttt{Writer} para ir
conservando los conjuntos de estados por los cuales vaya
avanzando, el monad \texttt{State} para mantener la entrada
restante y el conjunto de estados actuales, y el monad
\texttt{Error} para poder emitir las excepciones necesarias
para el rechazo.
\\

\noindent
Ambos rechazos serán manejados como excepciones, así que necesitará

\begin{lstlisting}

> data NFAReject = Stuck (DS.Set NFANode) String
>                | Reject (DS.Set NFANode)
>                deriving (Show)
>
> instance Error NFAReject

\end{lstlisting}

Este tipo de datos serán usados como error para:
\item
  Si la palabra es \emph{rechazada} porque se consumió la entrada
  pero el $\lambda-$NFA quedó en un estado no final, debe
  \emph{imprimir} en pantalla un mensaje indicando que rechazó
  y en cuales estados se encontraba la simulación.

\item
  Si la palabra es \emph{rechazada} porque no hay transiciones
  posibles sobre el siguiente símbolo de entrada, debe
  \emph{imprimir} en pantalla un mensaje indicando que rechazó
  indicando la entrada restante y el estado de estancamiento.
\end{itemize}

\noindent
Se crea el tipo \emph{NFATypeT}, para trabajar con los monads  
\begin{lstlisting}

> type NFATypeT a = 
>	ErrorT NFAReject 
>		(RWST NFA 
>			(Seq.Seq (DS.Set NFANode)) 
>				NFARun Identity) a  

\end{lstlisting}

\noindent
Necesitará un tipo de datos que se aprovechará en el Monad 
\texttt{State}

\begin{lstlisting}

> data NFARun = NFARun { w :: String, qs :: DS.Set NFANode }
>             deriving (Show,Eq)

\end{lstlisting}

\begin{itemize}
\item
  Se necesita una función para preparar el estado inicial 
  de la simulación, con la palabra de entrada y fijando 
  el estado actual en \texttt{(Node 0)}.
  \begin{lstlisting}

> initialState :: String -> NFARun
> initialState word = 
>	NFARun { w = word  , qs = DS.fromList[Node 0] }

  \end{lstlisting}
\item
  Una función para determinar si en un conjunto de estados hay uno
  o más que sean estados finales de un NFA.
  Para realizar esta función, simplemente se van eliminando del 
  conjunto de los nodos que son pasados como parámetros, los que no 
  pertenezcan al conjunto de los estados finales, entonces solamente
  quedarán los que sean estados finales, y si el resultado no es vacío
  entonces hay uno o más que sean estados finales de ese NFA.
  \begin{lstlisting}

> accepting :: NFA -> DS.Set NFANode -> Bool
> accepting nfa nodos = 
>	not $ DS.null (DS.filter 
>		(flip DS.member (final nfa)) nodos)

  \end{lstlisting}
\item
  Una función monádica \texttt{start} que comienza la simulación
  a partir del estado inicial.
  \begin{lstlisting}
  
> start :: NFATypeT ()
> start = do
>       nfa <- ask 
>       word <- get
>       put $ word { qs = fixSet (lambdaMoves nfa) 
>			(DS.fromList [Node 0])}
>       tell $ Seq.singleton $ 
>		(fixSet (lambdaMoves nfa) 
>			(DS.fromList [Node 0]))
>       return()  

\end{lstlisting}
  
\item
  Una función monádica \texttt{flow} que completa la simulación.
  Esta función es la que debe operar en el monad combinado y
  hacer avanzar el $\lambda-$NFA consumiendo la entrada. Si
  detecta que debe rechazar la palabra, lanza la excepción
  adecuada; si logra procesar todo la entrada y aceptar, debe
  permitir acceso al historial de movimientos.
\begin{lstlisting}

> flow :: NFATypeT ()      
> flow = do 
>      nfa <- ask
>      word <- get
>      if (null (w word)) then
>	if (accepting nfa (qs word)) then
>	  put $ word { w = "",qs = DS.empty}
>	else 
>	  throwError $ Reject (qs word)
>      else	 
>	if DS.null (extracStates nfa (head (w word)) 
>				(qs word) DS.empty) then
>	    throwError $ Stuck (qs word) (w word)
>	else
>	    do
>	      put $ word { w = tail (w word), 
>		qs = extracStates nfa (head (w word)) 
>					(qs word) DS.empty}
>	      tell $ Seq.singleton 
>		  $ extracStates nfa (head (w word)) 
>					(qs word) DS.empty
>	      flow	   
>	  
>  where extracStates n p e s = if (DS.null e) then s
>		else extracStates n p 
>			(DS.fromList (drop 1 (DS.toList e))) 
>	    		(DS.union s 
>				(destinations n p 
>				    ((head (DS.toList e)))))
  
\end{lstlisting}  

\end{itemize}  
Una vez que su simulación esté operando correctamente, escriba dos
propiedades QuickCheck y aproveche la instancia \texttt{Arbitrary} para
comprobar:

\begin{itemize}
\item
  Todo $\lambda-$NFA que tenga un estado final en la $\lambda-$clausura
  de su estado inicial acepta la palabra vacía.
  \begin{lstlisting}

> prop_acceptsemptyword :: NFA -> Property
> prop_acceptsemptyword nfa = 
>  (DS.empty /= (DS.intersection 
>			(fixSet (lambdaMoves nfa) 
>				(DS.fromList [Node 0])) 
>			(final nfa))) ==> 
>    				runNFAEmptyWord nfa ""
>  where runNFAEmptyWord nfa word = 
>		funEither $ runIdentity $ evalRWST 
>			(runErrorT (start >> flow )) 
>				nfa (initialState word)
>	 funEither (Right _ , _) = True
>	 funEither (Left _ , _) = False

  \end{lstlisting}
\item
  Cuando un $\lambda-$NFA acepta una palabra de longitud $n$, el camino
  recorrido tiene longitud $n+1$.
  Para probar esta propiedad, se tiene que aceptar la palabra
  para que se cumpla el antecedente, para ello se ejecuta el siguiente comando:
  \begin{verbatim}
  
ghci> quickCheckWith (stdArgs { maxDiscardRatio = 500}) 
				  prop_acceptancelenght
  
  \end{verbatim}
  
  \begin{lstlisting}

> prop_acceptancelength :: NFA -> String -> Property
> prop_acceptancelength nfa w = let (x,tam) = 
>					runNFAWord nfa w
>			      	in x ==> length w +1 == tam
>  where runNFAWord nfa word = funEither1 $ runIdentity 
>	  $ evalRWST (runErrorT (start >> flow )) 
>				nfa (initialState word)
>	 funEither1 (Right _ , l) = (True,Seq.length l)
>	 funEither1 (Left _ , _) = (False, 0)

  \end{lstlisting}
\end{itemize}

\newpage
\section*{Otro Beta}

\noindent
La vida es dura. Todos los días hay que salir a la calle,
buscando la fuerza para alcanzar otro beta y echar pa'lante.

\begin{lstlisting}

> data Otro a = Otro ((a -> Beta) -> Beta)

\end{lstlisting}

\noindent
Y se hace más difícil, porque el \texttt{Beta} está en una chamba o en
hacer llave con un convive, así hasta que te toque el quieto.

\begin{lstlisting}

> data Beta = Chamba (IO Beta)
>           | Convive Beta Beta
>           | Quieto

\end{lstlisting}

\noindent
Se complica ver el \texttt{Beta}, porque \texttt{IO} esconde lo que
tiene. Hay que inventarse una ahí para tener idea\ldots

\begin{lstlisting}

> instance Show Beta where
>    show (Chamba x)    = " chamba "
>    show (Convive x y) = " convive(" ++ show x 
>                                     ++ "," 
>                                     ++ show y ++ ") "
>    show Quieto        = " quieto "

\end{lstlisting}

\noindent
A veces hay suerte, y uno encuentra algo que hacer. Uno llega
con fuerza, hace lo que tiene que hacer, y allí sigues buscando
otro Beta
\begin{lstlisting}

> hacer :: Otro a -> Beta
> hacer (Otro f) = Convive (f (\_ -> Quieto)) Quieto

\end{lstlisting}

\noindent
pero es triste ver cuando simplemente es un quieto. No importa
si traes fuerza, te quedas quieto.

\begin{lstlisting}

> quieto :: Otro a
> quieto = Otro (\_ -> Quieto)

\end{lstlisting}

\noindent
Pero hay que ser positivo. Hay que pensar que si uno encuentra
un oficio, uno chambea. Sólo hay que darle a la chamba y de
allí uno saca fuerza. Se usa \texttt{fmap} para aplicar la funcion
y luego dejarlo en \texttt{IO}

\begin{lstlisting}

> chambea :: IO a -> Otro a
> chambea a = Otro (\f -> Chamba (fmap f a))

\end{lstlisting}

\noindent
y si el trabajo se complica, lo mejor es encontrar un convive
para compartir la fuerza, aunque al final quedes tablas

\begin{lstlisting}

> convive :: Otro a -> Otro ()
> convive a = Otro (\_ -> Convive (hacer a) Quieto)

\end{lstlisting}

\noindent
Para llegar lejos, es mejor cuadrar con un pana. Cada uno
busca por su lado, y luego se juntan.

\begin{lstlisting}

> pana :: Otro a -> Otro a -> Otro a
> pana (Otro x) (Otro y) = 
>		Otro(\f -> Convive (x f) (y (\_ -> x f)))

\end{lstlisting}

\noindent
y así al final, cuando se junten los panas, hacen una vaca
y se la vacilan

\begin{lstlisting}

> vaca :: [Beta] -> IO ()
> vaca []               = return ()
> vaca (Chamba x   :xs) = do 
>                          x' <- x
>                          vaca (x':xs)
> vaca (Convive x y:xs) = vaca (xs++[x,y])
> vaca (Quieto     :xs) = vaca xs

\end{lstlisting}

\noindent
Me pasaron el dato, que buscar el beta es como un perol, que
con cada mano que le metes, siempre te hace echar pa'lante. Que
consulte al chamo de sistemas, pa'que me muestre como hacer. Porque
a esos peroles con cosas, que paso a paso avanzan, dizque los
mentan ``Monads''. A mi no me dá el güiro, pero espero que ti
si, menor.

\begin{lstlisting}

> instance Monad Otro where
>   return x       = Otro (\f -> f x)
>   (Otro f) >>= g = Otro (\_ -> f (hacer.g))

\end{lstlisting}

\noindent
El resultado final es el siguiente: 

\begin{verbatim}
ghci> quedo cartel
http://tinyurl.com/2fcpre6

\end{verbatim}


\ignore{
\begin{lstlisting}

> cartel :: Otro ()
> cartel = pana (dale (clavo 42)) 
>               (pana (dale (clavo 69))
>                     (pana (dale (clavo 17)) 
>                           (dale (clavo 23) >> chambea (putStrLn ""))))
> 
> quedo :: Otro a -> IO ()
> quedo x = vaca [hacer x]
> 
> clavo :: Int -> String
> clavo 17 = "/nlmce"
> clavo 23 = "/y./p6"
> clavo 42 = "htptuc2"
> clavo 69 = "t:irofr"
> 
> dale :: String -> Otro ()
> dale xs = mapM_ (chambea . putChar) xs

\end{lstlisting}
}

\end{document}