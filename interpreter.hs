type Id      = String
type Numero  = Double
type Boolean = Bool
type Param   = String

data Termo = Var Id
          | Lit Numero
	  | Boo Boolean
          | Som Termo Termo
          | Lam Id Termo
          | Apl Termo Termo
          | Atr Id Termo
          | Seq Termo Termo
	  | While Boolean Termo
	  | If Boolean Termo Termo
	  | Else Termo
	  | F Id Id Termo
	  | Param Numero
	  | E Termo Termo -- Expressões
	  | Ou Termo Termo
	  | Not Termo Termo
	  | Igu Termo Termo
	  deriving Show

data Valor = Num Double
          | Fun (Valor -> StateTransformer Valor)
          | Boolean Bool
          | Erro
	  | Nada


type Estado = [(Id,Valor)]
type Ambiente = [(Id,Valor)]

data StateTransformer a = ST (Estado -> (a,Estado))

instance Show (Valor) where
     show (Num a) = show a
     show (Fun f) = show (f (Num 0))
     show (Boolean b) = show b
     show (Erro) = "erro"

instance (Show a) => Show (StateTransformer a) where
     show (ST f) = show (f [])

instance Monad (StateTransformer) where
  return r = ST (\e -> (r,e))
  (ST m) >>= f = ST (\e -> let (v,e1) = m e
                               (ST n) = f v
                           in (n e1)
                    )

int :: [(Id, Valor)] -> Termo -> StateTransformer Valor
int a (Var i) = ST (\e -> (search i (a++e),e))
int a (Lit n) = return (Num n)
int a (Boo b) = return (Boolean b)
int a (Param n) = return (Num n)

int a (Som t u) = do { t1 <- int a t;
                       u1 <- int a u;
                       return (somaVal t1 u1); }

int a (Lam i t) = return (Fun (\v -> int ((i,v):a) t))
int a (Apl f t) = do { f1 <- int a f;
                       t1 <- int a t;
                       app f1 t1; }

int a (Atr i t) = ST (\e -> let (ST f) = int a t
                                (v,ei) = f e
                            in (v,wr (i,v) ei))

int a (Seq t u) = do { int a t; int a u; } 
-- WHILE
int a (While b t) = do { bool <- int a (Boo b);
			if analizar bool then int a (While b t)  else return Nada }

--int a (Igu t1 t2) = do { bool1 <- int a (Boo t1);
--  		         bool2 <- int a (Boo t2);
--		         if ((analizar bool1) == (analizar bool2)) then int a (Boo True) else int a (Boo False)}

int a (If b t1 (Else t2)) = if b then int a t1 else int a t2
int a (F i p t) = int a t

analizar :: Valor -> Bool
analizar (Boolean b) = b

search :: (Eq i) => i -> [(i, Valor)] -> Valor
search i [] = Erro
search i ((j,v):l) = if i == j then v else search i l

somaVal :: Valor -> Valor -> Valor
somaVal (Num x) (Num y) = Num (x+y)
somaVal _ _ = Erro

app :: Valor -> Valor -> StateTransformer Valor
app (Fun f) v = f v
app _ _ = return Erro

wr :: (Eq t) => (t, t1) -> [(t, t1)] -> [(t, t1)]
wr (i,v) [] = [(i,v)]
wr (i,v) ((j,u):l) = if (i == j) then (j,v):l else [(j,u)] ++ (wr (i,v) l)

prog1 = do {
           int [] (Seq (Atr "f" (Lam "x" (Som (Var "x") (Lit 10)))) (Atr "v" (Apl (Var "f") (Lit 5))))
           }

prog2 = do {
           int [] (Atr "x" (Lit 10))
           }

prog3 = do {
           int [] (Som (Lit 1) (Lit 10))
           }

prog4 = do {
	   int [] (Lam "x" (Som (Var "x") (Lit 10)))
           }

prog5 = do {
	   int [] (Atr "x" (Lit 10));
	   }

progWhile = do {
	   int [] (While (True) (Som (Lit 1) (Lit 2)))
	   --int [] (Seq (Atr "x" (Lit 10)) (While (Igu (Var "x") (Lit 10)) (Atr "x" (Lit 9))) )
           }

progIfElse = do {
	   int [] (If (111 < 10) (Lit 100) (Else (Lit 10)))
           }

progFun = do {
	 -- atual
	 int [] (F "soma" "x" (Som (Param 1) (Lit 10)));

          -- ideal
	  --int [] (Apl (F "soma" "x" (Som (Param "x") (Lit 1))) (Lit 10))
	  }
