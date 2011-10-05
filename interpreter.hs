type Id = String
type Numero = Double

data Termo = Var Id
          | Lit Numero
          | Som Termo Termo
          | Lam Id Termo
          | Apl Termo Termo
          | Atr Id Termo
          | Seq Termo Termo
data Valor = Num Double
          | Fun (Valor -> StateTransformer Valor)
          | Erro

type Estado = [(Id,Valor)]
type Ambiente = [(Id,Valor)]
data StateTransformer a = ST (Estado -> (a,Estado))

instance Monad (StateTransformer) where
  return r = ST (\e -> (r,e))
  (ST m) >>= f = ST (\e -> let (v,e1) = m e
                               (ST n) = f v
                           in (n e1)
                    )

--int :: [(Id, Valor)] -> Termo -> StateTransformer Valor
int a (Var i) = ST (\e -> (search i (a++e),e))
int a (Lit n) = return (Num n)
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

search i [] = Erro
search i ((j,v):l) = if i == j then v else search i l

somaVal (Num x) (Num y) = Num (x+y)
somaVal _ _ = Erro

app (Fun f) v = f v
app _ _ = return Erro

wr (i,v) [] = [(i,v)]
wr (i,v) ((j,u):l) = if (i == j) then (j,v):l else [(j,u)] ++ (wr (i,v) l)