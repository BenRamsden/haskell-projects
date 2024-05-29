Monadic Compiler
================

Libraries
---------

> import Data.List
> import Data.Maybe
> import Control.Monad.Writer


Imperative language
-------------------

> data Prog  		=  Assign Name Expr
> 	   		|  If Expr Prog Prog
> 	   		|  While Expr Prog
> 	   		|  Seq [Prog]
>		           deriving Show
>
> data Expr  		=  Val Int | Var Name | App Op Expr Expr
>			   deriving Show
>
> type Name  		=  Char
>
> data Op    		=  Add | Sub | Mul | Div
>			   deriving Show


Virtual machine
---------------

> type Stack 		=  [Int]
>
> type Mem 		=  [(Name,Int)]
>
> type Code  		=  [Inst]
> 
> data Inst  		=  PUSH Int
>          		|  PUSHV Name
>          		|  POP Name
>          		|  DO Op
>          		|  JUMP Label
>          		|  JUMPZ Label
>          		|  LABEL Label
>		           deriving Show
> 
> type Label 		=  Int


Factorial example
-----------------

> fac  			:: Int -> Prog
> fac n			=  Seq [Assign 'A' (Val 1),
>			        Assign 'B' (Val n),
>			        While (Var 'B') (Seq
>			           [Assign 'A' (App Mul (Var 'A') (Var 'B')),
>			            Assign 'B' (App Sub (Var 'B') (Val (1)))])]


Monadic Compiler Function
-------------------------
Utilises The Writer Monad. The Writer accumulates the currently
compiled machine code in a log, whilst the current label is accessible
at any state during the program by calling (writeCode []). This
means both the code and current label can be stored and accessed,
without passing them into every compile function call. 

> comp                             :: Prog -> Code
> comp p                           = execWriter (cProg p)
>
> writeCode                        :: Code -> Writer Code Label  
> writeCode c                      = writer (numLabels, c)
>                                       where numLabels = length $ filter isLabel c
>                                             isLabel (LABEL _) = True
>                                             isLabel _         = False
>
> cProg                            :: Prog -> Writer Code Label
> cProg (Seq [])                   = return 0
>
> cProg (Seq ps)                   = do all <- mapM cProg ps
>                                       return (sum all)
>
> cProg p                          = writeCode $ codeCases p currentLabel
>                                       where currentLabel = fst $ runWriter $ writeCode []
>
> codeCases                        :: Prog -> Label -> Code
> codeCases p lb                   = case (p) of
>                                        Assign v exp         -> cExpr exp ++ [POP v]
>                                        If exp pTrue pFalse  -> cExpr exp ++ [JUMPZ (lb)] ++ comp pTrue ++ [JUMP (lb+1), LABEL (lb)] ++ comp pFalse ++ [LABEL (lb+1)]
>                                        While exp pWhile     -> [LABEL (lb)] ++ cExpr exp ++ [JUMPZ (lb+1)] ++ comp pWhile ++ [JUMP lb, LABEL (lb+1)]
>


Expression Compiler and Optimizations
-------------------------------------
In the case that the programmer combines two values of type Val,
it would be a waste of time in the execution stage to push both
values every time the result is needed to the stack, and then pop them
and combine the values. To reduce execution time, in the event that
a two Val expression is in the code, the operation is taken care of
below, resulting in a single PUSH over [PUSH v1, PUSH v2, DO op].

> cExpr                            :: Expr -> Code
> cExpr (Val x)                    = [PUSH x]
> cExpr (Var name)                 = [PUSHV name]
> cExpr (App op (Val v1) (Val v2)) = [PUSH (fastOp op v1 v2)]           --optimization step
> cExpr (App op exp1 exp2)         = cExpr exp1 ++ cExpr exp2 ++ [DO op]
>
> fastOp                           :: Op -> Int -> Int -> Int
> fastOp Div                       =  div
> fastOp Mul                       =  (*)
> fastOp Add                       =  (+)
> fastOp Sub                       =  (-)
>
> testCode                         = exec $ comp $ fac 10


Recursive Execution Function
----------------------------
Written recursively to reduce confusion about the flow of data
as in some operations like PUSHV one state (the stack) needs
access to data from another state (the memory). execLine simply
iterates through each line of code, following the instruction
given. After executing a line it will either go to the next line,
or set the line conditionally in the case of a JUMP/JUMPZ statement.

DO's fastExOp re-uses the fastOp function from above, to combine
the top two elements of the stack based on the operator in use.

JUMPZ's whileLn checks if the top of the stack is 0, which is
the case to jump out and stop executing the while loop expr.

exec initialises the VM's fields and sets it running, the arguments
are as follows c - code, ln - line number, s - stack, m - memory.

> exec                             :: Code -> Mem
> exec code                        =  execLine code 0 [] []
>
> execLine                         :: Code -> Int -> Stack -> Mem -> Mem
> execLine c ln s m                =  if ln == length c
>                                        then reverse m
>                                        else case (c!!ln) of
>                                          PUSH val    -> execLine c  (next ln)        (val : s)             m
>                                          PUSHV vname -> execLine c  (next ln)        (getVar vname m : s)  m
>                                          POP vname   -> execLine c  (next ln)        (tail s)              (putVar (vname, head s) m)
>                                          DO op       -> execLine c  (next ln)        (fastExOp)            m    where fastExOp = fastOp op (s!!1) (s!!0) : (drop 2 s)
>                                          JUMP l      -> execLine c  (labelLn l c)    s                     m
>                                          JUMPZ l     -> execLine c  (whileLn)        s                     m    where whileLn = if head s == 0 then labelLn l c else next ln
>                                          LABEL _     -> execLine c  (next ln)        s                     m
>


Execution Helper Functions and Optimizations
--------------------------------------------
Below are small functions written to help with code execution,
and make it clearer how each command changes the state of the
virtual machine.

labelLn finds the first (and only) index in the code where
there is a label of number l, and returns that index.

getVar filters memory for a variable of name varN, and then
returns the value of that variable.

putVar deletes the first (and only) occurrence of the variable with the same
name as the one being put into memory, to avoid un-needed memory usage.

> next                             :: Int -> Int
> next                             = (+1)
>
> labelLn                          :: Label -> Code -> Int
> labelLn l c                      = head $ findIndices labelMatch c           where labelMatch inst = case inst of {LABEL l' -> l == l'; _ -> False}
>
> getVar                           :: Name -> Mem -> Int
> getVar varN m                    = snd $ head $ filter varOfName m           where varOfName (vN, _) = vN == varN
>
> putVar                           :: (Name, Int) -> Mem -> Mem
> putVar newVar m                  = newVar : deleteBy sameVarName newVar m    where sameVarName (vN, _) (vN', _) = vN == vN'
> 