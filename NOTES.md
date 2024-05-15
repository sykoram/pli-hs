- TODO
  - convert Parsing.Program to internal representation that will be used in backtracking
  - backtracking

- `/**/` komentáře by mohlo jít detekovat lépe (na rozdíl od `%`)

- zabudované:
  - kdyby řezy nebyly složité udělat (zrušit zkoušení dalších možností), pak ty, jinak negation as failure `\+` - prostě zkusit splnit cíl a negovat, jestli to bylo úspěšné

- backtracking
  - "volání" predikátu => musí se přejmenovat (přečíslovat) jeho proměnné, až pak unifikovat s hlavou!
  - list je monáda (0/1/více výsledků)!
    - `>>=` aplikuje funkci na každý element
    - mzero je []
    - mplus je ++
    - `do x <- [1..5]; return (x+1)` -> `[2,3,4,5,6]`
  - asi prostě vracet seznam všech výsledků (lazy)
