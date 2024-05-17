- TODO
  - backtracking

- `/**/` komentáře by mohlo jít detekovat lépe (na rozdíl od `%`)

- zabudované:
  - kdyby řezy nebyly složité udělat (zrušit zkoušení dalších možností), pak ty, jinak negation as failure `\+` - prostě zkusit splnit cíl a negovat, jestli to bylo úspěšné

- backtracking
  - "volání" predikátu => musí se přejmenovat (přečíslovat) jeho proměnné, až pak unifikovat s hlavou!
  - list je monáda (0/1/více výsledků)!
    - `>>=` aplikuje funkci na každý element a concat výsledků: `[1..5] >>= enumFromTo 2` -> `[2,2,3,2,3,4,2,3,4,5]`
  - asi prostě vracet seznam všech výsledků (lazy)
