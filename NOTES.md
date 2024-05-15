- TODO
  - backtracking

- `/**/` komentáře by mohlo jít detekovat lépe (na rozdíl od `%`)

- zabudované:
  - unifikace `=` - najít algoritmus
  - negation as failure `\+` - prostě zkusit splnit cíl a negovat, jestli to bylo úspěšné
  - že by řezy nebyly moc složité udělat? (zrušit zkoušení dalších možností) - pak bychom nemuseli zabudovávat `\+`

- unifikace
  - "volání" predikátu => musí se přejmenovat proměnné!
    - budeme je číslovat
    - hlavní dict:  číslo proměnné -> hodnota
    - topmost dict: jméno proměnné z dotazu -> číslo proměnné
  - nová substituce proměnné (něco dílčího se unifikuje) => musíme projít hodnoty proměnných a updatovat je
  - hned před unifikací (i rekurzivně) dosadíme za proměnné podle bindings - nebo tohle nemusíme řešit??

- backtracking
  - list je monáda (0/1/více výsledků)!
    - `>>=` aplikuje funkci na každý element
    - mzero je []
    - mplus je ++
    - `do x <- [1..5]; return (x+1)` -> `[2,3,4,5,6]`
  - asi prostě vracet seznam všech výsledků (lazy)
