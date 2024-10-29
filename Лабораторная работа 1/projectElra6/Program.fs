
// Реализация модульной реализации с хвостовой рекурсией
let elra6Summ(): int  = 
    let rec accElra6Summ(acc: int, i: int): int =
        if i = 100 then acc
        else
            accElra6Summ (acc + i, i + 1)
    accElra6Summ(0, 0)

let elra6SummKv(): int = 
    let rec accElra6SummKv(acc: int, i: int): int = 
        if i = 100 then acc
        else
            accElra6SummKv(acc + i * i, i + 1)
    accElra6SummKv(1, 1)

let result(): int =
    let summ: int = elra6Summ()
    let summKv: int = elra6SummKv()
    let mutable res =0
    if summ >= summKv then 
        res <-  summ - summKv
    else
        res <- summKv - summ 
    res
let r: int  = result()

printf "%i\n" r

// Реализация рекурсивной программой
let rec Elra6SummKv(summ: int, i: int): int = 
        if i = 100 then summ
        else
            Elra6SummKv(summ + i * i, i + 1)


let rec Elra6Summ(summ: int, i: int): int =
    if i = 100 then summ
    else
        Elra6Summ (summ + i, i + 1)

let result2(): int = 
    let summ: int  = Elra6Summ(0, 0)
    let summKv  = Elra6SummKv(1, 1)
    let mutable res: int  = 0
    if summ >= summKv then
        res <- summ - summKv
    else
        res <- summKv - summ
    res
let res: int  = result2()
printf "%i\n" res