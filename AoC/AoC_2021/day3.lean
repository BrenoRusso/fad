/- # Problema https://adventofcode.com/2021/day/3 -/

namespace AoC2021D3

def content : String := include_str "../../data/AoC2021_day3.txt"


/- # Part 1

Calcular o consumo de energia do submarino.

O objetivo desta etapa é determinar duas taxas binárias: gamma rate e epsilon rate, com base na frequência dos bits em cada posição.
O gamma rate é formado pelos bits mais frequentes em cada posição, enquanto o epsilon rate usa os menos frequentes. A multiplicação dessas taxas retorna o consumo de energia do submarino.
-/


-- Converte cada caractere ('0' ou '1') em um par de contagens
-- '0' -> (1, 0), '1' -> (0, 1)
def parseBit : Char → Nat × Nat
| '0' => (1, 0)
| '1' => (0, 1)
| _ => (0, 0)  -- Caso inválido, retorna (0, 0)

-- Converte uma linha de bits (string) em uma lista de pares (Nat × Nat)
def parseLine (xs: String) : List (Nat × Nat) :=
  xs.toList.map parseBit

-- Soma dois pares (Nat × Nat), somando os primeiros e segundos elementos
def SumPair (a b : Nat × Nat) : Nat × Nat :=
  (a.fst + b.fst, a.snd + b.snd)

-- Soma as contagens de todas as linhas para cada coluna
def mergeLines (width : Nat) : List (List (Nat × Nat)) → List (Nat × Nat)
| [] => List.replicate width (0, 0)  -- Inicializa com zeros se a entrada for vazia
| (l :: ls) =>
  let rl := mergeLines width ls  -- Processa recursivamente o restante das linhas
  List.zipWith SumPair l rl      -- Soma as contagens da linha atual com o acumulado

-- Calcula o número epsilon (baseado nos bits menos frequentes)
def minbits (w : Nat) : List (Nat × Nat) → Nat
| [] => 0  -- Caso base: lista vazia retorna 0
| (x :: xs) =>
    let r := minbits (w - 1) xs  -- Calcula recursivamente para o próximo bit
    if x.fst < x.snd then        -- Se '0' é mais frequente, ignora o bit atual
      r
    else                         -- Caso contrário, adiciona 2^w (valor do bit atual)
      r + 2 ^ w

-- Calcula o número gamma (baseado nos bits mais frequentes)
def maxbits (w : Nat) : List (Nat × Nat) → Nat
| [] => 0  -- Caso base: lista vazia retorna 0
| (x :: xs) =>
    let r := maxbits (w - 1) xs  -- Calcula recursivamente para o próximo bit
    if x.fst > x.snd then        -- Se '0' é mais frequente, ignora o bit atual
      r
    else                         -- Caso contrário, adiciona 2^w (valor do bit atual)
      r + 2 ^ w

#eval
  let command_list := content.splitOn "\x0d\n" |>.filter (· ≠ "") |>.map parseLine
  let width := command_list.head!.length
  let merged := mergeLines width command_list
  let d := minbits (width - 1) merged
  let w := maxbits (width - 1) merged
  d * w


/- # Part 2

Calcular o consumo de energia do submarino.



-/

def string_to_nat_aux (w:Nat) : List Char → Nat
| [] => 0
| ('0'::xs) => string_to_nat_aux (w-1) xs
| (_::xs) => 2^w + string_to_nat_aux (w-1) xs


def string_to_nat (x:String) : Nat :=
  string_to_nat_aux (x.length - 1) x.toList


partial def bitfilter (f:Nat → Nat → Bool) (w:Nat) (lines: List String) : Nat :=
  if lines.length == 1
  then
      string_to_nat lines.head!
  else
    let hist := (lines.map (λx => x.get w))
    let ones := (hist.filter (λx => x == '1')).length
    let zeros := (hist.filter (λx => x == '0')).length
    let target := if f ones zeros then '1' else '0'
    let valid := lines.filter (λx => x.get w == target)
    bitfilter f (w+1) valid



unsafe def main (args : List String) : IO UInt32 :=
do let input ← IO.FS.lines "input.txt"
   let lines := input.toList
   let ogr := bitfilter (λx y => x >= y) 0 lines
   let csr := bitfilter (λx y => x < y) 0 lines
   IO.print (ogr * csr)
   IO.print "\n"
   return 0
