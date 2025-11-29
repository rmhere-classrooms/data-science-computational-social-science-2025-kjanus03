library(igraph)

# 2. Wygeneruj graf wedle modelu Barabási-Albert z tysiącem węzłów
g <- barabasi.game(n = 1000)

# 3. Zwizualizuj graf layoutem Fruchterman & Reingold
plot(g, layout = layout.fruchterman.reingold, vertex.label = NA, vertex.size = 3)

# 4. Znajdź najbardziej centralny węzeł według miary betweenness, jaki ma numer?
which.max(betweenness(g))

# 5. Jaka jest średnica grafu?
diameter(g)

# 6. W komentarzu napisz czym różnią się grafy Barabási-Albert i Erdős-Rényi.
# Model Erdős-Rényi zakłada losowe łączenie węzłów z takim samym prawdopodobieństwem gdzie rozkład stopni jest zbliżony do Poissona i nie powstają wyraźne huby.
# W model Barabási-Albert z  kolei nowe węzły chętniej łączą się z tymi, które już mają wysoki stopień. 
#Prowadzi to do powstania sieci bezskalowych z obecnością potężnych węzłów centralnych (hubów).