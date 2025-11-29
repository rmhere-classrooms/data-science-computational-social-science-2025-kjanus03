library(igraph)

# 2. Wygeneruj sieć Erdős-Rényi o stu wierzchołkach i prawdopodobieństwie krawędzi = 0.05.
g <- sample_gnp(n = 100, p = 0.05)

# 3. Wydrukuj podsumowanie grafu - czy graf jest ważony?
print(g)
# Graf nie jest ważony bo nie ma litery W w opisie IGRAPH oraz atrybutu weight (e/n).

# 4. Wylistuj wszystkie wierzchołki i krawędzie.
V(g)
E(g)

# 5. Ustaw wagi wszystkich krawędzi na losowe z zakresu 0.01 do 1
E(g)$weight <- runif(ecount(g), min = 0.01, max = 1)

# 6. Wydrukuj ponownie podsumowanie grafu - czy teraz graf jest ważony?
print(g)
#  Graf jest teraz ważony bo jest widoczna litera W w opisie IGRAPH oraz atrybut weight (e/n).

# 7. Jaki jest stopień każdego węzła? Następnie stwórz histogram stopni węzłów.
degree(g)
hist(degree(g), main="Histogram stopni węzłów", xlab="Stopień", ylab="Częstość")

# 8. Ile jest klastrów (connected components) w grafie?
components(g)$no

# 9. Zwizualizuj graf w taki sposób, aby rozmiar węzłów odpowiadał mierze PageRank.
pr_scores <- page_rank(g)$vector
plot(g, vertex.size = pr_scores*700, vertex.label=NA, main="Rozmiar węzła wg PageRank (przesklaowanie o 700 żeby było wszystko widać")
