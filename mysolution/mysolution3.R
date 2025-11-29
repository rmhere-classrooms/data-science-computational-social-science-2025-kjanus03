library(shiny)
library(igraph)

# 3. Użyj zbioru danych z pliku css/out.radoslaw_email_email (Manufacturing company e-mail communication)
file_path <- "out.radoslaw_email_email" 

#4. Zaimportuj zbiór out.radoslaw_email_email do data frame i zachowaj tylko pierwsze dwie kolumny (dodatkowo przeskocz dwa pierwsze wiersze), następnie stwórz z tego data frame'a graf skierowany. 
df <- read.table(file_path, skip = 2)[, 1:2]
colnames(df) <- c("from", "to")
g_raw <- graph_from_data_frame(df, directed = TRUE)

# 5. Użyj funkcji simplify aby pozbyć się wielokrotnych krawędzi i pętli. Zweryfikuj czy po tej operacji Twój graf ma 167 węzłów i 5783 krawędzie. Jeśli tak jest, możesz kontynuować.
E(g_raw)$weight <- 1
g <- simplify(g_raw, remove.multiple = TRUE, remove.loops = TRUE, 
              edge.attr.comb = list(weight = "sum"))

# WERYFIKACJA WYMIARÓW
cat("Liczba węzłów:", vcount(g))
cat("Liczba krawędzi:", ecount(g))

# 6. Waga na krawędziach niech zostanie ustalona wedle następującego podejścia:
# wij = cntij / cnti, gdzie
# wij - waga na krawędzi pomiędzy węzłęm vi a węzłem vj
# cntij - liczba maili wysłanych przez węzeł vi do węzła vj
# cnti - liczba wszystkich maili wysłanych przez węzeł vi
# Powyższa formuła zakłada, że będziesz musiał(a) użyć pierwotnego data frame’a aby wyliczyć te wagi. Zwróć uwagę, że wedle powyższej formuły suma wag krawędzi wychodzących z każdego węzła będzie wynosiła jeden.

node_out_strength <- strength(g, mode = "out", weights = E(g)$weight)
edge_ends <- ends(g, E(g), names = FALSE)
source_nodes <- edge_ends[, 1]
E(g)$weight <- E(g)$weight / node_out_strength[source_nodes]

# 7. Zasymuluj proces rozprzestrzeniania się informacji w grafie
# Powtórz eksperyment stukrotnie dla każdego sposobu wyboru węzła.
run_icm <- function(graph, seeds, prob_multiplier, max_iter, random_mode = FALSE) {
  n_nodes <- vcount(graph)
  results_sum <- numeric(max_iter)
  
  for(sim in 1:100) {
    if(random_mode) {
      current_seeds <- sample(n_nodes, length(seeds))
    } else {
      current_seeds <- seeds
    }
    
    is_active <- rep(FALSE, n_nodes)
    is_active[current_seeds] <- TRUE
    newly_active <- current_seeds
    
    sim_counts <- numeric(max_iter)
    sim_counts[1] <- length(current_seeds) #tylko zianra
    
    for(i in 2:max_iter) {
      if(length(newly_active) == 0) {
        sim_counts[i:max_iter] <- sim_counts[i-1]
        break
      }
      
      adj <- incident_edges(graph, newly_active, mode = "out")
      next_batch <- integer(0)
      
      for(edges in adj) {
        if(length(edges) == 0) next
        
        targets <- head_of(graph, edges)
        probs <- edges$weight * prob_multiplier
        
        success <- runif(length(probs)) < probs
        candidates <- targets[success]
        
        real_new <- candidates[!is_active[candidates]]
        next_batch <- c(next_batch, as.integer(real_new))
      }
      
      next_batch <- unique(next_batch)
      
      if(length(next_batch) > 0) {
        is_active[next_batch] <- TRUE
      }
      
      sim_counts[i] <- sum(is_active)
      newly_active <- next_batch
    }
    results_sum <- results_sum + sim_counts
  }
  return(results_sum / 100)
}

# 8. Wykonaj powyższy eksperyment dla pięciu różnych zbiorów węzłów początkowych.
n_seeds <- round(vcount(g) * 0.05) # 5% węzłów

# (i) węzłów o największym outdegree
seeds_deg <- order(degree(g, mode="out"), decreasing = TRUE)[1:n_seeds]

# (ii) dla najbardziej centralnych węzłów wedle metody betweenness
seeds_bet <- order(betweenness(g), decreasing = TRUE)[1:n_seeds]

# (iii) węzłów największym closeness
seeds_clo <- order(closeness(g, mode="all"), decreasing = TRUE)[1:n_seeds]

# (iv) dowolnych losowych węzłów (placeholder, losowanie wewnątrz funkcji run_icm)
seeds_rnd <- sample(vcount(g), n_seeds)

# (v) węzłów wybranych wedle innej miary - PageRank
seeds_pr <- order(page_rank(g)$vector, decreasing = TRUE)[1:n_seeds]

# 10. Opublikuj swój kod jako ShinyApp z suwakami (prawdopodobieństwo 10-200%, iteracje 1-50).

ui <- fluidPage(
  titlePanel("Model Independent Cascades (Email Network)"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("prob_mult", "Mnożnik prawdopodobieństwa (wij):",
                  min = 0.1, max = 2.0, value = 1.0, step = 0.1),
      sliderInput("iterations", "Liczba iteracji:",
                  min = 1, max = 50, value = 10),
      hr(),
      helpText("Symulacja liczona jest 100 razy dla każdej strategii. Proszę czekać na odświeżenie wykresu."),
      tags$ul(
        tags$li(style="color:red", "Out-degree"),
        tags$li(style="color:green", "Betweenness"),
        tags$li(style="color:blue", "Closeness"),
        tags$li(style="color:black", "PageRank (Własna)"),
        tags$li(style="color:gray", "Losowe")
      )
    ),
    
    mainPanel(
      # 9. Jako podsumowanie przygotuj wykres obrazujący jak przebiegał proces dyfuzji.
      plotOutput("diffPlot", height = "500px")
    )
  )
)

server <- function(input, output) {
  
  sim_data <- reactive({
    mult <- input$prob_mult
    iter <- input$iterations
    
    #  dla 5 strategii
    list(
      deg = run_icm(g, seeds_deg, mult, iter),
      bet = run_icm(g, seeds_bet, mult, iter),
      clo = run_icm(g, seeds_clo, mult, iter),
      pr  = run_icm(g, seeds_pr,  mult, iter),
      rnd = run_icm(g, seeds_rnd, mult, iter, random_mode = TRUE)
    )
  })
  
  output$diffPlot <- renderPlot({
    res <- sim_data()
    x_vals <- 1:input$iterations
    y_max <- vcount(g)
    
    plot(x_vals, res$deg, type="l", col="red", lwd=2, ylim=c(0, y_max),
         xlab="Iteracja", ylab="Liczba aktywowanych węzłów",
         main="Średnia liczba aktywowanych węzłów (100 symulacji)")
    lines(x_vals, res$bet, col="green", lwd=2)
    lines(x_vals, res$clo, col="blue", lwd=2)
    lines(x_vals, res$pr, col="black", lwd=2)
    lines(x_vals, res$rnd, col="gray", lwd=2, lty=2)
    grid()
  })
}

shinyApp(ui = ui, server = server)
