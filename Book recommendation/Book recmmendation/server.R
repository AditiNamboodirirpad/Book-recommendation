#Server

source('functions/cf_algorithm.R')
source('functions/similarity_measures.R')
# For user's rating 
#A Sparse matrix is created
get_user_ratings <- function(value_list) {
  dat <- data.table(book_id = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    rating = unlist(as.character(value_list)))
  dat <- dat[!is.null(rating) & !is.na(book_id)]
  dat[rating == " ", rating := 0]
  dat[, ':=' (book_id = as.numeric(book_id), rating = as.numeric(rating))]
  dat <- dat[rating > 0]
  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  user_ratings <- sparseMatrix(i = dat$book_id, 
                               j = rep(1,nrow(dat)), 
                               x = dat$rating, 
                               dims = c(nrow(ratingmat), 1))
}

books <- fread('data/books.csv')
ratings <- fread('data/ratings_cleaned.csv')

#Books and user matrix 
ratingmat <- sparseMatrix(ratings$book_id, ratings$user_id, x=ratings$rating)
ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] 
dimnames(ratingmat) <- list(book_id = as.character(1:10000), user_id = as.character(sort(unique(ratings$user_id))))
ratingmat
dimnames(ratingmat)

shinyServer(function(input, output, session) {
  
  #Displaying the book to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_books <- 6 # books per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_books, function(j) {
        list(box(background = "black",width = 2,
                 div(style = "text-align:center", img(src = books$image_url[(i - 1) * num_books + j], style = "max-height:150")),
                 div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(books$title[(i - 1) * num_books + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", books$book_id[(i - 1) * num_books + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Linked with helpers for when the button is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", {
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        #To get the users data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list)
        rmat <- cbind(user_ratings, ratingmat)
        
        #To predict all the books that the users hasnt rated
        items_to_predict <- which(rmat[, 1] == 0)
        prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
        
        # The cf algorithm call
        res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
        
        #Showing the recommended books in descending order
        user_results <- sort(res[, 1], decreasing = TRUE)[1:24]
        user_predicted_ids <- as.numeric(names(user_results))
        recom_results <- data.table(Rank = 1:24, 
                                    Book_id = user_predicted_ids, 
                                    Author = books$authors[user_predicted_ids], 
                                    Title = books$title[user_predicted_ids], 
                                    Predicted_rating =  user_results)
        
    })
    
  })
  

  #  To display the recommendations
  output$results <- renderUI({
    num_rows <- 4
    num_books <- 6
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(
        fluidRow(lapply(1:num_books, function(j) {
        
        box(background = "black",width = 2, status = "success", solidHeader = TRUE, 
            
          div(style = "text-align:center", 
              a(href = paste0('https://www.goodreads.com/book/show/', books$best_book_id[recom_result$Book_id[(i - 1) * num_books + j]]), 
                target='blank', 
                img(src = books$image_url[recom_result$Book_id[(i - 1) * num_books + j]], height = 150))
             ),
          div(style = "text-align:center; color: #999999; font-size: 80%", 
              books$authors[recom_result$Book_id[(i - 1) * num_books + j]]
             ),
          div(style="text-align:center; font-size: 100%", 
              strong(books$title[recom_result$Book_id[(i - 1) * num_books + j]])
             )
          
        )        
      })))
    }) 
    
  })
  
})

