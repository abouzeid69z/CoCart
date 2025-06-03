library(shiny)
library(shinyWidgets)
library(bslib)
library(DBI)
library(RSQLite)
library(dplyr)
library(opencv)
library(qrcode)
library(stats)
library(keyring)
library(uuid)
library(blastula)

TAX_RATE <- 0.14
LOW_STOCK_THRESHOLD <- 10
EXPIRATION_WARNING_DAYS <- 7
DISCOUNT_THRESHOLD <- 2000

# Debug: Print working directory and database path
cat(sprintf("Working directory: %s\n", getwd()))
cat(sprintf("Database file path: %s\n", normalizePath("users.sqlite", mustWork = FALSE)))

# Connect to the database with error handling
db <- tryCatch({
  dbConnect(SQLite(), "users.sqlite", synchronous = NULL)
}, error = function(e) {
  stop(sprintf("Failed to connect to database: %s", e$message))
})

# Log existing tables for debugging
tables <- dbListTables(db)
cat(sprintf("Existing tables in database: %s\n", paste(tables, collapse = ", ")))

# Verify items table exists
if (!"items" %in% tables) {
  stop("The 'items' table does not exist in the database. Please ensure it is present in users.sqlite.")
}

# Verify discount_codes table exists
if (!"discount_codes" %in% tables) {
  stop("The 'discount_codes' table does not exist in the database. Please ensure it is present in users.sqlite.")
}

# Check items table schema
if (!"Expiration_Date" %in% dbListFields(db, "items")) {
  stop("The 'Expiration_Date' column does not exist in the 'items' table. Please add it to the existing table.")
}

# Assign random expiration dates if missing
current_date <- as.Date("2025-05-17")
items <- dbReadTable(db, "items")
if (nrow(items) > 0) {
  for (i in 1:nrow(items)) {
    if (is.na(items$Expiration_Date[i]) || items$Expiration_Date[i] == "") {
      random_days <- sample(30:365, 1)
      random_expiration_date <- current_date + random_days
      dbExecute(db, sprintf("UPDATE items SET Expiration_Date = '%s' WHERE Item = %d",
                            as.character(random_expiration_date), items$Item[i]))
    }
  }
}

# Custom CSS for enhanced GUI
custom_css <- "
  body {
    background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%);
    color: #e0e0e0;
    font-family: 'Inter', sans-serif;
  }
  .glass-card {
    background: rgba(255, 255, 255, 0.05);
    backdrop-filter: blur(10px);
    border-radius: 15px;
    border: 1px solid rgba(255, 255, 255, 0.1);
    padding: 20px;
    margin-bottom: 20px;
    transition: transform 0.3s ease, box-shadow 0.3s ease;
  }
  .glass-card:hover {
    transform: translateY(-5px);
    box-shadow: 0 8px 32px rgba(31, 38, 135, 0.37);
  }
  .neon-button {
    background: linear-gradient(45deg, #ff00cc, #3333ff);
    color: white;
    border: none;
    padding: 10px 20px;
    border-radius: 8px;
    font-weight: bold;
    transition: all 0.3s ease;
    box-shadow: 0 0 10px #ff00cc;
  }
  .neon-button:hover {
    box-shadow: 0 0 20px #ff00cc, 0 0 30px #3333ff;
    transform: scale(1.05);
  }
  .sidebar-content {
    background: rgba(0, 0, 0, 0.2);
    border-radius: 10px;
    padding: 15px;
  }
  .tab-panel {
    padding: 20px;
  }
  h3, h4 {
    color: #00ccff;
    text-shadow: 0 0 5px rgba(0, 204, 255, 0.5);
  }
  .shiny-input-container {
    margin-bottom: 15px;
  }
  table {
    background: rgba(255, 255, 255, 0.1);
    color: #e0e0e0;
    border-radius: 8px;
  }
  .modal-content {
    background: rgba(26, 26, 46, 0.95);
    border: 1px solid #00ccff;
    color: #e0e0e0;
  }
  .modal-header, .modal-footer {
    border-color: rgba(255, 255, 255, 0.1);
  }
  .modal-title {
    color: #00ccff;
  }
  .shiny-notification {
    background: rgba(0, 204, 255, 0.2);
    border: 1px solid #00ccff;
    color: #e0e0e0;
  }
"

ui <- page_sidebar(
  title = tags$div(
    style = "display: flex; align-items: center; gap: 10px;",
    tags$img(src = "https://via.placeholder.com/40", style = "border-radius: 50%;"),
    "Smart POS System"
  ),
  sidebar = sidebar(
    width = 250,
    div(class = "sidebar-content",
        uiOutput("sidebar_ui")
    )
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#00ccff",
    secondary = "#ff00cc",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  tags$head(
    tags$style(HTML(custom_css))
  ),
  card(
    card_header("Main Dashboard"),
    uiOutput("main_ui")
  )
)

server <- function(input, output, session) {
  login_attempted <- reactiveVal(FALSE)
  user_session <- reactiveVal(NULL)
  updated_inventory <- reactiveVal(dbReadTable(db, "items"))
  cart <- reactiveVal(data.frame(Item = integer(), Quantity = integer(), stringsAsFactors = FALSE))
  applied_discount <- reactiveVal(NULL)
  transaction_discount <- reactiveVal(NULL)
  
  onSessionEnded(function() {
    dbDisconnect(db)
  })
  
  observe({
    showModal(modalDialog(
      title = "🔐 Login",
      textInput("login_user", "Username"),
      passwordInput("login_pass", "Password"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("login_btn", "Login", class = "btn-primary neon-button")
      ),
      easyClose = FALSE,
      fade = TRUE
    ))
  })
  
  output$total_income_today <- renderText({
    today_str <- format(Sys.Date(), "%Y-%m-%d")
    query <- sprintf("
    SELECT SUM(final_amount) AS income
    FROM sales
    WHERE date(datetime) = '%s'
  ", today_str)
    result <- dbGetQuery(db, query)
    today_income <- result$income[1]
    if (is.na(today_income)) today_income <- 0
    paste("Today's Income:", format(today_income, big.mark = ","), "EGP")
  })
  
  output$total_income_week <- renderText({
    start <- format(Sys.Date() - 6, "%Y-%m-%d")
    result <- dbGetQuery(db, sprintf("
    SELECT SUM(final_amount) AS total FROM sales
    WHERE DATE(datetime) BETWEEN '%s' AND DATE('now')", start))
    sprintf("Last 7 Days: %.2f EGP", result$total[1] %||% 0)
  })
  
  output$total_income_month <- renderText({
    start <- format(as.Date(format(Sys.Date(), "%Y-%m-01")), "%Y-%m-%d")
    result <- dbGetQuery(db, sprintf("
    SELECT SUM(final_amount) AS total FROM sales
    WHERE DATE(datetime) >= '%s'", start))
    sprintf("This Month: %.2f EGP", result$total[1] %||% 0)
  })
  
  output$top_selling_items <- renderPlot({
    df <- dbGetQuery(db, "
    SELECT item_id AS Item, SUM(quantity) AS total_qty
    FROM sale_items
    GROUP BY item_id
    ORDER BY total_qty DESC
    LIMIT 5
  ")
    barplot(df$total_qty, names.arg = df$Item, main = "Top 5 Sold Items", las = 2, col = "#00ccff")
  })
  
  output$sales_by_hour <- renderPlot({
    sales <- dbGetQuery(db, "SELECT datetime FROM sales")
    if (nrow(sales) == 0) return()
    sales$datetime <- as.POSIXct(sales$datetime, format = "%Y-%m-%d %H:%M:%S")
    sales$hour <- format(sales$datetime, "%H")
    rush <- as.data.frame(table(sales$hour))
    colnames(rush) <- c("Hour", "Sales")
    barplot(rush$Sales,
            names.arg = rush$Hour,
            main = "📈 Sales by Hour of Day (Rush Hour)",
            xlab = "Hour (24h)",
            ylab = "Number of Sales",
            col = "#00ccff")
  })
  
  output$discount_usage <- renderPlot({
    sales <- dbGetQuery(db, "SELECT datetime, discount_code FROM sales")
    if (nrow(sales) == 0) return()
    sales <- sales[sales$discount_code != "", ]
    if (nrow(sales) == 0) {
      plot.new()
      text(0.5, 0.5, "No discount usage found", col = "#ff8080", cex = 1.5)
      return()
    }
    sales$datetime <- as.POSIXct(sales$datetime, format = "%Y-%m-%d %H:%M:%S")
    sales$date <- as.Date(sales$datetime)
    usage <- as.data.frame(table(sales$date))
    if (nrow(usage) < 1) {
      plot.new()
      text(0.5, 0.5, "No data to display", col = "#ff8080", cex = 1.5)
      return()
    }
    colnames(usage) <- c("Date", "DiscountsUsed")
    barplot(usage$DiscountsUsed,
            names.arg = usage$Date,
            main = "🎟️ Discount Codes Used per Day",
            xlab = "Date",
            ylab = "Discounts Used",
            las = 2,
            col = "#ff00cc")
  })
  
  observeEvent(input$login_btn, {
    login_attempted(TRUE)
    req(input$login_user, input$login_pass)
    query <- sprintf("SELECT * FROM user WHERE username = '%s'", input$login_user)
    user <- dbGetQuery(db, query)
    if (nrow(user) == 1 && user$password[1] == input$login_pass) {
      user_session(user)
      removeModal()
      showNotification(paste("Welcome", user$name[1], "- logged in as", user$role[1]), type = "message")
    } else {
      showNotification("Login failed.", type = "error")
    }
  })
  
  observeEvent(input$cancel_btn, {
    cart_now <- cart()
    inventory_now <- updated_inventory()
    for (i in 1:nrow(cart_now)) {
      item_id <- cart_now$Item[i]
      qty_returned <- cart_now$Quantity[i]
      inventory_now$Quantity[inventory_now$Item == item_id] <- inventory_now$Quantity[inventory_now$Item == item_id] + qty_returned
    }
    dbBegin(db)
    tryCatch({
      dbWriteTable(db, "items", inventory_now, overwrite = TRUE)
      dbCommit(db)
      updated_inventory(inventory_now)
    }, error = function(e) {
      dbRollback(db)
      showNotification(paste("Failed to restore inventory:", e$message), type = "error")
      return()
    })
    cart(data.frame(Item = integer(), Quantity = integer(), stringsAsFactors = FALSE))
    applied_discount(NULL)
    transaction_discount(NULL)
    output$scan_status <- renderText("Transaction cancelled. Inventory restored. Ready for new customer.")
    showNotification("❌ Transaction cancelled. Inventory restored.", type = "warning")
  })
  
  observeEvent(input$reset_btn, {
    cart(data.frame(Item = integer(), Quantity = integer(), stringsAsFactors = FALSE))
    applied_discount(NULL)
    transaction_discount(NULL)
    output$scan_status <- renderText("Ready for new customer. Click 'Start Scanning' to begin.")
    showNotification("🛒 Reset complete. You can scan for the next customer.", type = "message")
  })
  
  output$sidebar_ui <- renderUI({
    user <- user_session()
    if (is.null(user)) return(NULL)
    name <- as.character(user$name[1])
    role <- tolower(as.character(user$role[1]))
    tagList(
      h4("Welcome, ", name),
      p("Role: ", role),
      hr(),
      actionButton("logout_btn", "Logout", class = "neon-button", width = "100%")
    )
  })
  
  output$main_ui <- renderUI({
    user <- user_session()
    if (is.null(user)) {
      if (login_attempted()) {
        return(HTML("<h3 style='color:#ff8080;'>Login error — try again</h3>"))
      }
      return(NULL)
    }
    role <- tolower(as.character(user$role[1]))
    tabsetPanel(
      id = "main_tabs",
      type = "pills",
      tabPanel(
        "POS System",
        div(class = "tab-panel",
            fluidRow(
              column(6,
                     card(
                       card_header("QR Scanner"),
                       card_body(
                         actionButton("scan_btn", "Start Scanning", class = "neon-button"),
                         br(),
                         verbatimTextOutput("scan_status"),
                         br(),
                         tableOutput("cart_table")
                       )
                     )
              ),
              column(6,
                     card(
                       card_header("Live Receipt"),
                       card_body(
                         verbatimTextOutput("receipt_output"),
                         br(),
                         textInput("customer_email", "Customer Email", placeholder = "Enter customer email"),
                         actionButton("email_receipt", "📧 Email Receipt", class = "neon-button"),
                         br(), br(),
                         actionButton("print_receipt", "🖨️ Print Receipt", class = "neon-button"),
                         br(), br(),
                         actionButton("reset_btn", "🔄 Reset for New Customer", class = "neon-button"),
                         br(), br(),
                         actionButton("cancel_btn", "❌ Cancel Transaction", class = "neon-button"),
                         br(), br(),
                         uiOutput("low_stock_alert")
                       )
                     )
              )
            )
        )
      ),
      if (role == "admin") {
        tabPanel(
          "📊 Sales Analytics",
          div(class = "tab-panel",
              fluidRow(
                column(4, card(card_body(verbatimTextOutput("total_income_today")))),
                column(4, card(card_body(verbatimTextOutput("total_income_week")))),
                column(4, card(card_body(verbatimTextOutput("total_income_month"))))
              ),
              fluidRow(
                column(6, card(card_body(plotOutput("top_selling_items")))),
                column(6, card(card_body(plotOutput("sales_by_hour"))))
              ),
              fluidRow(
                column(6, card(card_body(plotOutput("discount_usage"))))
              )
          )
        )
      },
      if (role == "admin") {
        tabPanel(
          "Admin Panel",
          div(class = "tab-panel",
              fluidRow(
                column(6,
                       card(
                         card_header("Add New Item to Inventory"),
                         card_body(
                           numericInput("new_item_id", "Item ID", value = NULL, min = 1),
                           numericInput("new_item_price", "Price", value = NULL, min = 0),
                           numericInput("new_item_quantity", "Quantity", value = NULL, min = 1),
                           dateInput("new_item_expiration", "Expiration Date", value = NULL, format = "yyyy-mm-dd"),
                           actionButton("add_item", "Add Item", class = "neon-button"),
                           verbatimTextOutput("add_status")
                         )
                       )
                ),
                column(6,
                       card(
                         card_header("Restock Item"),
                         card_body(
                           numericInput("restock_id", "Item ID", value = NULL, min = 1),
                           numericInput("restock_quantity", "Quantity to Add", value = NULL, min = 1),
                           dateInput("restock_expiration", "New Expiration Date (optional)", value = NULL, format = "yyyy-mm-dd"),
                           actionButton("restock_item", "Restock", class = "neon-button"),
                           verbatimTextOutput("restock_status")
                         )
                       )
                )
              ),
              fluidRow(
                column(6,
                       card(
                         card_header("Check Item Quantity"),
                         card_body(
                           numericInput("check_item_id", "Enter Item ID", value = NULL, min = 1),
                           actionButton("check_quantity", "Check Quantity", class = "neon-button"),
                           verbatimTextOutput("quantity_status")
                         )
                       )
                ),
                column(6,
                       card(
                         card_header("Expiration Warnings"),
                         card_body(
                           uiOutput("expiration_warning")
                         )
                       )
                )
              )
          )
        )
      }
    )
  })
  
  observeEvent(input$scan_btn, {
    output$scan_status <- renderText("Scanner started — scan items or discount code. Use QR code '0' to finish.")
    while (TRUE) {
      qr_code <- qr_scanner()
      if (is.null(qr_code)) {
        output$scan_status <- renderText("No QR code detected or scanner closed.")
        break
      }
      qr_code <- as.character(qr_code)
      cat(sprintf("Scanned QR code: %s\n", qr_code))
      output$scan_status <- renderText(sprintf("Scanned QR code: %s", qr_code))
      if (grepl("^DISC_", qr_code)) {
        discount_code <- sub("^DISC_", "", qr_code)
        cat(sprintf("Extracted discount code: %s\n", discount_code))
        query <- sprintf("SELECT * FROM discount_codes WHERE code = '%s' AND used = FALSE", discount_code)
        discount <- tryCatch({
          dbGetQuery(db, query)
        }, error = function(e) {
          output$scan_status <- renderText(sprintf("Database query error: %s", e$message))
          cat(sprintf("Query error: %s\n", e$message))
          return(data.frame())
        })
        cat(sprintf("Query result: %d rows\n", nrow(discount)))
        if (nrow(discount) == 0) {
          invalid_check <- dbGetQuery(db, sprintf("SELECT * FROM discount_codes WHERE code = '%s'", discount_code))
          if (nrow(invalid_check) > 0) {
            output$scan_status <- renderText(sprintf("Discount code %s already used or invalid (used = %s)", discount_code, invalid_check$used[1]))
            cat(sprintf("Code %s found, used = %s\n", discount_code, invalid_check$used[1]))
          } else {
            output$scan_status <- renderText(sprintf("Discount code %s not found in database", discount_code))
            cat(sprintf("Code %s not found\n", discount_code))
          }
          next
        }
        tryCatch({
          dbExecute(db, sprintf("UPDATE discount_codes SET used = TRUE WHERE code = '%s'", discount_code))
          cat(sprintf("Marked discount code %s as used immediately upon scanning\n", discount_code))
        }, error = function(e) {
          output$scan_status <- renderText(sprintf("Failed to mark discount as used: %s", e$message))
          cat(sprintf("Failed to mark discount as used: %s\n", e$message))
          next
        })
        updated_discount <- dbGetQuery(db, sprintf("SELECT * FROM discount_codes WHERE code = '%s'", discount_code))
        if (updated_discount$used[1] != TRUE) {
          output$scan_status <- renderText("Failed to mark discount as used. Please try again.")
          cat("Failed to mark discount as used in the database\n")
          next
        }
        applied_discount(updated_discount)
        output$scan_status <- renderText(sprintf("50%% discount code applied: %s (now marked as used)", discount_code))
        cat(sprintf("Applied discount: %s\n", discount_code))
        next
      }
      if (!grepl("^\\d+$", qr_code)) {
        output$scan_status <- renderText("Invalid QR code (must be numeric for items).")
        cat("Invalid QR code (non-numeric)\n")
        next
      }
      qr_code_num <- as.integer(qr_code)
      if (qr_code_num == 0) {
        output$scan_status <- renderText("Scanning finished.")
        break
      }
      inventory_now <- updated_inventory()
      item_row <- inventory_now %>% filter(Item == qr_code_num)
      if (nrow(item_row) == 0) {
        output$scan_status <- renderText(paste("Item not found:", qr_code_num))
        next
      }
      if (item_row$Quantity <= 0) {
        output$scan_status <- renderText(paste("Out of stock:", qr_code_num))
        next
      }
      cart_now <- cart()
      if (qr_code_num %in% cart_now$Item) {
        cart_now$Quantity[cart_now$Item == qr_code_num] <- cart_now$Quantity[cart_now$Item == qr_code_num] + 1
      } else {
        cart_now <- rbind(cart_now, data.frame(Item = qr_code_num, Quantity = 1))
      }
      cart(cart_now)
      inventory_now$Quantity[inventory_now$Item == qr_code_num] <- item_row$Quantity - 1
      updated_inventory(inventory_now)
      dbBegin(db)
      tryCatch({
        dbWriteTable(db, "items", inventory_now, overwrite = TRUE)
        dbCommit(db)
      }, error = function(e) {
        dbRollback(db)
        showNotification(paste("Database error:", e$message), type = "error")
      })
    }
  })
  
  output$receipt_output <- renderText({
    cart_now <- cart()
    if (nrow(cart_now) == 0) return("🛒 Cart is empty. Scan items to begin.")
    cart_df <- cart_now %>%
      rename(Qty_Bought = Quantity) %>%
      left_join(updated_inventory(), by = "Item") %>%
      mutate(
        Unit_Price = Price,
        Line_Total = Unit_Price * Qty_Bought,
        Tax = round(Line_Total * TAX_RATE, 2),
        Total = round(Line_Total + Tax, 2)
      )
    total_before_tax <- sum(cart_df$Line_Total)
    discount <- applied_discount()
    discount_amount <- 0
    if (!is.null(discount)) {
      discount_percentage <- discount$discount_percentage[1] / 100
      discount_amount <- total_before_tax * discount_percentage
      total_before_tax <- total_before_tax - discount_amount
      cart_df$Tax <- round(total_before_tax * TAX_RATE, 2)
      cart_df$Total <- round(total_before_tax + cart_df$Tax, 2)
      cat(sprintf("Discount applied: %s, amount = %.2f\n", discount$code[1], discount_amount))
    } else {
      cat("No discount applied\n")
    }
    total_tax <- sum(cart_df$Tax)
    total_after_tax <- sum(cart_df$Total)
    min_price <- min(cart_df$Unit_Price)
    max_price <- max(cart_df$Unit_Price)
    mean_before_tax <- mean(cart_df$Line_Total)
    variance_before_tax <- var(cart_df$Line_Total)
    receipt <- "\n------ 🧾 RECEIPT ------\n"
    for (i in 1:nrow(cart_df)) {
      row <- cart_df[i, ]
      receipt <- paste0(receipt, sprintf("Item: %d | Qty: %d | @ %.2f = %.2f\n", row$Item, row$Qty_Bought, row$Unit_Price, row$Line_Total))
    }
    receipt <- paste0(receipt, "--------------------------\n")
    if (discount_amount > 0) {
      receipt <- paste0(receipt, sprintf("Discount (50%%): -%.2f\n", discount_amount))
    }
    receipt <- paste0(receipt, sprintf("Subtotal:   %.2f\n", total_before_tax))
    receipt <- paste0(receipt, sprintf("Tax (14%%): %.2f\n", total_tax))
    receipt <- paste0(receipt, sprintf("TOTAL:     %.2f\n", total_after_tax))
    receipt <- paste0(receipt, "--------------------------\n")
    receipt <- paste0(receipt, "\n📊 RECEIPT STATISTICS 📊\n")
    receipt <- paste0(receipt, "--------------------------\n")
    receipt <- paste0(receipt, sprintf("Min Price:  %.2f\n", min_price))
    receipt <- paste0(receipt, sprintf("Max Price:  %.2f\n", max_price))
    receipt <- paste0(receipt, sprintf("Mean Line Total: %.2f\n", mean_before_tax))
    receipt <- paste0(receipt, sprintf("Variance Line Total: %.2f\n", variance_before_tax))
    receipt <- paste0(receipt, "--------------------------\n")
    receipt <- paste0(receipt, "Thanks for shopping!\n")
    return(receipt)
  })
  
  observeEvent(input$print_receipt, {
    cart_now <- cart()
    if (nrow(cart_now) == 0) {
      showNotification("Cart is empty. Nothing to print.", type = "error")
      transaction_discount(NULL)
      return()
    }
    cart_df <- cart_now %>%
      rename(Qty_Bought = Quantity) %>%
      left_join(updated_inventory(), by = "Item") %>%
      mutate(
        Unit_Price = Price,
        Line_Total = Unit_Price * Qty_Bought,
        Tax = round(Line_Total * TAX_RATE, 2),
        Total = round(Line_Total + Tax, 2)
      )
    total_before_tax <- sum(cart_df$Line_Total)
    discount <- applied_discount()
    discount_amount <- 0
    if (!is.null(discount)) {
      discount_percentage <- discount$discount_percentage[1] / 100
      discount_amount <- total_before_tax * discount_percentage
      total_before_tax <- total_before_tax - discount_amount
      cart_df$Tax <- round(total_before_tax * TAX_RATE, 2)
      cart_df$Total <- round(total_before_tax + cart_df$Tax, 2)
    }
    total_tax <- sum(cart_df$Tax)
    total_after_tax <- sum(cart_df$Total)
    dbExecute(db, sprintf(
      "INSERT INTO sales (datetime, total_amount, total_tax, discount_amount, final_amount, email_sent, discount_code)
   VALUES ('%s', %.2f, %.2f, %.2f, %.2f, '%s', '%s')",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      total_before_tax,
      total_tax,
      discount_amount,
      total_after_tax,
      input$customer_email,
      ifelse(is.null(discount), "", discount$code[1])
    ))
    sale_id <- dbGetQuery(db, "SELECT last_insert_rowid()")[[1]]
    for (i in 1:nrow(cart_df)) {
      row <- cart_df[i, ]
      dbExecute(db, sprintf(
        "INSERT INTO sale_items (sale_id, item_id, quantity, unit_price, line_total)
     VALUES (%d, %d, %d, %.2f, %.2f)",
        sale_id, row$Item, row$Qty_Bought, row$Unit_Price, row$Line_Total
      ))
    }
    receipt <- "\n------ 🧾 RECEIPT ------\n"
    for (i in 1:nrow(cart_df)) {
      row <- cart_df[i, ]
      receipt <- paste0(receipt, sprintf("Item: %d | Qty: %d | @ %.2f = %.2f\n", row$Item, row$Qty_Bought, row$Unit_Price, row$Line_Total))
    }
    receipt <- paste0(receipt, "--------------------------\n")
    if (discount_amount > 0) {
      receipt <- paste0(receipt, sprintf("Discount (50%%): -%.2f\n", discount_amount))
    }
    receipt <- paste0(receipt, sprintf("Subtotal:   %.2f\n", total_before_tax))
    receipt <- paste0(receipt, sprintf("Tax (14%%): %.2f\n", total_tax))
    receipt <- paste0(receipt, sprintf("TOTAL:     %.2f\n", total_after_tax))
    receipt <- paste0(receipt, "--------------------------\n")
    if (total_after_tax > DISCOUNT_THRESHOLD && is.null(transaction_discount())) {
      discount_uuid <- UUIDgenerate()
      discount_code <- paste0("DISC_", discount_uuid)
      tryCatch({
        dbExecute(db, sprintf("INSERT INTO discount_codes (code, discount_percentage, used) VALUES ('%s', 50.0, FALSE)", discount_uuid))
        cat(sprintf("Inserted discount code: %s (UUID: %s)\n", discount_code, discount_uuid))
      }, error = function(e) {
        showNotification(sprintf("Failed to insert discount code: %s", e$message), type = "error")
      })
      transaction_discount(list(code = discount_uuid))
    }
    receipt <- paste0(receipt, "Thanks for shopping!\n")
    temp_file <- tempfile(fileext = ".txt")
    writeLines(receipt, temp_file)
    system(sprintf('notepad /p "%s"', temp_file), wait = FALSE)
    showNotification("🖨️ Receipt sent to HPRT TP805 printer.", type = "message")
  })
  
  observeEvent(input$email_receipt, {
    cart_now <- cart()
    if (nrow(cart_now) == 0) {
      showNotification("Cart is empty. Nothing to email.", type = "error")
      return()
    }
    if (!grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", input$customer_email)) {
      showNotification("Please enter a valid email address.", type = "error")
      return()
    }
    inventory <- updated_inventory()
    print(colnames(inventory))
    cart_df <- cart_now %>%
      rename(Qty_Bought = Quantity) %>%
      left_join(inventory, by = "Item") %>%
      mutate(
        Unit_Price = Price,
        Line_Total = Unit_Price * Qty_Bought,
        Tax = round(Line_Total * TAX_RATE, 2),
        Total = round(Line_Total + Tax, 2)
      )
    print(cart_df)
    cat("Rows in cart_df:", nrow(cart_df), "\n")
    total_before_tax <- sum(cart_df$Line_Total)
    discount <- applied_discount()
    discount_amount <- 0
    if (!is.null(discount)) {
      discount_percentage <- discount$discount_percentage[1] / 100
      discount_amount <- total_before_tax * discount_percentage
      total_before_tax <- total_before_tax - discount_amount
      cart_df$Tax <- round(total_before_tax * TAX_RATE, 2)
      cart_df$Total <- round(total_before_tax + cart_df$Tax, 2)
    }
    total_tax <- sum(cart_df$Tax)
    total_after_tax <- sum(cart_df$Total)
    tryCatch({
      dbExecute(db, sprintf(
        "INSERT INTO sales (datetime, total_amount, total_tax, discount_amount, final_amount, email_sent, discount_code)
       VALUES ('%s', %.2f, %.2f, %.2f, %.2f, '%s', '%s')",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        total_before_tax,
        total_tax,
        discount_amount,
        total_after_tax,
        input$customer_email,
        ifelse(is.null(discount), "", discount$code[1])
      ))
      sale_id <- dbGetQuery(db, "SELECT last_insert_rowid()")[[1]]
      cat("Inserted sale_id:", sale_id, "\n")
      for (i in 1:nrow(cart_df)) {
        row <- cart_df[i, ]
        cat(sprintf("Saving item %d | Qty: %d | Price: %.2f | Line Total: %.2f\n",
                    row$Item, row$Qty_Bought, row$Unit_Price, row$Line_Total))
        tryCatch({
          dbExecute(db, sprintf(
            "INSERT INTO sale_items (sale_id, item_id, quantity, unit_price, line_total)
           VALUES (%d, %d, %d, %.2f, %.2f)",
            sale_id, row$Item, row$Qty_Bought, row$Unit_Price, row$Line_Total
          ))
        }, error = function(e) {
          cat("❌ Error inserting item:", e$message, "\n")
        })
      }
    }, error = function(e) {
      showNotification(sprintf("❌ Failed to save sale: %s", e$message), type = "error")
      cat("❌ Sale insert failed:", e$message, "\n")
      return()
    })
    min_price <- min(cart_df$Unit_Price)
    max_price <- max(cart_df$Unit_Price)
    mean_before_tax <- mean(cart_df$Line_Total)
    variance_before_tax <- var(cart_df$Line_Total)
    receipt <- "------ 🧾 RECEIPT ------\n"
    for (i in 1:nrow(cart_df)) {
      row <- cart_df[i, ]
      receipt <- paste0(receipt, sprintf("Item: %d | Qty: %d | @ %.2f = %.2f\n", row$Item, row$Qty_Bought, row$Unit_Price, row$Line_Total))
    }
    receipt <- paste0(receipt, "--------------------------\n")
    if (discount_amount > 0) {
      receipt <- paste0(receipt, sprintf("Discount (50%%): -%.2f\n", discount_amount))
    }
    receipt <- paste0(receipt, sprintf("Subtotal:   %.2f\n", total_before_tax))
    receipt <- paste0(receipt, sprintf("Tax (14%%): %.2f\n", total_tax))
    receipt <- paste0(receipt, sprintf("TOTAL:     %.2f\n", total_after_tax))
    receipt <- paste0(receipt, "--------------------------\n")
    receipt <- paste0(receipt, "\n📊 RECEIPT STATISTICS 📊\n")
    receipt <- paste0(receipt, "--------------------------\n")
    receipt <- paste0(receipt, sprintf("Min Price:  %.2f\n", min_price))
    receipt <- paste0(receipt, sprintf("Max Price:  %.2f\n", max_price))
    receipt <- paste0(receipt, sprintf("Mean Line Total: %.2f\n", mean_before_tax))
    receipt <- paste0(receipt, sprintf("Variance Line Total: %.2f\n", variance_before_tax))
    receipt <- paste0(receipt, "--------------------------\n")
    if (total_after_tax > DISCOUNT_THRESHOLD) {
      if (is.null(transaction_discount())) {
        discount_uuid <- UUIDgenerate()
        discount_code <- paste0("DISC_", discount_uuid)
        tryCatch({
          dbExecute(db, sprintf(
            "INSERT INTO discount_codes (code, discount_percentage, used) VALUES ('%s', 50.0, FALSE)",
            discount_uuid
          ))
          cat(sprintf("Inserted discount code: %s\n", discount_code))
        }, error = function(e) {
          showNotification(sprintf("Failed to insert discount code: %s", e$message), type = "error")
        })
        transaction_discount(list(code = discount_uuid))
      } else {
        discount_code <- paste0("DISC_", transaction_discount()$code)
      }
      qr_filename <- tempfile(fileext = ".png")
      png(qr_filename)
      plot(qrcode::qr_code(discount_code))
      dev.off()
      receipt <- paste0(receipt, "A QR code for your next 50% discount is attached to this email!\n")
    }
    receipt <- paste0(receipt, "Thanks for shopping!\n")
    email <- compose_email(
      body = md(receipt),
      footer = md("Thank you for shopping with us!")
    )
    if (exists("qr_filename") && file.exists(qr_filename)) {
      email <- email %>% add_attachment(qr_filename)
    }
    tryCatch({
      smtp_send(
        email,
        to = input$customer_email,
        from = "fawzyabouzeid99@gmail.com",
        subject = "Your Purchase Receipt",
        credentials = creds_key("gmail_key")
      )
      showNotification("📧 Receipt sent to customer email with QR code attached.", type = "message")
    }, error = function(e) {
      showNotification(paste("Failed to send email:", e$message), type = "error")
    })
  })
  
  observeEvent(input$add_item, {
    req(input$new_item_id, input$new_item_price, input$new_item_quantity, input$new_item_expiration)
    inventory_now <- updated_inventory()
    if (input$new_item_id %in% inventory_now$Item) {
      output$add_status <- renderText("Item ID already exists. Cannot add duplicate.")
      return()
    }
    new_row <- data.frame(
      Item = input$new_item_id,
      Price = input$new_item_price,
      Quantity = input$new_item_quantity,
      Expiration_Date = as.character(input$new_item_expiration)
    )
    inventory_updated <- bind_rows(inventory_now, new_row)
    updated_inventory(inventory_updated)
    dbBegin(db)
    tryCatch({
      dbWriteTable(db, "items", inventory_updated, overwrite = TRUE)
      dbCommit(db)
    }, error = function(e) {
      dbRollback(db)
      showNotification(paste("Database error:", e$message), type = "error")
    })
    dir.create("qrcodes", showWarnings = FALSE)
    qr_content <- as.character(input$new_item_id)
    qr_filename <- file.path("qrcodes", paste0("number_", qr_content, ".png"))
    png(qr_filename)
    plot(qrcode::qr_code(qr_content))
    dev.off()
    output$add_status <- renderText(paste("Item added and QR code saved as", qr_filename))
  })
  
  observeEvent(input$restock_item, {
    req(input$restock_id, input$restock_quantity)
    inventory_now <- updated_inventory()
    if (!(input$restock_id %in% inventory_now$Item)) {
      output$restock_status <- renderText("Item ID does not exist.")
      return()
    }
    inventory_now$Quantity[inventory_now$Item == input$restock_id] <- inventory_now$Quantity[inventory_now$Item == input$restock_id] + input$restock_quantity
    if (!is.null(input$restock_expiration) && !is.na(input$restock_expiration)) {
      inventory_now$Expiration_Date[inventory_now$Item == input$restock_id] <- as.character(input$restock_expiration)
    }
    updated_inventory(inventory_now)
    dbBegin(db)
    tryCatch({
      dbWriteTable(db, "items", inventory_now, overwrite = TRUE)
      dbCommit(db)
    }, error = function(e) {
      dbRollback(db)
      showNotification(paste("Database error:", e$message), type = "error")
    })
    output$restock_status <- renderText(paste("Item", input$restock_id, "restocked successfully."))
  })
  
  observeEvent(input$check_quantity, {
    req(input$check_item_id)
    inventory_now <- updated_inventory()
    item_row <- inventory_now %>% filter(Item == input$check_item_id)
    if (nrow(item_row) == 0) {
      output$quantity_status <- renderText("Item not found.")
    } else {
      output$quantity_status <- renderText(paste("Item", input$check_item_id, "has", item_row$Quantity, "in stock."))
    }
  })
  
  observeEvent(input$logout_btn, {
    user_session(NULL)
    login_attempted(FALSE)
    cart(data.frame(Item = integer(), Quantity = integer(), stringsAsFactors = FALSE))
    applied_discount(NULL)
    transaction_discount(NULL)
    showModal(modalDialog(
      title = "🔐 Login",
      textInput("login_user", "Username"),
      passwordInput("login_pass", "Password"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("login_btn", "Login", class = "btn-primary neon-button")
      ),
      easyClose = FALSE,
      fade = TRUE
    ))
    showNotification("You have been logged out.", type = "message")
  })
  
  output$cart_table <- renderTable({ if (nrow(cart()) > 0) cart() })
  output$low_stock_alert <- renderUI({
    low_stock <- updated_inventory() %>% filter(Quantity < LOW_STOCK_THRESHOLD)
    if (nrow(low_stock) > 0) {
      tagList(h4("Low Stock Warning", style = "color: #ff8080;"),
              tableOutput("low_stock_table"))
    }
  })
  output$low_stock_table <- renderTable({
    updated_inventory() %>% filter(Quantity < LOW_STOCK_THRESHOLD)
  })
  output$expiration_warning <- renderUI({
    user <- user_session()
    if (tolower(user$role[1]) != "admin") return(NULL)
    current_date <- Sys.Date()
    inventory_now <- updated_inventory()
    expiring_items <- inventory_now %>%
      filter(!is.na(Expiration_Date)) %>%
      mutate(Expiration_Date = as.Date(Expiration_Date)) %>%
      mutate(Days_Until_Expiration = as.integer(Expiration_Date - current_date)) %>%
      filter(Days_Until_Expiration <= EXPIRATION_WARNING_DAYS & Days_Until_Expiration >= 0) %>%
      mutate(Expiration_Date = format(Expiration_Date, "%Y-%m-dd"))
    if (nrow(expiring_items) > 0) {
      tagList(
        h4("Expiration Warnings", style = "color: #ff8080;"),
        p("Items expiring within 7 days:"),
        tableOutput("expiring_items_table")
      )
    } else {
      tagList(
        h4("Expiration Warnings"),
        p("No items expiring within 7 days.")
      )
    }
  })
  output$expiring_items_table <- renderTable({
    expiring_items <- updated_inventory() %>%
      filter(!is.na(Expiration_Date)) %>%
      mutate(Expiration_Date = as.Date(Expiration_Date)) %>%
      mutate(Days_Until_Expiration = as.integer(Expiration_Date - Sys.Date())) %>%
      filter(Days_Until_Expiration <= EXPIRATION_WARNING_DAYS & Days_Until_Expiration >= 0) %>%
      mutate(Expiration_Date = format(Expiration_Date, "%Y-%m-dd"))
    expiring_items %>% select(Item, Price, Quantity, Expiration_Date, Days_Until_Expiration)
  })
}

shinyApp(ui, server)