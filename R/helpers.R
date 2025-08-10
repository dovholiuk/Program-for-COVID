`%||%` <- function(a, b) if (!is.null(a)) a else b
idify <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

# Порядок підвкладок-предикторів
PREDICTOR_ORDER <- c("NLR", "d.NLR", "IGLR.100", "PLR", "SII")

# Якщо список без імен — підставимо їх за замовчуванням (коли довжина збігається)
ensure_names <- function(x, wanted) {
  if (is.null(x)) return(NULL)
  nms <- names(x)
  if (is.null(nms) || any(nms == "")) {
    if (length(x) == length(wanted)) names(x) <- wanted
  }
  x
}

# Склеїти всі індекси в один рядок для експорту
collapse_list_for_predictor <- function(L, pred) {
  if (is.null(L) || is.null(L[[pred]])) return(NA_character_)
  paste(as.character(unlist(L[[pred]])), collapse = ", ")
}