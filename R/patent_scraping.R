
patent_scraping <- function(url, 
                            directory = NULL, 
                            tables = c('abstract', 'claims', 'cited_by', 'patent_citations', 'ipc'), 
                            show_progress = F) {

  if (!is.character(tables)) stop('`tables` must be a character string.')
  if (!is.logical(show_progress)) stop('`show_progress` must be logical.')
  if (!is.null(directory)) directory <- as.character(directory)

  # -----
  # directory
  if (!is.null(directory)) {
    fs::dir_create(directory)
    estou_aqui <- here::here(directory)
  } else {
    estou_aqui <- here::here()
  }

  # -----
  # id
  id <- tibble::tibble(url = url, day = format(Sys.time(), '%Y-%m-%d'), hour = format(Sys.time(), '%T'))
  id$id <- gsub('https://patents.google.com/patent/', '', url) |> {\(x) gsub('\\/.*$', '', x)}()
  id$tables <- tables

  # progress bar
  if (show_progress == T) print(paste(id$day, id$hour, url, sep = ' '))

  ## Inspect Browser with Disabled JavaScript
  # - Enter about:config into the search bar and select Accept the Risk and Continue.
  # - Enter javascript.enabled into the search box at the top of the page.
  # - Select the javascript.enabled toggle to change the value to false.

  html <- rvest::read_html(url) 

  # -----
  # abstract
  if (any(tables == 'abstract')) {

    html |>
      rvest::html_element('xpath' = '//abstract') ->
        abstract_path

      if (class(abstract_path) == 'xml_node') { 

        abstract_path |> 
          rvest::html_text2() -> 
          abstract 

      readr::write_csv(tibble::tibble(id = id$id, abstract = abstract), here::here(estou_aqui, 'abstract.csv'), append = T, col_names = !file.exists(here::here(estou_aqui, 'abstract.csv')))

      } else {  

      readr::write_csv(tibble::tibble(id = id$id, abstract = NA), here::here(estou_aqui, 'abstract.csv'), append = T, col_names = !file.exists(here::here(estou_aqui, 'abstract.csv'))) 
      }
  }

  # -----
  # claims
  if (any(tables == 'claims')) {

    html |>
      rvest::html_element('xpath' = '//claims') ->
        claims_path

      if (class(claims_path) == 'xml_node') { 
        claims_path |> 
          rvest::html_text2() -> 
          claims 

      readr::write_csv(tibble::tibble(id = id$id, claims = claims), here::here(estou_aqui, 'claims.csv'), append = T, col_names = !file.exists(here::here(estou_aqui, 'claims.csv')))

      } else { 

      readr::write_csv(tibble::tibble(id = id$id, claims = NA), here::here(estou_aqui, 'claims.csv'), append = T, col_names = !file.exists(here::here(estou_aqui, 'claims.csv')))
      }
  }

  # -----
  # Cited By: sem javascript
  # Families Citing this family: com javascript
  if (any(tables == 'cited_by')) {

    html |>
      rvest::html_element('xpath' = '//*[contains (text(), "Families Citing this family")]//following::table/thead') ->
        cited_by_path

      if (class(cited_by_path) == 'xml_node') { 

        cited_by_path |> 
          rvest::html_table() |>
          janitor::clean_names() ->
          thead_cited_by

        html |> 
          rvest::html_element('xpath' = '//*[contains (text(), "Families Citing this family")]//following::table/tbody') |> 
          rvest::html_table() |>
          stats::setNames(names(thead_cited_by)) |>
          dplyr::mutate(publication_number = gsub('\n.*$', '', publication_number)) |>
          dplyr::mutate(id = id$id) |>
          dplyr::relocate(id) -> 
          cited_by  

        readr::write_csv(cited_by, here::here(estou_aqui, 'cited_by.csv'), append = T, col_names = !file.exists(here::here(estou_aqui, 'cited_by.csv')))

      } else {  

        readr::write_csv(tibble::tibble(id = id$id, publication_number = NA, priority_date = NA, publication_date = NA, assignee = NA, title = NA), 
                         here::here(estou_aqui, 'cited_by.csv'), append = T, col_names = !file.exists(here::here(estou_aqui, 'cited_by.csv'))) 
      }
  }

  # -----
  # Family Cites Families: sem javascript
  # Patent Citations: com javascript 
  if (any(tables == 'patent_citations')) {
    html |>
      rvest::html_element('xpath' = '//*[contains (text(), "Family Cites Families")]//following::table/thead') ->
      patent_citations_path

      if (class(patent_citations_path) == 'xml_node') { 
        patent_citations_path |> 
          rvest::html_table() |>
          janitor::clean_names() ->
          thead_patent_citations

        html |>
          rvest::html_element('xpath' = '//*[contains (text(), "Family Cites Families")]//following::table/tbody') |>
          rvest::html_table() |>
          stats::setNames(names(thead_patent_citations)) |>
          dplyr::mutate(publication_number = gsub('\n.*$', '', publication_number)) |>
          dplyr::mutate(id = id$id) |>
          dplyr::relocate(id) -> 
          patent_citations

        readr::write_csv(patent_citations, here::here(estou_aqui, 'patent_citations.csv'), append = T, col_names = !file.exists(here::here(estou_aqui, 'patent_citations.csv')))

      } else { 
        readr::write_csv(tibble::tibble(id = id$id, publication_number = NA, priority_date = NA, publication_date = NA, assignee = NA, title = NA), 
                         here::here(estou_aqui, 'patent_citations.csv'), append = T, col_names = !file.exists(here::here(estou_aqui, 'patent_citations.csv'))) 
      }
  }

  # -----
  # Classifications: sem javascript
  if (any(tables == 'ipc')) {
    html |>
      rvest::html_element('xpath' = '//*[contains (text(), "Classifications")]//following::*') |>
      as.character() ->
      ipc_text

      if (!is.null(ipc_text)) { 
        stringr::str_locate_all(ipc_text, '[A-Z]{1}[0-9]{2}[A-Z]{1}[0-9]{2}\\/[0-9]{2}') |>
          {\(x) stringr::str_sub(ipc_text, x[[1]]) }() ->
          ipc

        readr::write_csv(tibble::tibble(id = id$id, ipc = ipc), here::here(estou_aqui, 'ipc.csv'), append = T, col_names = !file.exists(here::here(estou_aqui, 'ipc.csv'))) 

      } else {  
        readr::write_csv(tibble::tibble(id = id$id, ipc = NA), here::here(estou_aqui, 'ipc.csv'), append = T, col_names = !file.exists(here::here(estou_aqui, 'ipc.csv')))
      }
  }

  # -----
  # id export csv
  readr::write_csv(id, here::here(estou_aqui, 'id.csv'), append = T, col_names = !file.exists(here::here(estou_aqui, 'id.csv')))

}
