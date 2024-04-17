# HPO restful API search
Ronald Moura
2024-04-17

## HPO: general search

query: is any character with identifier (HPO, OMIM, ORPHA), #an entrez
id, or text.

max: maximum number de results to retrieve. Default is -1, which is
everything

category: optional with a category to filter the results. #May be
“terms”, “genes” or “diseases”. Default is NULL, i.e., everything.

<details>
<summary>Code</summary>

``` r
hpo_general_search = function(query, max = -1, category = NULL) {

  require(httr2, quietly = TRUE, warn.conflicts = FALSE) 
  require(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
  
  base_url = "https://hpo.jax.org/api/hpo/search"
  
  response = request(base_url) |>
    req_url_query(q = query,
                  max = max,
                  category = category) |>
    req_perform()
  
  #HPO general terms
  hpo_general_terms = response |> 
    resp_body_json() |>
    pluck("terms") |> 
    map_dfr(
      \(x) {
        tibble(name          = x |> pluck("name"), 
               id            = x |> pluck("id"), 
               childrenCount = x |> pluck("childrenCount"),
               ontologyId    = x |> pluck("ontologyId"),
               synonym       = x |> pluck("synonym")
        )
      })
  
  #HPO general diseases
  hpo_general_diseases = response |> 
    resp_body_json() |>
    pluck("diseases") |> 
    map_dfr(
      \(x) {
        tibble(db        = x |> pluck("db"), 
               dbName    = x |> pluck("dbName"), 
               dbRef     = x |> pluck("dbRef"),
               diseaseId = x |> pluck("diseaseId")
        )
      })
  
  #HPO general genes
  hpo_general_genes = response |> 
    resp_body_json() |>
    pluck("genes") |> 
    map_dfr(
      \(x) {
        tibble(geneId     = x |> pluck("geneId"), 
               geneSymbol = x |> pluck("geneSymbol")
        )
      })
  
  return(list(hpo_general_terms = hpo_general_terms,
              hpo_general_diseases = hpo_general_diseases,
              hpo_general_genes = hpo_general_genes))

}
```

</details>

## HPO gene search

entrez_id: a single character variable with entrez gene id

<details>
<summary>Code</summary>

``` r
hpo_gene_search = function(entrez_id) {
  
  require(httr2, quietly = TRUE, warn.conflicts = FALSE) 
  require(tidyverse, quietly = TRUE, warn.conflicts = FALSE)

  base_url = "https://hpo.jax.org/api/hpo/gene" 
  
  response = request(base_url) |>
    req_url_path_append(entrez_id) |>
    req_perform()
  
  #HPO associations
  hpo_assoc = response |> 
    resp_body_json() |> 
    pluck("termAssoc") |> 
    map_dfr(
      \(x) {
        tibble(ontologyID = x |> pluck("ontologyId"), 
               name       = x |> pluck("name"), 
               definition = x |> pluck("definition")
        )
        })
  
  #disease associated
  hpo_disease = response |> 
    resp_body_json() |> 
    pluck("diseaseAssoc") |> 
    map_dfr(
      \(x) {
        tibble(diseaseId   = x |> pluck("diseaseId"), 
               diseaseName = x |> pluck("diseaseName"), 
               dbId        = x |> pluck("dbID"),
               db          = x |> pluck("db")
        )
      })

  return(list(hpo_assoc   = hpo_assoc,
              hpo_disease = hpo_disease))
  
}
```

</details>

## HPO disease search

disease_id: a single character with disease code. May be OMIM, ORPHA.

<details>
<summary>Code</summary>

``` r
hpo_disease_search = function(disease_id) {
  
  require(httr2, quietly = TRUE, warn.conflicts = FALSE) 
  require(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
  
  base_url = ("https://hpo.jax.org/api/hpo/disease") 
  
  response = request(base_url) |>
    req_url_path_append(disease_id) |>
    req_perform()
  
  #disease details
  disease_details = response |> 
    resp_body_json() |> 
    pluck("disease") |> 
    as_tibble()
  
  #disease-associated genes
  disease_genes = response |> 
    resp_body_json() |> 
    pluck("geneAssoc") |> 
    map_dfr(
      \(x) {
        tibble(geneSymbol  = x |> pluck("geneSymbol"), 
               geneId      = x |> pluck("geneId")
        )
      })
  
  #disease-associated terms
  disease_terms = response |> 
    resp_body_json() |> 
    pluck("catTermsMap") |> 
    map_dfr(
      \(x) {
        tibble(catLabel  = x |> pluck("catLabel"), 
               terms     = x |> pluck("terms")
        )
      }) %>%
    unnest_wider(terms)
  
  return(list(disease_details = disease_details,
              disease_genes   = disease_genes,
              disease_terms   =  disease_terms))

}
```

</details>

## HPO term search

term_id: a simple character with the hpo term.

<details>
<summary>Code</summary>

``` r
hpo_term_search = function(term_id) {
  
  require(httr2, quietly = TRUE, warn.conflicts = FALSE) 
  require(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
  
  base_url = "https://hpo.jax.org/api/hpo/term"
  
  response = request(base_url) |>
    req_url_path_append(term_id) |>
    req_perform()
  
  #term details
  term_details = response |> 
    resp_body_json() |> 
    map_dfr(
      \(x) {
            tibble(name        = x |>  pluck("name"),
                   id          = x |>  pluck("id"),
                   altTermIds  = x |>  pluck("altTermIds") |> list(),
                   definition  = x |>  pluck("definition"),
                   comment     = x |>  pluck("comment"),
                   synonyms    = x |>  pluck("synonyms") |> list(),
                   isObsolete  = x |>  pluck("isObsolete"),
                   xrefs       = x |>  pluck("xrefs") |> list(),
                   pubmedXrefs = x |>  pluck("pubmedXrefs")
            )
        }) |>
    drop_na()
  
  #term relations
  term_relations = response |> 
    resp_body_json() |> 
    map_dfr(
      \(x) {
        tibble(termCount = x |>  pluck("termCount"),
               parents   = x |>  pluck("parents")  |> list(),
               children  = x |>  pluck("children") |> list()
        )
      }) |> 
    drop_na()
  
  #genes associated to term
  response = request(base_url) |>
    req_url_path_append(term_id,
                        "genes") |> 
    req_perform()
  
  term_genes_association = response |> 
    resp_body_json() |> 
    pluck("genes") |> 
    map_dfr(
      \(x) {
        tibble(geneSymbol = x |>  pluck("geneSymbol"),
               dbDiseases = x |>  pluck("dbDiseases") |> list(),
               geneId     = x |>  pluck("geneId")
        )
      }) |> 
    drop_na()
  
  #diseases associated to term
  response = request(base_url) |>
    req_url_path_append(term_id,
                        "diseases") |>
    req_perform()
  
  term_disiases_association = response |> 
    resp_body_json() |>
    pluck("diseases") |> 
    map_dfr(
      \(x) {
        tibble(diseaseId   = x |>  pluck("diseaseId"),
               dbGenes     = x |>  pluck("dbGenes") |> list(),
               diseaseName = x |>  pluck("diseaseName"),
               dbId        = x |>  pluck("dbId"),
               db          = x |>  pluck("db")
        )
      }) |> 
    drop_na()
  
  #returning results
  return(list(term_details = term_details,
              term_relations = term_relations,
              term_genes_association = term_genes_association,
              term_disiases_association = term_disiases_association))

}
```

</details>

## HPO diseases by intersecting terms

term_ids: a vector with terms.

max: a integer value with the maximum number of hits. Defaut is -1 for
everything.

<details>
<summary>Code</summary>

``` r
hpo_term_intersecting = function(term_ids, max = -1) {
  
  require(httr2, quietly = TRUE, warn.conflicts = FALSE) 
  require(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
  
  base_url = "http://hpo.jax.org/api/hpo/term/intersecting"
  
    response = request(base_url) |>
      req_url_query(q = paste0(term_ids, collapse = ","),
                    max = max) |>
      req_perform()
    
    #term details
    term_intersecting = response |> 
      resp_body_json() |> 
      pluck("associations") |> 
      map_dfr(
        \(x) {
          tibble(diseaseId   = x |>  pluck("diseaseId"),
                 diseaseName = x |>  pluck("diseaseName"),
                 dbId        = x |>  pluck("dbId") |> list(),
                 db          = x |>  pluck("db")
                 )
        }) |>
      drop_na()
  
}
```

</details>

## HPO Medical Action Ontology term search

medical_term_id: A character with a Medical Action Ontology identifer or
search term, such as “therapy”.

<details>
<summary>Code</summary>

``` r
hpo_medical_search = function(medical_term_id) {
  
  require(httr2, quietly = TRUE, warn.conflicts = FALSE) 
  require(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
  
  base_url = "http://hpo.jax.org/api/maxo/search/"
  
  response = request(base_url) |>
    req_url_query(q = medical_term_id) |>
    req_perform()
  
  #term details
  medical_term = response |> 
    resp_body_json() |>
    pluck("terms") |> 
    map_dfr(
      \(x) {
        tibble(term           = x |>  pluck("term") |> list(),
               synonymMatched = x |>  pluck("synonymMatched"),
               synonym        = x |>  pluck("synonym")
        )
      }) |>
    drop_na()
  
  return(medical_term)

}
```

</details>

## Testing the functions

<details>
<summary>Code</summary>

``` r
#general term: wilson disease
query = "ATP"
max = -1
category = NULL

atp_general_search = hpo_general_search(query, max, category)

glimpse(atp_general_search)
```

</details>

    List of 3
     $ hpo_general_terms   : tibble [4 × 5] (S3: tbl_df/tbl/data.frame)
      ..$ name         : chr [1:4] "Elevated erythrocyte adenosine triphosphate concentration" "Decreased activity of mitochondrial ATP synthase complex" "Abnormal platelet dense granule ATP/ADP ratio" "Abnormal platelet ATP dense granule secretion"
      ..$ id           : chr [1:4] "HP:4000186" "HP:0011925" "HP:0030401" "HP:0030398"
      ..$ childrenCount: int [1:4] 0 0 0 0
      ..$ ontologyId   : chr [1:4] "HP:4000186" "HP:0011925" "HP:0030401" "HP:0030398"
      ..$ synonym      : chr [1:4] "Adenosine triphosphate (ATP) high in erythrocytes" NA NA NA
     $ hpo_general_diseases: tibble [12 × 4] (S3: tbl_df/tbl/data.frame)
      ..$ db       : chr [1:12] "ORPHA" "ORPHA" "OMIM" "OMIM" ...
      ..$ dbName   : chr [1:12] "ATP13A2-related juvenile neuronal ceroid lipofuscinosis" "Isolated ATP synthase deficiency" "Mitochondrial complex V (ATP synthase) deficiency nuclear type 5" "Mitochondrial complex V (ATP synthase) deficiency, nuclear type 1" ...
      ..$ dbRef    : chr [1:12] "314632" "254913" "618120" "604273" ...
      ..$ diseaseId: chr [1:12] "ORPHA:314632" "ORPHA:254913" "OMIM:618120" "OMIM:604273" ...
     $ hpo_general_genes   : tibble [42 × 2] (S3: tbl_df/tbl/data.frame)
      ..$ geneId    : int [1:42] 57194 23250 286410 23400 79572 476 477 478 481 487 ...
      ..$ geneSymbol: chr [1:42] "ATP10A" "ATP11A" "ATP11C" "ATP13A2" ...

The function will return a list object containing three tibbles: terms,
diseases and genes. Each one of these tibbles stores information about
the results among the categories. Let’s inspect the results for the
terms category:

<details>
<summary>Code</summary>

``` r
gt::gt(atp_general_search[["hpo_general_terms"]])
```

</details>
<div id="lbhihumscm" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lbhihumscm table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#lbhihumscm thead, #lbhihumscm tbody, #lbhihumscm tfoot, #lbhihumscm tr, #lbhihumscm td, #lbhihumscm th {
  border-style: none;
}

#lbhihumscm p {
  margin: 0;
  padding: 0;
}

#lbhihumscm .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#lbhihumscm .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#lbhihumscm .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#lbhihumscm .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#lbhihumscm .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lbhihumscm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbhihumscm .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#lbhihumscm .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#lbhihumscm .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#lbhihumscm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lbhihumscm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lbhihumscm .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#lbhihumscm .gt_spanner_row {
  border-bottom-style: hidden;
}

#lbhihumscm .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#lbhihumscm .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#lbhihumscm .gt_from_md > :first-child {
  margin-top: 0;
}

#lbhihumscm .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lbhihumscm .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#lbhihumscm .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#lbhihumscm .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#lbhihumscm .gt_row_group_first td {
  border-top-width: 2px;
}

#lbhihumscm .gt_row_group_first th {
  border-top-width: 2px;
}

#lbhihumscm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbhihumscm .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lbhihumscm .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lbhihumscm .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbhihumscm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbhihumscm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lbhihumscm .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#lbhihumscm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lbhihumscm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbhihumscm .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lbhihumscm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbhihumscm .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#lbhihumscm .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbhihumscm .gt_left {
  text-align: left;
}

#lbhihumscm .gt_center {
  text-align: center;
}

#lbhihumscm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lbhihumscm .gt_font_normal {
  font-weight: normal;
}

#lbhihumscm .gt_font_bold {
  font-weight: bold;
}

#lbhihumscm .gt_font_italic {
  font-style: italic;
}

#lbhihumscm .gt_super {
  font-size: 65%;
}

#lbhihumscm .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#lbhihumscm .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lbhihumscm .gt_indent_1 {
  text-indent: 5px;
}

#lbhihumscm .gt_indent_2 {
  text-indent: 10px;
}

#lbhihumscm .gt_indent_3 {
  text-indent: 15px;
}

#lbhihumscm .gt_indent_4 {
  text-indent: 20px;
}

#lbhihumscm .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="name">name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="id">id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="childrenCount">childrenCount</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="ontologyId">ontologyId</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="synonym">synonym</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="name" class="gt_row gt_left">Elevated erythrocyte adenosine triphosphate concentration</td>
<td headers="id" class="gt_row gt_left">HP:4000186</td>
<td headers="childrenCount" class="gt_row gt_right">0</td>
<td headers="ontologyId" class="gt_row gt_left">HP:4000186</td>
<td headers="synonym" class="gt_row gt_left">Adenosine triphosphate (ATP) high in erythrocytes</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Decreased activity of mitochondrial ATP synthase complex</td>
<td headers="id" class="gt_row gt_left">HP:0011925</td>
<td headers="childrenCount" class="gt_row gt_right">0</td>
<td headers="ontologyId" class="gt_row gt_left">HP:0011925</td>
<td headers="synonym" class="gt_row gt_left">NA</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Abnormal platelet dense granule ATP/ADP ratio</td>
<td headers="id" class="gt_row gt_left">HP:0030401</td>
<td headers="childrenCount" class="gt_row gt_right">0</td>
<td headers="ontologyId" class="gt_row gt_left">HP:0030401</td>
<td headers="synonym" class="gt_row gt_left">NA</td></tr>
    <tr><td headers="name" class="gt_row gt_left">Abnormal platelet ATP dense granule secretion</td>
<td headers="id" class="gt_row gt_left">HP:0030398</td>
<td headers="childrenCount" class="gt_row gt_right">0</td>
<td headers="ontologyId" class="gt_row gt_left">HP:0030398</td>
<td headers="synonym" class="gt_row gt_left">NA</td></tr>
  </tbody>
  
  
</table>
</div>

For the diseases category:

<details>
<summary>Code</summary>

``` r
gt::gt(atp_general_search[["hpo_general_diseases"]])
```

</details>
<div id="zjaxmsexis" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zjaxmsexis table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zjaxmsexis thead, #zjaxmsexis tbody, #zjaxmsexis tfoot, #zjaxmsexis tr, #zjaxmsexis td, #zjaxmsexis th {
  border-style: none;
}

#zjaxmsexis p {
  margin: 0;
  padding: 0;
}

#zjaxmsexis .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zjaxmsexis .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zjaxmsexis .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zjaxmsexis .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zjaxmsexis .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zjaxmsexis .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zjaxmsexis .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zjaxmsexis .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zjaxmsexis .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zjaxmsexis .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zjaxmsexis .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zjaxmsexis .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zjaxmsexis .gt_spanner_row {
  border-bottom-style: hidden;
}

#zjaxmsexis .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#zjaxmsexis .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zjaxmsexis .gt_from_md > :first-child {
  margin-top: 0;
}

#zjaxmsexis .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zjaxmsexis .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zjaxmsexis .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#zjaxmsexis .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#zjaxmsexis .gt_row_group_first td {
  border-top-width: 2px;
}

#zjaxmsexis .gt_row_group_first th {
  border-top-width: 2px;
}

#zjaxmsexis .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zjaxmsexis .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zjaxmsexis .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zjaxmsexis .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zjaxmsexis .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zjaxmsexis .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zjaxmsexis .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zjaxmsexis .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zjaxmsexis .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zjaxmsexis .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zjaxmsexis .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zjaxmsexis .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zjaxmsexis .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zjaxmsexis .gt_left {
  text-align: left;
}

#zjaxmsexis .gt_center {
  text-align: center;
}

#zjaxmsexis .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zjaxmsexis .gt_font_normal {
  font-weight: normal;
}

#zjaxmsexis .gt_font_bold {
  font-weight: bold;
}

#zjaxmsexis .gt_font_italic {
  font-style: italic;
}

#zjaxmsexis .gt_super {
  font-size: 65%;
}

#zjaxmsexis .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zjaxmsexis .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zjaxmsexis .gt_indent_1 {
  text-indent: 5px;
}

#zjaxmsexis .gt_indent_2 {
  text-indent: 10px;
}

#zjaxmsexis .gt_indent_3 {
  text-indent: 15px;
}

#zjaxmsexis .gt_indent_4 {
  text-indent: 20px;
}

#zjaxmsexis .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="db">db</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="dbName">dbName</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="dbRef">dbRef</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="diseaseId">diseaseId</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="db" class="gt_row gt_left">ORPHA</td>
<td headers="dbName" class="gt_row gt_left">ATP13A2-related juvenile neuronal ceroid lipofuscinosis</td>
<td headers="dbRef" class="gt_row gt_right">314632</td>
<td headers="diseaseId" class="gt_row gt_left">ORPHA:314632</td></tr>
    <tr><td headers="db" class="gt_row gt_left">ORPHA</td>
<td headers="dbName" class="gt_row gt_left">Isolated ATP synthase deficiency</td>
<td headers="dbRef" class="gt_row gt_right">254913</td>
<td headers="diseaseId" class="gt_row gt_left">ORPHA:254913</td></tr>
    <tr><td headers="db" class="gt_row gt_left">OMIM</td>
<td headers="dbName" class="gt_row gt_left">Mitochondrial complex V (ATP synthase) deficiency nuclear type 5</td>
<td headers="dbRef" class="gt_row gt_right">618120</td>
<td headers="diseaseId" class="gt_row gt_left">OMIM:618120</td></tr>
    <tr><td headers="db" class="gt_row gt_left">OMIM</td>
<td headers="dbName" class="gt_row gt_left">Mitochondrial complex V (ATP synthase) deficiency, nuclear type 1</td>
<td headers="dbRef" class="gt_row gt_right">604273</td>
<td headers="diseaseId" class="gt_row gt_left">OMIM:604273</td></tr>
    <tr><td headers="db" class="gt_row gt_left">OMIM</td>
<td headers="dbName" class="gt_row gt_left">Mitochondrial complex V (ATP synthase) deficiency, nuclear type 2</td>
<td headers="dbRef" class="gt_row gt_right">614052</td>
<td headers="diseaseId" class="gt_row gt_left">OMIM:614052</td></tr>
    <tr><td headers="db" class="gt_row gt_left">OMIM</td>
<td headers="dbName" class="gt_row gt_left">Mitochondrial complex V (atp synthase) deficiency, nuclear type 3</td>
<td headers="dbRef" class="gt_row gt_right">614053</td>
<td headers="diseaseId" class="gt_row gt_left">OMIM:614053</td></tr>
    <tr><td headers="db" class="gt_row gt_left">OMIM</td>
<td headers="dbName" class="gt_row gt_left">Mitochondrial complex V (atp synthase) deficiency, nuclear type 4</td>
<td headers="dbRef" class="gt_row gt_right">615228</td>
<td headers="diseaseId" class="gt_row gt_left">OMIM:615228</td></tr>
    <tr><td headers="db" class="gt_row gt_left">OMIM</td>
<td headers="dbName" class="gt_row gt_left">Mitochondrial complex V (ATP synthase) deficiency, nuclear type 4A</td>
<td headers="dbRef" class="gt_row gt_right">620358</td>
<td headers="diseaseId" class="gt_row gt_left">OMIM:620358</td></tr>
    <tr><td headers="db" class="gt_row gt_left">OMIM</td>
<td headers="dbName" class="gt_row gt_left">Mitochondrial complex V (ATP synthase) deficiency, nuclear type 6</td>
<td headers="dbRef" class="gt_row gt_right">618683</td>
<td headers="diseaseId" class="gt_row gt_left">OMIM:618683</td></tr>
    <tr><td headers="db" class="gt_row gt_left">OMIM</td>
<td headers="dbName" class="gt_row gt_left">Mitochondrial complex V (ATP synthase) deficiency, nuclear type 7</td>
<td headers="dbRef" class="gt_row gt_right">620359</td>
<td headers="diseaseId" class="gt_row gt_left">OMIM:620359</td></tr>
    <tr><td headers="db" class="gt_row gt_left">ORPHA</td>
<td headers="dbName" class="gt_row gt_left">MT-ATP6-related mitochondrial spastic paraplegia</td>
<td headers="dbRef" class="gt_row gt_right">320360</td>
<td headers="diseaseId" class="gt_row gt_left">ORPHA:320360</td></tr>
    <tr><td headers="db" class="gt_row gt_left">OMIM</td>
<td headers="dbName" class="gt_row gt_left">Sodium-Potassium-Atpase activity of red cell</td>
<td headers="dbRef" class="gt_row gt_right">270425</td>
<td headers="diseaseId" class="gt_row gt_left">OMIM:270425</td></tr>
  </tbody>
  
  
</table>
</div>

and for the genes:

<details>
<summary>Code</summary>

``` r
gt::gt(atp_general_search[["hpo_general_genes"]])
```

</details>
<div id="wksxeygnvh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#wksxeygnvh table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#wksxeygnvh thead, #wksxeygnvh tbody, #wksxeygnvh tfoot, #wksxeygnvh tr, #wksxeygnvh td, #wksxeygnvh th {
  border-style: none;
}

#wksxeygnvh p {
  margin: 0;
  padding: 0;
}

#wksxeygnvh .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#wksxeygnvh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#wksxeygnvh .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#wksxeygnvh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#wksxeygnvh .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wksxeygnvh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wksxeygnvh .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wksxeygnvh .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#wksxeygnvh .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#wksxeygnvh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wksxeygnvh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wksxeygnvh .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#wksxeygnvh .gt_spanner_row {
  border-bottom-style: hidden;
}

#wksxeygnvh .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#wksxeygnvh .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#wksxeygnvh .gt_from_md > :first-child {
  margin-top: 0;
}

#wksxeygnvh .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wksxeygnvh .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#wksxeygnvh .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#wksxeygnvh .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#wksxeygnvh .gt_row_group_first td {
  border-top-width: 2px;
}

#wksxeygnvh .gt_row_group_first th {
  border-top-width: 2px;
}

#wksxeygnvh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wksxeygnvh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#wksxeygnvh .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#wksxeygnvh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wksxeygnvh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wksxeygnvh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wksxeygnvh .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#wksxeygnvh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wksxeygnvh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wksxeygnvh .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wksxeygnvh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wksxeygnvh .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wksxeygnvh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wksxeygnvh .gt_left {
  text-align: left;
}

#wksxeygnvh .gt_center {
  text-align: center;
}

#wksxeygnvh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wksxeygnvh .gt_font_normal {
  font-weight: normal;
}

#wksxeygnvh .gt_font_bold {
  font-weight: bold;
}

#wksxeygnvh .gt_font_italic {
  font-style: italic;
}

#wksxeygnvh .gt_super {
  font-size: 65%;
}

#wksxeygnvh .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#wksxeygnvh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#wksxeygnvh .gt_indent_1 {
  text-indent: 5px;
}

#wksxeygnvh .gt_indent_2 {
  text-indent: 10px;
}

#wksxeygnvh .gt_indent_3 {
  text-indent: 15px;
}

#wksxeygnvh .gt_indent_4 {
  text-indent: 20px;
}

#wksxeygnvh .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="geneId">geneId</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="geneSymbol">geneSymbol</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="geneId" class="gt_row gt_right">57194</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP10A</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">23250</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP11A</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">286410</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP11C</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">23400</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP13A2</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">79572</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP13A3</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">476</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP1A1</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">477</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP1A2</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">478</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP1A3</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">481</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP1B1</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">487</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP2A1</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">488</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP2A2</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">490</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP2B1</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">491</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP2B2</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">492</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP2B3</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">27032</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP2C1</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">495</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP4A</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">498</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP5F1A</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">506</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP5F1B</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">513</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP5F1D</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">514</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP5F1E</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">518</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP5MC3</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">84833</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP5MK</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">539</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP5PO</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">537</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP6AP1</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">10159</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP6AP2</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">535</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP6V0A1</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">23545</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP6V0A2</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">50617</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP6V0A4</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">527</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP6V0C</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">523</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP6V1A</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">525</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP6V1B1</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">526</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP6V1B2</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">529</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP6V1E1</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">538</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP7A</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">540</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP7B</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">51761</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP8A2</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">5205</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP8B1</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">10079</td>
<td headers="geneSymbol" class="gt_row gt_left">ATP9A</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">64756</td>
<td headers="geneSymbol" class="gt_row gt_left">ATPAF1</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">91647</td>
<td headers="geneSymbol" class="gt_row gt_left">ATPAF2</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">4508</td>
<td headers="geneSymbol" class="gt_row gt_left">MT-ATP6</td></tr>
    <tr><td headers="geneId" class="gt_row gt_right">4509</td>
<td headers="geneSymbol" class="gt_row gt_left">MT-ATP8</td></tr>
  </tbody>
  
  
</table>
</div>
<details>
<summary>Code</summary>

``` r
#MECP2 entrez id: 4204
MECP2 = hpo_gene_search("4204")

#PLEC entrez id: 5339
PLEC = hpo_gene_search("5339")

#Rett syndrome: OMIM:312750
rett_syndrome = hpo_disease_search("OMIM:312750")

#PFIC1: OMIM:211600
pfic1 = hpo_disease_search("OMIM:211600")

#Autistic behavior: HP:0000729
autistic_behavior = hpo_term_search("HP:0000729")

#Intrahepatic cholestasis with episodic jaundice: HP:0006575
intrahepatic_jaundice = hpo_term_search("HP:0006575")

#Autistic behavior: HP:0000729
#X-linked dominant inheritance: HP:0001423
#Childhood onset: HP:0011463
term_ids = c("HP:0000729", "HP:0001423", "HP:0011463")
similar_to_rett = hpo_term_intersecting(term_ids, max = 20)

#terms related to therapy
medical_therapy = hpo_medical_search("therapy")
```

</details>
