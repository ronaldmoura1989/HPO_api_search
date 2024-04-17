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
atp_general_search[["hpo_general_terms"]] |>
  gt::gt() |>
  gt::fmt_markdown(columns = everything()) |>
  gt::tab_options(table.width = gt::px(400))
```

</details>
<div id="jngtgjwpyh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jngtgjwpyh table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#jngtgjwpyh thead, #jngtgjwpyh tbody, #jngtgjwpyh tfoot, #jngtgjwpyh tr, #jngtgjwpyh td, #jngtgjwpyh th {
  border-style: none;
}

#jngtgjwpyh p {
  margin: 0;
  padding: 0;
}

#jngtgjwpyh .gt_table {
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
  width: 400px;
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

#jngtgjwpyh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#jngtgjwpyh .gt_title {
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

#jngtgjwpyh .gt_subtitle {
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

#jngtgjwpyh .gt_heading {
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

#jngtgjwpyh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jngtgjwpyh .gt_col_headings {
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

#jngtgjwpyh .gt_col_heading {
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

#jngtgjwpyh .gt_column_spanner_outer {
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

#jngtgjwpyh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jngtgjwpyh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jngtgjwpyh .gt_column_spanner {
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

#jngtgjwpyh .gt_spanner_row {
  border-bottom-style: hidden;
}

#jngtgjwpyh .gt_group_heading {
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

#jngtgjwpyh .gt_empty_group_heading {
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

#jngtgjwpyh .gt_from_md > :first-child {
  margin-top: 0;
}

#jngtgjwpyh .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jngtgjwpyh .gt_row {
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

#jngtgjwpyh .gt_stub {
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

#jngtgjwpyh .gt_stub_row_group {
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

#jngtgjwpyh .gt_row_group_first td {
  border-top-width: 2px;
}

#jngtgjwpyh .gt_row_group_first th {
  border-top-width: 2px;
}

#jngtgjwpyh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jngtgjwpyh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#jngtgjwpyh .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#jngtgjwpyh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jngtgjwpyh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jngtgjwpyh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jngtgjwpyh .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#jngtgjwpyh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jngtgjwpyh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jngtgjwpyh .gt_footnotes {
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

#jngtgjwpyh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jngtgjwpyh .gt_sourcenotes {
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

#jngtgjwpyh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jngtgjwpyh .gt_left {
  text-align: left;
}

#jngtgjwpyh .gt_center {
  text-align: center;
}

#jngtgjwpyh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jngtgjwpyh .gt_font_normal {
  font-weight: normal;
}

#jngtgjwpyh .gt_font_bold {
  font-weight: bold;
}

#jngtgjwpyh .gt_font_italic {
  font-style: italic;
}

#jngtgjwpyh .gt_super {
  font-size: 65%;
}

#jngtgjwpyh .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#jngtgjwpyh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#jngtgjwpyh .gt_indent_1 {
  text-indent: 5px;
}

#jngtgjwpyh .gt_indent_2 {
  text-indent: 10px;
}

#jngtgjwpyh .gt_indent_3 {
  text-indent: 15px;
}

#jngtgjwpyh .gt_indent_4 {
  text-indent: 20px;
}

#jngtgjwpyh .gt_indent_5 {
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
    <tr><td headers="name" class="gt_row gt_left"><div data-qmd="Elevated erythrocyte adenosine triphosphate concentration"><div class='gt_from_md'><p>Elevated erythrocyte adenosine triphosphate concentration</p>
</div></div></td>
<td headers="id" class="gt_row gt_left"><div data-qmd="HP:4000186"><div class='gt_from_md'><p>HP:4000186</p>
</div></div></td>
<td headers="childrenCount" class="gt_row gt_right"><div data-qmd="0"><div class='gt_from_md'><p>0</p>
</div></div></td>
<td headers="ontologyId" class="gt_row gt_left"><div data-qmd="HP:4000186"><div class='gt_from_md'><p>HP:4000186</p>
</div></div></td>
<td headers="synonym" class="gt_row gt_left"><div data-qmd="Adenosine triphosphate (ATP) high in erythrocytes"><div class='gt_from_md'><p>Adenosine triphosphate (ATP) high in erythrocytes</p>
</div></div></td></tr>
    <tr><td headers="name" class="gt_row gt_left"><div data-qmd="Decreased activity of mitochondrial ATP synthase complex"><div class='gt_from_md'><p>Decreased activity of mitochondrial ATP synthase complex</p>
</div></div></td>
<td headers="id" class="gt_row gt_left"><div data-qmd="HP:0011925"><div class='gt_from_md'><p>HP:0011925</p>
</div></div></td>
<td headers="childrenCount" class="gt_row gt_right"><div data-qmd="0"><div class='gt_from_md'><p>0</p>
</div></div></td>
<td headers="ontologyId" class="gt_row gt_left"><div data-qmd="HP:0011925"><div class='gt_from_md'><p>HP:0011925</p>
</div></div></td>
<td headers="synonym" class="gt_row gt_left">NA</td></tr>
    <tr><td headers="name" class="gt_row gt_left"><div data-qmd="Abnormal platelet dense granule ATP/ADP ratio"><div class='gt_from_md'><p>Abnormal platelet dense granule ATP/ADP ratio</p>
</div></div></td>
<td headers="id" class="gt_row gt_left"><div data-qmd="HP:0030401"><div class='gt_from_md'><p>HP:0030401</p>
</div></div></td>
<td headers="childrenCount" class="gt_row gt_right"><div data-qmd="0"><div class='gt_from_md'><p>0</p>
</div></div></td>
<td headers="ontologyId" class="gt_row gt_left"><div data-qmd="HP:0030401"><div class='gt_from_md'><p>HP:0030401</p>
</div></div></td>
<td headers="synonym" class="gt_row gt_left">NA</td></tr>
    <tr><td headers="name" class="gt_row gt_left"><div data-qmd="Abnormal platelet ATP dense granule secretion"><div class='gt_from_md'><p>Abnormal platelet ATP dense granule secretion</p>
</div></div></td>
<td headers="id" class="gt_row gt_left"><div data-qmd="HP:0030398"><div class='gt_from_md'><p>HP:0030398</p>
</div></div></td>
<td headers="childrenCount" class="gt_row gt_right"><div data-qmd="0"><div class='gt_from_md'><p>0</p>
</div></div></td>
<td headers="ontologyId" class="gt_row gt_left"><div data-qmd="HP:0030398"><div class='gt_from_md'><p>HP:0030398</p>
</div></div></td>
<td headers="synonym" class="gt_row gt_left">NA</td></tr>
  </tbody>
  
  
</table>
</div>

For the diseases category:

<details>
<summary>Code</summary>

``` r
atp_general_search[["hpo_general_diseases"]] |>
  gt::gt() |>
  gt::fmt_markdown(columns = everything()) |>
  gt::tab_options(table.width = gt::px(400))
```

</details>
<div id="nhxakgbqzs" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#nhxakgbqzs table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#nhxakgbqzs thead, #nhxakgbqzs tbody, #nhxakgbqzs tfoot, #nhxakgbqzs tr, #nhxakgbqzs td, #nhxakgbqzs th {
  border-style: none;
}

#nhxakgbqzs p {
  margin: 0;
  padding: 0;
}

#nhxakgbqzs .gt_table {
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
  width: 400px;
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

#nhxakgbqzs .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#nhxakgbqzs .gt_title {
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

#nhxakgbqzs .gt_subtitle {
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

#nhxakgbqzs .gt_heading {
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

#nhxakgbqzs .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nhxakgbqzs .gt_col_headings {
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

#nhxakgbqzs .gt_col_heading {
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

#nhxakgbqzs .gt_column_spanner_outer {
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

#nhxakgbqzs .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#nhxakgbqzs .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#nhxakgbqzs .gt_column_spanner {
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

#nhxakgbqzs .gt_spanner_row {
  border-bottom-style: hidden;
}

#nhxakgbqzs .gt_group_heading {
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

#nhxakgbqzs .gt_empty_group_heading {
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

#nhxakgbqzs .gt_from_md > :first-child {
  margin-top: 0;
}

#nhxakgbqzs .gt_from_md > :last-child {
  margin-bottom: 0;
}

#nhxakgbqzs .gt_row {
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

#nhxakgbqzs .gt_stub {
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

#nhxakgbqzs .gt_stub_row_group {
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

#nhxakgbqzs .gt_row_group_first td {
  border-top-width: 2px;
}

#nhxakgbqzs .gt_row_group_first th {
  border-top-width: 2px;
}

#nhxakgbqzs .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nhxakgbqzs .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#nhxakgbqzs .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#nhxakgbqzs .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nhxakgbqzs .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nhxakgbqzs .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#nhxakgbqzs .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#nhxakgbqzs .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#nhxakgbqzs .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nhxakgbqzs .gt_footnotes {
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

#nhxakgbqzs .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#nhxakgbqzs .gt_sourcenotes {
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

#nhxakgbqzs .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#nhxakgbqzs .gt_left {
  text-align: left;
}

#nhxakgbqzs .gt_center {
  text-align: center;
}

#nhxakgbqzs .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#nhxakgbqzs .gt_font_normal {
  font-weight: normal;
}

#nhxakgbqzs .gt_font_bold {
  font-weight: bold;
}

#nhxakgbqzs .gt_font_italic {
  font-style: italic;
}

#nhxakgbqzs .gt_super {
  font-size: 65%;
}

#nhxakgbqzs .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#nhxakgbqzs .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#nhxakgbqzs .gt_indent_1 {
  text-indent: 5px;
}

#nhxakgbqzs .gt_indent_2 {
  text-indent: 10px;
}

#nhxakgbqzs .gt_indent_3 {
  text-indent: 15px;
}

#nhxakgbqzs .gt_indent_4 {
  text-indent: 20px;
}

#nhxakgbqzs .gt_indent_5 {
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
    <tr><td headers="db" class="gt_row gt_left"><div data-qmd="ORPHA"><div class='gt_from_md'><p>ORPHA</p>
</div></div></td>
<td headers="dbName" class="gt_row gt_left"><div data-qmd="ATP13A2-related juvenile neuronal ceroid lipofuscinosis"><div class='gt_from_md'><p>ATP13A2-related juvenile neuronal ceroid lipofuscinosis</p>
</div></div></td>
<td headers="dbRef" class="gt_row gt_right"><div data-qmd="314632"><div class='gt_from_md'><p>314632</p>
</div></div></td>
<td headers="diseaseId" class="gt_row gt_left"><div data-qmd="ORPHA:314632"><div class='gt_from_md'><p>ORPHA:314632</p>
</div></div></td></tr>
    <tr><td headers="db" class="gt_row gt_left"><div data-qmd="ORPHA"><div class='gt_from_md'><p>ORPHA</p>
</div></div></td>
<td headers="dbName" class="gt_row gt_left"><div data-qmd="Isolated ATP synthase deficiency"><div class='gt_from_md'><p>Isolated ATP synthase deficiency</p>
</div></div></td>
<td headers="dbRef" class="gt_row gt_right"><div data-qmd="254913"><div class='gt_from_md'><p>254913</p>
</div></div></td>
<td headers="diseaseId" class="gt_row gt_left"><div data-qmd="ORPHA:254913"><div class='gt_from_md'><p>ORPHA:254913</p>
</div></div></td></tr>
    <tr><td headers="db" class="gt_row gt_left"><div data-qmd="OMIM"><div class='gt_from_md'><p>OMIM</p>
</div></div></td>
<td headers="dbName" class="gt_row gt_left"><div data-qmd="Mitochondrial complex V (ATP synthase) deficiency nuclear type 5"><div class='gt_from_md'><p>Mitochondrial complex V (ATP synthase) deficiency nuclear type 5</p>
</div></div></td>
<td headers="dbRef" class="gt_row gt_right"><div data-qmd="618120"><div class='gt_from_md'><p>618120</p>
</div></div></td>
<td headers="diseaseId" class="gt_row gt_left"><div data-qmd="OMIM:618120"><div class='gt_from_md'><p>OMIM:618120</p>
</div></div></td></tr>
    <tr><td headers="db" class="gt_row gt_left"><div data-qmd="OMIM"><div class='gt_from_md'><p>OMIM</p>
</div></div></td>
<td headers="dbName" class="gt_row gt_left"><div data-qmd="Mitochondrial complex V (ATP synthase) deficiency, nuclear type 1"><div class='gt_from_md'><p>Mitochondrial complex V (ATP synthase) deficiency, nuclear type 1</p>
</div></div></td>
<td headers="dbRef" class="gt_row gt_right"><div data-qmd="604273"><div class='gt_from_md'><p>604273</p>
</div></div></td>
<td headers="diseaseId" class="gt_row gt_left"><div data-qmd="OMIM:604273"><div class='gt_from_md'><p>OMIM:604273</p>
</div></div></td></tr>
    <tr><td headers="db" class="gt_row gt_left"><div data-qmd="OMIM"><div class='gt_from_md'><p>OMIM</p>
</div></div></td>
<td headers="dbName" class="gt_row gt_left"><div data-qmd="Mitochondrial complex V (ATP synthase) deficiency, nuclear type 2"><div class='gt_from_md'><p>Mitochondrial complex V (ATP synthase) deficiency, nuclear type 2</p>
</div></div></td>
<td headers="dbRef" class="gt_row gt_right"><div data-qmd="614052"><div class='gt_from_md'><p>614052</p>
</div></div></td>
<td headers="diseaseId" class="gt_row gt_left"><div data-qmd="OMIM:614052"><div class='gt_from_md'><p>OMIM:614052</p>
</div></div></td></tr>
    <tr><td headers="db" class="gt_row gt_left"><div data-qmd="OMIM"><div class='gt_from_md'><p>OMIM</p>
</div></div></td>
<td headers="dbName" class="gt_row gt_left"><div data-qmd="Mitochondrial complex V (atp synthase) deficiency, nuclear type 3"><div class='gt_from_md'><p>Mitochondrial complex V (atp synthase) deficiency, nuclear type 3</p>
</div></div></td>
<td headers="dbRef" class="gt_row gt_right"><div data-qmd="614053"><div class='gt_from_md'><p>614053</p>
</div></div></td>
<td headers="diseaseId" class="gt_row gt_left"><div data-qmd="OMIM:614053"><div class='gt_from_md'><p>OMIM:614053</p>
</div></div></td></tr>
    <tr><td headers="db" class="gt_row gt_left"><div data-qmd="OMIM"><div class='gt_from_md'><p>OMIM</p>
</div></div></td>
<td headers="dbName" class="gt_row gt_left"><div data-qmd="Mitochondrial complex V (atp synthase) deficiency, nuclear type 4"><div class='gt_from_md'><p>Mitochondrial complex V (atp synthase) deficiency, nuclear type 4</p>
</div></div></td>
<td headers="dbRef" class="gt_row gt_right"><div data-qmd="615228"><div class='gt_from_md'><p>615228</p>
</div></div></td>
<td headers="diseaseId" class="gt_row gt_left"><div data-qmd="OMIM:615228"><div class='gt_from_md'><p>OMIM:615228</p>
</div></div></td></tr>
    <tr><td headers="db" class="gt_row gt_left"><div data-qmd="OMIM"><div class='gt_from_md'><p>OMIM</p>
</div></div></td>
<td headers="dbName" class="gt_row gt_left"><div data-qmd="Mitochondrial complex V (ATP synthase) deficiency, nuclear type 4A"><div class='gt_from_md'><p>Mitochondrial complex V (ATP synthase) deficiency, nuclear type 4A</p>
</div></div></td>
<td headers="dbRef" class="gt_row gt_right"><div data-qmd="620358"><div class='gt_from_md'><p>620358</p>
</div></div></td>
<td headers="diseaseId" class="gt_row gt_left"><div data-qmd="OMIM:620358"><div class='gt_from_md'><p>OMIM:620358</p>
</div></div></td></tr>
    <tr><td headers="db" class="gt_row gt_left"><div data-qmd="OMIM"><div class='gt_from_md'><p>OMIM</p>
</div></div></td>
<td headers="dbName" class="gt_row gt_left"><div data-qmd="Mitochondrial complex V (ATP synthase) deficiency, nuclear type 6"><div class='gt_from_md'><p>Mitochondrial complex V (ATP synthase) deficiency, nuclear type 6</p>
</div></div></td>
<td headers="dbRef" class="gt_row gt_right"><div data-qmd="618683"><div class='gt_from_md'><p>618683</p>
</div></div></td>
<td headers="diseaseId" class="gt_row gt_left"><div data-qmd="OMIM:618683"><div class='gt_from_md'><p>OMIM:618683</p>
</div></div></td></tr>
    <tr><td headers="db" class="gt_row gt_left"><div data-qmd="OMIM"><div class='gt_from_md'><p>OMIM</p>
</div></div></td>
<td headers="dbName" class="gt_row gt_left"><div data-qmd="Mitochondrial complex V (ATP synthase) deficiency, nuclear type 7"><div class='gt_from_md'><p>Mitochondrial complex V (ATP synthase) deficiency, nuclear type 7</p>
</div></div></td>
<td headers="dbRef" class="gt_row gt_right"><div data-qmd="620359"><div class='gt_from_md'><p>620359</p>
</div></div></td>
<td headers="diseaseId" class="gt_row gt_left"><div data-qmd="OMIM:620359"><div class='gt_from_md'><p>OMIM:620359</p>
</div></div></td></tr>
    <tr><td headers="db" class="gt_row gt_left"><div data-qmd="ORPHA"><div class='gt_from_md'><p>ORPHA</p>
</div></div></td>
<td headers="dbName" class="gt_row gt_left"><div data-qmd="MT-ATP6-related mitochondrial spastic paraplegia"><div class='gt_from_md'><p>MT-ATP6-related mitochondrial spastic paraplegia</p>
</div></div></td>
<td headers="dbRef" class="gt_row gt_right"><div data-qmd="320360"><div class='gt_from_md'><p>320360</p>
</div></div></td>
<td headers="diseaseId" class="gt_row gt_left"><div data-qmd="ORPHA:320360"><div class='gt_from_md'><p>ORPHA:320360</p>
</div></div></td></tr>
    <tr><td headers="db" class="gt_row gt_left"><div data-qmd="OMIM"><div class='gt_from_md'><p>OMIM</p>
</div></div></td>
<td headers="dbName" class="gt_row gt_left"><div data-qmd="Sodium-Potassium-Atpase activity of red cell"><div class='gt_from_md'><p>Sodium-Potassium-Atpase activity of red cell</p>
</div></div></td>
<td headers="dbRef" class="gt_row gt_right"><div data-qmd="270425"><div class='gt_from_md'><p>270425</p>
</div></div></td>
<td headers="diseaseId" class="gt_row gt_left"><div data-qmd="OMIM:270425"><div class='gt_from_md'><p>OMIM:270425</p>
</div></div></td></tr>
  </tbody>
  
  
</table>
</div>

and for the genes:

<details>
<summary>Code</summary>

``` r
atp_general_search[["hpo_general_genes"]][1:5,] |>
  gt::gt() |>
  gt::fmt_markdown(columns = everything()) |>
  gt::tab_options(table.width = gt::px(400))
```

</details>
<div id="rxedcvtydc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rxedcvtydc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#rxedcvtydc thead, #rxedcvtydc tbody, #rxedcvtydc tfoot, #rxedcvtydc tr, #rxedcvtydc td, #rxedcvtydc th {
  border-style: none;
}

#rxedcvtydc p {
  margin: 0;
  padding: 0;
}

#rxedcvtydc .gt_table {
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
  width: 400px;
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

#rxedcvtydc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rxedcvtydc .gt_title {
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

#rxedcvtydc .gt_subtitle {
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

#rxedcvtydc .gt_heading {
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

#rxedcvtydc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rxedcvtydc .gt_col_headings {
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

#rxedcvtydc .gt_col_heading {
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

#rxedcvtydc .gt_column_spanner_outer {
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

#rxedcvtydc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rxedcvtydc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rxedcvtydc .gt_column_spanner {
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

#rxedcvtydc .gt_spanner_row {
  border-bottom-style: hidden;
}

#rxedcvtydc .gt_group_heading {
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

#rxedcvtydc .gt_empty_group_heading {
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

#rxedcvtydc .gt_from_md > :first-child {
  margin-top: 0;
}

#rxedcvtydc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rxedcvtydc .gt_row {
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

#rxedcvtydc .gt_stub {
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

#rxedcvtydc .gt_stub_row_group {
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

#rxedcvtydc .gt_row_group_first td {
  border-top-width: 2px;
}

#rxedcvtydc .gt_row_group_first th {
  border-top-width: 2px;
}

#rxedcvtydc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rxedcvtydc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rxedcvtydc .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rxedcvtydc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rxedcvtydc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rxedcvtydc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rxedcvtydc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#rxedcvtydc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rxedcvtydc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rxedcvtydc .gt_footnotes {
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

#rxedcvtydc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rxedcvtydc .gt_sourcenotes {
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

#rxedcvtydc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rxedcvtydc .gt_left {
  text-align: left;
}

#rxedcvtydc .gt_center {
  text-align: center;
}

#rxedcvtydc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rxedcvtydc .gt_font_normal {
  font-weight: normal;
}

#rxedcvtydc .gt_font_bold {
  font-weight: bold;
}

#rxedcvtydc .gt_font_italic {
  font-style: italic;
}

#rxedcvtydc .gt_super {
  font-size: 65%;
}

#rxedcvtydc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#rxedcvtydc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rxedcvtydc .gt_indent_1 {
  text-indent: 5px;
}

#rxedcvtydc .gt_indent_2 {
  text-indent: 10px;
}

#rxedcvtydc .gt_indent_3 {
  text-indent: 15px;
}

#rxedcvtydc .gt_indent_4 {
  text-indent: 20px;
}

#rxedcvtydc .gt_indent_5 {
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
    <tr><td headers="geneId" class="gt_row gt_right"><div data-qmd="57194"><div class='gt_from_md'><p>57194</p>
</div></div></td>
<td headers="geneSymbol" class="gt_row gt_left"><div data-qmd="ATP10A"><div class='gt_from_md'><p>ATP10A</p>
</div></div></td></tr>
    <tr><td headers="geneId" class="gt_row gt_right"><div data-qmd="23250"><div class='gt_from_md'><p>23250</p>
</div></div></td>
<td headers="geneSymbol" class="gt_row gt_left"><div data-qmd="ATP11A"><div class='gt_from_md'><p>ATP11A</p>
</div></div></td></tr>
    <tr><td headers="geneId" class="gt_row gt_right"><div data-qmd="286410"><div class='gt_from_md'><p>286410</p>
</div></div></td>
<td headers="geneSymbol" class="gt_row gt_left"><div data-qmd="ATP11C"><div class='gt_from_md'><p>ATP11C</p>
</div></div></td></tr>
    <tr><td headers="geneId" class="gt_row gt_right"><div data-qmd="23400"><div class='gt_from_md'><p>23400</p>
</div></div></td>
<td headers="geneSymbol" class="gt_row gt_left"><div data-qmd="ATP13A2"><div class='gt_from_md'><p>ATP13A2</p>
</div></div></td></tr>
    <tr><td headers="geneId" class="gt_row gt_right"><div data-qmd="79572"><div class='gt_from_md'><p>79572</p>
</div></div></td>
<td headers="geneSymbol" class="gt_row gt_left"><div data-qmd="ATP13A3"><div class='gt_from_md'><p>ATP13A3</p>
</div></div></td></tr>
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
