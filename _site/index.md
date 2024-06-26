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

disease_id: a single character with disease code. May be OMIM, ORHPA.

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
query = "wilson disease"
max = -1
category = NULL

wilson_disease_search = hpo_general_search(query, max, category)
```

</details>

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

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
