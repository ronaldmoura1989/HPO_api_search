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

atp_general_search |> 
  glimpse()
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
  simplermarkdown::md_table()
```

</details>

    |name                                                     |id        |childrenCount|ontologyId|synonym                                          |
    |---------------------------------------------------------|----------|-------------|----------|-------------------------------------------------|
    |Elevated erythrocyte adenosine triphosphate concentration|HP:4000186|0            |HP:4000186|Adenosine triphosphate (ATP) high in erythrocytes|
    |Decreased activity of mitochondrial ATP synthase complex |HP:0011925|0            |HP:0011925|NA                                               |
    |Abnormal platelet dense granule ATP/ADP ratio            |HP:0030401|0            |HP:0030401|NA                                               |
    |Abnormal platelet ATP dense granule secretion            |HP:0030398|0            |HP:0030398|NA                                               |

In this table we can find the name corresponding to the term, the term
id, number of children terms derived from it, the ontology id and
synonymous. For the diseases category, we have the type of database
associated with the disease (OMIM or ORPHA); the name of the diseases
according to the db; the db reference number for the diseases and the
database disease id.

<details>
<summary>Code</summary>

``` r
atp_general_search[["hpo_general_diseases"]] |>
  simplermarkdown::md_table()
```

</details>

    |db   |dbName                                                            |dbRef |diseaseId   |
    |-----|------------------------------------------------------------------|------|------------|
    |ORPHA|ATP13A2-related juvenile neuronal ceroid lipofuscinosis           |314632|ORPHA:314632|
    |ORPHA|Isolated ATP synthase deficiency                                  |254913|ORPHA:254913|
    |OMIM |Mitochondrial complex V (ATP synthase) deficiency nuclear type 5  |618120|OMIM:618120 |
    |OMIM |Mitochondrial complex V (ATP synthase) deficiency, nuclear type 1 |604273|OMIM:604273 |
    |OMIM |Mitochondrial complex V (ATP synthase) deficiency, nuclear type 2 |614052|OMIM:614052 |
    |OMIM |Mitochondrial complex V (atp synthase) deficiency, nuclear type 3 |614053|OMIM:614053 |
    |OMIM |Mitochondrial complex V (atp synthase) deficiency, nuclear type 4 |615228|OMIM:615228 |
    |OMIM |Mitochondrial complex V (ATP synthase) deficiency, nuclear type 4A|620358|OMIM:620358 |
    |OMIM |Mitochondrial complex V (ATP synthase) deficiency, nuclear type 6 |618683|OMIM:618683 |
    |OMIM |Mitochondrial complex V (ATP synthase) deficiency, nuclear type 7 |620359|OMIM:620359 |
    |ORPHA|MT-ATP6-related mitochondrial spastic paraplegia                  |320360|ORPHA:320360|
    |OMIM |Sodium-Potassium-Atpase activity of red cell                      |270425|OMIM:270425 |

For the genes, we have only two columns: the entrez ID; and gene HGNC
symbol.

<details>
<summary>Code</summary>

``` r
atp_general_search[["hpo_general_genes"]][1:5,] |>
  simplermarkdown::md_table()
```

</details>

    |geneId|geneSymbol|
    |------|----------|
    | 57194|ATP10A    |
    | 23250|ATP11A    |
    |286410|ATP11C    |
    | 23400|ATP13A2   |
    | 79572|ATP13A3   |

Now, we can test the gene search:

<details>
<summary>Code</summary>

``` r
#MECP2 entrez id: 4204
MECP2 = hpo_gene_search("4204")

MECP2 |> 
  glimpse()
```

</details>

    List of 2
     $ hpo_assoc  : tibble [251 × 3] (S3: tbl_df/tbl/data.frame)
      ..$ ontologyID: chr [1:251] "HP:0011968" "HP:0003677" "HP:0000733" "HP:0007281" ...
      ..$ name      : chr [1:251] "Feeding difficulties" "Slowly progressive" "Abnormal repetitive mannerisms" "Developmental stagnation" ...
      ..$ definition: chr [1:251] "Impaired ability to eat related to problems gathering food and getting ready to suck, chew, or swallow it." "Applies to a disease manifestation that only slowly increases in scope or severity over the course of time." "Use of the same abnormal action in response to certain triggers or at random. They may be used as a way to regu"| __truncated__ "A cessation of the development of a child in the areas of motor skills, speech and language, cognitive skills, "| __truncated__ ...
     $ hpo_disease: tibble [11 × 3] (S3: tbl_df/tbl/data.frame)
      ..$ diseaseId  : chr [1:11] "OMIM:300260" "ORPHA:778" "OMIM:300055" "ORPHA:3095" ...
      ..$ diseaseName: chr [1:11] "Mental retardation, x-linked syndromic, Lubs type" "Rett syndrome" "Mental retardation, X-linked, syndromic 13" "Atypical Rett syndrome" ...
      ..$ db         : chr [1:11] "OMIM" "ORPHA" "OMIM" "ORPHA" ...

<details>
<summary>Code</summary>

``` r
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
