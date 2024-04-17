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
                   pubmedXrefs = x |>  pluck("pubmedXrefs") |> list()
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

### Testing general search

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
atp_general_search$hpo_general_terms |>
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
atp_general_search$hpo_general_diseases |>
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
atp_general_search$hpo_general_genes[1:5,] |>
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

### Testing gene search

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
      ..$ diseaseId  : chr [1:11] "ORPHA:778" "OMIM:300260" "ORPHA:3095" "OMIM:300496" ...
      ..$ diseaseName: chr [1:11] "Rett syndrome" "Mental retardation, x-linked syndromic, Lubs type" "Atypical Rett syndrome" "Autism susceptibility, X-linked 3" ...
      ..$ db         : chr [1:11] "ORPHA" "OMIM" "ORPHA" "OMIM" ...

The gene search function retrieves a object with two lists: one with the
terms and other with diseases. The earlier is a tibble showing the HPO
terms associated with that particular gene.

<details>
<summary>Code</summary>

``` r
MECP2$hpo_assoc[1:5,] |> 
    simplermarkdown::md_table()
```

</details>

    |ontologyID|name                          |definition                                                                                                                                                                                      |
    |----------|------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
    |HP:0011968|Feeding difficulties          |Impaired ability to eat related to problems gathering food and getting ready to suck, chew, or swallow it.                                                                                      |
    |HP:0003677|Slowly progressive            |Applies to a disease manifestation that only slowly increases in scope or severity over the course of time.                                                                                     |
    |HP:0000733|Abnormal repetitive mannerisms|Use of the same abnormal action in response to certain triggers or at random. They may be used as a way to regulate one's internal state but must otherwise have no apparent functional purpose.|
    |HP:0007281|Developmental stagnation      |A cessation of the development of a child in the areas of motor skills, speech and language, cognitive skills, and social and/or emotional skills.                                              |
    |HP:0025300|Malar rash                    |An erythematous (red), flat facial rash that affects the skin in the malar area (over the cheekbones) and extends over the bridge of the nose.                                                  |

Here, we can see the ontology ID, the name corresponding to the term and
its definition. For the latter, we can see a tibble showing disease ID,
disease name and the database related to the disease:

<details>
<summary>Code</summary>

``` r
MECP2$hpo_disease[1:5,] |> 
    simplermarkdown::md_table()
```

</details>

    |diseaseId  |diseaseName                                                       |db   |
    |-----------|------------------------------------------------------------------|-----|
    |ORPHA:778  |Rett syndrome                                                     |ORPHA|
    |OMIM:300260|Mental retardation, x-linked syndromic, Lubs type                 |OMIM |
    |ORPHA:3095 |Atypical Rett syndrome                                            |ORPHA|
    |OMIM:300496|Autism susceptibility, X-linked 3                                 |OMIM |
    |ORPHA:3077 |X-linked intellectual disability-psychosis-macroorchidism syndrome|ORPHA|

### Testing disease search

Next one, we can search for a particular disease term and get some
interesting results:

<details>
<summary>Code</summary>

``` r
#PFIC1: OMIM:211600
pfic1 = hpo_disease_search("OMIM:211600")

pfic1 |> 
  glimpse()
```

</details>

    List of 3
     $ disease_details: tibble [1 × 4] (S3: tbl_df/tbl/data.frame)
      ..$ diseaseId  : chr "OMIM:211600"
      ..$ diseaseName: chr "Cholestasis, progressive familial intrahepatic 1"
      ..$ dbId       : chr "211600"
      ..$ db         : chr "OMIM"
     $ disease_genes  : tibble [1 × 2] (S3: tbl_df/tbl/data.frame)
      ..$ geneSymbol: chr "ATP8B1"
      ..$ geneId    : int 5205
     $ disease_terms  : tibble [19 × 7] (S3: tbl_df/tbl/data.frame)
      ..$ catLabel  : chr [1:19] "Inheritance" "Blood and blood-forming tissues" "Metabolism/Laboratory abnormality" "Digestive System" ...
      ..$ ontologyId: chr [1:19] "HP:0000007" "HP:0000421" "HP:0002908" "HP:0002014" ...
      ..$ name      : chr [1:19] "Autosomal recessive inheritance" "Epistaxis" "Conjugated hyperbilirubinemia" "Diarrhea" ...
      ..$ definition: chr [1:19] "A mode of inheritance that is observed for traits related to a gene encoded on one of the autosomes (i.e., the "| __truncated__ "Epistaxis, or nosebleed, refers to a hemorrhage localized in the nose." "" "Abnormally increased frequency (usually defined as three or more) loose or watery bowel movements a day." ...
      ..$ frequency : chr [1:19] "" "17/31" "33/33" "" ...
      ..$ onset     : chr [1:19] "" "" "" "" ...
      ..$ sources   : chr [1:19] "PMID:9500542" "PMID:7912266" "PMID:7912266" "OMIM:211600" ...

Now, he have a new list with three tibbles: disease details, genes and
terms. The first one is as follows:

<details>
<summary>Code</summary>

``` r
pfic1$disease_details |> 
    simplermarkdown::md_table()
```

</details>

    |diseaseId  |diseaseName                                     |dbId  |db  |
    |-----------|------------------------------------------------|------|----|
    |OMIM:211600|Cholestasis, progressive familial intrahepatic 1|211600|OMIM|

Basically, is the same output as the disease tibble generated by the
*hpo_gene_search* function. The second tibble is the results for the
genes, which is also quite similar to the gene results from the
*hpo_general_search* function:

<details>
<summary>Code</summary>

``` r
pfic1$disease_genes |> 
    simplermarkdown::md_table()
```

</details>

    |geneSymbol|geneId|
    |----------|------|
    |ATP8B1    |5205  |

The third tibble with the terms associated with te searched diseases is
like this:

<details>
<summary>Code</summary>

``` r
pfic1$disease_terms[1:5,] |> 
    simplermarkdown::md_table()
```

</details>

    |catLabel                         |ontologyId|name                           |definition                                                                                                                                                                                                                                                                                                                                                   |frequency|onset|sources     |
    |---------------------------------|----------|-------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------|-----|------------|
    |Inheritance                      |HP:0000007|Autosomal recessive inheritance|A mode of inheritance that is observed for traits related to a gene encoded on one of the autosomes (i.e., the human chromosomes 1-22) in which a trait manifests in individuals with two pathogenic alleles, either homozygotes (two copies of the same mutant allele) or compound heterozygotes (whereby each copy of a gene has a distinct mutant allele).|         |     |PMID:9500542|
    |Blood and blood-forming tissues  |HP:0000421|Epistaxis                      |Epistaxis, or nosebleed, refers to a hemorrhage localized in the nose.                                                                                                                                                                                                                                                                                       |17/31    |     |PMID:7912266|
    |Metabolism/Laboratory abnormality|HP:0002908|Conjugated hyperbilirubinemia  |                                                                                                                                                                                                                                                                                                                                                             |33/33    |     |PMID:7912266|
    |Digestive System                 |HP:0002014|Diarrhea                       |Abnormally increased frequency (usually defined as three or more) loose or watery bowel movements a day.                                                                                                                                                                                                                                                     |         |     |OMIM:211600 |
    |Digestive System                 |HP:0002630|Fat malabsorption              |Abnormality of the absorption of fat from the gastrointestinal tract.                                                                                                                                                                                                                                                                                        |         |     |OMIM:211600 |

The columns are: a category label in which the term is part of; the
ontologyId; the name associated with the id; the definition for the
term; frequency, i.e., how many patients are known to present that
feature; onset; and some reference sources (PMID, OMIM, etc).

### Testing term search

We can also search for information about a particular term with the
function *hpo_term_search*:

<details>
<summary>Code</summary>

``` r
#Intrahepatic cholestasis with episodic jaundice: HP:0006575
intrahepatic_jaundice = hpo_term_search("HP:0006575")

intrahepatic_jaundice |> 
  glimpse()
```

</details>

    List of 4
     $ term_details             : tibble [1 × 9] (S3: tbl_df/tbl/data.frame)
      ..$ name       : chr "Intrahepatic cholestasis with episodic jaundice"
      ..$ id         : chr "HP:0006575"
      ..$ altTermIds :List of 1
      ..$ definition : chr ""
      ..$ comment    : chr ""
      ..$ synonyms   :List of 1
      ..$ isObsolete : logi FALSE
      ..$ xrefs      :List of 1
      ..$ pubmedXrefs:List of 1
     $ term_relations           : tibble [1 × 3] (S3: tbl_df/tbl/data.frame)
      ..$ parents  :List of 1
      ..$ children :List of 1
      ..$ termCount: int 0
     $ term_genes_association   : tibble [2 × 3] (S3: tbl_df/tbl/data.frame)
      ..$ geneSymbol: chr [1:2] "ASAH1" "ATP8B1"
      ..$ dbDiseases:List of 2
      ..$ geneId    : int [1:2] 427 5205
     $ term_disiases_association: tibble [4 × 5] (S3: tbl_df/tbl/data.frame)
      ..$ diseaseId  : chr [1:4] "OMIM:243300" "OMIM:211600" "ORPHA:333" "ORPHA:100085"
      ..$ dbGenes    :List of 4
      ..$ diseaseName: chr [1:4] "Cholestasis, benign recurrent intrahepatic 1" "Cholestasis, progressive familial intrahepatic 1" "Farber disease" "Primary hepatic neuroendocrine carcinoma"
      ..$ dbId       : chr [1:4] "243300" "211600" "333" "100085"
      ..$ db         : chr [1:4] "OMIM" "OMIM" "ORPHA" "ORPHA"

The output produces a list with four tibbles: term details; relations,
i.e., the parent and/or child terms associated; genes associated; and
diseases associated. Here are an example of these results for the term
details:

<details>
<summary>Code</summary>

``` r
intrahepatic_jaundice$term_details |> 
  simplermarkdown::md_table()
```

</details>

    |name                                           |id        |altTermIds|definition|comment|synonyms|isObsolete|xrefs        |pubmedXrefs|
    |-----------------------------------------------|----------|----------|----------|-------|--------|----------|-------------|-----------|
    |Intrahepatic cholestasis with episodic jaundice|HP:0006575|NULL      |          |       |NULL    |FALSE     |UMLS:C4025019|NULL       |

Next one is for parents and children terms:

<details>
<summary>Code</summary>

``` r
intrahepatic_jaundice$term_relations |> 
  simplermarkdown::md_table()
```

</details>

    |parents                                                                                               |children|termCount|
    |------------------------------------------------------------------------------------------------------|--------|---------|
    |Intrahepatic cholestasis, 3222                    , 1                       , HP:0001406              |NULL    |0        |

Here, we have the diseases associated to the term:

<details>
<summary>Code</summary>

``` r
intrahepatic_jaundice$term_genes_association |> 
  simplermarkdown::md_table()
```

</details>

    |geneSymbol|dbDiseases                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |geneId|
    |----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------|
    |ASAH1     |2265                                                           , OMIM:159950                                                    , Spinal muscular atrophy with progressive myoclonic epilepsy    , 159950                                                         , OMIM                                                           , 2823                                                           , OMIM:228000                                                    , Farber lipogranulomatosis                                      , 228000                                                         , OMIM                                                           , 3091                                                           , ORPHA:333                                                      , Farber disease                                                 , 333                                                            , ORPHA                                                          , 4159                                                           , ORPHA:2590                                                     , Spinal muscular atrophy-progressive myoclonic epilepsy syndrome, 2590                                                           , ORPHA                                                          | 427  |
    |ATP8B1    |1030                                            , OMIM:243300                                     , Cholestasis, benign recurrent intrahepatic 1    , 243300                                          , OMIM                                            , 1378                                            , OMIM:147480                                     , Cholestasis, intrahepatic, of pregnancy, 1      , 147480                                          , OMIM                                            , 1471                                            , ORPHA:69665                                     , Intrahepatic cholestasis of pregnancy           , 69665                                           , ORPHA                                           , 11091                                           , OMIM:211600                                     , Cholestasis, progressive familial intrahepatic 1, 211600                                          , OMIM                                                                                                                                                                                                                                                                                                                                                        |5205  |

Finally, a list of genes associated with the term:

<details>
<summary>Code</summary>

``` r
intrahepatic_jaundice$term_genes_association |> 
  simplermarkdown::md_table()
```

</details>

    |geneSymbol|dbDiseases                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |geneId|
    |----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------|
    |ASAH1     |2265                                                           , OMIM:159950                                                    , Spinal muscular atrophy with progressive myoclonic epilepsy    , 159950                                                         , OMIM                                                           , 2823                                                           , OMIM:228000                                                    , Farber lipogranulomatosis                                      , 228000                                                         , OMIM                                                           , 3091                                                           , ORPHA:333                                                      , Farber disease                                                 , 333                                                            , ORPHA                                                          , 4159                                                           , ORPHA:2590                                                     , Spinal muscular atrophy-progressive myoclonic epilepsy syndrome, 2590                                                           , ORPHA                                                          | 427  |
    |ATP8B1    |1030                                            , OMIM:243300                                     , Cholestasis, benign recurrent intrahepatic 1    , 243300                                          , OMIM                                            , 1378                                            , OMIM:147480                                     , Cholestasis, intrahepatic, of pregnancy, 1      , 147480                                          , OMIM                                            , 1471                                            , ORPHA:69665                                     , Intrahepatic cholestasis of pregnancy           , 69665                                           , ORPHA                                           , 11091                                           , OMIM:211600                                     , Cholestasis, progressive familial intrahepatic 1, 211600                                          , OMIM                                                                                                                                                                                                                                                                                                                                                        |5205  |

### Testing intersecting terms to find diseases in common

This is a very interesting and useful function in which we can provide a
vector with HPO terms and get diseases that share all these terms in
common. Here is an example:

<details>
<summary>Code</summary>

``` r
#Autistic behavior: HP:0000729
#X-linked dominant inheritance: HP:0001423
#Childhood onset: HP:0011463
term_ids = c("HP:0000729", "HP:0001423", "HP:0011463")
similar_to_rett = hpo_term_intersecting(term_ids, max = 20)

similar_to_rett |> 
  glimpse()
```

</details>

    Rows: 3
    Columns: 4
    $ diseaseId   <chr> "OMIM:300624", "OMIM:300387", "OMIM:312750"
    $ diseaseName <chr> "Fragile X mental retardation syndrome", "Mental retardati…
    $ dbId        <list> "300624", "300387", "312750"
    $ db          <chr> "OMIM", "OMIM", "OMIM"

The result of the function is a tibble with the diseases associated to
the terms. The columns are: disease id, disease name, db ID and db name.

<details>
<summary>Code</summary>

``` r
similar_to_rett |> 
  simplermarkdown::md_table()
```

</details>

    |diseaseId  |diseaseName                          |dbId  |db  |
    |-----------|-------------------------------------|------|----|
    |OMIM:300624|Fragile X mental retardation syndrome|300624|OMIM|
    |OMIM:300387|Mental retardation, X-linked 63      |300387|OMIM|
    |OMIM:312750|Rett syndrome                        |312750|OMIM|

### Testing medical terms

<details>
<summary>Code</summary>

``` r
#terms related to therapy
medical_therapy = hpo_medical_search("therapy")

medical_therapy |> 
  glimpse()
```

</details>

    Rows: 372
    Columns: 3
    $ term           <list> ["MAXO:0001499", "neuroprotective agent therapy", "A d…
    $ synonymMatched <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
    $ synonym        <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "",…

The resulting tibble shows all the medical ontology ids associated to
that term. The columns are: a list with each term, if there are
synonymous or not and the synonymous.

<details>
<summary>Code</summary>

``` r
medical_therapy[1:5,] |> 
  simplermarkdown::md_table()
```

</details>

    |term                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |synonymMatched|synonym|
    |------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------|-------|
    |MAXO:0001499                                                                                                                          , neuroprotective agent therapy                                                                                                         , A drug or pharmaceutical agent tht is used to attempt to prevent or reverse neuronal damage, thereby retaining physiological function., treatment with neuroprotective agent                                                                                                  , ECLS treatment                                                                                                                        , excretory system therapeutic procedure                                                                                                , consultation with sports medicine specialist                                                                                          , branched chain amino acid intake avoidance                                                                                            , avoid copulation                                                                                                                      , abdomen exam                                                                                                                          , hydrotherapy treatment                                                                                                                , BCAA-free diet recommendation                                                                                                         , lactose restriction                                                                                                                   , referral to a sports medicine specialist                                                                                              , examination of abdomen                                                                                                                , water therapy                                                                                                                         , extracorporeal life support                                                                                                           , sports medicine specialist consultation                                                                                               , physical examination of abdomen                                                                                                       , avoiding dietary branched-chain amino acid intake                                                                                     , treatment with dopamine uptake inhibitor                                                                                              , skin of body therapeutic procedure                                                                                                    , barley malt beer beverage restriction                                                                                                 , examination of the abdomen                                                                                                            , avoiding dietary barley malt beer beverage intake                                                                                     , placement of nasopharyngeal stent                                                                                                     , avoid branched-chain amino acid intake                                                                                                , low BCAA diet recommendation                                                                                                          , branched-chain amino acid consumption avoidance                                                                                       , hydropathy                                                                                                                            , barley malt beer beverage restricted diet intake                                                                                      , resection                                                                                                                             , lactose consumption avoidance                                                                                                         , water cure                                                                                                                            , avoid lactose intake                                                                                                                  , branched-chain amino acid restriction                                                                                                 , placement of nasal stent                                                                                                              , avoiding dietary lactose intake                                                                                                       , barley malt beer beverage consumption avoidance                                                                                       , lactose restricted diet intake                                                                                                        , low intake of purine-rich diet                                                                                                        , branched-chain amino acid restricted diet intake                                                                                      , low intake of purine-rich food                                                                                                        , toxicologic drug therapy                                                                                                              , avoid branched chain amino acids ingestion                                                                                            , removal of a joint                                                                                                                    , branched chain amino acid restriction                                                                                                 , ECMO treatment                                                                                                                        , purine restrictive diet intake                                                                                                        , low BCAA diet intake                                                                                                                  , skeletal joint surgical removal                                                                                                       , avoid barley malt beer beverage intake                                                                                                ,                                                                                                                                       |FALSE         |       |
    |MAXO:0001498                                                                                                  , prodrug therapy                                                                                               , A pharmaceutical substance that is biologically inactive, but it is metabolized by the body to produce a drug., abscission                                                                                                    , treatment with a glucocorticoid medication                                                                    , hydro-thermal therapy                                                                                         , appendix removal                                                                                              , surgical removal                                                                                              , avoid fructose intake                                                                                         , breast self-surveillance                                                                                      , avoiding dietary ascorbic acid intake                                                                         , ascorbic acid restricted diet intake                                                                          , crustacean restriction                                                                                        , treatment with prodrug                                                                                        , crustacean consumption avoidance                                                                              , treatment with a glucocorticoid drug                                                                          , avoid ascorbic acid intake                                                                                    , implantation of a single chamber pacemaker                                                                    , CPT                                                                                                           , breast self-exam                                                                                              , evaluation by pain medicine specialist                                                                        , hydrothermal therapy                                                                                          , excision                                                                                                      , carbohydrate restrictive diet intake                                                                          , appendicectomy                                                                                                , fructose restriction                                                                                          , treatment with http://purl.obolibrary.org/obo/CHEBI_131795                                                    , fructose restricted diet intake                                                                               , avoiding dietary fructose intake                                                                              , nutritionist evaluation                                                                                       , thermal water therapy                                                                                         , chest physical therapy                                                                                        , referral to a pain medicine specialist                                                                        , vermiform appendix surgical removal                                                                           , ascorbic acid restriction                                                                                     , low fructose diet intake                                                                                      , avoid crustacean intake                                                                                       , consultation with pain medicine specialist                                                                    , pain medicine specialist consultation                                                                         , bathing in thermal waters                                                                                     , fructose consumption avoidance                                                                                , Chest PT                                                                                                      , avoid stress related behavior                                                                                 , ascorbic acid consumption avoidance                                                                           , avoiding dietary crustacean intake                                                                            , assessment by pain medicine specialist                                                                        , treatment with a glucocorticoid therapy                                                                       , removal of appendix                                                                                           , musculoskeletal system therapeutic procedure                                                                  , low carbohydrate diet intake                                                                                  , circulatory system therapeutic procedure                                                                      , crustacean restricted diet intake                                                                             ,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |FALSE         |       |
    |MAXO:0001497                                                                                            , antidepressant agent therapy avoidance                                                                  , Avoid the administration or use of antidepressant agent therapy.                                        , desmopressine agent therapy                                                                             , treatment with NSAID medication                                                                         , avoiding dietary iron atom intake                                                                       , avoid purine intake                                                                                     , evaluation by palliative care specialist                                                                , avoiding dietary cephalopod intake                                                                      , 1-(3-mercaptopropionic acid)-8-D-arginine-vasopressin, 1-deamino-8-D-arginine vasopressin drug treatment, purine restricted diet intake                                                                           , referral to palliative care specialist                                                                  , iron atom restriction                                                                                   , examination of breast                                                                                   , avoid cephalopod intake                                                                                 , avoiding antidepressant agent therapy                                                                   , palliative care specialist referral                                                                     , purine restriction                                                                                      , treatment with NSAID therapy                                                                            , desmopresina agent therapy                                                                              , cardiovascular system therapeutic procedure                                                             , purine restricted diet intake                                                                           , cephalopod restricted diet intake                                                                       , avoid iron atom intake                                                                                  , avoiding dietary purine intake                                                                          , excessive iron consumption avoidance                                                                    , purine restriction                                                                                      , tooth root surgical removal                                                                             , assessment by palliative care specialist                                                                , nervous system therapeutic procedure                                                                    , consultation with palliative care specialist                                                            , 1-deamino-8-D-arginine vasopressin agent therapy                                                        , desmopressinum agent therapy                                                                            , treatment with NSAID drug                                                                               , hospice and palliative medicine specialist consultation                                                 , secondary palate surgical plastic reconstruction                                                        , cephalopod restriction                                                                                  , iron atom consumption avoidance                                                                         , avoid antidepressant agent therapy                                                                      , physical examination of breast                                                                          , avoiding dietary purine intake                                                                          , avoid purine intake                                                                                     , purine consumption avoidance                                                                            , treatment with non-steroidal anti-inflammatory medication                                               , desmopressin drug treatment                                                                             , cephalopod consumption avoidance                                                                        , non-steroidal anti-inflammatory medication treatment                                                    , clinical breast exam                                                                                    , dietary iron supplementation avoidance                                                                  , breast exam                                                                                             , DDAVP agent therapy                                                                                     , treatment with non-steroidal anti-inflammatory drug                                                     , 1-desamino-8-D-arginine vasopressin agent therapy                                                       , palliative care specialist consultation                                                                 , purine consumption avoidance                                                                            , iron atom restricted diet intake                                                                        , consultation with hospice and palliative medicine specialist                                            , referral to hospice and palliative medicine specialist                                                  , palliative care specialist evaluation                                                                   , dietary iron restriction                                                                                , implantation of a dual-chamber pacemaker                                                                , PSG                                                                                                     , breast examination                                                                                      ,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |FALSE         |       |
    |MAXO:0001496                                                                                 , serotonin-norepinephrine reuptake inhibitor agent therapy avoidance                          , Avoid the administration or use of serotonin-norepinephrine reuptake inhibitor agent therapy., standard migraine therapies                                                                  , anti migraine drug therapy                                                                   , adrenal gland surgical removal                                                               , fructose restriction                                                                         , fructose restricted diet intake                                                              , stenting                                                                                     , treatment of migraine                                                                        , avoiding dietary fructose intake                                                             , antifibrinolytic drug treatment                                                              , blood gas measurement testing                                                                , rapeseed meal restriction                                                                    , gustatory function testing                                                                   , treatment for migraine                                                                       , collection of cerebrospinal fluid specimen                                                   , avoid sterol intake                                                                          , low fructose diet intake                                                                     , migraine medication treatment                                                                , sterol restriction                                                                           , referral to geriatric medicine specialist                                                    , implantation of biventricular pacemaker                                                      , avoid rapeseed meal intake                                                                   , avoid serotonin-norepinephrine reuptake inhibitor agent therapy                              , migraine headache therapy                                                                    , rapeseed meal consumption avoidance                                                          , avoid fructose intake                                                                        , vegetarian diet intake                                                                       , rapeseed meal restricted diet intake                                                         , avoiding serotonin-norepinephrine reuptake inhibitor agent therapy                           , cerebrospinal fluid collection                                                               , sterol restricted diet intake                                                                , consultation with geriatric medicine specialist                                              , sterol consumption avoidance                                                                 , blood gas test                                                                               , taste function test                                                                          , avoiding dietary sterol intake                                                               , lip surgical plastic reconstruction                                                          , avoiding dietary rapeseed meal intake                                                        , evaluation by geriatric medicine specialist                                                  , fructose consumption avoidance                                                               , anti-migraine drug therapy                                                                   , assessment by geriatric medicine specialist                                                  , geriatric medicine specialist consultation                                                   , anion gap measurement testing                                                                , arterial blood gas measurement                                                               , anti-migraine medication therapy                                                             , treatment with antimigraine agent                                                            , cerebral spinal fluid collection                                                             , migraine treatment                                                                           ,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |FALSE         |       |
    |MAXO:0001493                                                        , tumor necrosis factor inhibitor therapy                             , Therapeutic administration of an inhibitor of tumor necrosis factor., cauliflower food product restriction                                , antimyasthenic medication therapy                                   , urine HCG pregancy test                                             , avoid arginine intake                                               , choline consumption avoidance                                       , avoiding dietary arginine intake                                    , avoiding dietary choline intake                                     , arginine restricted diet intake                                     , serum electrolyte measurement                                       , Treatment with a macrolide antibacterial drug                       , tummy tuck                                                          , avoiding dietary cauliflower food product intake                    , choline restriction                                                 , avoid choline intake                                                , macrolides                                                          , head surgical removal                                               , knee joint ligament repair                                          , choline restricted diet intake                                      , choline restriction                                                 , collection of nasal mucus specimen                                  , arginine consumption avoidance                                      , macrolide antibacterial drug therapy                                , avoid choline intake                                                , macrolide antibacterial therapy                                     , Treatment with a macrolide antibiotic medication                    , erythropoietin modulating treatment                                 , anti-myasthenic drug therapy                                        , urine human chorionic gonadotropin measurement                      , choline restricted diet intake                                      , Treatment with a macrolide antibiotic drug                          , Treatment with a macrolide antibacterial medication                 , urine HCG pregnancy measurement                                     , repair of ligament of knee joint                                    , decapitation                                                        , macrolide drug treatment                                            , cauliflower food product consumption avoidance                      , choline consumption avoidance                                       , electrolytes- serum testing                                         , avoiding dietary choline intake                                     , cauliflower food product restricted diet intake                     , macrolide drug therapy                                              , arginine restriction                                                , nasal mucus collection                                              , antimyasthenic drug therapy                                         , avoid cauliflower food product intake                               , Treatment with a macrolide antibacterial agent                      , Treatment with a macrolide antibiotic agent                         , respiratory system surgical procedure                               ,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |FALSE         |       |
