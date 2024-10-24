# HPO programmatic searches
Sindacato dei Bioinformatici
Last updated on: 23-10-2024

> **Note**
>
> On June 2024, HPO decided to close this API endpoint. Go to the
> RSelenium functions (topic 3) to find an alternative. :)

## 1. Building functions

### HPO: general search

query: is any character with identifier (HPO, OMIM, ORPHA), #an entrez
id, or text.

max: maximum number de results to retrieve. Default is -1, which is
everything

category: optional with a category to filter the results. #May be
“terms”, “genes” or “diseases”. Default is NULL, i.e., everything.

<details class="code-fold">
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

### HPO gene search

entrez_id: a single character variable with entrez gene id

<details class="code-fold">
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

### HPO disease search

disease_id: a single character with disease code. May be OMIM, ORPHA.

<details class="code-fold">
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

### HPO term search

term_id: a simple character with the hpo term.

<details class="code-fold">
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

### HPO diseases by intersecting terms

term_ids: a vector with terms.

max: a integer value with the maximum number of hits. Defaut is -1 for
everything.

<details class="code-fold">
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

### HPO Medical Action Ontology term search

medical_term_id: A character with a Medical Action Ontology identifer or
search term, such as “therapy”.

<details class="code-fold">
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

## ~~2. Testing the functions~~

### ~~Testing general search~~

<details class="code-fold">
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

The function will return a list object containing three tibbles: terms,
diseases and genes. Each one of these tibbles stores information about
the results among the categories. Let’s inspect the results for the
terms category:

<details class="code-fold">
<summary>Code</summary>

``` r
atp_general_search$hpo_general_terms |>
  simplermarkdown::md_table()
```

</details>

In this table we can find the name corresponding to the term, the term
id, number of children terms derived from it, the ontology id and
synonymous. For the diseases category, we have the type of database
associated with the disease (OMIM or ORPHA); the name of the diseases
according to the db; the db reference number for the diseases and the
database disease id.

<details class="code-fold">
<summary>Code</summary>

``` r
atp_general_search$hpo_general_diseases |>
  simplermarkdown::md_table()
```

</details>

For the genes, we have only two columns: the entrez ID; and gene HGNC
symbol.

<details class="code-fold">
<summary>Code</summary>

``` r
atp_general_search$hpo_general_genes[1:5,] |>
  simplermarkdown::md_table()
```

</details>

### ~~Testing gene search~~

Now, we can test the gene search:

<details class="code-fold">
<summary>Code</summary>

``` r
#MECP2 entrez id: 4204
MECP2 = hpo_gene_search("4204")

MECP2 |> 
  glimpse()
```

</details>

The gene search function retrieves a object with two lists: one with the
terms and other with diseases. The earlier is a tibble showing the HPO
terms associated with that particular gene.

<details class="code-fold">
<summary>Code</summary>

``` r
MECP2$hpo_assoc[1:5,] |> 
    simplermarkdown::md_table()
```

</details>

Here, we can see the ontology ID, the name corresponding to the term and
its definition. For the latter, we can see a tibble showing disease ID,
disease name and the database related to the disease:

<details class="code-fold">
<summary>Code</summary>

``` r
MECP2$hpo_disease[1:5,] |> 
    simplermarkdown::md_table()
```

</details>

### ~~Testing disease search~~

Next one, we can search for a particular disease term and get some
interesting results:

<details class="code-fold">
<summary>Code</summary>

``` r
#PFIC1: OMIM:211600
pfic1 = hpo_disease_search("OMIM:211600")

pfic1 |> 
  glimpse()
```

</details>

Now, he have a new list with three tibbles: disease details, genes and
terms. The first one is as follows:

<details class="code-fold">
<summary>Code</summary>

``` r
pfic1$disease_details |> 
    simplermarkdown::md_table()
```

</details>

Basically, is the same output as the disease tibble generated by the
*hpo_gene_search* function. The second tibble is the results for the
genes, which is also quite similar to the gene results from the
*hpo_general_search* function:

<details class="code-fold">
<summary>Code</summary>

``` r
pfic1$disease_genes |> 
    simplermarkdown::md_table()
```

</details>

The third tibble with the terms associated with te searched diseases is
like this:

<details class="code-fold">
<summary>Code</summary>

``` r
pfic1$disease_terms[1:5,] |> 
    simplermarkdown::md_table()
```

</details>

The columns are: a category label in which the term is part of; the
ontologyId; the name associated with the id; the definition for the
term; frequency, i.e., how many patients are known to present that
feature; onset; and some reference sources (PMID, OMIM, etc).

### ~~Testing term search~~

We can also search for information about a particular term with the
function *hpo_term_search*:

<details class="code-fold">
<summary>Code</summary>

``` r
#Intrahepatic cholestasis with episodic jaundice: HP:0006575
intrahepatic_jaundice = hpo_term_search("HP:0006575")

intrahepatic_jaundice |> 
  glimpse()
```

</details>

The output produces a list with four tibbles: term details; relations,
i.e., the parent and/or child terms associated; genes associated; and
diseases associated. Here are an example of these results for the term
details:

<details class="code-fold">
<summary>Code</summary>

``` r
intrahepatic_jaundice$term_details |> 
  simplermarkdown::md_table()
```

</details>

Next one is for parents and children terms:

<details class="code-fold">
<summary>Code</summary>

``` r
intrahepatic_jaundice$term_relations |> 
  simplermarkdown::md_table()
```

</details>

Here, we have the diseases associated to the term:

<details class="code-fold">
<summary>Code</summary>

``` r
intrahepatic_jaundice$term_genes_association |> 
  simplermarkdown::md_table()
```

</details>

Finally, a list of genes associated with the term:

<details class="code-fold">
<summary>Code</summary>

``` r
intrahepatic_jaundice$term_genes_association |> 
  simplermarkdown::md_table()
```

</details>

### ~~Testing intersecting terms to find diseases in common~~

This is a very interesting and useful function in which we can provide a
vector with HPO terms and get diseases that share all these terms in
common. Here is an example:

<details class="code-fold">
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

The result of the function is a tibble with the diseases associated to
the terms. The columns are: disease id, disease name, db ID and db name.

<details class="code-fold">
<summary>Code</summary>

``` r
similar_to_rett |> 
  simplermarkdown::md_table()
```

</details>

### ~~Testing medical terms~~

<details class="code-fold">
<summary>Code</summary>

``` r
#terms related to therapy
medical_therapy = hpo_medical_search("therapy")

medical_therapy |> 
  glimpse()
```

</details>

The resulting tibble shows all the medical ontology ids associated to
that term. The columns are: a list with each term, if there are
synonymous or not and the synonymous.

<details class="code-fold">
<summary>Code</summary>

``` r
medical_therapy[1:5,] |> 
  simplermarkdown::md_table()
```

</details>

## 3. RSelenium:

> **Note**
>
> We choose to use Firefox as engine. Therefore, you need to have this
> browser installed. Also, it is recommended to install
> [geckodriver](https://github.com/mozilla/geckodriver/releases) and
> attach it to the PATH.

## Basic Search

<details class="code-fold">
<summary>Code</summary>

``` r
hpo_search = function(keyword, type_association = c("hpo", "disease", "gene")) {
  
  require(RSelenium, quietly = TRUE, warn.conflicts = FALSE)
  require(netstat, quietly = TRUE, warn.conflicts = FALSE) 
  require(tidyverse, quietly = TRUE, warn.conflicts = FALSE)  
  require(rvest, quietly = TRUE, warn.conflicts = FALSE) 
  
  #iniciar o servidor no firefox
  rs_driver_object = rsDriver(browser = "firefox", 
                              port = free_port(),
                              verbose = FALSE, 
                              chromever = NULL, 
                              extraCapabilities = list(
                                "moz:firefoxOptions" = list(
                                  args = list('--headless')))
  )
  
  #criar um cliente
  remDr = rs_driver_object$client
  
  #change space to "%20"
  keyword = gsub(" ", "%20", keyword)
  
  #entrar no site
  remDr$navigate(paste0("https://hpo.jax.org/browse/search?q=", 
                        keyword, 
                        "&navFilter=all"))
  
  Sys.sleep(3)
  
  #atraves no navegador, utilizar a funcao de inspecionar elemento e copiar a xpath
  
  
  if(type_association == "hpo"){
    
    type_association = 1
    
    botao = remDr$findElement(using = "xpath", 
                              value = paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-search-results/div/div/div/div/div/mat-tab-group/mat-tab-header/div/div/div/div[", type_association, "]/div"))
    
    botao$getElementAttribute("class")
    botao$clickElement()
    
    
    #create null object
    hpo_associations = data.frame(identifier = character(),
                                  name = character(),
                                  matching = character(), 
                                  synonym = character())    
    
    #initiate while loop
    cond = TRUE
    
    while (cond == TRUE) {
      
      page_source = remDr$getPageSource()[[1]]
      webpage = read_html(page_source)
      
      identifier =  webpage %>%
        html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-search-results/div/div/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/mat-table/mat-row/mat-cell[1]")) %>%
        html_text2()
      
      name =  webpage %>%
        html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-search-results/div/div/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/mat-table/mat-row/mat-cell[2]")) %>%
        html_text2()
      
      matching =  webpage %>%
        html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-search-results/div/div/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/mat-table/mat-row/mat-cell[3]")) %>%
        html_text2()
      
      synonym =  webpage %>%
        html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-search-results/div/div/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/mat-table/mat-row/mat-cell[4]")) %>%
        html_text2()
    
      df = data.frame(identifier,
                      name,
                      matching,
                      synonym)
      
      if(df %>% anti_join(., hpo_associations) %>% nrow() > 0){
        hpo_associations = bind_rows(hpo_associations, df)
        
        next_button = remDr$findElement(using = 'css selector', 
                                        value = 'button.mat-tooltip-trigger:nth-child(3) > span:nth-child(1) > svg:nth-child(1)')
        next_button$clickElement()
        
        } else {
          cond = FALSE}
    
    }
     
    return(hpo_associations)
    
  } else if(type_association == "disease") {
    
    type_association = 2
    
    botao = remDr$findElement(using = "xpath", 
                              value = paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-search-results/div/div/div/div/div/mat-tab-group/mat-tab-header/div/div/div/div[", type_association, "]/div"))
    
    botao$getElementAttribute("class")
    botao$clickElement()
    
    page_source = remDr$getPageSource()[[1]]
    webpage = read_html(page_source)
    
    #create null object
    hpo_associations = data.frame(identifier = character(),
                                  name = character(),
                                  matching = character())
    
    #initiate while loop
    cond = TRUE
    
    while (cond == TRUE) {
    
      identifier =  webpage %>%
        html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-search-results/div/div/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/mat-table/mat-row/mat-cell[1]")) %>%
        html_text2()
      
      name =  webpage %>%
        html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-search-results/div/div/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/mat-table/mat-row/mat-cell[2]")) %>%
        html_text2()
      
      matching =  webpage %>%
        html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-search-results/div/div/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/mat-table/mat-row/mat-cell[3]")) %>%
        html_text2()
      
      df = data.frame(identifier,
                      name,
                      matching)
      
      
      if(df %>% anti_join(., hpo_associations) %>% nrow() > 0){
        hpo_associations = bind_rows(hpo_associations, df)
        
        next_button = remDr$findElement(using = 'css selector', 
                                        value = 'button.mat-tooltip-trigger:nth-child(3) > span:nth-child(1) > svg:nth-child(1)')
        next_button$clickElement()
        
      } else {
        cond = FALSE}
      
    }
    
    return(hpo_associations)
    
  } else if(type_association == "gene"){
      
      type_association = 3
      
      botao = remDr$findElement(using = "xpath", 
                                value = paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-search-results/div/div/div/div/div/mat-tab-group/mat-tab-header/div/div/div/div[", type_association, "]/div"))
      
      botao$getElementAttribute("class")
      botao$clickElement()
      
      page_source = remDr$getPageSource()[[1]]
      webpage = read_html(page_source)
      
      #create null object
      hpo_associations = data.frame(identifier = character(),
                                    name = character(),
                                    matching = character())
      
      #initiate while loop
      cond = TRUE
      
      while (cond == TRUE) {
        
        identifier =  webpage %>%
          html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-search-results/div/div/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/mat-table/mat-row/mat-cell[1]")) %>%
          html_text2()
        
        name =  webpage %>%
          html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-search-results/div/div/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/mat-table/mat-row/mat-cell[2]")) %>%
          html_text2()
        
        matching =  webpage %>%
          html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-search-results/div/div/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/mat-table/mat-row/mat-cell[3]")) %>%
          html_text2()
        
        df = data.frame(identifier,
                        name,
                        matching)
        
        
        if(df %>% anti_join(., hpo_associations) %>% nrow() > 0){
          hpo_associations = bind_rows(hpo_associations, df)
          
          next_button = remDr$findElement(using = 'css selector', 
                                          value = 'button.mat-tooltip-trigger:nth-child(3) > span:nth-child(1) > svg:nth-child(1)')
          next_button$clickElement()
          
        } else {
          cond = FALSE}
        
      }
      
      return(hpo_associations)
      
    } else {
      message("invalid type of association")
    }
  
  #desconect from server
  system("taskkill /im java.exe /f")
}
```

</details>

### Gene search

<details class="code-fold">
<summary>Code</summary>

``` r
hpo_gene = function(Gene.refGene, type_association = c("hpo", "disease")) {

  require(RSelenium, quietly = TRUE, warn.conflicts = FALSE)
  require(netstat, quietly = TRUE, warn.conflicts = FALSE) 
  require(tidyverse, quietly = TRUE, warn.conflicts = FALSE)  
  require(rvest, quietly = TRUE, warn.conflicts = FALSE) 
      
  #iniciar o servidor no firefox
  rs_driver_object = rsDriver(browser = "firefox", 
                              port = free_port(),
                              verbose = FALSE, 
                              chromever = NULL, 
                              extraCapabilities = list(
                                "moz:firefoxOptions" = list(
                                  args = list('--headless')))
                              )
  
  #criar um cliente
  remDr = rs_driver_object$client
  
  #get entrez id from gene name
  
  ENTREZID = AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db,keys = Gene.refGene ,
                                   columns = "ENTREZID", 
                                   keytype = "SYMBOL")
  
  ENTREZID = ENTREZID$ENTREZID
  
  #entrar no site
  remDr$navigate(paste0("https://hpo.jax.org/browse/gene/NCBIGene:", 
                        ENTREZID))
  
  Sys.sleep(5)
  
  #atraves no navegador, utilizar a funcao de inspecionar elemento e copiar a xpath
  
  
  if(type_association == "hpo"){
    type_association = 1
    page_source = remDr$getPageSource()[[1]]
    webpage = read_html(page_source)
    
    identifier =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-gene/div/div/div[2]/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div[2]/mat-table/mat-row/mat-cell[1]")) %>%
      html_text2()
    
    name =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-gene/div/div/div[2]/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div[2]/mat-table/mat-row/mat-cell[2]")) %>%
      html_text2()
    
  } else if(type_association == "disease") {
    type_association = 2
    
    botao = remDr$findElement(using = "xpath", 
                              value = paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-gene/div/div/div[2]/div[2]/div/mat-tab-group/mat-tab-header/div/div/div/div[", type_association, "]/div"))
    
    botao$getElementAttribute("class")
    botao$clickElement()
    
    page_source = remDr$getPageSource()[[1]]
    webpage = read_html(page_source)
    
    identifier =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-gene/div/div/div[2]/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div[2]/mat-table/mat-row/mat-cell[1]")) %>%
      html_text2()
    
    name =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-gene/div/div/div[2]/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div[2]/mat-table/mat-row/mat-cell[2]")) %>%
      html_text2()
  } else {
    message("invalid type of association")
  }
  
  hpo_associations = data.frame(identifier,
                                name)
  
  #desconect from server
  system("taskkill /im java.exe /f")
  
  if(nrow(hpo_associations) == 0){
    print(paste("The gene", Gene.refGene, "is not in the database"))
  } else return(hpo_associations) 
  

}
```

</details>

### Term search

<details class="code-fold">
<summary>Code</summary>

``` r
hpo_term = function(term, type_association = c("disease", "gene", 
                                               "medical", "loinc")) {
  
  require(RSelenium, quietly = TRUE, warn.conflicts = FALSE)
  require(netstat, quietly = TRUE, warn.conflicts = FALSE) 
  require(tidyverse, quietly = TRUE, warn.conflicts = FALSE)  
  require(rvest, quietly = TRUE, warn.conflicts = FALSE) 
  
  #iniciar o servidor no firefox
  rs_driver_object = rsDriver(browser = "firefox", 
                              port = free_port(),
                              verbose = FALSE, 
                              chromever = NULL
  )
  
  #criar um cliente
  remDr = rs_driver_object$client
  
  #entrar no site
  remDr$navigate(paste0("https://hpo.jax.org/browse/term/", 
                        term))
  
  Sys.sleep(3)
  
  #atraves no navegador, utilizar a funcao de inspecionar elemento e copiar a xpath
  
  
  if(type_association == "disease"){
    type_association = 1
    page_source = remDr$getPageSource()[[1]]
    webpage = read_html(page_source)
    
    identifier =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-term/div/div/div/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div[2]/mat-table/mat-row/mat-cell[1]")) %>%
      html_text2()
    
    name =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-term/div/div/div/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div[2]/mat-table/mat-row/mat-cell[2]")) %>%
      html_text2()
    
    hpo_associations = data.frame(identifier, 
                                  name)
    
  } else if(type_association == "gene") {
    type_association = 2
    
    botao = remDr$findElement(using = "xpath", 
                              value = paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-term/div/div/div/div[2]/div/mat-tab-group/mat-tab-header/div/div/div/div[", type_association, "]/div"))
    
    botao$getElementAttribute("class")
    botao$clickElement()
    
    page_source = remDr$getPageSource()[[1]]
    webpage = read_html(page_source)
    
    identifier =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-term/div/div/div/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div[2]/mat-table/mat-row/mat-cell[1]")) %>%
      html_text2()
    
    name =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-term/div/div/div/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div[2]/mat-table/mat-row/mat-cell[2]")) %>%
      html_text2()
    
    hpo_associations = data.frame(identifier, 
                                  name)
    
  } else if(type_association == "medical") {
    type_association = 3
    
    botao = remDr$findElement(using = "xpath", 
                              value = paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-term/div/div/div/div[2]/div/mat-tab-group/mat-tab-header/div/div/div/div[", type_association, "]/div"))
    
    botao$getElementAttribute("class")
    botao$clickElement()
    
    page_source = remDr$getPageSource()[[1]]
    webpage = read_html(page_source)
    
    identifier =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-term/div/div/div/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div/mat-table/mat-row/mat-cell[1]")) %>%
      html_text2()
    
    name =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-term/div/div/div/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div/mat-table/mat-row/mat-cell[2]")) %>%
      html_text2()
    
    relation =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-term/div/div/div/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div/mat-table/mat-row/mat-cell[3]")) %>%
      html_text2()
    
    rows = webpage %>% 
      html_nodes(xpath = paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-term/div/div/div/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div/mat-table/mat-row"))
    sources = map(rows, ~ {
      .x %>%
        html_nodes(xpath = "./mat-cell[4]/div/div/a") %>%
        html_attr("href") %>%
        str_c(collapse = " ")}) %>%
      unlist()
  
    
    hpo_associations = data.frame(identifier, 
                                  name,
                                  relation,
                                  sources)
    
  } else if(type_association == "loinc") {
    type_association = 4
    
    botao = remDr$findElement(using = "xpath", 
                              value = paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-term/div/div/div/div[2]/div/mat-tab-group/mat-tab-header/div/div/div/div[", type_association, "]/div"))
    
    botao$getElementAttribute("class")
    botao$clickElement()
    
    page_source = remDr$getPageSource()[[1]]
    webpage = read_html(page_source)
    
    identifier =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-term/div/div/div/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div[2]/mat-table/mat-row/mat-cell[1]")) %>%
      html_text2()
    
    name =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-term/div/div/div/div[2]/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div[2]/mat-table/mat-row/mat-cell[2]")) %>%
      html_text2()
    
    hpo_associations = data.frame(identifier, 
                                  name)
    
  } else {
    message("invalid type of association")
  }
  
  
  return(hpo_associations)
  
  #desconect from server
  system("taskkill /im java.exe /f")
  
  if(nrow(hpo_associations) == 0){
    print(paste("The term", term, "is not in the database"))
  } else return(hpo_associations) 
  
  
}
```

</details>

### Disease search

<details class="code-fold">
<summary>Code</summary>

``` r
hpo_disease = function(disease, type_association = c("term", "gene", "medical")) {
  
  require(RSelenium, quietly = TRUE, warn.conflicts = FALSE)
  require(netstat, quietly = TRUE, warn.conflicts = FALSE) 
  require(tidyverse, quietly = TRUE, warn.conflicts = FALSE)  
  require(rvest, quietly = TRUE, warn.conflicts = FALSE) 
  
  #iniciar o servidor no firefox
  rs_driver_object = rsDriver(browser = "firefox", 
                              port = free_port(),
                              verbose = FALSE, 
                              chromever = NULL, 
                              extraCapabilities = list(
                                "moz:firefoxOptions" = list(
                                  args = list('--headless')))
  )
  
  #criar um cliente
  remDr = rs_driver_object$client
  
  #entrar no site
  remDr$navigate(paste0("https://hpo.jax.org/browse/disease/", 
                        disease))
  
  Sys.sleep(3)
  
  #atraves no navegador, utilizar a funcao de inspecionar elemento e copiar a xpath
  
  
  if(type_association == "term"){
    type_association = 1
    page_source = remDr$getPageSource()[[1]]
    webpage = read_html(page_source)
    
    number_of_terms = webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/div")) %>%
      html_text2() %>%
      str_count("annotation")
    
    identifier = NULL 
    name = NULL
    onset = NULL 
    frequency = NULL
    source = NULL
    
    for(i in 1:number_of_terms){
      
      x =  webpage %>%
        html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div/div/div[", i, "]/mat-table/mat-row/mat-cell[1]")) %>%
        html_text2() %>%
        str_replace_all("[\r\n]" , "")
      
      identifier = c(identifier, x)
      
      x =  webpage %>%
        html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div/div/div[", i, "]/mat-table/mat-row/mat-cell[2]")) %>%  
        html_text2() %>%
        str_replace_all("[\r\n]" , "")
      
      name = c(name, x)
      
      x =  webpage %>%
        html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div/div/div[", i, "]/mat-table/mat-row/mat-cell[3]")) %>%
        html_text2() %>%
        str_replace_all("[\r\n]" , "")
      
      onset = c(onset, x)
      
      x =  webpage %>%
        html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div/div/div[", i, "]/mat-table/mat-row/mat-cell[4]")) %>%
        html_text2() %>%
        str_replace_all("[\r\n]" , "")
      
      frequency = c(frequency, x)
      
      x =  webpage %>%
        html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div/div/div[",i,"]/mat-table/mat-row/mat-cell[5]/div/a")) %>%
        html_attr("href")
      
      source = c(source, x)
      
    }
    
    hpo_associations = data.frame(identifier, 
                                  name,
                                  onset, 
                                  frequency,
                                  source)
    
  } else if(type_association == "gene") {
    type_association = 2
    
    botao = remDr$findElement(using = "xpath", 
                              value = paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/mat-tab-header/div/div/div/div[", type_association, "]/div"))
    
    botao$getElementAttribute("class")
    botao$clickElement()
    
    page_source = remDr$getPageSource()[[1]]
    webpage = read_html(page_source)
    
    identifier =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div[2]/mat-table/mat-row/mat-cell[1]")) %>%
      html_text2() %>%
      str_replace_all("[\r\n]" , "")
    
    name =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div[2]/mat-table/mat-row/mat-cell[2]")) %>%
      html_text2() %>%
      str_replace_all("[\r\n]" , "")
    
    hpo_associations = data.frame(identifier, 
                                  name)
    
  } else if(type_association == "medical") {
    
    type_association = 3
    
    botao = remDr$findElement(using = "xpath", 
                              value = paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/mat-tab-header/div/div/div/div[", type_association, "]/div"))
    
    botao$getElementAttribute("class")
    botao$clickElement()
    
    page_source = remDr$getPageSource()[[1]]
    webpage = read_html(page_source)
    
    identifier =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div/mat-table/mat-row/mat-cell[1]")) %>%
      html_text2()
    
    name =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div/mat-table/mat-row/mat-cell[2]")) %>%
      html_text2()
    
    relation =  webpage %>%
      html_nodes(xpath=paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div/mat-table/mat-row/mat-cell[3]")) %>%
      html_text2()
    
    rows = webpage %>% 
      html_nodes(xpath = paste0("/html/body/app-root/mat-sidenav-container/mat-sidenav-content/div/app-disease/div/div/div[2]/div/div/div/mat-tab-group/div/mat-tab-body[", type_association, "]/div/div[1]/div/div/mat-table/mat-row"))
    target = map(rows, ~ {
      .x %>%
        html_nodes(xpath = "./mat-cell[4]/div/div/a") %>%
        html_attr("href") %>%
        str_c(collapse = " ")}) %>%
      unlist()
    
    target = gsub("/browse/term/", "", target)
    
    
    hpo_associations = data.frame(identifier, 
                                  name,
                                  relation,
                                  target)
    
  }  else {
    message("invalid type of association")
  }
  
  return(hpo_associations)
  
  #desconect from server
  system("taskkill /im java.exe /f")
  
  
}
```

</details>
