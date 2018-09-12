# More Effective Than We Thought
This repository contains the replication material of the article "More Effective Than We Thought: Accounting for Legislative Hitchhikers Reveals a More Inclusive and Productive Lawmaking Process", in the _American Journal of Political Science_, by [Andreu Casas](http://andreucasas.com/), [Matthew Denny](http://www.mjdenny.com/), and [John Wilkerson](https://faculty.washington.edu/jwilker/Bio.pdf).


## Data
The `./data/` directory contains the necessary data to replicate the analytical figures and tables of the paper. We describe in here each of the datasets:

  - `main_db.csv`: this is the main dataset with bill-level information. Here a description of all the variables in the dataset:

| Name                | Description                                                                                                                                                                                                                                                                              |
|---------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `BillID`            | (string) a bill identifier composed of the Congress number, the type of bill, and the bill number (e.g. 103-HR-1).                                                                                                                                                                       |
| `BillType`          | (categorical) the type of bill: house bill (`hr`), senate bill (`s`), house joint resolution (`hjres`), senate joint resoluation (`sjres`).                                                                                                                                              |
| `BillNum`           | (numeric) the bill number.                                                                                                                                                                                                                                                               |
| `Title`             | (string) the bill title.                                                                                                                                                                                                                                                                 |
| `ImpBill`           | (binary) whether the bills is an important (`1`) or minor (`0`) bill.                                                                                                                                                                                                                    |
| `outcome1`          | (categorical) whether the bill was enacted as stand alone law (`law`), hitchhiker (`insertion`), or was not enacted (`no law`).                                                                                                                                                          |
| `LawID`             | (string) for hitchhiker bills, this variable provides the `BillID` of the bill into which they were inserted.                                                                                                                                                                            |
| `Major`             | (categorical) indicates the topic of the bill, according to the Congressional Bills Project Major topic codes, which follow the [Comparative Agendas Policy codebook](https://www.comparativeagendas.net/pages/master-codebook) (CAP).                                                   |
| `Minor`             | (categorical) indicates the mintor topic code (following the CAP codebook as well).                                                                                                                                                                                                      |
| `Chamber`           | (binary) the chamber in which the bill was introduced (`1` = Senate).                                                                                                                                                                                                                    |
| `RefArr`            | (string) provides the codes of the committees to which the bills have been referred.                                                                                                                                                                                                     |
| `Cong`              | (categorical) the Congress in which the bill was introdued.                                                                                                                                                                                                                              |
| `NameFull`          | (string) the name of the member who sponsored the bill.                                                                                                                                                                                                                                  |
| `Majority`          | (binary) whether the sponsor was a member of the majority party (=`1`).                                                                                                                                                                                                                  |
| `PooleID`           | (numeric) member-level numeric identifier.                                                                                                                                                                                                                                               |
| `Gender`            | (binary) whether the sponsor is female (=`1`).                                                                                                                                                                                                                                           |
| `AA`                | (binary) whether the sponsor is African American.                                                                                                                                                                                                                                        |
| `Hisp`              | (binary) whether the sponsor is Hispanic.                                                                                                                                                                                                                                                |
| `MRef`              | (binary) whether the sponsor is a member of one of the committees to which the bill has been referred.                                                                                                                                                                                   |
| `ChRef`             | (binary) whether the sponsor is Chair of one of the committees to which the bill has been referred.                                                                                                                                                                                      |
| `SubChRef`          | (binary) whether the sponsor is Chair of one of the subcommittees to which the bill has been referred.                                                                                                                                                                                   |
| `RankRef`           | (binary) whether the sponsor is Ranking Member of one of the committees to which the bill has been referred.                                                                                                                                                                             |
| `SubRankRef`        | (binary) whether the sponsor is Ranking Member of one of the subcommittees to which the bill has been referred.                                                                                                                                                                          |
| `extremism`         | (numeric) indicates the ideological extremism of the sponsor. The absolute difference between the sponsor's DW-Nominate score and the Congress-Chamber mean.                                                                                                                             |
| `revenue_bill`      | (binary) whether the bill is a revenue bill. The variable has a value of `1` if the bills has been referred to either the Senate Finance Committee (SSFI) or the House Committee on Ways and Means (HSWM).                                                                               |
| `reauth`            | (binary) whether the bill is a reauthorization bill. The variable has a value of `1` if the string `reauth*` is present in the title.                                                                                                                                                    |
| `companion`         | (binary) whether the bill as a companion bill in the other chamber. The variable has a value of `1` if there is a bill in the other Chamber in that Congress that shares at least 95% of the unigrams (at least 95% of the bill's unigrams need to be in the other bill and vice versa). |
| `by_request`        | (binary) whether the bill has been promoted by the Administration. Bills that are primarily about defense, trade or international affairs and that have been introduced "by request".                                                                                                    |
| `unified_cong`       | (binary) congress-level covariate indicating whether the same party has the majority in both chambers (`=1`).                                                                                                                                                                            |
| `nomgrid`           | (numeric) [Gray and Jenkings's (2017)](https://www.cambridge.org/core/journals/journal-of-public-policy/article/pivotal-politics-and-the-ideological-content-of-landmark-laws/F4E72D32E886045889EED124866643C7) Gridlock Interval measure.                                               |
| `Cosponsr_log`      | (numeric) the log of the number of co-sponsors of the bill.                                                                                                                                                                                                                              |
| `all_vers`          | (string) -_only available for hitchhiker bills_- all versions of the bill (e.g. `IH;RH:EH`)                                                                                                                                                                                              |
| `last_vers`         | (string) -_only available for hitchhiker bills_- the last version of the bill (e.g. `EH`)                                                                                                                                                                                                |
| `last_vers_generic` | (string) -_only available for hitchhiker bills_- a generic way of indicating the last version of a given bill (e.g. IH and IS bills become `introduced`).                                                                                                                                |
| `first_match`       | (string) -_only available for hitchhiker bills_- stage in the legislative process in which the target law incorporated the hitchhiker bill.                                                                                                                                              |
| `Major_Law`         | (string) -_only available for hitchhiker bills_- Major topic of the target law into which the bill has been inserted, according as well to the CAP classification/codebook.                                                                                                              |

   - `house_assignments_103-115-3.xls`: House committee assignments dataset (103 through 115th Congress), from Charles Stewart III's [website](http://web.mit.edu/17.251/www/data_page.html). 
   - `senators_103-115-2.xls`: Senate committee assignments dataset (103rd through 115th Congress), from Charles Stewart III's [website](http://web.mit.edu/17.251/www/data_page.html). 
   - `LEPData93to110Congresses.xlsx`: Legislative Effectiveness Scores (93rd through 110th Congress), from the [Center for Effective Lawmaking](https://thelawmakers.org/data-download) (Volden and Weiseman).
   - `LEPData111to113Congresses.xlsx`: Legislative Effectiveness Scores (111th through 113th Congress), from the [Center for Effective Lawmaking](https://thelawmakers.org/data-download) (Volden and Weiseman).
   - in the `./data/predictions/` subdirectory there are files related to the hitchhiker discovering process: the hitchhikers predicted at each stage of the process by the best and high performing models. These files are used in [09-supporting-info-C-summary-of-hitchhiker-discovering-process.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/09-supporting-information-C-summary-of-hitchhiker-discovering-process.R) to replicate Table 3 in Supporting Information C, where we summarize the process and report the ensemble precision and recall at each stage.
## Code
The `./code/` directory contains separate scripts to replicate each analytical figure of the article. The `./figures/` directory contains a copy of each of the figures generated by these scripts. 

  - [01-figure2-hitchhikers-versus-laws-by-congress-by-importantbill.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/01-figure2-hitchhikers-versus-laws-by-congress-by-importantbill.R): Code to replicate Figure 2 of the paper, showing the number of hitchhiker bills and laws By Congress, distinguishing between important and minor bills.

<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure2_BW.png">

  - [02-figure3-how-far-hitchhiker-get-on-their-own.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/02-figure3-how-far-hitchhikers-get-on-their-own.R): Code to replicate Figure 3 of the paper, showing how far hitchhiker bills get though the legislative process on their own
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure3_BW.png">

  - [03-models.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/03-models.R): Code to replicate Figure 4, and the model coefficients in Table 4 (in Supporting Information D), showing the relationship between a relevant set of covariates and the probability of a bill to be enacted as stand alone legislation or as a hitchhiker bill.
  
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure4-coefficient-plot.png">

  - [04-figure5-general-effects-on-effectiveness.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/04-figure5-general-effect-on-effectiveness.R): Code to replicate figure 5 of the paper, showing how counting hitchhikers as enacted legislation increases the proportion of different types of members that get at least 1 bill enacted in any given Congress
 
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure5_so_what_BW.png">

  - [05-figure6-LES-v-our-measure-of-effectiveness.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/05-figure6-LES-v-our-measure-of-effectiveness.R): Code to replicate Figure 6 of the paper, comparing our measure of effectivenes (legislation enacted as proportion of legislation introduced) v. Legislative Effectiveness Scores, of Volden and Weiseman. This script generates 2 figures: `figure6a-LES-vs-OUR-indiv-diff.png` and `figure6b-LES-vs-OUR-indiv-diff-FULL-DIST.png`. For the article we manually placed the second on into the upper right corner of the first. The first one is a truncated distribution whereas the second one illustrate the full distribution we aim to capture in this section of the article.
 
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure6a-LES-vs-OUR-indiv-diff.png">


  - [06-figure7-where-insertions-occur.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/06-figure7-where-insertions-occur.R): Code to replicate Figure 7 of the paper, showing where in the legislative process hitchhiker bills get picked up.
 
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure7_where_hitchhiker_get_picked_up.png">

  - [07-supporting-info-A-preprocessing.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/07-supporting-info-A-preprocessing.R): Code to replicate the text pre-processing procedure described in the Supporting Information A of the paper. We remove all the procedural text and sections that should not be taken into consideration when comparing the substantive content of bills, as well as meaningless words such as stop words and other frequent tokens (e.g. section, act, secretary, etc.). For simplicity, in this script we show how to pre-process two example bills (103-HR-1-IH and 103-HR-2-RH). The same process can then be applied to pre-process all the bill versions collected for the study. The rest of the text files for each bill version can be easily downloaded from [congress.gov](http://congress.gov). The two example raw files are located in the `./data/bills/raw/` directory, and the pre-processed versions are located in `./data/bills/clean/`.
 
  - [Supporting Information B](https://github.com/matthewjdenny/SpeedReader/blob/master/R/document_similarities.R): This is a link to the `document_similarities()` function of the `SpeedReader` package, written by one of the authors of the article (Matthew Denny), and that we use the perform the pairwise comparisons of bills, and to extract the features described in the Supporting Information B section of the article.
  
  - [08-supporting-info-C-stage01-predicted-hitchhikers.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/08-supporting-info-C-stage01-predicted-hitchhikers.R): Code to replicate Figure 8 of the paper, where we show the distribution of the number of models (out of 99 high performing models) that predicted the same hitchhiker in the first stage of the hitchhiker discovering process.
  
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure8-hitchhikers_barplot_iter1.png">  

  - [09-supporting-info-C-summary-of-hitchhiker-discovering-process.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/09-supporting-information-C-summary-of-hitchhiker-discovering-process.R): Code to replicate Table 3 of the article (in Supporting Information C), where we provide a summary of the hitchhikers discovered at each stage of the discovering process, as well as information about the best performing models at each stage.
  
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/table3-summary-hitchhiker-discovering-process.png">

  - [10-table4-descriptive-stats-table-supporting-information-D.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/10-table4-descriptive-stats-table-supporting-information-D.R): Code to replicate Table 4 in Supporting Information D, where we show descriptive statistics for the variables in the statistical models we use in the paper to test our hitchhiker hypotheses.
  
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/table4-descriptive-stats-model-data.png">

  - [11-figure9-heterogeneous-effects-supporting-information-D.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/11-figure9-heterogeneous-effects-supporting-information-D.R): Code to replicate Figure 9 in Supporting Information D, where we show that the key model coefficients do not vary much by Congress, strengthening the robustness of our findings.
  
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure9-hetero-effects.png">

  - [12-supporting-information-E-hitchhiker-examples-table.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/12-supporting-information-E-hitchhiker-examples-table.R): Code to replicate the table in Supporting Information E, where we provide a list of hitchhiker bills added to the Affordable Care Act and the Financial Freedom Act of 1999.
  
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/SI-D-hitchhiker-examples.png">

