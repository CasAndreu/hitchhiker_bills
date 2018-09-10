# Hitchhiker Bills
This repository contains the replication material of the article "More Effective Than We Thought: Accounting for Legislative Hitchhikers Reveals a More Inclusive and Productive Lawmaking Process", in the _American Journal of Political Science_, by [Andreu Casas](http://andreucasas.com/), [Matthew Denny](http://www.mjdenny.com/), and [John Wilkerson](https://faculty.washington.edu/jwilker/Bio.pdf).


## Data
The `./data/` directory contains the main replication dataset: `main_db.csv`. This is a bill-level dataset with the following information-variables:

  - `BillID`: (string) a bill identifier composed of the Congress number, the type of bill, and the bill number (e.g. 103-HR-1). 
  - `BillType`: (categorical) the type of bill: house bill (`hr`), senate bill (`s`), house joint resolution (`hjres`), senate joint resoluation (`sjres`).
  - `BillNum`: (numeric) the bill number.
  - `Title`: (string) the bill title.
  - `ImpBill`: (binary) whether the bills is an important (`1`) or minor (`0`) bill.
  - `outcome1`: (categorical) whether the bill was enacted as stand alone law (`law`), hitchhiker (`insertion`), or was not enacted (`no law`).
  - `LawID`: (string) for hitchhiker bills, this variable provides the `BillID` of the bill into which they were inserted.
  - `Major`: (categorical) indicates the topic of the bill, according to the Congressional Bills Project Major topic codes, which follow the [Comparative Agendas Policy codebook](https://www.comparativeagendas.net/pages/master-codebook) (CAP).
  - `Minor`: (categorical) indicates the mintor topic code (following the CAP codebook as well).
  - `Chamber`: (binary) the chamber in which the bill was introduced (`1` = Senate).
  - `RefArr`: (string) provides the codes of the committees to which the bills have been referred. 
  - `Cong`: (categorical) the Congress in which the bill was introdued.
  - `NameFull`: (string) the name of the member who sponsored the bill.
  - `Majority`: (binary) whether the sponsor was a member of the majority party (=`1`).
  - `PooleID`: (numeric) member-level numeric identifier.
  - `Gender`: (binary) whether the sponsor is female (=`1`).
  - `AA`: (binary) whether the sponsor is African American.
  - `Hisp`: (binary) whether the sponsor is Hispanic.
  - `MRef`: (binary) whether the sponsor is a member of one of the committees to which the bill has been referred.
  - `ChRef`: (binary) whether the sponsor is Chair of one of the committees to which the bill has been referred.
  - `SubChRef`: (binary) whether the sponsor is Chair of one of the subcommittees to which the bill has been referred.
  - `RankkRef`: (binary) whether the sponsor is Ranking Member of one of the committees to which the bill has been referred.
  - `SubRankkRef`: (binary) whether the sponsor is Ranking Member of one of the subcommittees to which the bill has been referred.
  - `extremism`: (numeric) indicates the ideological extremism of the sponsor. The absolute difference between the sponsor's DW-Nominate score and the Congress-Chamber mean.
  - `revenue_bill`: (binary) whether the bill is a revenue bill. The variable has a value of `1` if the bills has been referred to either the Senate Finance Committee (SSFI) or the House Committee on Ways and Means (HSWM).
  - `reauth`: (binary) whether the bill is a reauthorization bill. The variable has a value of `1` if the string `reauth*` is present in the title.
  - `companion`: (binary) whether the bill as a companinon bill in the other chamber. The variable has a value of `1` if there is a bill in the other Chamber in that Congress that shares at least 95% of the unigrams (at least 95% of the bill's unigrams need to be in the other bill and vice versa).
  - `by_request`: (binary) whether the bill has been promoted by the Administration. Bills that are primarily about defense, trade or international affairs and that have been introduced "by request". 
  - `unified_cong`: (binary) congress-level covariate indicating whether the same party has the majority in both chambers (`=1`).
  - `nomgrid`: (numeric) [Gray and Jenkings's (2017)](https://www.cambridge.org/core/journals/journal-of-public-policy/article/pivotal-politics-and-the-ideological-content-of-landmark-laws/F4E72D32E886045889EED124866643C7) Gridlock Interval measure.
  - `Cosponsr_log`: (numeric) the log of the number of co-sponsors of the bill.
  - `all_vers`: (string) -_only available for hitchhiker bills_- all versions of the bill (e.g. `IH;RH:EH`)
  - `last_vers`: (string) -_only available for hitchhiker bills_- the last version of the bill (e.g. `EH`)
  - `last_vers_generic`: (string) -_only available for hitchhiker bills_- a generic way of indicating the last version of a given bill (e.g. IH and IS bills become `introduced`).
  
## Code
The `./code/` directory contains separate scripts to replicate each analytical figure of the article.

  - [01-figure2-hitchhikers-versus-laws-by-congress-by-importantbill.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/01-figure2-hitchhikers-versus-laws-by-congress-by-importantbill.R): Code to replicate Figure 2 of the paper, showing the number of hitchhiker bills and laws By Congress, distinguishing between important and minor bills.

<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure2_BW.png">

  - [02-figure3-how-far-hitchhiker-get-on-their-own.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/02-figure3-how-far-hitchhikers-get-on-their-own.R): Code to replicate Figure 3 of the paper, showing how far hitchhiker bills get though the legislative process on their own
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure3_BW.png">

  - [03-models.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/03-models.R): Code to replicate Figure 4, and the model coefficients in Table 4 (in Supporting Information D), showing the relationship between a relevant set of covariates and the probability of a bill to be enacted as stand alone legislation or as a hitchhiker bill.
  
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure4-coefficient-plot.png">

  - [04-figure5-general-effects-on-effectiveness.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/04-figure5-general-effect-on-effectiveness.R): Code to replicate figure 5 of the paper, showing how counting hitchhikers as enacted legislation increases the proportion of different types of members that get at least 1 bill enacted in any given Congress
 
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure5_so_what_BW.png">

  - [05-figure6-LES-v-our-measure-of-effectiveness.R](https://github.com/CasAndreu/hitchhiker_bills/blob/master/code/05-figure6-LES-v-our-measure-of-effectiveness.R): Code to replicate Figure 6 of the paper, comparing our measure of effectivenes (legislation enacted as proportion of legislation introduced) v. Legislative Effectiveness Scores, of Volden and Weiseman.
 
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure6a-LES-vs-OUR-indiv-diff.png">
<img src = "https://github.com/CasAndreu/hitchhiker_bills/blob/master/figures/figure6b-LES-vs-OUR-indiv-diff-FULL-DIST.png">

