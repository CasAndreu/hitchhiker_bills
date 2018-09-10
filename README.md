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
