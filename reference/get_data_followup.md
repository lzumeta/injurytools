# Extract follow-up data frame

Extract follow-up data frame from the injd object.

## Usage

``` r
get_data_followup(injd)
```

## Arguments

- injd:

  `injd` **S3** object (see
  [`prepare_all()`](https://lzumeta.github.io/injurytools/reference/prepare_data.md)).

## Value

The follow-up data frame containing the necessary columns: "person_id",
"t0" and "tf".

## Examples

``` r
get_data_followup(injd)
#>                  person_id         t0         tf
#> 1             adam-lallana 2017-07-01 2019-06-30
#> 2           alberto-moreno 2017-07-01 2018-06-30
#> 3  alex-oxlade-chamberlain 2017-07-01 2018-04-25
#> 4                  alisson 2018-07-01 2019-06-30
#> 5         andrew-robertson 2017-07-01 2019-06-30
#> 6         daniel-sturridge 2017-07-01 2019-06-30
#> 7               danny-ings 2017-07-01 2018-06-30
#> 8             dejan-lovren 2017-07-01 2019-06-30
#> 9             divock-origi 2018-07-01 2019-06-30
#> 10         dominic-solanke 2017-07-01 2018-06-30
#> 11                emre-can 2017-07-01 2018-06-30
#> 12                 fabinho 2018-07-01 2019-06-30
#> 13     georginio-wijnaldum 2017-07-01 2019-06-30
#> 14            james-milner 2017-07-01 2019-06-30
#> 15               joe-gomez 2017-07-01 2019-06-30
#> 16              joel-matip 2017-07-01 2019-06-30
#> 17        jordan-henderson 2017-07-01 2019-06-30
#> 18            loris-karius 2017-07-01 2018-06-30
#> 19           mohamed-salah 2017-07-01 2019-06-30
#> 20              naby-keita 2018-07-01 2019-06-30
#> 21       philippe-coutinho 2017-07-01 2018-06-30
#> 22           ragnar-klavan 2017-07-01 2018-06-30
#> 23         roberto-firmino 2017-07-01 2019-06-30
#> 24              sadio-mane 2017-07-01 2019-06-30
#> 25          simon-mignolet 2017-07-01 2018-06-30
#> 26  trent-alexander-arnold 2017-07-01 2019-06-30
#> 27         virgil-van-dijk 2017-07-01 2019-06-30
#> 28         xherdan-shaqiri 2018-07-01 2019-06-10
```
