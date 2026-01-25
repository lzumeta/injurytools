# Extract exposures data frame

Extract exposures data frame from the injd object.

## Usage

``` r
get_data_exposures(injd)
```

## Arguments

- injd:

  `injd` **S3** object (see
  [`prepare_all()`](https://lzumeta.github.io/injurytools/reference/prepare_data.md)).

## Value

The exposure data frame containing the necessary columns: "person_id",
"date" and "time_expo".

## Examples

``` r
get_data_exposures(injd)
#>                  person_id       date time_expo
#> 1             adam-lallana 2017-07-31       236
#> 2             adam-lallana 2018-09-04       464
#> 3           alberto-moreno 2017-12-07      1264
#> 4  alex-oxlade-chamberlain 2018-04-25      1483
#> 5                  alisson 2019-06-30      3420
#> 6         andrew-robertson 2018-03-28      1943
#> 7         andrew-robertson 2018-12-03      3219
#> 8         daniel-sturridge 2017-08-02       431
#> 9         daniel-sturridge 2019-06-30       496
#> 10              danny-ings 2018-06-30       265
#> 11            dejan-lovren 2017-07-27      2124
#> 12            dejan-lovren 2018-07-15       985
#> 13            divock-origi 2018-11-26       366
#> 14         dominic-solanke 2018-06-30       581
#> 15                emre-can 2018-03-18      2094
#> 16                 fabinho 2018-08-09      2013
#> 17     georginio-wijnaldum 2017-10-20      2387
#> 18     georginio-wijnaldum 2018-09-23      2735
#> 19            james-milner 2018-08-05      1763
#> 20            james-milner 2018-10-07      1785
#> 21               joe-gomez 2018-03-23      1900
#> 22               joe-gomez 2018-08-05      1060
#> 23              joel-matip 2017-11-30      1994
#> 24              joel-matip 2018-12-13      1532
#> 25        jordan-henderson 2017-08-03      2172
#> 26        jordan-henderson 2018-10-21      1982
#> 27            loris-karius 2018-05-28      1710
#> 28           mohamed-salah 2018-05-27      2920
#> 29           mohamed-salah 2018-10-13      3261
#> 30              naby-keita 2018-10-16      1393
#> 31       philippe-coutinho 2017-08-03      1117
#> 32           ragnar-klavan 2018-06-30      1448
#> 33         roberto-firmino 2018-09-16      2778
#> 34         roberto-firmino 2019-02-24      2620
#> 35              sadio-mane 2017-10-09      2208
#> 36              sadio-mane 2018-10-15      3086
#> 37          simon-mignolet 2018-06-30      1710
#> 38  trent-alexander-arnold 2018-12-11      1576
#> 39  trent-alexander-arnold 2019-01-13      2467
#> 40         virgil-van-dijk 2017-12-22      1260
#> 41         virgil-van-dijk 2018-09-23      3385
#> 42         xherdan-shaqiri 2019-03-18      1057
```
