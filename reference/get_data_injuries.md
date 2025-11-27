# Extract injury/illness data frame

Extract injury/illness data frame from the injd object.

## Usage

``` r
get_data_injuries(injd)
```

## Arguments

- injd:

  `injd` **S3** object (see
  [`prepare_all()`](https://lzumeta.github.io/injurytools/reference/prepare_data.md)).

## Value

The injury/illness data frame containing the necessary columns:
"person_id", "date_injured" and "date_recovered".

## Examples

``` r
get_data_injuries(injd)
#>                  person_id date_injured date_recovered
#> 1             adam-lallana   2017-07-31     2017-11-25
#> 2             adam-lallana   2018-03-31     2018-05-13
#> 3             adam-lallana   2018-09-04     2018-10-19
#> 4             adam-lallana   2018-11-09     2018-12-04
#> 5             adam-lallana   2019-01-06     2019-01-18
#> 6             adam-lallana   2019-04-01     2019-05-31
#> 7           alberto-moreno   2017-12-07     2018-01-26
#> 8  alex-oxlade-chamberlain   2018-04-25     2019-03-07
#> 9         andrew-robertson   2018-03-28     2018-03-30
#> 10        andrew-robertson   2018-12-03     2018-12-07
#> 11        andrew-robertson   2019-02-25     2019-02-26
#> 12        andrew-robertson   2019-05-08     2019-05-11
#> 13        andrew-robertson   2019-06-12     2019-06-24
#> 14        daniel-sturridge   2017-08-02     2017-08-18
#> 15        daniel-sturridge   2017-12-08     2018-01-28
#> 16        daniel-sturridge   2018-02-12     2018-04-08
#> 17            dejan-lovren   2017-07-27     2017-08-09
#> 18            dejan-lovren   2018-07-15     2018-09-25
#> 19            dejan-lovren   2018-12-01     2018-12-10
#> 20            dejan-lovren   2019-01-08     2019-01-29
#> 21            dejan-lovren   2019-02-01     2019-03-09
#> 22            dejan-lovren   2019-04-16     2019-04-25
#> 23            divock-origi   2018-11-26     2018-12-01
#> 24                emre-can   2018-03-18     2018-05-25
#> 25                 fabinho   2018-08-09     2018-08-11
#> 26                 fabinho   2019-01-20     2019-01-29
#> 27                 fabinho   2019-04-22     2019-05-03
#> 28     georginio-wijnaldum   2017-10-20     2017-10-27
#> 29     georginio-wijnaldum   2018-02-23     2018-03-09
#> 30     georginio-wijnaldum   2018-09-23     2018-09-28
#> 31     georginio-wijnaldum   2019-01-13     2019-01-29
#> 32     georginio-wijnaldum   2019-02-01     2019-02-08
#> 33     georginio-wijnaldum   2019-04-11     2019-04-13
#> 34            james-milner   2018-08-05     2018-08-11
#> 35            james-milner   2018-10-07     2018-10-19
#> 36            james-milner   2018-12-13     2018-12-20
#> 37            james-milner   2018-12-22     2019-01-02
#> 38            james-milner   2019-03-04     2019-03-16
#> 39               joe-gomez   2018-03-23     2018-04-20
#> 40               joe-gomez   2018-05-08     2018-07-01
#> 41               joe-gomez   2018-08-05     2018-08-11
#> 42               joe-gomez   2018-12-05     2019-04-08
#> 43              joel-matip   2017-11-30     2017-12-25
#> 44              joel-matip   2018-04-02     2018-07-02
#> 45              joel-matip   2018-12-13     2019-01-10
#> 46        jordan-henderson   2017-08-03     2017-08-07
#> 47        jordan-henderson   2017-11-02     2017-11-13
#> 48        jordan-henderson   2017-12-23     2018-01-26
#> 49        jordan-henderson   2018-10-21     2018-11-08
#> 50        jordan-henderson   2019-01-04     2019-01-11
#> 51        jordan-henderson   2019-02-01     2019-02-08
#> 52        jordan-henderson   2019-03-14     2019-03-22
#> 53        jordan-henderson   2019-05-09     2019-05-11
#> 54            loris-karius   2018-05-28     2018-06-11
#> 55           mohamed-salah   2018-05-27     2018-06-14
#> 56           mohamed-salah   2018-10-13     2018-10-18
#> 57           mohamed-salah   2019-05-05     2019-05-11
#> 58              naby-keita   2018-10-16     2018-11-05
#> 59              naby-keita   2019-03-12     2019-03-30
#> 60              naby-keita   2019-05-01     2019-06-21
#> 61       philippe-coutinho   2017-08-03     2017-08-28
#> 62       philippe-coutinho   2017-10-26     2017-11-07
#> 63       philippe-coutinho   2017-12-30     2018-01-24
#> 64         roberto-firmino   2018-09-16     2018-09-17
#> 65         roberto-firmino   2019-02-24     2019-03-02
#> 66         roberto-firmino   2019-04-25     2019-04-30
#> 67         roberto-firmino   2019-05-03     2019-05-21
#> 68              sadio-mane   2017-10-09     2017-11-02
#> 69              sadio-mane   2018-10-15     2018-10-24
#> 70              sadio-mane   2018-12-03     2018-12-07
#> 71              sadio-mane   2019-04-11     2019-04-13
#> 72  trent-alexander-arnold   2018-12-11     2018-12-26
#> 73  trent-alexander-arnold   2019-01-13     2019-02-08
#> 74  trent-alexander-arnold   2019-03-18     2019-03-31
#> 75         virgil-van-dijk   2017-12-22     2018-01-15
#> 76         virgil-van-dijk   2018-09-23     2018-09-28
#> 77         virgil-van-dijk   2018-10-14     2018-10-19
#> 78         virgil-van-dijk   2019-01-05     2019-01-11
#> 79         virgil-van-dijk   2019-01-20     2019-01-29
#> 80         virgil-van-dijk   2019-04-01     2019-04-04
#> 81         xherdan-shaqiri   2019-03-18     2019-03-31
#> 82         xherdan-shaqiri   2019-06-10     2019-08-03
```
