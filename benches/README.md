# Benchmarks for JSON formatting

|  Size |        File Name           | Source |
|------:|---------------------------:|--------|
| 1.3MB | cccbr-methods.json         | [GitHub](https://raw.githubusercontent.com/kneasle/cc-method-lib/0b57bd03e6f08f20d2fdf0cb9e688bbd01b62e0a/cccbr-methods.json) |
|  10kB | earthquakes.json           | [US Government](https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson) |
| 891kB | historical-events.json     | [vizgr.org](https://www.vizgr.org/historical-events/search.php?format=json&begin_date=-3000000&end_date=20151231&lang=en) |
| 145kB | monument-test-results.json | [Monument's GitHub Repo](https://github.com/kneasle/ringing/blob/4bec69f28e6bcae76476a4054d9d3ccee635d6cb/monument/test/results.json) |
| 237kB | meteorites.json            | [NASA](https://data.nasa.gov/resource/y77d-th95.json) (modified) |

Some data files (e.g. `meteorites.json`) contain numbers inside strings (e.g. `"41"` instead of just
`41`).  I've pre-processed these so that the numbers are outside the strings using the command `rg
--passthru -N "\"(?P<num>(-?)[0-9]*\.?[0-9]*)\"" benches/meteorites.json --replace "\$num" >
/tmp/json && mv /tmp/json benches/meteorites.json` (requires
[ripgrep](https://github.com/BurntSushi/ripgrep) and a Unix-like OS).

Also, the original `historical-events.json` is missing two trailing `}`s, which I've added.
