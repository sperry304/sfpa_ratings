SFPA Player Ratings, v.1
================
Skip Perry
March 2019

These ratings are based on the Bradley-Terry model, which since the 1950s has been one of the standard methods of rating individuals or teams that are repeatedly paired against one another. Considering two players *i* and *j* with ratings *π*<sub>*i*</sub> and *π*<sub>*j*</sub>, this framework assumes player *i* has the following probability of defeating player *j*:

$$\\text{P\[Player } i \\text{ defeats player } j\] = p\_{i &gt; j} = \\frac{\\pi\_i}{\\pi\_i + \\pi\_j}$$

While there is no analytical solution for the maximum likelihood estimate (MLE) of the collection of player ratings *π*<sub>*i* ∈ \[1, *n*\]</sub> for players 1 through *n*, iterative methods can be used to find a result:

$$\\pi\_i^{new} = \\frac{\\text{\# of total wins by player } i}{\\sum\_{\\text{All games played by } i} \\frac{\\text{\# of games between } i, j}{\\pi\_i^{current} + \\pi\_j^{current}}}$$

Or, using more traditional mathematical notation, where *π*<sub>*i*</sub> is player *i*'s rating, *w*<sub>*i*</sub> is the number of times player *i* won a game, *n*<sub>*i**j*</sub> is the number of games played between players *i* and *j*, and (*t*) indicates the result at the *t*<sup>*t**h*</sup> iteration:

$$\\pi\_i^{(t)} = \\frac{w\_i}{\\sum\_{j \\neq i} \\frac{n\_{ij}}{\\pi\_i^{(t-1)} + \\pi\_j^{(t-1)}}}$$

This adaptation of the expectation-maximization (EM) algorithm loops through each player, updating their rating using the latest estimates of all their opponents' ratings. (A little experimentation with Google Calculator or Excel will demonstrate how this formula gives players more credit for beating higher-ranked opponents.) Eventually, each update will have such a small effect on the vector of player ratings *π* that we can stop the process.

Maximum likelihood estimation is the single most widely used method of parameter estimation, but it has problems. While convergence is guaranteed under certain conditions, one deals with having a sufficient number of connections in the data, such as you might find in a series of round-robin games played between players ([see Assumption 3 here](http://personal.psu.edu/drh20/papers/bt.pdf)). Additionally, the MLE converges toward positive (and negative) infinity for unbeaten (and winless) players. The SFPA data set faces both of these issues - sparse data with many players playing only a handful of games against a limited subset of opponents, and some examples of players with no wins at all.

Thankfully, a Bayesian approach can be used to find the maximum a priori (MAP) estimate of ***π*** without resorting to omitting offending data or including only results from players with a minimum number of games. We get what we need by setting a *G*(*a*, *b*) prior on *p*(*π*), which is conjugate to the complete data likelihood function and results in the following update formula for *π*:

$$\\pi\_i^{(t)} = \\frac{a - 1 + w\_i}{b + \\sum\_{j \\neq i} \\frac{n\_{ij}}{\\pi\_i^{(t-1)} + \\pi\_j^{(t-1)}}}$$

Like the MLE, MAP estimation provides a point estimate, though in this case it is the mode of a posterior distribution rather than the maximization of a likelihood function. (Note that for *a* = 1 and *b* = 0, the MAP and maximum likelihood estimates are equivalent; see [here](http://www.stats.ox.ac.uk/~doucet/caron_doucet_bayesianbradleyterry.pdf) for more information.) Setting *b* = *a*/500 leads to a mean player rating of around 500 while satisfying identifiability constraints. Hyperparameter tuning for *a* resulted in a choice of 2.75 - a weakly informative prior.

Other considerations included:

-   Home-table advantage: In the past three seasons, about 52% of games were won by the home team, a small but significant difference. There also exists a simple way to incorporate the home-table advantage into the ratings, setting $p\_{i&gt;j} = \\frac{\\theta \\pi\_i}{\\theta \\pi\_i + \\pi\_j}$ where *θ* &gt; 1 is the home-table advantage (or less than 1 if it's a disavantage). Unfortunately, this is a noisy input: due to scheduling conflicts and bar remodeling, teams often play "home" games at other bars; most week 1 games are home games for both teams; and playoff games represent an uneven playing field since higher-seeded teams play at home while lower-seeded teams play on the road, among other factors. After quite a bit of experimentation in this area failed to improve model performance, I omitted the home-table advantage from the rating system.

-   Time decay: There are a few different ways of handling ratings that change over time. One extreme is to count every match someone has ever played; another would be to set a hard cutoff and only count the most recent, say, 50 or 75 games played. The middle ground used here is a sliding scale that results in older matches contribute less to a player's rating than newer ones. Matches are given a half-life of 1500 days from the date of a player's latest game. (As an example, matches played in January 2018 contribute about 80% of the value of a match played in March 2019.) Since parameter tuning showed little overall impact on the ratings as a whole, this number was chosen arbitrarily. The end result is a rating system that is responsive to change but results in fewer wild swings, especially for players with a lot of data.

-   Robustness: In order to prevent new players from affecting the ratings of established players, games against new players are down-weighted until they reach a certain number of games. As with the time decay, experimentation showed that different values of this parameter had little impact on the ultimate results. The number 10 was chosen based on an exploratory examination of when many players' ratings begin to converge to a relatively steady-state level.

Without further ado, the ratings as of March 19, 2019:

|  rank| player                 | current\_team                |  rating|  games\_in\_system|
|-----:|:-----------------------|:-----------------------------|-------:|------------------:|
|     1| Hector Ortega          | --                           |    2299|                 46|
|     2| Skip Perry             | Tandy Tokers                 |    1894|                 69|
|     3| Mike Maxwell           | Route 101 Rawhides           |    1842|                136|
|     4| Ryan Piaget            | Clean Slate                  |    1655|                102|
|     5| Nick Callado           | --                           |    1508|                 39|
|     6| Jesse La Fear          | --                           |    1458|                 73|
|     7| Stefano Lopez          | --                           |    1446|                 48|
|     8| Bob Simon              | Route 101 Rawhides           |    1395|                141|
|     9| Diogo Martini          | Golden Slate Warriors        |    1387|                 98|
|    10| Rhys Hughes            | Golden Slate Warriors        |    1356|                 94|
|    11| Evan Burgess           | Lucky Horseshoe Caballeros   |    1337|                139|
|    12| Tom Seymour            | Route 101 Rawhides           |    1326|                115|
|    13| Andy Luong             | --                           |    1305|                 83|
|    14| Thayer McDougle        | Lucky Horseshoe Caballeros   |    1286|                133|
|    15| Matt Frisbie           | --                           |    1280|                 62|
|    16| Nick Lansdown          | Lucky Horseshoe Caballeros   |    1274|                105|
|    17| Jon Williams           | Cafe Ballbusters             |    1205|                 22|
|    18| Alonza Bear Davis      | --                           |    1196|                 12|
|    19| Tae Yim                | Cafe 2 for 1's               |    1191|                 18|
|    20| Hugo Valseca           | --                           |    1190|                 51|
|    21| Rudy Guzman            | --                           |    1185|                 47|
|    22| Buddy Giguere          | Smoke & Rumors               |    1155|                 71|
|    23| Ben Green              | Golden Slate Warriors        |    1154|                 98|
|    24| Patty West             | Golden Slate Warriors        |    1066|                 94|
|    25| Wyatt Moss             | Naked Lunch Nice Rack        |    1015|                117|
|    26| Chris DuCoing          | Smoke & Rumors               |     997|                106|
|    27| Leon Waki              | --                           |     989|                 81|
|    28| Joshua Maldonado       | Route 101 Rawhides           |     976|                 75|
|    29| James Neale            | Lucky Horseshoe Caballeros   |     970|                132|
|    30| Crystal Kelem          | Cafe Strikes Again           |     953|                118|
|    31| Danny Mullan           | Route 101 Rawhides           |     953|                111|
|    32| Skinner Arteaga        | Lucky Horseshoe Caballeros   |     950|                101|
|    33| Pancho Palma           | --                           |     946|                 47|
|    34| Dave Ward              | Dovre & Out                  |     939|                129|
|    35| Joel Talevi            | Clean Slate                  |     938|                 88|
|    36| Darrell Haslip         | Smoke & Rumors               |     928|                 89|
|    37| Adam Simpson           | Cafe Cafaholics              |     900|                 41|
|    38| Joina Liao             | Pilsner Penguins             |     892|                 44|
|    39| Hugh Fountain          | Mixfits                      |     880|                 82|
|    40| Will Chadwick          | Cafe Ballbusters             |     879|                131|
|    41| Rick Mariani           | Route 101 Rawhides           |     877|                101|
|    42| Eugene Fan             | Rumors Never Die             |     865|                 92|
|    43| Paul Krohn             | Clean Slate                  |     861|                109|
|    44| Tommy Mudd             | --                           |     854|                 39|
|    45| Adam Moore             | Dovre & Out                  |     848|                 76|
|    46| Tony Tully             | Ice Willows                  |     845|                103|
|    47| Rajat Kansal           | --                           |     843|                110|
|    48| Eric Babaki            | Pilsner Penguins             |     834|                 10|
|    49| Alan Lowe              | Ice Willows                  |     827|                108|
|    50| Rene Denis             | Smoke & Rumors               |     811|                119|
|    51| Mark Cooper            | --                           |     810|                 43|
|    52| Jerry Ball             | Route 101 Rawhides           |     807|                 64|
|    53| Juan Chicho            | Dovre & Out                  |     798|                 33|
|    54| Bob Schnatterly        | Cinch Pack                   |     794|                117|
|    55| Astra Sodarsono        | --                           |     790|                 36|
|    56| Ari Cowen              | Dovre & Out                  |     787|                147|
|    57| Gilbert Morales        | --                           |     782|                 44|
|    58| Jae Bigley             | --                           |     778|                 31|
|    59| Martin Smidak          | Golden Slate Warriors        |     763|                102|
|    60| Max Schroeder          | Harry's Hooligans            |     751|                 88|
|    61| Nima Gaadadsuren       | Cinch You're Down There      |     744|                132|
|    62| Polo Black Golde       | Clean Slate                  |     742|                108|
|    63| Chris Beal             | --                           |     737|                 34|
|    64| Robert Hoo             | --                           |     735|                 46|
|    65| Thom Moyer             | Cinch Phoenix                |     729|                107|
|    66| Isaac Wong             | Smoke & Rumors               |     727|                 57|
|    67| Bob Rice               | Cafe Cafaholics              |     726|                 15|
|    68| Paul Campbell          | --                           |     724|                 10|
|    69| Jason Rogers           | Clean Slate                  |     723|                 65|
|    70| Dave Timko             | --                           |     714|                 41|
|    71| Preston Hudson         | Clean Slate                  |     712|                 78|
|    72| Rodney Zarnegar        | Pilsner Penguins             |     711|                113|
|    73| Humberto HJ Gonzalez   | Smoke & Rumors               |     711|                100|
|    74| Conor O'Neill          | --                           |     711|                 29|
|    75| Amy Peterson           | --                           |     710|                 34|
|    76| Alex Gilbert           | Ginger Strokes               |     706|                 16|
|    77| Perry Logan            | Cafe Strikes Again           |     702|                114|
|    78| Rio                    | --                           |     701|                 53|
|    79| Cuong Vuong            | --                           |     686|                 98|
|    80| Dan Nguyen             | --                           |     683|                 61|
|    81| Mike Kavanaugh         | --                           |     670|                 21|
|    82| Andrew Creech          | Cafe Ballbusters             |     668|                 78|
|    83| Roberto Aguilar        | --                           |     666|                 40|
|    84| Fran Herman            | Pilsner Penguins             |     663|                112|
|    85| Rene Loria             | --                           |     651|                 28|
|    86| Mark Thomas            | --                           |     643|                 48|
|    87| Mark Butler            | Golden Slate Warriors        |     643|                 67|
|    88| Ian Montbrun           | Cinch You're Down There      |     638|                119|
|    89| Brian Paris            | Mix VANGIE                   |     629|                120|
|    90| Sam Khozindar          | Ice Willows                  |     624|                 67|
|    91| Ben Napili             | Cinch Pack                   |     617|                 75|
|    92| Brendan Kendrick       | --                           |     617|                 43|
|    93| Julia Landholt         | House of Ginger              |     612|                 26|
|    94| Justin Taylor          | Mixfits                      |     611|                 61|
|    95| Salvador Miranda       | Lucky Horseshoe Caballeros   |     601|                114|
|    96| Niecy Sorrell          | --                           |     599|                  8|
|    97| Chris Forester         | Mixfits                      |     593|                 74|
|    98| Nick Wells             | --                           |     592|                 88|
|    99| Brian Bolitho          | Pilsner Penguins             |     589|                108|
|   100| Isaac Lopez            | Ginger Strokes               |     588|                 60|
|   101| Matt Kwong             | --                           |     579|                 76|
|   102| Mike Romano            | --                           |     577|                 44|
|   103| Alex Peralta           | Rumors Never Die             |     576|                 28|
|   104| Roy Luo                | Cafe Cafaholics              |     574|                117|
|   105| Michelle Hironimus     | --                           |     571|                 42|
|   106| Nick Radford           | Harry's Hooligans            |     570|                 17|
|   107| Rick Bradford          | Lucky Horseshoe Glue Factory |     569|                 22|
|   108| Matt Raine             | Black Willows                |     558|                 20|
|   109| Nithin Tharakan        | Bare Naked 6 Holes           |     558|                 76|
|   110| Carlos Gonzalez        | Rumors Never Die             |     555|                103|
|   111| Mikki Paull            | Wicked Bitches of the West   |     551|                112|
|   112| Leif Smith             | Hole in the Wall Howlers     |     546|                128|
|   113| Elvis McElhatton       | Dovre & Out                  |     545|                 12|
|   114| Sheree Taft            | Cafe Cafaholics              |     542|                 70|
|   115| James Horsefall        | Lucky Horseshoe Glue Factory |     542|                 16|
|   116| Emily Adams            | Pilsner Innmates             |     541|                 55|
|   117| Victor Ramos           | Cafe Ballbusters             |     540|                 50|
|   118| Marcelo Aviles         | Clean Slate                  |     538|                 71|
|   119| JoJo Cheung            | --                           |     538|                 42|
|   120| Travis Yallup          | Cafe Cafaholics              |     538|                103|
|   121| Nick Giangreco         | Lone Star Rebels             |     534|                103|
|   122| Ell Jackson            | Hole in the Wall Howlers     |     528|                106|
|   123| Casey O'Neill          | Cinch You're Down There      |     525|                132|
|   124| John Frakes            | Cinch Pack                   |     521|                 47|
|   125| Dan Sorge              | Dovre & Out                  |     515|                138|
|   126| Jonathan Addy          | Harry's Humdingers           |     511|                 62|
|   127| Chris Kline            | Cinch Phoenix                |     511|                104|
|   128| Michael Romano         | Lucky Horseshoe Glue Factory |     505|                 68|
|   129| James Bavuso           | Cinch Pack                   |     499|                106|
|   130| Ryan Robison           | Tandy Tokers                 |     498|                124|
|   131| Dylan Scandalios       | Bare Naked 6 Holes           |     491|                126|
|   132| Jerz Zuluaga           | Rumors Never Die             |     485|                 74|
|   133| JT                     | --                           |     484|                  8|
|   134| Juan Carlos Buenrostro | Harry's Hooligans            |     484|                 22|
|   135| Eric Gruttemeyer       | Cinchsationals               |     480|                126|
|   136| Yassine Laassel        | Mix VANGIE                   |     477|                 42|
|   137| Jasper Thomas          | --                           |     476|                 44|
|   138| Quinn Reilly           | --                           |     473|                 74|
|   139| Mika Kerr              | Cinchsationals               |     469|                125|
|   140| Justin Dayton          | Bare Naked 6 Holes           |     466|                127|
|   141| Spencer Branson        | --                           |     466|                 34|
|   142| Jack O'Toole           | --                           |     457|                 42|
|   143| Travis Abernathy       | --                           |     455|                  4|
|   144| Hakim Boukhaloua       | Ginger Strokes               |     450|                 30|
|   145| Chris Logan            | Cafe Ballbusters             |     446|                119|
|   146| Greg Morgan            | Cafe Strikes Again           |     443|                118|
|   147| Gerlie Mendoza         | Pilsner Penguins             |     442|                107|
|   148| Fearghal McEleney      | --                           |     441|                  4|
|   149| Jonathan Garcia        | --                           |     441|                 45|
|   150| Sandra Davis           | Cinch Phoenix                |     439|                 81|
|   151| Tom Flanagan           | --                           |     438|                 52|
|   152| Monica Kicklighter     | Wicked Bitches of the West   |     436|                 57|
|   153| Jackson Kon            | --                           |     435|                 31|
|   154| Ben Becker             | --                           |     426|                 95|
|   155| Doug Johnston          | Naked Lunch Nice Rack        |     421|                 54|
|   156| Mike Schlatter         | --                           |     421|                 16|
|   157| Estelle Mays           | --                           |     419|                 32|
|   158| Julien Roeser          | Lucky Horseshoe Glue Factory |     414|                114|
|   159| Anthony Hydron         | Lucky Horseshoe Glue Factory |     413|                 66|
|   160| Steven Park            | --                           |     412|                 23|
|   161| Clarke Curtis          | --                           |     410|                 62|
|   162| Geoff Vessels          | --                           |     409|                 27|
|   163| Spencer Smith          | --                           |     408|                 65|
|   164| Gary Simmons           | --                           |     408|                 48|
|   165| Simone Manganelli      | Mix VANGIE                   |     407|                 89|
|   166| Huu Nguyen             | Naked Lunch Nice Rack        |     404|                115|
|   167| Andrew Keller          | --                           |     403|                 48|
|   168| Steven Pease           | Naked Lunch Nice Rack        |     398|                 88|
|   169| Rob Thacher            | --                           |     398|                 28|
|   170| Arthur Patterson       | Bare Naked 6 Holes           |     395|                117|
|   171| Mark Sorensen          | Cafe Ballbusters             |     384|                123|
|   172| Julian Ostrow          | Rumors Never Die             |     379|                111|
|   173| Paul McCue             | Cinch Phoenix                |     379|                 82|
|   174| Walt Bartas            | Cinchsationals               |     379|                 95|
|   175| Julie Le               | Cinch Phoenix                |     377|                 15|
|   176| Joan Pettijohn         | Cinch Pack                   |     372|                 91|
|   177| Sean Flanagan          | --                           |     371|                 38|
|   178| Charles Carr           | --                           |     370|                  1|
|   179| Monica Jacqueline      | --                           |     369|                 36|
|   180| Marty                  | --                           |     369|                 43|
|   181| Dylan Hirsch-Shell     | --                           |     368|                 28|
|   182| Eric Kalisa            | Tandy Tokers                 |     363|                109|
|   183| Ali Rad                | --                           |     362|                 23|
|   184| Travis Santos          | Pilsner Innmates             |     361|                 22|
|   185| John Kiltinen          | Hole in the Wall Bangers     |     358|                127|
|   186| Fintan Sullivan        | Tandy Tokers                 |     358|                 16|
|   187| Patrick Picard         | Pilsner Innmates             |     355|                 22|
|   188| Stern Montoya          | Ice Willows                  |     353|                103|
|   189| Bryan Hoff             | Pilsner Innmates             |     351|                 22|
|   190| Jocelyn Angeles        | Cafe Strikes Again           |     351|                106|
|   191| Thomas Kleyn           | --                           |     350|                  4|
|   192| Adam Lucero            | Lone Star Longhorns          |     349|                  2|
|   193| Thomas Messer          | Hole in the Wall Howlers     |     348|                 64|
|   194| Mark Deal              | Cinch Phoenix                |     347|                 98|
|   195| Scott Marfield         | Mix VANGIE                   |     347|                 79|
|   196| Jaime Dizon            | Golden Slate Warriors        |     342|                 19|
|   197| Cloaky Jones           | Harry's Humdingers           |     341|                110|
|   198| Bob Ponze              | --                           |     338|                  4|
|   199| Lisa Filippini         | --                           |     336|                 57|
|   200| Alex Mendes da Costa   | Rumors Never Die             |     335|                 19|
|   201| Fernando Reyes         | --                           |     335|                 69|
|   202| David Norris           | House of Ginger              |     335|                 77|
|   203| Dallas Emmett          | --                           |     334|                  2|
|   204| Greg Weed              | Hole in the Wall Bangers     |     333|                 62|
|   205| Patrick Hunt           | --                           |     328|                 37|
|   206| Keith Deming           | Naked Lunch Nice Rack        |     324|                123|
|   207| Robin Brun             | Black Willows                |     320|                 18|
|   208| Rob Cosgriff           | Naked Lunch Nice Rack        |     316|                 57|
|   209| Miguel Chimas          | Cinch You're Down There      |     313|                 19|
|   210| Matt Paul              | Dovre & Out                  |     311|                106|
|   211| Mathieu Guglielmi      | Tandy Tokers                 |     311|                135|
|   212| Justin Nicholson       | --                           |     310|                 18|
|   213| Ana Stewart            | Wicked Bitches of the West   |     309|                 64|
|   214| Vijay Alexander        | Ginger Strokes               |     309|                 51|
|   215| Peter Lee              | Ginger Strokes               |     309|                108|
|   216| Matt Morrish           | Cafe 2 for 1's               |     306|                118|
|   217| Anthony Vasquez        | --                           |     304|                 44|
|   218| Mar Ronquillo          | Black Willows                |     303|                123|
|   219| Erik Proctor           | Cafe Cafaholics              |     300|                 76|
|   220| Cristina Urreaga       | Cinch You're Down There      |     287|                118|
|   221| Rohan Kurane           | Black Willows                |     285|                 21|
|   222| Mike Britt             | Cafe Ballbusters             |     281|                 94|
|   223| Taylor Hobbs           | --                           |     277|                 67|
|   224| Eric Marrujo           | Mix VANGIE                   |     275|                104|
|   225| Josalyn Rosen          | --                           |     274|                 27|
|   226| Milan Plevnik          | Cinchsationals               |     272|                101|
|   227| Douglas Cox            | --                           |     269|                 67|
|   228| JM Reasonda            | Mixfits                      |     267|                 82|
|   229| Chris Penska           | --                           |     264|                 12|
|   230| Bernie Herschbein      | Cafe 2 for 1's               |     261|                 64|
|   231| Matt Myers             | Ice Willows                  |     258|                 55|
|   232| Kunal Lakhan-Pa        | Naked Lunch Nice Rack        |     258|                 59|
|   233| Piroskki               | --                           |     258|                  4|
|   234| Ben Ames               | --                           |     258|                 16|
|   235| Richard Oliva          | Hole in the Wall Bangers     |     256|                 62|
|   236| Chris Peterson         | Tandy Tokers                 |     255|                 98|
|   237| Taylor PoolNarwhal M   | --                           |     250|                 33|
|   238| Tone                   | --                           |     250|                 10|
|   239| Rich Hatcher           | --                           |     249|                  3|
|   240| CJ Simmons             | --                           |     249|                 58|
|   241| Danny Joseph           | --                           |     242|                 62|
|   242| John Thong             | --                           |     242|                 75|
|   243| Charles Montague       | --                           |     238|                 44|
|   244| Steven Chamberlin      | Cinchsationals               |     238|                107|
|   245| Rudy Torres            | --                           |     238|                 13|
|   246| Caleb Christian        | Hole in the Wall Bangers     |     237|                 16|
|   247| Fiona Manzella         | --                           |     232|                 39|
|   248| Christine Concho       | --                           |     230|                 55|
|   249| John McNulty           | Lone Star Longhorns          |     229|                101|
|   250| Ian Jolly              | Hole in the Wall Howlers     |     228|                 72|
|   251| Gerald Borjas          | Hole in the Wall Howlers     |     227|                 75|
|   252| NAPPYYBOYY             | --                           |     226|                 35|
|   253| Matt Weyls             | Bare Naked 6 Holes           |     225|                 37|
|   254| Max Sanchez            | Smoke & Rumors               |     224|                 56|
|   255| Adam Bowley            | Mix VANGIE                   |     224|                 73|
|   256| Tim Doyle              | Bare Naked 6 Holes           |     222|                 85|
|   257| Keelin Ingoldsby       | Harry's Humdingers           |     222|                 13|
|   258| Ninad Desei            | Pilsner Penguins             |     217|                 75|
|   259| Michael Moreno         | Mix VANGIE                   |     216|                 95|
|   260| Rocel Lhai             | Ginger Strokes               |     214|                 82|
|   261| Hajime Miyazaki        | --                           |     212|                 42|
|   262| Lasalete Sousa         | Wicked Bitches of the West   |     210|                 38|
|   263| Rick Potts             | Cinch Phoenix                |     209|                 80|
|   264| Les Mullin             | --                           |     208|                 72|
|   265| Troy Brunet            | Hole in the Wall Bangers     |     206|                106|
|   266| Dominik Bolton         | --                           |     203|                 58|
|   267| Nick Weyls             | --                           |     201|                  4|
|   268| Keith Neal             | Ginger Strokes               |     201|                 42|
|   269| Paul Landholt          | --                           |     200|                 46|
|   270| Evan                   | --                           |     196|                 34|
|   271| Ari Fehrenkamp         | Wicked Bitches of the West   |     195|                 86|
|   272| Lawrence Mortensen     | Pilsner Innmates             |     194|                100|
|   273| Chad Sloane            | Black Willows                |     193|                  4|
|   274| Jules Tanseco          | Harry's Hooligans            |     190|                 10|
|   275| Shepard Gault          | Lone Star Rebels             |     190|                 12|
|   276| Carlos Rodrigues       | Lucky Horseshoe Glue Factory |     187|                 14|
|   277| Zoe Hagfeldt           | --                           |     186|                 29|
|   278| Radley Roberts         | Cinch Pack                   |     186|                 84|
|   279| Frank Augustine        | --                           |     184|                 53|
|   280| Buddy Jacques          | Hole in the Wall Bangers     |     182|                109|
|   281| Jill Cheviron          | --                           |     178|                 57|
|   282| John Larkin            | Harry's Humdingers           |     175|                 23|
|   283| Jason Valdez           | --                           |     175|                  4|
|   284| Savonna Hasson         | Harry's Hooligans            |     174|                 83|
|   285| Jonathen Diego         | House of Ginger              |     173|                 20|
|   286| Charles Hancock        | Harry's Humdingers           |     170|                 71|
|   287| Tommy Torstenson       | --                           |     164|                 69|
|   288| Tetyana Swan           | Lone Star Longhorns          |     163|                 19|
|   289| Aaron Woodworth        | --                           |     163|                 59|
|   290| Jonathan               | --                           |     163|                 42|
|   291| Dorien Lezinski        | House of Ginger              |     161|                 20|
|   292| Edmond                 | --                           |     160|                 22|
|   293| Violet Moyer           | --                           |     157|                 23|
|   294| Andy Cunningham        | Lone Star Longhorns          |     154|                123|
|   295| Michael Bouey          | Lone Star Longhorns          |     152|                106|
|   296| Brenden Chadwick       | Mixfits                      |     151|                 43|
|   297| Brandi Alexandra       | --                           |     151|                  5|
|   298| Roy Perkel             | Pilsner Innmates             |     147|                 72|
|   299| John Putnam            | Cafe 2 for 1's               |     146|                112|
|   300| Sunny D                | --                           |     143|                 42|
|   301| Jen Pearson            | Wicked Bitches of the West   |     142|                 54|
|   302| David Evans            | --                           |     138|                 34|
|   303| Jerry Ervin            | Cafe Strikes Again           |     137|                 82|
|   304| Chris Grimm            | --                           |     137|                 58|
|   305| Pujan Desei            | --                           |     137|                 32|
|   306| Antonio Herrera        | Cafe Cafaholics              |     135|                 15|
|   307| Siedra Loeffler        | Wicked Bitches of the West   |     130|                 48|
|   308| Robbie Connelly        | Lone Star Rebels             |     130|                 65|
|   309| Brian Johnson          | Black Willows                |     129|                 62|
|   310| Brian Martin           | --                           |     128|                 29|
|   311| Malcolm King           | Harry's Hooligans            |     125|                 53|
|   312| Trish Gardner          | Cafe 2 for 1's               |     124|                113|
|   313| Lorax                  | Lone Star Rebels             |     116|                 87|
|   314| Kurt Weitzmann         | Black Willows                |     116|                 18|
|   315| Riva T                 | --                           |     116|                 38|
|   316| Killer K               | --                           |     113|                 94|
|   317| Sharon Yencharis       | Lone Star Rebels             |     113|                 11|
|   318| Zeon Kitchiner         | Hole in the Wall Howlers     |     113|                 89|
|   319| Brandon Blackmon       | --                           |     105|                 10|
|   320| Stefani Furness        | --                           |      97|                 39|
|   321| Mike Myers             | --                           |      94|                 33|
|   322| Levon Sanossian        | Lone Star Rebels             |      88|                 26|
|   323| Austin Day             | Cafe 2 for 1's               |      84|                 18|
|   324| Ashley Miller          | --                           |      84|                 10|
|   325| Brady Ralston          | Lone Star Longhorns          |      63|                 14|
|   326| Aja Cayetano           | Harry's Humdingers           |      59|                 57|
|   327| Jacob Messing          | --                           |      55|                 21|
|   328| Patrick Phillips       | House of Ginger              |      48|                 62|
