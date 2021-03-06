# Bridge Notation Parser
A parser for various notations used for recording, archiving and exchanging bridge deals, starting with RPN (Richard's Bridge Notation)


## Richard's Bridge Notation

This notation is described in detail on [Richard Pavlicek's website](http://www.rpbridge.net/7a12.htm), major points repeated here for ease of reference.

### Labels & Structure

- Each line starts with a one character **label**
- A blank line marks the end of each record

Following are the 16 basic labels

```
D - Date and Time
T - Title and Author
L - Location
E - Event or Main Heading
S - Session, Stage or Subheading
F - Form of Scoring
K - Team Names and Carryovers
N - Names of Players and Room or Table
B - Board Number
H - Hands (cards held by each player)
A - Auction (dealer, vul and call sequence)
C - Contract and Declarer
P - Play sequence
R - Result (tricks won) and Score
M - Makes at Double-Dummy
I - Item List
```

- any single digit (0-9) may be used to label an **explanatory note**.
  - This pertains to the A (auction) label unless the P (play) label appears first, then it pertains to the play

```
1 forcing one round
```

-  free text may be entered at any place using the following structure
  -  Each text paragraph must begin on a new line and start with a left curly brace. The paragraph continues, using multiple lines if desired (but no blank lines) until terminated by a right curly brace at the end of a line.

```
{Text paragraph}
```

---
---

### Detailed explanation of Labels

---
---

#### D = Date and Time

- Max len: 18
- Valid char: 0123456789CG:?

##### Specs

- The date is given in fixed format (yyyymmdd) without separators.
- Unknown parts may be filled with a question mark (?), and mmdd or dd may be truncated.
- The letter C may be used after the year (or year and month) to mean “circa” (approximation).
- Optionally, a second date may be given (yyyymmdddd) to indicate a span (if lower wraps to next month).
- The date (if not a span) may be followed by a colon (:) and the time in 24-hour fixed format (hhmmss), of which mmss or ss may be truncated.
- Midnight may be indicated at the end of a day as 24 or at the start as 00.
- Date and time are local to the location (L label) unless followed by the letter G (GMT)

Examples:

- [x] - D 19980131 (January 31, 1998)
- [x] - D 199709 (some date in September, 1997)
- [ ] - D 1986120911 (December 9-11, 1986)
- [ ] - D 1955C (not sure but about 1955)
- [ ] - D 20010704:1930 (July 4, 2001, 7:30 pm)
- [ ] - D 20120428:072315G (to exact second GMT)

---

#### T = Title and Author

- Max len: 128
- Valid char: (all)

##### Spec

- Title of an article about the deal and author's name, separated by colon
- If there is a colon in the title, it is escaped by double colon (::) (applies to labels TLES only)

Examples:

- [x] - T Greed Costs Contract:Richard Pavlicek
- [x] - T :Joe Blow (untitled, written by Joe Blow)
- [ ] - T Preempts:: Fact or Fiction?:Marty Bergen

---

#### L = Location

- Max len : 128
- Valid char: (all)

##### Spec

- represents where the deal occurred
- Maybe a single string without a separator (:)
- If (:) is present, the more general location is given first

##### Examples

- [x] - L Fort Lauderdale FL
- [x] - L Toronto ON:Royal York Hotel
- [x] - L Valkenberg NL:Holland Casino
- [ ] - L Edmonton::AB:Royal Alex

---

#### E = Event

- Max len : 128
- Valid char: (all)

##### Spec

- The name of the event, or a main heading such as a catalog of lesson deals.
- This can be a single string, or two strings separated by a colon (:). If two strings, the more general one should be given first

##### Examples

- [x] - E 1999 Grand National Teams
- [x] - E Southeastern Regional:Flight A Open Pairs
- [x] - E Beginning Bridge:Lesson 11

---

#### S = Session, Stage or Subheading

- Max len: 128
- Valid char: (all)

##### Spec

- The part of an event, or a subheading such as a lesson topic.
- A number alone indicates the session (typically in a pair event).
- Allowed abbreviations are
  - F = Final,
  - P = Playoff,
  - S = Semifinal,
  - Q = Quarterfinal,
  - I = Initial stage (Qualifying).
  - R16 = Round of 16 (likewise for any number) or
- A separator (:) allows further clarification, where a number alone indicates the segment or round.

##### Exampes

- [x] - S 2 (Session 2)
- [x] - S S:3 (Semifinal, Segment 3)
- [x] - S R32:1 (Round of 32, Segment 1)
- [x] - S I:12 (Qualifying, Round 12)
- [x] - S Slam Bidding:Blackwood

---

#### F = Form of Scoring

- Max len: 64
- Valid char: (all)

##### Spec

- The kind of scoring in effect.
- This can be a word, phrase or one-letter abbreviation:
  - I = IMPs
  - B = Board-a-match (aka Point-a-board)
  - T = Total points
  - X = IMP pairs (aka Cross-IMPs)
  - M = Matchpoints
  - N = Instant matchpoints
  - R = Rubber bridge
  - C = Chicago
  - A = Cavendish (same as Chicago but second/third dealer nonvulnerable) or
  - P = Plus-or-fishfood (my silliness for the side that goes plus wins; honors do not count).
- Note that abbreviations I, B and T imply team competition; all others imply pair competition.

- Optionally, this may be followed by a separator (:) and modifying information. The word “old” is assumed to mean “prior to the scoring changes of 1987.” A year may be entered to indicate the scoring scale in use at that time.

- [x] - F I (IMPs)
- [x] - F N (Instant matchpoints)
- [x] - F X:Butler (IMP pairs, Butler type)
- [x] - F I:1952 (IMPs, using scale from 1952)
- [x] - F M:old (Matchpoints before 1987 changes)
- [x] - F R (Rubber bridge)
- [x] - F C:NS 60 (Chicago, N-S have 60 partscore)
- [x] - F A:no honors or partscore carryover

---

#### K = Team Names and Carryovers

- Max len: 84
- Valid char: (all)

##### Spec

- The names of two teams in a bridge match, which may be a captain’s name, country name, etc., separated by a colon.
- The team given first is North-South for the first of each pair of identical deals.

- Optionally, this may be followed by a colon and carryover for Team 1; then another colon and carryover for Team 2.
- Carryovers may be IMPs, matchpoints or total points, depending on form of scoring.
- Fractional carryovers are allowed and must be expressed in hundredths (two decimal places).
- Negative carryovers are allowed and are useful to deduct a penalty assessed in the current segment (e.g., slow play)

##### Examples

- [x] - K Nickell:Schwartz
- [x] - K Italy:USA1:999:22 (Italy leads by 977)
- [x] - K Meckstroth's Marauders:Rodwell's Rockets
- [x] - K Iceland:Bulgaria:76.33:91.50
- [ ] - K France:Spain::-5 (Spain penalized 5)
- [ ] - K GIB:Jack:99-8 (GIB up 99 but penalized 8)


---

#### N = Names of Players

Max len: 128
Valid char: (all)

##### Spec

- The names of the players in the specific sequence: North+South:West+East.
- Partners are separated by a plus sign (+) and pairs by a colon (:).
- West is given before East to keep the left-right relationship.
- Names may be first or last names (or both) or even non-human names such as computers.
- If a name is unknown, just leave it empty.
- This may be followed by a second colon and the room (C = Closed, O = Open) or table number (1-250), and optionally a third colon followed by any other information.

##### Examples

- [x] - N Wolff+Hamman:Stansby+Martel
- [x] - N Wolff+Hamman:Stansby+Martel:O:US Open
- [x] - N Wolff+Hamman:Stansby+Martel:6:US National Trials
- [x] - N Soloway (Soloway was North)
- [x] - N :Jan+Joe:O (Jan and Joe E-W in Open Room)
- [x] - N :+Norman Kay:6 (Norman Kay East Table 6)
- [x] - N +Roth:GIB 4.0 (Roth South, computer West)

---

#### B = Board Number

Max len: 24
Valid char: (all)

##### Spec

- The board or sequence number of the deal — usually just a plain number, but any notation is acceptable.
- Optionally, this may be followed by a separator (:) and a second number or notation, such as the section letter in a large tournament.

##### Examples

- [x] - B 7 (Board 7)
- [x] - B 15:C (Board 15, Section C)
- [x] - B 10:7X01 (Board 10, and RP numbering)
- [x] - B 9:97-7-542 (Board 9, and OKbridge number)
- [x] - B xy (Board xy)

---

#### H = Hands

Max len: 71
Valid char: 23456789TJQKNSEWx:.;?

##### Spec

- The cards for each player, starting with the direction given and moving clockwise.
- Each hand begins with a colon (:).
- Suit holdings are given in descending order (SHDC) separated by a dot (.) and cards in descending rank (AKQJT98765432).

- Hands do not necessarily have to form a complete deal.
- Empty or incomplete hands are fine as long as no card is used twice, and no hand has more than 13 cards.
- This is useful for storing single hands, paired hands (e.g., only E-W for bidding practice), bridge endings, single-suit layouts, etc.

- For full deals, the fourth hand is redundant and may be omitted, but the initial colon must be present (else fourth hand would be assumed not to exist).
- In other words, three full hands followed by a colon means “fourth hand gets the rest.”

#### Pseudo Cards (Not implemented)

- Cards may be entered as 'x' to designate one of the lowest unassigned cards in that suit.

- Cards may also be entered as '?' to designate an unknown rank. This might be useful for presentation purposes, but of course precludes any card play.


##### Examples

- [x] - H W:873.A6.KT864.KQ8:96.T54.97.AJ9643:T542.K93.AQ53.52:
- [x] - H S:9.AK6.AKT982.K87:K7654.J73.Q65.T6:QT2.T94.J4.AQ953:
- [x] - H N:AKQJT98765432...:.AKQJT98765432..:..AKQJT98765432.:

- [x] - H N:AKQ72..AKQ72.753::.AKQ72.753.AKQ72

- [x] - H E:T4.8642.AKT8.K65 (only the East hand)

- [x] - H W:K9.K9.9:3.A3.Q.4:Q82..J.A:A7.7.K8 (Note that West and South are void in clubs, and East is void in hearts. )

- [x] - H W:43:KJ2:Q976:AT85 (only the spade suit)

- [x] - H W:A875.632.76.8643;Q632.Q87.J842.Q2:KJT4.KJT4.AQ3.AJ; (North & South hands hidden)
- [x] - H W:A8765.QT.K9.AT87;J42.AJ7632.J.632;QT3.85.Q86.KQJ54; (All except West hidden)

(Same deals with different starting player)
- [x] - H E;K5.T.KQJT98.KQJT:A876.A2.765.A876;32.KQJ9876543..9:
- [x] - H S:A876.A2.765.A876;32.KQJ9876543..9:QJT94..A432.5432;
- [x] - H W;32.KQJ9876543..9:QJT94..A432.5432;K5.T.KQJT98.KQJT:
- [x] - H N:QJT94..A432.5432;K5.T.KQJT98.KQJT:A876.A2.765.A876;


----


#### A = Auction

Max len: 71
Valid char: 123456789ABCDEHNPRSWXYZ:!?*^

##### Spec

- The dealer (NESW) and vulnerability (ZNEB?) then each bidding round is preceded by a colon.
- No other separators are used in the sequence of calls.
-  Normal calls are indicated as:
   -  P = Pass
   -  X = Double
   -  R = Redouble
   -  1C = One Club
   -  3N = Three Notrump
   -  A = All Pass.
-  Also, the letter Y may be used as a query point (multiple times allowed) to mean “Your call?” as in a bidding quiz or poll.
-  Partial or incomplete auctions are allowed (just stop writing).
-  Complete auctions must end in A or PPP.
-  Any call (except A and Y) may by followed by a notation:
   -  ! = good
   -  ? = poor
   -  !! = very good
   -  ?? = very poor
   -  !? = speculative
   -  ?! = questionable
   -  * = conventional (no explanation follows)
   -  ^1 = see Note 1.

##### Examples

- [x] - A SZ:1SP2SP:4SA (South deals, none vul)
- [x] - A WE:A (West deals, E-W vul, passed out)
- [x] - A NB:1SXY (your call as South?)
- [x] - A S?:1SP2CP:2D (unknown vul, call sequence)
- [x] - A EB:3C*P3N?P:P??X!RA (various annotations)
- [x] - A EB:3CP3N?P:PX!R^1A
        1 Lost his mind

---

#### C = Contract & Declarer

Max len: 24  Valid char: 0123456789CDEHMNRSWX:

##### Spec

- The contract consists of level, strain, jeopardy and goal (in that order) but only the strain is required.
  - Level is a digit (1-7);
  - strain is a letter (CDHSN);
  - jeopardy is a letter (XR);
  - and goal is a number of tricks to win (1-13) or the letter M (maximum).

- Stating a goal is only necessary for endings (without a level) but is useful to show a true objective, as in a deliberate sacrifice bid.

- Contract must be followed by a colon (:) and the declarer (NESW).
- If the first leader is not to the left of declarer (as for an ending) append a second colon (:) and the first leader (NESW).

##### Examples

- [x] - C 5D:N (Five diamonds, North declarer)
- [x] - C 6NR:E (Six notrump rdbld, East declarer)
- [x] - C 7CX:N (7 Clubs Doubled, North declarer)
- [x] - C 4SX8:S (true goal to win 8 tricks)
- [x] - C H6:S:S (Hearts win 6, by S and S leads)
- [x] - C CM:W (Clubs win max, by W and N leads)

---

#### P = Play

Max len: 128 Valid char: 123456789ACDHJKQSTY:;+-~.!?*^

##### Spec

- Represents the play sequence in the exact order of cards played.
- Each trick begins with the lead and is followed by the second, third and fourth plays.
- Tricks are separated by a colon, or a semicolon to mean “plays that follow are unimportant or trivial.”
- The play can end at any time (including mid-trick); just stop writing.

- Plays are shown by suit and rank, except when following to the suit led, then only the rank is given.
- This method not only saves keystrokes but should be the universal norm, as discards and ruffs stand out, making it easier to follow the play.

- Any play may be followed by a notation:
  - ! = good
  - ? = poor
  - !! = very good
  - ?? = very poor
  - !? = speculative
  - ?! = questionable
  - * = conventional (no explanation follows)
  - ^1 = see note (1-9 allowed)


##### Pseudo Plays

- The ability to denote plays as insignificant can be useful.
  - A minus sign (-) indicates the lowest unplayed card;
  - so does a tilde (~) although the choice of suit is significant;
  - a plus sign (+) means the highest unplayed card;
  - and a dot (.) means an immaterial discard.

To illustrate, suppose I want to diagram the play of this suit, say spades: North A-J-8-4-3-2, South K-10-5, West Q-9-7-6 and East void. With South on lead the first two tricks would be:

- [x] - P SK--.:STQA.

I could have written ‘62’ instead of the two minus signs, but as shown it conveys that these plays are insignificant (unlike the second trick). Poor East has no cards, so the dots represent his irrelevant discards. Note that to run this suit, I must return to the South hand in another suit to finesse again. Enter the suitless trick, which has three possibilities (-+--, --+-, ---+) with the plus sign denoting the hand relative to leader to which the lead will be transferred. Thus, the essential play of this suit can be fully described by:

- [x] - P SK--.:STQA.:--+-:S578

Another pseudo play, analogous to the A (auction) label, is the letter Y to mean “Your play?” but it can only be the last play. This is useful for quizzes. (NOT IMPLEMENTED)

##### Examples

- [x] - P SK54T:SA87H3:HA245:HKQ8J
- [x] - P HQ*3J2:HK47A:DJA53:HT68C7^1
- [x] - P HQ*3J2:HK47A:DJA53:HT68C7^1
        1 subtle falsecard

---

#### R = Result & Score

Max len: 16  Valid char: 0123456789P:+-=.

##### Spec

- The result is the number of tricks won by declarer, or the letter P if passed out.
- This may be followed immediately by a sign (+/-) and the actual raw score.

- Optionally, this may be followed by a separator (:) and the effective score.
- This may be
  - an equal sign (=) to indicate a push or tie,
  - a sign (+/-) and a number of IMPs (+1 or -1 for board-a-match),
  - or a plain number to indicate a matchpoint percentage.

- Decimal fractions of two places (hundredths) are allowed.

- All scores (raw or effective) are relative to North-South.

##### Examples

- [x] - R 11 (declarer won 11 tricks)
- [ ] - R 10+620 (declarer won 10 tricks, NS +620)
- [ ] - R 8-300:+4 (8 tricks, EW +300, NS +4 IMPs)
- [ ] - R P:-2 (passed out, EW won 2 IMPs)
- [ ] - R 12-980:-11 (12 tricks, EW +980, +11 IMPs)
- [ ] - R :66.67 (result unknown, NS 66.67 percent)
- [ ] - R 5:-1 (5 tricks won, EW won board if BAM)
- [ ] - R +2140:+12.29 (NS +2140 and +12.29 IMPs)
- [ ] - R 7-750:7 (7 tricks, EW +750, 93 percent)
- [ ] - R 9-100:= (9 tricks, EW +100, tie board)

