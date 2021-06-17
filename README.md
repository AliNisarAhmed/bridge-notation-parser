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

#####

- Title of an article about the deal and author's name, separated by colon
- If there is a colon in the title, it is escaped by double colon (::) (applies to labels TLES only)

Examples:

- [x] - T Greed Costs Contract:Richard Pavlicek
- [x] - T :Joe Blow (untitled, written by Joe Blow)
- [ ] - T Preempts:: Fact or Fiction?:Marty Bergen