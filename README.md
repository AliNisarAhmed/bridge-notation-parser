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

