# Lazega law firm

The data is part of a study carried out in a Northeastern US corporate
law firm, referred to as SG&R, 1988-1991 in New England. The data were
collected by Emmanuel Lazega (2001). This is a multiplex network of
attorneys (partners and associates) of this firm. It includes (among
others) measurements of networks among the 71 attorneys (partners and
associates) of this firm, i.e. their strong-coworker network, advice
network, friendship network, and indirect control networks.

## Usage

``` r
data(lazega_lawfirm)
```

## Format

Three 71 X 71 matrices:

- cowork:

  A matrix indicating "cowork" relationships among the attorneys, based
  on their work together on cases and other professional activities.

- advice:

  A matrix indicating "advice" relationships, where the matrix shows to
  whom attorneys went for professional advice.

- friends:

  A matrix indicating "friends" relationships, showing social
  connections outside of work.

- attributes:

  A data frame with attributes of the actors, including:

## Source

Lazega, Emmanuel (2001) The Collegial Phenomenon: The Social Mechanisms
of Cooperation Among Peers in a Corporate Law Partnership. Oxford
University Press.
