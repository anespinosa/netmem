# Camp 92 network

Interactions among the 18 people of a three-week course, the 1992 NSF
Summer Institute on Research Methods in Cultural Anthropology, 14
participants and 4 instructors. The data were collected by Steve
Borgatti, Russ Bernard, Bert Pelto and Gery Ryan. At the end of the
second week, each person sorted cards with the names of the others by
how much they had interacted with them since the beginning of the
course.

## Usage

``` r
data(campnet)
```

## Format

A list with:

- network:

  An 18 x 18 binary matrix, known as Campnet, where `network[i, j] = 1`
  if `j` is among the three people with whom `i` interacted most.

- ranks:

  An 18 x 18 matrix with the rank that each respondent (row) gave to
  each of the others (1 = most interaction, 17 = least).

- attributes:

  A data frame with the attributes of the people:

## Source

UCINET datasets, Camp 92
(<https://sites.google.com/site/ucinetsoftware/datasets/camp-92>).

Borgatti, S. P., Everett, M. G. and Johnson, J. C. (2018). Analyzing
Social Networks. Second edition. SAGE.

Everett, M. G. and Borgatti, S. P. (2026). Alter composition with
overlapping group memberships. Social Networks, 85, 80–88.
[doi:10.1016/j.socnet.2025.12.001](https://doi.org/10.1016/j.socnet.2025.12.001)

## Details

The network gives the betweenness scores reported by Everett and
Borgatti (2026: Table 5).
