#let title = [$title$]
#let date = toml(bytes("date = $date$")).date
#let months = ("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
#let show-date = date => [#date.day() #months.at(date.month() - 1) #date.year()]
#set document(
  title: title,
  author: "Bulletin author",
  date: date,
)
#set text(lang: "en")
#set page(
  paper: "a4",
  number-align: right,
)
#set par(justify: true)
#set text(
  font: "IBM Plex Sans",
  size: 11pt,
  hyphenate: true,
)

#set page(numbering: "1")
#show heading.where(level: 1): it => [
  #it
]

#block(
  breakable: false,
  [
    #par(text([*#title*], size: 36pt))
    #v(0.8em)
    #par([
      This is an example bulletin, compiled with Bully \
      Bulletin date: #show-date(date)
    ])
  ]
)

#outline()

$for(contributions)$
= $it.title$
_$it.author$_ (#show-date(toml(bytes("date = $it.date$")).date))

$it.body$
$endfor$