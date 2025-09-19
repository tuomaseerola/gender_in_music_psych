// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}

//#assert(sys.version.at(1) >= 11 or sys.version.at(0) > 0, message: "This template requires Typst Version 0.11.0 or higher. The version of Quarto you are using uses Typst version is " + str(sys.version.at(0)) + "." + str(sys.version.at(1)) + "." + str(sys.version.at(2)) + ". You will need to upgrade to Quarto 1.5 or higher to use apaquarto-typst.")

// counts how many appendixes there are
#let appendixcounter = counter("appendix")
// make latex logo
// https://github.com/typst/typst/discussions/1732#discussioncomment-11286036
#let TeX = {
  set text(font: "New Computer Modern",)
  let t = "T"
  let e = text(baseline: 0.22em, "E")
  let x = "X"
  box(t + h(-0.14em) + e + h(-0.14em) + x)
}

#let LaTeX = {
  set text(font: "New Computer Modern")
  let l = "L"
  let a = text(baseline: -0.35em, size: 0.66em, "A")
  box(l + h(-0.32em) + a + h(-0.13em) + TeX)
}

#let firstlineindent=0.5in

// documentmode: man
#let man(
  title: none,
  runninghead: none,
  margin: (x: 1in, y: 1in),
  paper: "us-letter",
  font: ("Times", "Times New Roman"),
  fontsize: 12pt,
  leading: 18pt,
  spacing: 18pt,
  firstlineindent: 0.5in,
  toc: false,
  lang: "en",
  cols: 1,
  numbersections: false,
  numberdepth: 3,
  first-page: 1,
  suppresstitlepage: false,
  doc,
) = {

  if suppresstitlepage {counter(page).update(first-page)}

  set page(
    margin: margin,
    paper: paper,
    header-ascent: 50%,
    header: grid(
      columns: (9fr, 1fr),
      align(left)[#upper[#runninghead]],
      align(right)[#context counter(page).display()]
    )
  )
  

  

 

  set table(    
    stroke: (x, y) => (
        top: if y <= 1 { 0.5pt } else { 0pt },
        bottom: .5pt,
      )
  )

  set par(
    justify: false, 
    leading: leading,
    first-line-indent: firstlineindent
  )

  // Also "leading" space between paragraphs
  set block(spacing: spacing, above: spacing, below: spacing)

  set text(
    font: font,
    size: fontsize,
    lang: lang
  )
  
  show link: set text(blue)
  show "al.'s": "al.\u{2019}s"

  show quote: set pad(x: 0.5in)
  show quote: set par(leading: leading)
  show quote: set block(spacing: spacing, above: spacing, below: spacing)
  // show LaTeX
  show "TeX": TeX
  show "LaTeX": LaTeX

  // format figure captions
  show figure.where(kind: "quarto-float-fig"): it => block(width: 100%, breakable: false)[
    #if int(appendixcounter.display().at(0)) > 0 [
      #heading(level: 2, outlined: false)[#it.supplement #appendixcounter.display("A")#it.counter.display()]
    ] else [
      #heading(level: 2, outlined: false)[#it.supplement #it.counter.display()]
    ]
    #align(left)[#par[#emph[#it.caption.body]]]
    #align(center)[#it.body]
  ]
  
  // format table captions
  show figure.where(kind: "quarto-float-tbl"): it => block(width: 100%, breakable: false)[#align(left)[
  
    #if int(appendixcounter.display().at(0)) > 0 [
      #heading(level: 2, outlined: false, numbering: none)[#it.supplement #appendixcounter.display("A")#it.counter.display()]
    ] else [
      #heading(level: 2, outlined: false, numbering: none)[#it.supplement #it.counter.display()]
    ]
    #par[#emph[#it.caption.body]]
    #block[#it.body]
  ]]
  
    set heading(numbering: "1.1")
    
    show heading: set text(size: fontsize)


 // Redefine headings up to level 5 
  show heading.where(
    level: 1
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(center)
    #if(numbersections and it.outlined and numberdepth > 0 and counter(heading).get().at(0) > 0) [#counter(heading).display()] #it.body
  ]
  
  show heading.where(
    level: 2
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(left)
    #if(numbersections and it.outlined and numberdepth > 1 and counter(heading).get().at(0) > 0) [#counter(heading).display()] #it.body
  ]
  
  show heading.where(
    level: 3
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(left)
    #set text(style: "italic")
    #if(numbersections and it.outlined and numberdepth > 2 and counter(heading).get().at(0) > 0) [#counter(heading).display()] #it.body
  ]

  show heading.where(
    level: 4
  ): it => text(
    weight: "bold",
    it.body
  )

  show heading.where(
    level: 5
  ): it => text(
    weight: "bold",
    style: "italic",
    it.body
  )
  
  

  if cols == 1 {
    doc
  } else {
    columns(cols, gutter: 4%, doc)
  }
  



}


#show: document => man(
  runninghead: "GENDER IN MUSIC PSYCHOLOGY",
  lang: "en",
  numberdepth: 3,
  document,
)

\
\
#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Gender Distribution of Authors in Music Psychology
]
)
]
#set align(center)
#block[
\
Tuomas Eerola#super[1] and Anna Czepiel#super[2]

#super[1];Department of Music, Durham University

#super[2];Department of Psychology, University of Toronto Mississauga

]
#set align(left)
\
\
#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Author Note
]
)
]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Author roles were classified using the Contributor Role Taxonomy (CRediT; https:\/\/credit.niso.org/) as follows: #emph[Tuomas Eerola];#strong[: ];conceptualization, methodology, formal analysis, and writing -- original draft. #emph[Anna Czepiel];#strong[: ];data curation, formal analysis, and writing -- original draft

Correspondence concerning this article should be addressed to Tuomas Eerola, Department of Music, Durham University, Palace Green, Durham DH1 3DA, United Kingdom, Email: #link("mailto:tuomas.eerola@durham.ac.uk")[tuomas.eerola\@durham.ac.uk]

#pagebreak()

#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Abstract
]
)
]
#block[
Academia suffers from various issues regarding equity. One aspect is gender inequality in academic authorship, where many studies show that women are often underrepresented. Due to authorship differences across disciplines, the goal of this study is to provide a current overview the gender distribution of authorship within music psychology. A total of 3,373 papers published between 2000 and 2025 across five core music psychology journals were analyzed with respect to author gender and authorship role (first, last, solo, coauthor). In addition, we explored gender patterns related to citations, open-access publishing, and keywords. Overall, female authors were in the minority, accounting for 40.2% of all authors. However, the distribution of authorship roles diverged notably from this overall proportion: women were more likely to be first authors (OR = 1.41), equally likely to be solo authors (odds ratio, OR = 1.11) or coauthors (OR = 1.00), but less likely to be last authors (OR = 0.73). These patterns have become more pronounced over the past 15 years. No substantial gender differences in citation counts were observed. Analyses of geographical variation revealed clear cross-country differences, potentially reflecting both the status of women and the historical development of the discipline in particular regions. Finally, an examination of keywords suggested broad thematic preferences differing by gender. Although this work overall shows that the field of music psychology is more balanced than some other (STEM) fields, it provides a picture that hopefully inspires discussions and initiatives to further promote fairness and equity in academia.

]
#emph[Keywords];:gender, transparency, music psychology, equality, meta-research
#pagebreak()

#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Gender Distribution of Authors in Music Psychology
]
)
]
= Introduction
<introduction>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
In recent years, meta-scientific attention has increasingly turned toward music psychology, with a focus on how WEIRD (#link(<ref-jakubowski2024>)[Jakubowski et al., 2025];) and transparent (#link(<ref-eerola2023transparency>)[Eerola, 2024];) the discipline is. In this study, we examine in particular an indicator of equity within the field: gender equality in authorship. Authorship visibility is critical for academic career progression, influencing recognition, funding, and advancement opportunities. Disparities in gender distribution may signal structural barriers and disciplinary cultures that constrain the diversity of contributors and hinder optimal knowledge production (#link(<ref-ni2021gendered>)[Ni et al., 2021];). Establishing a clear understanding of gender representation in music psychology is therefore essential for supporting a more inclusive and sustainable academic community. Previous studies across various disciplines---including psychology---have consistently revealed persistent gender inequalities in authorship, though the extent of these disparities varies by field (#link(<ref-gonzalez2020women>)[González-Alvarez & Sos-Peña, 2020];; #link(<ref-rock2021quantifying>)[Rock et al., 2021];; #link(<ref-shah2021gender>)[Shah et al., 2021];; #link(<ref-son2022scientific>)[Son & Bell, 2022];).

Academia suffers various equity issues, impacting productivity, innovation, and job satisfaction in the workplace, which-in turn-hugely impact academic progress. Although there are several aspects, the focus of this paper is gender inequity. Women are more at a disadvantage than men, with evidence suggesting that women are less present in positions of power. For instance, in the UK in 2016-17, 24.6% of professors are women (#link(<ref-bhopal2021competing>)[Bhopal & Henderson, 2021];) when compared to the 44% of all grades in the UK academia (#link(<ref-harris2025gender>)[Harris et al., 2025];). A similar situation holds for Italy (24% female professors) (#link(<ref-Filandri03082021>)[Filandri & Pasqua, 2021];), and to some extent for the United States (14%) (#link(<ref-spoon2023>)[Spoon et al., 2023];). A pay gap between men and women in academia is also significant, (e.g., female-male wage ratio is 0.85 in 2020 in the United Kingdom) (#link(<ref-quadlin2023higher>)[Quadlin et al., 2023];). In a "publish or perish" culture of science and academia (#link(<ref-kiai2019protect>)[Kiai, 2019];), one crucial aspect to focus on is authorship. The number and quality (e.g., journal reputation) of authorships an individual determines crucial career development such as grant funding and future (permanent) positions.

Research across several scientific disciplines show that women tend to be less represented as authors compared to men (#link(<ref-banks2025women>)[Banks et al., 2025];; #link(<ref-son2022scientific>)[Son & Bell, 2022];). As the topics, approaches, and methods attract differential interest from men and women, it is important to recognise that fields of scholarship and academic disciplines vary considerably in terms of gender distribution (#link(<ref-huang2020>)[Huang et al., 2020];). For example, a large survey of authors by González-Alvarez and Sos-Peña (#link(<ref-gonzalez2020women>)[2020];) showed that the hard sciences (N = 119,592) had the lowest prevalence of women (14.8%), whereas in the biological and social sciences (N = 262,122) the proportion was substantially higher (43.3%), but not equal. In psychology, which is perhaps the closest benchmark for the present focus on music psychology, female authors accounted for 45.2% of the sample (N = 90,067). Encouragingly, however, studies looking at various year ranges between 1960 until 2024 show a general trend that the ratio of women to men in authorships is improving (#link(<ref-gonzalez2020women>)[González-Alvarez & Sos-Peña, 2020];; #link(<ref-ioannidis2023gender>)[Ioannidis et al., 2023];; #link(<ref-jemielniak2025gender>)[Jemielniak & Wilamowski, 2025];; #link(<ref-sanchez2024analysis>)[Sánchez-Jiménez et al., 2024];). Yet, there is evidence that this improvement is plateauing (#link(<ref-jemielniak2025gender>)[Jemielniak & Wilamowski, 2025];). Further, although women are more likely to be first author, women are still generally unfairly underrepresented as the last author (#link(<ref-bruck2023bibliometric>)[Brück, 2023];; #link(<ref-gonzalez2020women>)[González-Alvarez & Sos-Peña, 2020];; #link(<ref-rock2021quantifying>)[Rock et al., 2021];; #link(<ref-shah2021gender>)[Shah et al., 2021];). There could be several reasons for such a disparity: two key aspects that are typically explored in such
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
papers: geographical locations and keyword/topics.

Past studies have explored the gender distribution could by due to geographical regions. For example, men out-numbered women more so in high-income compared to low-income countries (#link(<ref-ioannidis2023gender>)[Ioannidis et al., 2023];). However, there are also some unexpected trends: while female representation is lowest in countries like Latin America, Caribbean (in physics journals, #link(<ref-son2022scientific>)[Son & Bell, 2022];) and in South Korea, Japan, and Austria (in medical journals, #link(<ref-bruck2023bibliometric>)[Brück, 2023];), females were most highly represented in countries like India and Africa (#link(<ref-bruck2023bibliometric>)[Brück, 2023];; #link(<ref-son2022scientific>)[Son & Bell, 2022];). Human development indicators and gender inequality indicators have also revealed systematic inequalities in authorship. women's academic success is correlated positively with national indicators of gender equity (#link(<ref-chan2020>)[Chan & Torgler, 2020];). However, this does not seem to be as clearly reflected in authorships. Female scientific output is lower in countries ranked in lowest quartile for human development and gender equity, female scientific output is also lower in countries very high in gender equality; better equality seemed to occur in high and medium-developed countries (#link(<ref-sugimoto2015relationship>)[Sugimoto et al., 2015];). Another recent paper shows that female authorship rates are most balanced in countries with low to intermediate Human Development Index (#link(<ref-sanchez2024analysis>)[Sánchez-Jiménez et al., 2024];). Overall, while socioeconomic status of certain countries might be a reason for unbalanced female authorship (#link(<ref-ioannidis2023gender>)[Ioannidis et al., 2023];), human development might not fully explain such differences (#link(<ref-bruck2023bibliometric>)[Brück, 2023];; #link(<ref-sanchez2024analysis>)[Sánchez-Jiménez et al., 2024];; #link(<ref-son2022scientific>)[Son & Bell, 2022];), and further research is required to better understand the trends.

Another aspect that differs between distribution of gender in authorship is the research methodologies and topics. In terms of methodologies, women are are less likely to publish quantitative and experimental studies, but rather publish qualitative work (#link(<ref-ashmos2011gendering>)[Ashmos Plowman & Smith, 2011];; #link(<ref-nunkoo2020three>)[Nunkoo et al., 2020];; #link(<ref-sebo2020gender>)[Sebo et al., 2020];; #link(<ref-thelwall2019gender>)[Thelwall et al., 2019];; #link(<ref-zhang2023gender>)[C. Zhang et al., 2023];), perhaps as they demonstrate better relational skill involved in qualitative studies, like prolonged relationships and emotional ties with research participants (#link(<ref-sebo2020gender>)[Sebo et al., 2020];). Interestingly, such stereotypes occur in the opposite direction: when assessing fictive research abstracts-one with quantitative and one with a qualitative design- scientific quality of the qualitative abstract was ranked more trustworthy and accurate when assigned to a female author (#link(<ref-johansson2002gender>)[Johansson et al., 2002];). In an analyses of article keywords of medical journals, female-authored articles were more associated with keywords more related to healthcare-related themes like patient involvement, insurance, and quality-of-care compared to males (#link(<ref-bruck2023bibliometric>)[Brück, 2023];). Similarly, in computational linguistics, woman are more likely to publish on speech, social, and conversational topics than males; the latter are more likely to publish in formal mathematical, syntax, and semantic approaches (#link(<ref-vogel2012he>)[Vogel & Jurafsky, 2012];). These trends could be explained by the idea that female researchers often conduct research on societal progress, while males prefer research aimed at scientific progress (#link(<ref-zhang2021gender>)[L. Zhang et al., 2021];). Overall, research shows that women choose qualitative research on social and societal topics, while males author papers using more quantitative methods on theoretical and analytical topics.

Looking more broadly past the information within papers themselves, other research provides some further explanations for discrepancies in female/male authorships. Despite efforts to give fair credit for author contributions (#link(<ref-ni2021gendered>)[Ni et al., 2021];) using, for example, the Contributor Roles Taxonomy (CRediT) system, assigning authorship can be unclear. Women are more likely to experience authorship disagreements, for example, when to decide authorship: women prefer to discuss in earlier stages, while men choose authorship at the final stage (#link(<ref-ni2021gendered>)[Ni et al., 2021];). Another reason for the gender gap could also come from parenthood and parental leave, which account for about \~40% of gender gap in career advancement (#link(<ref-nielsen2024getting>)[Nielsen et al., 2024];). Additionally, women faculty perform more university, campus, and departmental services than men in general (#link(<ref-guarino2017faculty>)[Guarino & Borden, 2017];). Fortunately, there are strides to improve the current situation, with opinion papers discussing way to improve receipt and reporting of intellectual credit (#link(<ref-banks2025women>)[Banks et al., 2025];) as well as creating equitable environments in academic science (#link(<ref-martinez2024comparison>)[Martı́nez-Menéndez et al., 2024];). To improve academic equity issues, it is worthwhile focusing on our discipline and to assess within subdisciplines to provide more 'localised' support to improve the equity in music psychology. Indeed, the past efforts such as Anglada-Tort and Sanfilippo (#link(<ref-anglada2019visualizing>)[2019];) have already provided a current overview the publications in the music psychology field with suggestions of future directions to improve the field. Eerola (#link(<ref-eerola2023transparency>)[2024];) showcases how Open Science practices (e.g., preregistrations, sharing research materials, data, and analysis scripts) are relatively limited, and encourage authors in the field to start implementing such practices. Jakubowski et al. (#link(<ref-jakubowski2024>)[2025];) explores participant samples and musical stimuli used across a wide range of experiment to give an idea of how limited or generalizable the field is. In a similar way to these papers, the current paper aims to give a current overview of gender patterns in authorship in the field of music psychology.

== Aims
<aims>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Our aim is to find out what is the gender distribution in the specialist journals of music psychology. More specifically, we want to identify what is the proportion of different types of authorships (single, first, coauthors, and last authorships) for men and women in the published papers in the last 25 years? Are there specific patterns in the authorship distributions across countries, topics of the studies, or temporal trends?

= Methods
<methods>
== Materials and analyses
<materials-and-analyses>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We retrieved bibliographic information for all articles published between 2000 and June 2025 from five specialist journals, resulting in 3,373 unique articles: Musicae Scientiae (N = 639), Psychology of Music (N = 1,231), Music Perception (N = 675), Journal of New Music Research (N = 563), and Music & Science (N = 265). These journals have also been used in previous meta-science studies to characterise research practices in music psychology (#link(<ref-jakubowski2024>)[Jakubowski et al., 2025];). Author affiliations were extracted automatically and converted into country-level data. However, these were not manually verified for each entry, as affiliations are not always clearly matched to individual authors due to variations in reporting conventions, such as multiple or partial affiliations.

In total, the dataset included 9066 authors, of whom 5312 were unique. Author affiliations spanned 63 countries. We also extracted citation counts and Open Access status from Scopus. Information on joint first authorship was not available in the data.

Gender attribution was initially based on first names. We recognise that treating gender as a binary category is inherently problematic. Gender is a complex and multidimensional social construct. However, consistent with prior meta-science research on gender and authorship (#link(<ref-gonzalez2020women>)[González-Alvarez & Sos-Peña, 2020];; #link(<ref-ni2021gendered>)[Ni et al., 2021];; #link(<ref-rock2021quantifying>)[Rock et al., 2021];; #link(<ref-shah2021gender>)[Shah et al., 2021];; #link(<ref-son2022scientific>)[Son & Bell, 2022];; #link(<ref-wais2006>)[Wais, 2006];) and that non-binary authorship generally is less than 0.1% (#link(<ref-son2022scientific>)[Son & Bell, 2022];), we adopt a binary classification---male and female---for analytical purposes and assume that first names allow for a reasonable, though imperfect, attribution of gender. We used the #emph[genderize API] (#link(<ref-wais2006>)[Wais, 2006];), which predicts gender from first names and can be supplemented with country information derived from author affiliations to improve accuracy. This method resolved the gender of 89.3% of authors with a probability greater than 0.90. Only `89` names had a low attribution probability (\< 0.55). Unattributed cases were then checked manually, resulting in `185` manual corrections. After this process, `32` names remained ambiguous and `27` were unknown---some likely due to data entry errors in Scopus (e.g.~only initials or surnames). These `59` cases were excluded from the dataset. It is likely that the due to challenge of attributing gender accurately not all the gender attributions are correct. Although genderize.io has been shown to achieve 96.6% accuracy in a diverse multinational test database without using a country of origin information -- 98% with the information included -- it also underperforms (82% accuracy) in Asian names (#link(<ref-vanhelene2024inferring>)[VanHelene et al., 2024];). However, the error rates of these processes have been previously been shown to be non-biased, i.e.~showing similar number of mistakes for both genders (#link(<ref-sebo2021performance>)[Sebo, 2021];; #link(<ref-thelwall2019gender>)[Thelwall et al., 2019];; #link(<ref-vanhelene2024inferring>)[VanHelene et al., 2024];).

We carried out manual corrections of the gender attributions where we were familiar with the author or the database had the full first name coded with initials, or the second name used as the first name. This led to `53` corrections.

We defined four types of authorship positions: single authorship, first authorship (which does not include papers with a single author), coauthorship, and last authorship. Coauthorship includes all positions other than first and last. These positions carry different academic prestige: first authorship is typically associated with the primary contributor, while last authorship is often held by senior researchers with established reputations (#link(<ref-tscharntke2007author>)[Tscharntke et al., 2007];).

For the analyses, we analysed gender disparities in authorship by comparing the frequency of these authorship types between male and female authors. Our analysis focused on the female author proportion (FAP) and utilised odds ratios (ORs) to compare the likelihood of occupying each authorship position by gender.

= Results
<results>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Among all authors (N = 9066), 40.2% (N = 3647) were identified as female. To account for the unequal number of male and female authors across the dataset, we used odds ratios (ORs) to compare the relative likelihood of females occupying different authorship positions. We first investigate the authorship types across gender and then examine the author order and the overall number of coauthors in more detail.

== Authorship positions
<authorship-positions>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
A summary of the authorship types and positions are given in Table 1. While the single-authored papers are in minority (8.6%) in this sample overall, there does not seem to be a significant difference between men and women authoring such articles (#emph[p] = .21); 38.05% of single-authored papers are women with an odds ratio of 0.90 \[95% 0.78-1.05\] suggesting no clear gender difference despite the figures leaning towards higher frequency of male authors. The overall OR for women as first authors was 1.41 \[95% CI: 1.28--1.54\], indicating that the odds of first authorship are 41% higher for females compared to the male authors (Fisher's exact test #emph[p] \< .001). For coauthorship positions, the OR was 1.00 \[0.91--1.09\], suggesting no substantial gender difference (#emph[p] = .96). In contrast, the odds ratio for women as last authors was 0.73 \[0.67--0.81\], indicating that the odds of last authorship are 27% lower for women compared to men and significantly different (#emph[p] \< .001).

#table(
  columns: (17.91%, 10.45%, 7.46%, 13.43%, 7.46%, 20.9%, 22.39%),
  align: (left,right,right,right,right,right,right,),
  table.header([Author Type], [Female], [Male], [Female %], [OR], [OR (CI lower)], [OR (CI higher)],),
  table.hline(),
  [Single], [296], [482], [38.05], [0.90], [0.78], [1.05],
  [First], [1198], [1397], [46.17], [1.41], [1.28], [1.54],
  [Co-author], [1245], [1853], [40.19], [1.00], [0.91], [1.09],
  [Last], [908], [1687], [34.99], [0.73], [0.67], [0.81],
  [All], [3647], [5419], [40.23], [NA], [NA], [NA],
)
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
To illustrate the trends across time in the authorship positions across the gender, we looked at 5-year-windows of the authorship types across gender, shown in Figure 1. The overall numbers across the whole data are illustrated in the first column of the graph and the remaining five columns portray the chronology in 5-year windows. The higher odds for female authors at first author position seems to be established in 2010 onwards. Female first author position exhibits a positive 5-year growth rate of +11.1%. The coauthor positions for female authors are relative stable at the odds around 1, indicating relatively equal odds between female and male authors. However, the last author positions have become relatively more rare after 2010, showing statistically significantly lower odds ratios from 2010 onwards and negative 5-year growth rate (-9.5%). The odds ratio for female single authored papers hovers around 1.0, showing no meaningful trend across the time.

#figure([
#box(image("manuscript-apaquarto_files/figure-typst/fig1-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Authorship types across time.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
The variation of the number of authors across the papers is considerable. Here we have the maximum of 34 authors in a paper and a median of 2 (M=2.69, SD=1.77). Previous research has calculated the collaboration index across gender (#link(<ref-gonzalez2020women>)[González-Alvarez & Sos-Peña, 2020];), suggesting that male author tend to have larger number of coauthors (M = 1.5) than female (M = 1.34) in psychology. Here the average author numbers for male and female authors (disregarding authorship positions) indicate similar difference, higher mean author numbers for males (M = 1.96, \[1.91-2.01\]) than for females (M = 1.71, \[1.66-1.76\]), #emph[t];(4640.58) = 7.21, #emph[p] \< .001, Cohen's #emph[d] = -0.21, 95% CI \[-0.26, -0.15\]. This analysis, however, does not allow us to distinguish the authorship roles in the counts.

To illustrate how the actual number of authors in a paper impacts female first author likelihood, we calculated the odds ratios across the number of authors in paper, pooling the papers with 7 or more authors together (N=749, 8.3%). We added the single-authored papers to the plot to indicate the odds of being female to be single authors compared to all studies with multiple authors. The results are shown in Figure 2. We can observe that while female authors are not more likely to be single authors than men, they tend to be more likely to be first authors than males in papers with 2 or 3 authors; the odds ratios for first authorship position for female authors for papers with 2 authors is 1.55 \[130-1.85\], and for 3 authors the odds ratio is 1.61 \[1.35-1.92\], both significantly different from equal proportion. The last authorship positions for female authors are consequently lower, odds ratio of 0.65 \[0.54--0.77\] for 2 authors and 0.59 \[0.49--0.71\] for 3 authors, both also significant at #emph[p] \< .001 level. At four and five authors, the higher odds ratio for female is still evident (OR of 1.37 both both), but the last authorship positions are no longer relative rare for papers with 4 and 5 authors (OR 0.90 and 0.95, respectively). At six authors and above, the odds ratios indicate the same trend (higher ratio for first author, lower for last) although due to lower number of observations, these are no longer significantly different.

#figure([
#box(image("manuscript-apaquarto_files/figure-typst/fig2-1.svg"))
], caption: figure.caption(
position: bottom, 
[
A) Number of authors and B) female odds ratio for authorship types across the number of authors.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
For comparison, the closest disciplinary benchmark comes from psychology, where González-Alvarez and Sos-Peña (#link(<ref-gonzalez2020women>)[2020];) analyzed the gender distribution of 74,413 authors publishing in psychology journals in 2009. Women accounted for 45.2% of the sample, indicating near gender parity overall. However, substantial variation was observed across sub-disciplines: developmental psychology (57.0%) and educational psychology (53.8%) showed higher proportions of female authors, whereas biological psychology (39.4%), experimental psychology (37.5%), and mathematical psychology (25.8%) showed lower representation. Across the full dataset, women were slightly overrepresented in first authorship positions (OR = 1.10) but underrepresented in last authorship positions (OR = 0.86). This pattern resembles the trends identified in the present study.

== Citations and Open Access
<citations-and-open-access>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
One potential difference established in previous bibliometric analyses of gendered authorship is citations (#link(<ref-chatterjee2021gender>)[Chatterjee & Werner, 2021];; #link(<ref-west2013role>)[West et al., 2013];). Here we tested whether the citations -- as indexed by Scopus -- show differences across gender. The median citation for studies with female lead authors is 10 \[9--11\], and for males, the numbers are identical (Md=10 \[9--11\]) and the difference is not statistically significant ($chi^2$(1)=2.14, #emph[p] = 0.144) using rank-based Wilcoxon test. The same comparison of citations for studies with last authors yields similar results 9.5 \[8--11\], and for males, the numbers are similar (Md=11 \[10--12\]). Again, the difference is not statistically significant ($chi^2$(1)=2.04, #emph[p] = 0.153). If we assume that we can weight in the gender contribution of all authors in a publication to its citation count, we observe a minor difference, where median citations for female authors is 9 \[9--10\], and for males, the central measure are statistically significantly higher (Md=10 \[9--10\], $chi^2$(1)=16.05, #emph[p] \< 0.0001). For reference, prior study of citation patterns across gender in psychology has reported a small effect favoring men (M = 16.57) over women (M = 15.90) (#link(<ref-gonzalez2020women>)[González-Alvarez & Sos-Peña, 2020];).

#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
We also explored whether the open access status of the articles is associated with gender. Out of 3373 articles, 32.2% are Open Access in this sample, as indexed by Scopus. For first-authored articles, female odds ratio for Open Access is 1.49 \[1.29--1.73\], suggesting nearly 50% higher odds associated with publishing open access by women as compared to men. If we observe only the last authorship status of the publications, the difference vanishes with the odds ratio indisguishable from even division (1.00 and the confidence interval), OR = 0.86 \[0.72--1.02\].

== Geographical differences
<geographical-differences>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Past studies have identified consistent geographical patterns in female authorship. For example, a large-scale analysis of psychology publications found that 46.5% of authorships were by women, with European countries showing a lower average proportion of 42.8% (#link(<ref-gonzalez2020women>)[González-Alvarez & Sos-Peña, 2020];). To examine geographical differences more closely, we calculated the odds of female authorship by the countries that have published at least 60 publications in the sample, as shown in Figure 3.

#figure([
#box(image("manuscript-apaquarto_files/figure-typst/countrywise-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Female author odds ratio across the prolific countries publishing music psychology (with more than sixty publications).
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Panel A in Figure 3 shows a small number of countries, such as Australia, China, and Canada, with a high odds ratio for female authorship. At the other end of the spectrum, Japan, Belgium, Austria, Sweden, and Turkey all have significantly lower odds ratios for female authors. There is no simple rationale immediately apparent (e.g., country size, membership in a particular political bloc, gender equality index, or domination of articles by a single eminent scholar from one country) that would explain these differences, but this representation provides some starting points for discussion and may highlight the impact of academic career structures and principles implemented in these countries. Panel B shows data aggregated by continent, though we note that the representation is markedly different between continents. Africa has meagre representation of only 52 authors, Oceania consists only of Australia and New Zealand, and the Americas are represented almost entirely by the US and Canada. While this level of presentation may be too coarse for detailed analysis, it does offer a broad summary of the current situation. Overall, Oceanie and Americas have higher female author representation, while Asia and Europe have more a male representation.

== Gendered topics through analysis of keywords
<gendered-topics-through-analysis-of-keywords>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
To explore the potential differences within music psychology topics of choice between female and males, we analysed the keywords in the articles (41,438 in total, 5,960 unique). To reduce variant keywords, we manually simplified the variants (e.g., "arts in health", "healthy music use", "health musicking" were converted to "music health" and "music cognition" to "cognition" and "music perception" to "perception") and eliminated single use of the term "music". We aggregated the counts of keywords across all authors in the articles and Figure 4 shows the odds ratios for female authors to be associated (in any author position) with the 40 most frequently used keywords. The keywords signifying related to children, development, emotion regulation, well-being, music performance anxiety, motivation, expertise, music listening, mental health, and music therapy show significantly higher odds ratios (OR $gt.eq$ 1.50) for females in comparison to overall gender ratios in the data. At the opposite end of this continuum, topics such as music analysis, harmony, machine learning, computation, cognition, and performance expression show the opposite trend, suggesting that the articles with these keywords tend to have overall more prevalent male authorships (female OR $lt.eq$ 0.75). There is much nuance and detail in the data that would warrant separate analyses; however, the purpose here is simply to provide reference points for comparing gender distributions of authorship with other disciplines, where thematic analyses have already been conducted (e.g., in sub-disciplines of psychology).

#figure([
#box(image("manuscript-apaquarto_files/figure-typst/keywords-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Female author odds ratio across 40 most frequent keywords in all articles as aggregated across all authors.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


= Discussion
<discussion>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Authorship is a crucial aspect in academia as a marker of scholarly success and strongly influences academic career progression, influencing recognition, funding, and advancement opportunities. However, there are several inequities within this aspect of academia, with women often being underrepresented in aspects such as authorships. The goal of the current paper was to provide a snapshot of authorship prevalence and roles across gender. While we recognise that gender is a complex and multidimensional social construct, and there are limitations to treating gender as a binary category, we used a binary male-female classification consistent with prior meta-science research on gender and authorship (#link(<ref-gonzalez2020women>)[González-Alvarez & Sos-Peña, 2020];; #link(<ref-ni2021gendered>)[Ni et al., 2021];; #link(<ref-rock2021quantifying>)[Rock et al., 2021];; #link(<ref-shah2021gender>)[Shah et al., 2021];; #link(<ref-son2022scientific>)[Son & Bell, 2022];; #link(<ref-wais2006>)[Wais, 2006];). As male-female authorship representations differ across different academic disciplines, it is worthwhile to diagnose the situation in music psychology.

In music psychology, the distribution of authors by gender remains unequal, with women representing 40.2% of authors. This proportion is higher than in musicology (34.8% Eerola (#link(<ref-eerola2026inprep>)[in prep.];)) and higher than in psychology more broadly, where women account for 33.2% of authors (#link(<ref-huang2020>)[Huang et al., 2020];). In terms of authorship roles, notable differences emerge. Women are relatively more likely than men to appear as first authors, a trend that has become characteristic of the field only in the past 15 years. This supports previous research showing that there is a positive trend with female representation increasing (#link(<ref-gonzalez2020women>)[González-Alvarez & Sos-Peña, 2020];; #link(<ref-ioannidis2023gender>)[Ioannidis et al., 2023];; #link(<ref-jemielniak2025gender>)[Jemielniak & Wilamowski, 2025];; #link(<ref-sanchez2024analysis>)[Sánchez-Jiménez et al., 2024];). In contrast, women remain underrepresented in the last authorship position, and this trend has not changed substantially over the last 15 years. This corresponds to the situation in other academic fields such as medicine (#link(<ref-bruck2023bibliometric>)[Brück, 2023];; #link(<ref-shah2021gender>)[Shah et al., 2021];), psychology (#link(<ref-gonzalez2020women>)[González-Alvarez & Sos-Peña, 2020];), and biology (#link(<ref-rock2021quantifying>)[Rock et al., 2021];). Given that last authorship in the social sciences is not universally defined but is increasingly associated with seniority, supervisory responsibility, or principal investigator status (#link(<ref-drivas2024evolution>)[Drivas, 2024];), this underrepresentation suggests that women are less likely to occupy leadership roles such as lab heads or holders of substantial research resources. This pattern is consistent with broader evidence showing that women remain underrepresented in senior academic positions, including professorial appointments (#link(<ref-bakker2016tenure>)[Bakker & Jacobs, 2016];), invitations to contribute to journals, and authorship in high-prestige outlets (#link(<ref-holman2018gender>)[Holman et al., 2018];). Related analyses from science and medicine further show that women are more often associated with conducting experiments, whereas men are disproportionately represented in authorship roles linked to leadership and oversight (#link(<ref-macaluso2016science>)[Macaluso et al., 2016];). This suggests that the more 'leadership' author position is more likely to go to the male author.

There were no substantial gender differences in the citation patterns. However, one notable observation is that first-authored articles by women were significantly more likely to be published open access, with an odds ratio of 1.49, a result that warrants further exploration. This effect does not appear to be tied to specific journals. Although the odds ratio for women publishing open access is highest in #emph[Music and Science];, the broader trend suggests a temporal shift: since around 2010, women have increasingly overtaken men in publishing open access. The data does not provide any direct reasons for this, although the legislation in few of the European nations (United Kingdom, EU) require open access publishing and when women have higher odds ratio for being first authors, this might be due to the high number of female from these countries. While past surveys among, for instance, UK academics have not indicated substantial differences in experiences with open access (#link(<ref-zhu2017support>)[Zhu, 2017];), financial and other inequalities in emerging countries have been suggested to limit women's capacity to publish in open access journals (#link(<ref-vuong2021adopting>)[Vuong et al., 2021];). The idea that publishing in a more open and transparent way benefit society in general (#link(<ref-pervsic2023open>)[Peršić & Straza, 2023];), and this fits with the general idea that female researchers value research on societal progress, while males prefer research aimed at scientific progress (#link(<ref-zhang2021gender>)[L. Zhang et al., 2021];). However, nothing in the extant literature would point out to the reverse pattern, which requires further investigation about the reasons for adopting open access publishing models.

\-\> (potential reasons: women faculty perform more university/campus/departmental services Guarino and Borden (#link(<ref-guarino2017faculty>)[2017];) ) \[TE: is this supporting the idea that women are publishing more access than men? Or is this more about the lack of senior positions, which are probably not going to be achieved with internal service?\]

Turning to potential drivers of these gender differences, geographical and cultural factors appear to play a major role. Our summaries of gender patterns across continents and countries indicate clear cross-national variation. Large Commonwealth nations, namely Australia, Canada, and the United Kingdom, seem to provide a cultural climate more supportive of women in research, as reflected in the relatively high odds ratios of women appearing as authors (in any authorship position) in music psychology. This pattern, however, is not universal across all countries examined (#link(<ref-chan2020>)[Chan & Torgler, 2020];). Prior work shows that women's research success is positively correlated with national indicators of gender equity (#link(<ref-chan2020>)[Chan & Torgler, 2020];). This is likely to be related to a career length, which strongly correlates with overall productivity. Women exhibit higher dropout rates than men, particularly during the 5--10 years following their first publication (#link(<ref-huang2020>)[Huang et al., 2020];), but policies geared towards equity and support for career breaks and caring responsibilities may well contribute towards the observed country-specific differences. Nevertheless, prior analyses demonstrate that even when women constitute a smaller share of researchers within a discipline or country, their contributions are often more impactful than those of their male colleagues (#link(<ref-chan2020>)[Chan & Torgler, 2020];). When examining countries where women have significantly lower odds of authorship compared to men (Japan, Belgium, Austria, Sweden, and Turkey), it is not straightforward to conclude that the lower proportion of female author is related gender equality rankings. The notable exception is Turkey, which is positioned lower according to the Gender Inequality Index (#link(<ref-UNDPHDR2024>)[United Nations Development Programme (UNDP), 2025];).

The keyword analysis of the articles indicated thematic differences in the topics studied by women and men. Research areas related to children, well-being, health, and mental health were more frequently associated with women. This pattern is consistent with observations in psychology more broadly, where women are more strongly represented in sub-disciplines that focus on (health)care and nurturing (#link(<ref-bruck2023bibliometric>)[Brück, 2023];; #link(<ref-gonzalez2020women>)[González-Alvarez & Sos-Peña, 2020];). The higher likelihood of women studying topics related to singing may be connected to prevailing gender stereotypes, or to the early negative perceptions of singing among boys (#link(<ref-Warzecha2013>)[Warzecha, 2013];) and men (#link(<ref-palkki2015gender>)[Palkki, 2015];). In contrast, topics related to cognition and computation were more often associated with men, which reflects a wider trend in psychology (#link(<ref-gonzalez2020women>)[González-Alvarez & Sos-Peña, 2020];). In a similar trend in computational linguistics, woman are more likely to publish on speech, social and conversational topics than males, who are more likely to publish in formal mathematical, syntax, and semantic approaches (#link(<ref-vogel2012he>)[Vogel & Jurafsky, 2012];). It perhaps might be interesting to note that 'emotion' has an equal balance between men and women. Although this may come as a surprise hen coming from different disciplines, music emotion can be a relatively 'analytical' aspect in music psychology, where emotion from music is attempted to be decoded in a more analytical way (top 3 authors for the keyword "emotion" are majority male, Saarikallio, Juslin, and Schubert, but "emotion regulation" top three is fully female), as well as being used in a more 'emotional' sense. This is the same for the keyword "expression". Although on first glance, it might fit the stereotype that this keyword might be more related to females, the "performance expression" is again related to a more analytical coding aspect (Marc Leman, Laura Bishop, and Roger Chaffin as top 3 authors).

In summary, music psychology has similar groupings of topic for female and male authors as in other disciplines of psychology and medicines. Below we also discuss how this information can help benefit the general field towards more equity with academia.

= Conclusions
<conclusions>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
The analysis of gender distributions in the five most prominent music psychology journals over the last 25 years presented a rich and evolving history of gender distribution in various authorship roles. While overall women hold smaller number of authorship overall (40.2%), they are significantly more likely to hold first author positions in coauthored publications but significantly less likely likely to hold last author positions than men. For solo publications and being a coauthor, there are no significant gender differences.

There was small but significant effect of gender on citations that favoured men when the citations were aggregated all authors in the papers. There was no, however, differences in citations for female first or last authors. The aggregated results are in line with previous findings in psychology that favored men (#link(<ref-gonzalez2020women>)[González-Alvarez & Sos-Peña, 2020];). While the present analysis cannot determine the underlying causes of the observed trends, we can point to potential factors that may help explain why this discipline appears to be more equitably distributed by gender for the first authors than, for example, psychology. Female role models (past and present) exist in music psychology; many of the founding members of the societies have been women (e.g., Diana Deutsch for ICMPC in 1989, Irène Deliège for ESCOM in 1991, and similarly many founding editors of the journals have been women (e.g., Diana Deutsch for #emph[Music Perception] 1983-1995, and Irène Deliège for #emph[Musicae Scientiae] 1997-2009). However, this does not fully explain the rise of female first authors over the past 15 years, nor is it consistent with the reverse pattern observed in the last authorship position. A cautious interpretation is that the increase in first authorship may signal the discipline becoming more gender-balanced, but this shift has not yet extended to last authorship positions, which still reflect either career longevity or barriers to women attaining senior academic positions.

While we present analysis of gender of the authorships across the discipline-specific journal in a relatively simple manner here, there would be more elaborate ways into obtaining insights into the gendered career progression in music psychology; for instance, future work with the same dataa could analyse the precarity of career options as judged from the years of authorship attributions (#link(<ref-lundine2018>)[Lundine et al., 2018];). One could also analyse the last author positions in more detail using direct contacts to the authors (interviews and surveys) and try to identify the factors contributing to the barriers experienced by women to take up senior author positions.

Current findings from authorship counts in music psychology present a more optimistic outlook of female authors in comparison what the prevalence of female academics or professors in academia in general are (#link(<ref-bhopal2021competing>)[Bhopal & Henderson, 2021];; #link(<ref-Filandri03082021>)[Filandri & Pasqua, 2021];; #link(<ref-harris2025gender>)[Harris et al., 2025];). While the results are broadly comparable to psychology as a whole, musicology and its closely related disciplines such as music theory, popular music studies, and ethnomusicology tend to show less balanced gender representation in overall authorship. Gender, however, is only one dimension of equity; racial, ethnic, geographical, institutional, and other inequalities also merit attention in music psychology. Although women remain underrepresented in terms of absolute authorship counts in specialist music psychology journals, even a simple tally of contributions can help open discussion and provide evidence of progress toward greater inclusion and equality. To interpret these patterns more fully, one must examine how music psychology operates through its journals, conferences, training programmes, and curricula, as well as how the discipline presents itself both in countries with a long-standing tradition in the field and in regions where it has yet to become firmly established.

=== Funding statement
<funding-statement>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Authors received no funding for this research.

=== Competing interests statement
<competing-interests-statement>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
There were no competing interests.

=== Open practices statement
<open-practices-statement>
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Study data, analysis scripts and supporting information is available at GitHub, #link("https://tuomaseerola.github.io/gender_in_music_psych");.

= References
<references>
#set par(first-line-indent: 0in, hanging-indent: 0.5in)
#block[
#block[
Anglada-Tort, M., & Sanfilippo, K. R. M. (2019). Visualizing music psychology: A bibliometric analysis of psychology of music, music perception, and musicae scientiae from 1973 to 2017. #emph[Music & Science];, #emph[2];, 2059204318811786.

] <ref-anglada2019visualizing>
#block[
Ashmos Plowman, D., & Smith, A. D. (2011). The gendering of organizational research methods: Evidence of gender patterns in qualitative research. #emph[Qualitative Research in Organizations and Management: An International Journal];, #emph[6];(1), 64--82.

] <ref-ashmos2011gendering>
#block[
Bakker, M. M., & Jacobs, M. H. (2016). Tenure track policy increases representation of women in senior academic positions, but is insufficient to achieve gender balance. #emph[PLoS One];, #emph[11];(9), e0163376.

] <ref-bakker2016tenure>
#block[
Banks, G. C., Rasmussen, L. M., Tonidandel, S., Pollack, J. M., Hausfeld, M. M., Williams, C., Albritton, B. H., Allen, J. A., Bastardoz, N., Batchelor, J. H., et al. (2025). Women's and men's authorship experiences: A prospective meta-analysis. In #emph[Journal of Management] (4; Vol. 51, pp. 1273--1287). Sage Publications Sage CA: Los Angeles, CA.

] <ref-banks2025women>
#block[
Bhopal, K., & Henderson, H. (2021). Competing inequalities: Gender versus race in higher education institutions in the UK. #emph[Educational Review];, #emph[73];(2), 153--169.

] <ref-bhopal2021competing>
#block[
Brück, O. (2023). A bibliometric analysis of the gender gap in the authorship of leading medical journals. #emph[Communications Medicine];, #emph[3];(1), 179.

] <ref-bruck2023bibliometric>
#block[
Chan, H. F., & Torgler, B. (2020). Gender differences in performance of top cited scientists by field and country. #emph[Scientometrics];, #emph[125];(3), 2421--2447. #link("https://doi.org/10.1007/s11192-020-03733-w")

] <ref-chan2020>
#block[
Chatterjee, P., & Werner, R. M. (2021). Gender disparity in citations in high-impact journal articles. #emph[JAMA Network Open];, #emph[4];(7), e2114509--e2114509.

] <ref-chatterjee2021gender>
#block[
Drivas, K. (2024). The evolution of order of authorship based on researchers' age. #emph[Scientometrics];, #emph[129];(9), 5615--5633.

] <ref-drivas2024evolution>
#block[
Eerola, T. (2024). Prevalence of transparency and reproducibility-related research practices in music psychology (2017-2022). #emph[Musicae Scientiae];. #link("https://doi.org/10.1177/10298649241300885")

] <ref-eerola2023transparency>
#block[
Eerola, T. (in prep.). #emph[Gender distribution in music research: A bibliometric analysis];.

] <ref-eerola2026inprep>
#block[
Filandri, M., & Pasqua, S. (2021). “Being good isn't good enough”: Gender discrimination in italian academia. #emph[Studies in Higher Education];, #emph[46];(8), 1533--1551. #link("https://doi.org/10.1080/03075079.2019.1693990")

] <ref-Filandri03082021>
#block[
González-Alvarez, J., & Sos-Peña, R. (2020). Women publishing in american psychological association journals: A gender analysis of six decades. #emph[Psychological Reports];, #emph[123];(6), 2441--2458.

] <ref-gonzalez2020women>
#block[
Guarino, C. M., & Borden, V. M. (2017). Faculty service loads and gender: Are women taking care of the academic family? #emph[Research in Higher Education];, #emph[58];(6), 672--694.

] <ref-guarino2017faculty>
#block[
Harris, R., Mate-Sanchez-Val, M., & Ruiz Marı́n, M. (2025). Gender disparities in promotions and exiting in UK russell group universities. #emph[Applied Economics];, #emph[57];(31), 4441--4457.

] <ref-harris2025gender>
#block[
Holman, L., Stuart-Fox, D., & Hauser, C. E. (2018). The gender gap in science: How long until women are equally represented? #emph[PLoS Biology];, #emph[16];(4), e2004956.

] <ref-holman2018gender>
#block[
Huang, J., Gates, A. J., Sinatra, R., & Barabási, A.-L. (2020). Historical comparison of gender inequality in scientific careers across countries and disciplines. #emph[Proceedings of the National Academy of Sciences];, #emph[117];(9), 4609--4616. #link("https://doi.org/10.1073/pnas.1914221117")

] <ref-huang2020>
#block[
Ioannidis, J. P., Boyack, K. W., Collins, T. A., & Baas, J. (2023). Gender imbalances among top-cited scientists across scientific disciplines over time through the analysis of nearly 5.8 million authors. #emph[PLoS Biology];, #emph[21];(11), e3002385.

] <ref-ioannidis2023gender>
#block[
Jakubowski, K., Ahmad, N., Armitage, J., Barrett, L., Edwards, A., Galbo, E., Gómez-Cañón, J., Graves, T., Jadzgevičiūtė, A., Kirts, C., Lahdelma, I., Lennie, T., Ramatally, A., Schlichting, J., Steliou, C., Vishwanath, K., & Eerola, T. (2025). Participant and musical diversity in music psychology research. #emph[Music & Science];. #link("https://doi.org/10.1177/20592043251317180")

] <ref-jakubowski2024>
#block[
Jemielniak, D., & Wilamowski, M. (2025). Gender gap in all academic fields over time. #emph[Social Science Computer Review];, #emph[43];(4), 888--896.

] <ref-jemielniak2025gender>
#block[
Johansson, E. E., Risberg, G., Hamberg, K., & Westman, G. (2002). Gender bias in female physician assessments. Women considered better suited for qualitative research. #emph[Scandinavian Journal of Primary Health Care];, #emph[20];(2), 79--84.

] <ref-johansson2002gender>
#block[
Kiai, A. (2019). To protect credibility in science, banish “publish or perish.” #emph[Nature Human Behaviour];, #emph[3];(10), 1017--1018.

] <ref-kiai2019protect>
#block[
Lundine, J., Bourgeault, I. L., Clark, J., Heidari, S., & Balabanova, D. (2018). The gendered system of academic publishing \[Doi: 10.1016/S0140-6736(18)30950-4\]. #emph[The Lancet];, #emph[391];(10132), 1754--1756. #link("https://doi.org/10.1016/S0140-6736(18)30950-4")

] <ref-lundine2018>
#block[
Macaluso, B., Larivière, V., Sugimoto, T., & Sugimoto, C. R. (2016). Is science built on the shoulders of women? A study of gender differences in contributorship. #emph[Academic Medicine];, #emph[91];(8), 1136--1142.

] <ref-macaluso2016science>
#block[
Martı́nez-Menéndez, I., Acosta-Pavas, J. C., Corrales, D. C., Villela, S. M. A., Bouhaouala-Zahar, B., Georgakilas, G. K., Mexis, K., Xenios, S., Dalamagas, T., Kokosis, A., et al. (2024). Comparison of machine learning-enhanced dynamic hybrid models for a nanobody scorpion antivenom production with escherichia coli. #emph[International Symposium on Distributed Computing and Artificial Intelligence];, 307--316.

] <ref-martinez2024comparison>
#block[
Ni, C., Smith, E., Yuan, H., Larivière, V., & Sugimoto, C. R. (2021). The gendered nature of authorship. #emph[Science Advances];, #emph[7];(36), eabe4639.

] <ref-ni2021gendered>
#block[
Nielsen, M. W., Pedersen, J. V., & Larregue, J. (2024). Getting ahead in the social sciences: How parenthood and publishing contribute to gender gaps in academic career advancement. #emph[The British Journal of Sociology];, #emph[75];(3), 322--346.

] <ref-nielsen2024getting>
#block[
Nunkoo, R., Thelwall, M., Ladsawut, J., & Goolaup, S. (2020). Three decades of tourism scholarship: Gender, collaboration and research methods. #emph[Tourism Management];, #emph[78];, 104056.

] <ref-nunkoo2020three>
#block[
Palkki, J. (2015). Gender trouble: Males, adolescence, and masculinity in the choral context. #emph[Choral Journal];, #emph[56];(4), 25--35.

] <ref-palkki2015gender>
#block[
Peršić, A., & Straza, T. (2023). Open science for all: Implementing the UNESCO recommendation on open science for an equitable and just transition to open science. #emph[College & Research Libraries News];, #emph[84];(10), 377.

] <ref-pervsic2023open>
#block[
Quadlin, N., VanHeuvelen, T., & Ahearn, C. E. (2023). Higher education and high-wage gender inequality. #emph[Social Science Research];, #emph[112];, 102873. #link("https://doi.org/10.1016/j.ssresearch.2023.102873")

] <ref-quadlin2023higher>
#block[
Rock, K. N., Barnes, I. N., Deyski, M. S., Glynn, K. A., Milstead, B. N., et al. (2021). Quantifying the gender gap in authorship in herpetology. #emph[Herpetologica];, #emph[77];(1), 1--13.

] <ref-rock2021quantifying>
#block[
Sánchez-Jiménez, R., Guerrero-Castillo, P., Guerrero-Bote, V. P., Halevi, G., & De-Moya-Anegón, F. (2024). Analysis of the distribution of authorship by gender in scientific output: A global perspective. #emph[Journal of Informetrics];, #emph[18];(3), 101556.

] <ref-sanchez2024analysis>
#block[
Sebo, P. (2021). Performance of gender detection tools: A comparative study of name-to-gender inference services. #emph[Journal of the Medical Library Association];, #emph[109];(3), 414--421.

] <ref-sebo2021performance>
#block[
Sebo, P., Maisonneuve, H., & Fournier, J. P. (2020). Gender gap in research: A bibliometric study of published articles in primary health care and general internal medicine. #emph[Family Practice];, #emph[37];(3), 325--331.

] <ref-sebo2020gender>
#block[
Shah, S. G. S., Dam, R., Milano, M. J., Edmunds, L. D., Henderson, L. R., Hartley, C. R., Coxall, O., Ovseiko, P. V., Buchan, A. M., & Kiparoglou, V. (2021). Gender parity in scientific authorship in a national institute for health research biomedical research centre: A bibliometric analysis. #emph[BMJ Open];, #emph[11];(3), e037935.

] <ref-shah2021gender>
#block[
Son, J., & Bell, M. (2022). Scientific authorship by gender: Trends before and during a global pandemic. Humanities and social sciences communication 9 (348). #emph[Humanities and Social Sciences Communications];, #emph[9];(348). #link("https://doi.org/10.1057/s41599-022-01365-4")

] <ref-son2022scientific>
#block[
Spoon, K., LaBerge, N., Wapman, K. H., Zhang, S., Morgan, A. C., Galesic, M., Fosdick, B. K., Larremore, D. B., & Clauset, A. (2023). Gender and retention patterns among u.s. faculty. #emph[Science Advances];, #emph[9];(42), eadi2205. #link("https://doi.org/10.1126/sciadv.adi2205")

] <ref-spoon2023>
#block[
Sugimoto, C. R., Ni, C., & Larivière, V. (2015). On the relationship between gender disparities in scholarly communication and country-level development indicators. #emph[Science and Public Policy];, #emph[42];(6), 789--810.

] <ref-sugimoto2015relationship>
#block[
Thelwall, M., Bailey, C., Tobin, C., & Bradshaw, N.-A. (2019). Gender differences in research areas, methods and topics: Can people and thing orientations explain the results? #emph[Journal of Informetrics];, #emph[13];(1), 149--169.

] <ref-thelwall2019gender>
#block[
Tscharntke, T., Hochberg, M. E., Rand, T. A., Resh, V. H., & Krauss, J. (2007). Author sequence and credit for contributions in multiauthored publications. #emph[PLoS Biology];, #emph[5];(1), e18.

] <ref-tscharntke2007author>
#block[
United Nations Development Programme (UNDP). (2025). #emph[Human development report 2024/25];. #link("https://hdr.undp.org/data-center/thematic-composite-indices/gender-inequality-index")

] <ref-UNDPHDR2024>
#block[
VanHelene, A. D., Khatri, I., Hilton, C. B., Mishra, S., Gamsiz Uzun, E. D., & Warner, J. L. (2024). Inferring gender from first names: Comparing the accuracy of genderize, gender API, and the gender r package on authors of diverse nationality. #emph[PLOS Digital Health];, #emph[3];(10), e0000456. #link("https://doi.org/10.1371/journal.pdig.0000456")

] <ref-vanhelene2024inferring>
#block[
Vogel, A., & Jurafsky, D. (2012). He said, she said: Gender in the ACL anthology. #emph[Proceedings of the ACL-2012 Special Workshop on Rediscovering 50 Years of Discoveries];, 33--41.

] <ref-vogel2012he>
#block[
Vuong, Q.-H., Nguyen, H. T. T., Ho, M.-T., & Nguyen, M.-H. (2021). Adopting open access in an emerging country: Is gender inequality a barrier in humanities and social sciences? #emph[Learned Publishing];, #emph[34];(4), 487--498.

] <ref-vuong2021adopting>
#block[
Wais, K. (2006). Gender prediction methods based on first names with genderizeR. #emph[The R Journal];, #emph[8];(1), 17--37. #link("https://doi.org/10.32614/RJ-2016-002")

] <ref-wais2006>
#block[
Warzecha, M. (2013). Boys' perceptions of singing: A review of the literature. #emph[Applications of Research in Music Education];, #emph[32];(1), 43--51. #link("https://doi.org/10.1177/8755123313502341")

] <ref-Warzecha2013>
#block[
West, J. D., Jacquet, J., King, M. M., Correll, S. J., & Bergstrom, C. T. (2013). The role of gender in scholarly authorship. #emph[PloS One];, #emph[8];(7), e66212.

] <ref-west2013role>
#block[
Zhang, C., Wei, S., Zhao, Y., & Tian, L. (2023). Gender differences in research topic and method selection in library and information science: Perspectives from three top journals. #emph[Library & Information Science Research];, #emph[45];(3), 101255.

] <ref-zhang2023gender>
#block[
Zhang, L., Sivertsen, G., Du, H., Huang, Y., & Glänzel, W. (2021). Gender differences in the aims and impacts of research. #emph[Scientometrics];, #emph[126];(11), 8861--8886.

] <ref-zhang2021gender>
#block[
Zhu, Y. (2017). Who support open access publishing? Gender, discipline, seniority and other factors associated with academics' OA practice. #emph[Scientometrics];, #emph[111];(2), 557--579.

] <ref-zhu2017support>
] <refs>
#set par(first-line-indent: 0.5in, hanging-indent: 0in)


 
  
#set bibliography(style: "_extensions/wjschne/apaquarto/apa.csl") 


