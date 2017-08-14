                        Characteristics of WordNet-Affect 1.1

This version includes a smaller number of synsets but the semantic
organization is more well-structured.

a) Affective Hierarchy

The affective label "emotion" is expanded in order to include a subset of
new labels, identifying emotional states. These labels, named "affective
categories", are hierarchically organized.


b) Valence

The hierarchy was initially obtained from the hyponym subtree of the
synset "n#feeling#1", but some modifications were performed in order to
classify affective synsets according to emotional valence. In particular,
affective categories are partitioned in 4 classes: "positive" (e.g. joy),
"negative" (e.g. sadness), "ambiguous" (e.g. surprise), and "neutral"
(e.g. apathy).

c) Causative/Stative Attribute

Synsets of part of speech (pos) "adjective", "verb", and "adverb"
present an addictional label representing their "causative" or "stative"
semantic function. For example, an emotional adjective is "causative"
if it refers to some emotion that is caused by the entity represented by
the modified noun (e.g. "amusing movie"). On the other hand, an emotional
adjective is "stative" if it refers to the emotion owned or felt by the
subject denoted by the modified noun (e.g. "cheerful/happy boy").

---------------------------------------------------------------------
                        Differences with respect to WordNet-Affect 1.0

- Source files are formatted in XML standard.

- Previous affective labels were renamed and expressed without
abbreviations. The mapping between previous and present labels is the
following:

        phy -> physical-state
        beh -> behaviour
        sit -> (emotion eliciting) situation
        tra -> trait
        sen -> sensation
        cog -> cognitive-state
        moo -> mood
        emo -> emotion
        eds -> edonic-signal

- We removed the label "core" (referring to manually annotated synsets)
and other labels automatically added to synsets (by application of WordNet
relations, such as "similar-to"). In fact, all synsets in WordNet-Affect
1.1 were manually reviewed and it is no more useful to trace how they
were collected.

- Synsets that are not tagged with the label "emo(tion)" in the previous
version are not present in current release. In order to retrieve these
synsets, you have to refer to source files of WordNet-Affect 1.0

---------------------------------------------------------------------
                        File description

a-hierarchy.xml:

Includes the affective hierarchy. Each item has 2 attributes:

         name = affective category label
     isa  = category parent in the hierarchy


a-synsets.xml:

Includes synsets associated with the affective hierarchy. Synsets are
classified according to their pos.

Synsets of pos "noun" have the following attributes:

           id = label identifying current synset
        categ = affective category label

Synsets of other pos ("adjective", "verb", and "adverb") have the
following attributes:

                id = label identifying current synset
           noun-id = id of the noun synset from which the current one was derived
       causat-stat = causative/stative label

The reason why not-noun synsets are connected to the affective categories
via noun synsets is because this relation allows us to study to what
extent the causative/stative character of adjectives, verbs and adverbs
depends on the morphological variation of nouns. In the next release of
WordNet-Affect, it is reasonable to characterize this semantic function
of morphology.

---------------------------------------------------------------------
                        Plans for the future

The next version of WordNet-Affect will include all synsets that
in WordNet-Affect 1.0 are annotated with labels different from "emo"
(emotion) and that are not included in the current release. In particular,
we want to distinguish labels representing mental states (e.g. cognitive
states, attitudes), attributes of mental states (e.g. valence, intensity
or level of arousal), and other semantic characteristics (e.c. behaviours,
emotion-eliciting situations, emotional responces). Finally, we want to
select only one label for each synset, taking into account its hypernyms.
