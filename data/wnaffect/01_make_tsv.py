#!/usr/bin/env python3

import re
import xml.etree.ElementTree as etree


def read_synset(pos):
    synset = {}
    with open('wordnet-1.6/dict/data.' + pos) as f:
        for l in f:
            if l[0:2] == '  ':
                continue
            fields = l.split()
            synset_offset = fields[0]
            lex_filenum = fields[1]
            ss_type = fields[2] # n v a s r
            w_cnt = int(fields[3], 16)
            ss = set()
            for i in range(w_cnt):
                off = 4 + i * 2
                word = fields[off].replace('_', ' ')
                lex_id = fields[off + 1]
                #ss.add((word, lex_id))
                ss.add(word)
            synset[synset_offset] = ss
    return(synset)

def read_synsets():
    synsets = {}
    for pos in ['adj', 'adv', 'noun', 'verb']:
        synsets[pos] = read_synset(pos)
    return(synsets)

def read_glosses():
    glosses = {}
    with open('wordnet-1.6/dict/index.gloss') as f:
        for l in f:
            fields = l.split()
            name = fields[0]
            gloss = set()
            for elt in fields[1:]:
                parts = elt.split(',')
                pos_id = parts[0]
                pos = ['noun', 'verb', 'adj', 'adv'][int(pos_id) - 1]
                synset_offset = parts[1]
                gloss.add((pos, synset_offset))
            glosses[name] = gloss
        return(glosses)

def get_gloss(name, glosses, synsets):
    gloss = glosses[name]
    words = set()
    for elt in gloss:
        pos = elt[0]
        synset_off = elt[1]
        synset = synsets[pos][synset_off]
        for syn in synset:
            words.add(syn[0])
    return(words)

def read_hierarchy():
    hierarchy = {}
    tree = etree.parse('wn-affect-1.1/a-hierarchy.xml')
    root = tree.getroot()
    for child in root:
        name = child.attrib['name']
        if name == 'simpathy':
            name = 'sympathy'
        hierarchy[name] = []
        isa = child.attrib.get('isa')
        if isa:
            parent = hierarchy[isa]
            parent.append(name)
    return(hierarchy)

def print_hierarchy(hierarchy, node = 'root', indent = 0):
    print(' ' * indent + node)
    for child in hierarchy.get(node, []):
        print_hierarchy(hierarchy, child, indent + 1)

synlist_edits = {
    'joy-pride': 'self-pride',
    'levity-gaiety': 'playfulness',
    'general-gaiety': 'merriment'
    }


def read_synlists():
    synlists = {}
    tree = etree.parse('wn-affect-1.1/a-synsets.xml')
    root = tree.getroot()
    synlists = {}
    nouns = {}
    for child in root:
        if child.tag == 'noun-syn-list':
            for elt in child:
                synset_off = elt.attrib['id'].split('#')[1]
                name = elt.attrib['categ']
                name = synlist_edits.get(name, name)
                nouns[synset_off] = name
                synlists[name] = set((('noun', synset_off),))
        else:
            for elt in child:
                elt_id = elt.attrib['id'].split('#')
                pos = {'a': 'adj', 'v': 'verb', 'r': 'adv'}[elt_id[0]]
                synset_off = elt_id[1]
                noun_off = elt.attrib['noun-id'].split('#')[1]
                noun = nouns[noun_off]
                synlists[noun].add((pos, synset_off))
    return(synlists)


def get_categ(name, hierarchy):
    node = hierarchy.get(name)
    if node is None or len(node) == 0:
        return(None)
    categ = set(node)
    for n in node:
        child = get_categ(n, hierarchy)
        if child:
            categ.update(child)
    return(categ)


synsets = read_synsets()
glosses = read_glosses()
hierarchy = read_hierarchy()
synlists = read_synlists()

def unmatched_categs(synlists, hierarchy):
    for k in synlists.keys():
        if k not in hierarchy:
            print(k)

def unmatched_nodes(hierarchy, synlists):
    for k in hierarchy.keys():
        if k not in synlists:
            print(k)

#print(unmatched_categs(synlists, hierarchy))
#print(unmatched_nodes(hierarchy, synlists))

def clean_emotion(emotion):
    return({
        'positive': 'Positive',
        'negative': 'Negative',
        'neutral': 'Neutral',
        'ambiguous': 'Neutral'}.get(emotion))
            
def clean_categ(categ):
    return({
        'self-pride': 'pride',
        'positive-expectation': 'expectation',
        'positive-fear': 'fear',
        'positive-hope': 'hope',
        'negative-fear': 'fear',
        'general-dislike': 'dislike',
        'neutral-unconcern': 'unconcern',
        'ambiguous-agitation': 'agitation',
        'ambiguous-fear': 'fear',
        'ambiguous-expectation': 'expectation'}.get(categ, categ).title())


def clean_term(t):
    return(re.sub("\(.*\)$", "", t.lower()))

f = open('wnaffect.tsv', 'w')
nterm = 0
print('term', 'pos', 'category', 'emotion', sep='\t', file = f)
for emotion in ['positive', 'negative', 'neutral', 'ambiguous']:
    toplevel = emotion + '-emotion'
    for categ in hierarchy[toplevel]:
        subcats = get_categ(categ, hierarchy)
        if subcats is None:
            subcats = set()
        subcats.add(categ)
        for sc in subcats:
            if sc not in synlists:
                continue
            for sl in synlists[sc]:
                pos = sl[0]
                synset_offset = sl[1]
                terms = synsets[pos][synset_offset]
                for t in terms:
                    t = clean_term(t)
                    if len(t.split()) > 1:
                        continue
                    print(t, pos.upper(), clean_categ(categ),
                            clean_emotion(emotion), sep="\t", file = f)
                    nterm = nterm + 1
f.close()
