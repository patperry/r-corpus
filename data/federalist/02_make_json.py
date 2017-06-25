#!/usr/bin/env python3

import json
import re

# <file>:
#
# <file header>
# <paper>*
# <file footer>


# <paper>:
# FEDERALIST\.? No\. (\d+)
# <meta>
# <text>


# <meta>:
#
# <title>\s+<subtitle>?
#
# <venue>.\s+<date>?
#
# <author>


# <title>:
#
# <line>+
#

# <subtitle>:
#
# (<line>+)
#

# <venue>:
#
# (For the Independent Journal|From the New York Packet|From the Daily Advertiser|From McLEAN's Edition, New York).
#

# <date>:
#
# Weekday, Month Day, Year.

# <author>
#
# [A-Z ]+
#


# <text>:
#
# <word>*
#
# PUBLIUS.
#
# (<footnote>)*
#
#
# (*There are two slightly different versions of No. 70 included here.)?


def parse(filename):
    papers = []
    with open(filename, 'r') as f:
        l = parse_file_header(f)
        while l:
            (p,l) = parse_paper(l, f)
            papers.append(p)
    return papers


def parse_file_header(f):
    for l in f:
        if re.match(r'^FEDERALIST\. No\. 1', l):
            return l
    raise ValueError("invalid input file; couldn't find 'FEDERALIST. No. 1.'")


def parse_paper(l, f):
    pid = parse_id(l)
    h = parse_header(f)
    (b,l) = parse_body(f)
    paper = { 'paper_id': pid ,
              'title': h['title'],
              'venue': h['venue'],
              'date': h['date'],
              'author': h['author'],
              'text': clean_body(b) }
    return (paper, l)


def parse_id(l):
    m = re.match(r'^FEDERALIST\.? No\. (\d+)\s*$', l)
    if not m:
        raise ValueError("invalid starting line: '{0}'".format(l))
    return int(m.group(1))


def parse_header(f):
    meta = ""
    for l in f:
        m = re.match(r'^([A-Z ]+)\s*$', l)
        if m:
            d = parse_meta(meta)
            d['author'] = m.group(1)
            return d
        else:
            meta += " " + l.strip()
    raise ValueError("malformed header; couldn't find author")


def parse_meta(meta):
    r = re.compile(r"""^
                       ([^.]*)     # title
                       ((For\ the\ Independent\ [FJ]ournal
                        |From\ the\ New\ York\ Packet
                        |From\ the\ Daily\ Advertiser
                        |From\ M[Cc]LEAN'[Ss]\ Edition,\ New\ York
                        )
                        \.
                       )?        # venue
                       \s*
                       (((\w+,?\ )?\w+\ \d+,\ \d+).)? # date: (Weekday,?)? Month Day, Year.
                       \s*
                       $""", re.VERBOSE)
    m = re.match(r, meta)
    if not m:
        raise ValueError("malformed header: '{0}'".format(meta))
    title = m.group(1).strip().replace("  ", " ")
    venue = m.group(3)
    date = m.group(5)

    return { 'title': title, 'venue': venue, 'date': date }


def parse_body(f):
    addressee = parse_addressee(f)
    tl = parse_text(f)
    t = tl[0]
    l0 = tl[1]
    d = { 'addressee': addressee,
          'text': t['text'],
          'signature': t['signature'],
          'footnotes': t['footnotes'] }
    return (d, l0)


def parse_addressee(f):
    for l in f:
        if re.match(r'^\s*$', l):
            continue
        m = re.match(r'^(To .*)[.:]\s*$', l)
        if m:
            return m.group(1)
        else:
            break
    raise ValueError("malformed body; couldn't find addressee: '{0}'", l)


def parse_text(f):
    r = re.compile(r'''^(FEDERALIST\.?\ No\.\ \d+\s*)
                        |(End\ of\ the\ Project\ Gutenberg\ EBook.*)
                        |((.*)PUBLIUS\.(.*))
                        $''', re.VERBOSE)
    text = ""
    for l in f:
        m = re.match(r, l)
        if m:
            if m.group(1):
                return ({ 'text':text, 'signature':None, 'footnotes':None }, l)
            elif m.group(2):
                return ({ 'text':text, 'signature':None, 'footnotes':None }, None)
            else:
                text += m.group(4)
                fl = parse_footnotes(m.group(5), f)
                return ({ 'text':text, 'signature':'PUBLIUS.', 'footnotes':fl[0] }, fl[1])
        else:
            text += l
    raise ValueError("malformed body; couldn't find end of paper")


def parse_footnotes(l0, f):
    foot = l0
    for l in f:
        m1 = re.match(r'^FEDERALIST\.? No\. \d+\s*$', l)
        m2 = re.match(r'^End of the Project Gutenberg EBook of The Federalist Papers', l)
        m3 = re.match(r'^\*There are two slightly different versions of No\. 70', l)

        if m1:
            return (foot, l)
        elif m2:
            return (foot, None)
        elif not m3:
            foot += l

    raise ValueError("malformed input file; couldn't find 'End of Project Gutenberg EBook'")


def clean_body(b):
    body = b['addressee'] + ":\n\n" + clean_text(b['text'])
    if b['signature']:
        body += "\n\n" + b['signature']
    else:
        body += "\n\nPUBLIUS." # add missing signature
    foot = clean_footnotes(b['footnotes'])
    if len(foot) > 0:
        body += "\n\n" + '\n\n'.join(foot)
    return body + "\n"


def superscript(num):
    s = ""
    for n in str(num):
        n = int(n)
        if n == 2:
            code = 0x00B2
        elif n == 3:
            code = 0x00B3
        elif n == 1:
            code = 0x00B9
        else:
            code = 0x2070 + int(n)
        s += chr(code)
    return s


def clean_text(text):
    s = ""
    for l in text.splitlines():
        # 1: period, comma after word, quote, semicolon, letter 
        # 2: digits, possibly followed by %
        # 3: not a period or end of input
        l = re.sub(r'(\.|\D,|"|;|[A-Za-z])(\d+)%?([^.]|$)', r'\1 [\2]\3', l)
        # replace bracket footnotes with Unicode superscript
        while True:
            m = re.match(r'^(.*) \[(\d+)\](.*)$', l)
            if m is None:
                break
            foot = superscript(m.group(2))
            l = m.group(1) + foot + m.group(3)
        s += l + "\n"
    return s.strip()


def clean_footnotes(footnotes):
    if footnotes == None:
        return []
    notes = []
    s = None
    for l in footnotes.splitlines():
        if re.match(r'^\s*$', l):
            if s != None:
                notes.append(s)
                s = None
        elif s == None:
            s = l.strip()
        else:
            s += "\n" + l.strip()
    return [clean_note(np[0], np[1]) for np in zip(notes, range(len(notes)))]


def clean_note(note, pos):
    lines = note.splitlines()
    m = re.match(r'^((FNA1-@)?(\d+)\.?\s+)?([\]P]\s+)?(\S.*)$', lines[0])
    if not m:
        raise ValueError("malformed footnote: `{0}'".format(note))
    fid = m.group(3)
    if fid == None:
        fid = pos + 1
    else:
        fid = int(fid)
    lines[0] = superscript(fid) + " " + m.group(5)
    return "\n".join(lines)


def output(papers):
    with open("federalist.json", "w") as f:
        for p in papers:
            json.dump(p, f)
            f.write("\n")

def main():
    print("Parsing raw file 'pg18.txt' to create 'federalist.json'")
    papers = parse("pg18.txt")
    papers.pop(70) # remove duplicate
    output(papers)


if __name__ == '__main__':
    main()

