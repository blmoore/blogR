#!/usr/bin/python
# -*- coding: utf-8 -*-

import imdb
import sys, time
from itertools import chain

def getActor(name, count):
  """
  For a given actor, get their film list from IMDb,
  seek budget, gross, release date, then double-check
  they are *in* the film, THEN print to stdout.
  """
  print >>sys.stderr, "(%d) Trying %s ..." % (count, actor)
  a = i.get_person(poi[actor])
  _in = 0
  _out = 0
  movies = [movie['title'] for movie in a['actor']]

  # Get the first two hits for movie title. i.e. Arnie
  # starred in some cheap TV movie "Lincoln" a while back,
  # not in the 2012 Lincoln which is the first hit. Try
  # first couple of hits, check at the end if they contain
  # the actor you're looking at.
  m_obj = [i.search_movie(m)[0:2] for m in movies]
  m_obj = list(chain(*m_obj))
  for film in m_obj:
    i.update(film, ['business', 'release dates'])

    try:
      budget = ";".join(film['business']['budget'])
    except KeyError:
      budget = "U"
    try:
        # This field is awful, split by country and into
        # random time periods. Need "worldwide" + date for
        # inflation adjustment. Just grab all and clean it
        # up later, I guess.
      gross = ";;".join(film['business']['gross'])
    except KeyError:
      gross = "U"
    try:
      releaseDate = film['release dates'][0].encode("utf-8")
    except KeyError:
      releaseDate = "U"

    # Check for "kind" == film not TV series etc.
    # Double-check actos is in film
    if film in a:
      print actor.encode('utf-8') + "\t" + \
      film['smart long imdb canonical title'].encode("utf-8") + \
      "\t" + budget.encode('utf-8') + "\t" + gross.encode("utf-8") + \
      "\t" + releaseDate
      print >>sys.stderr, "\t%s IS in %s" % \
        (a['name'], film['smart long imdb canonical title'])
      _in += 1
    else:
      print >>sys.stderr, "\t\t %s not in %s" % \
        (a['name'], film['smart long imdb canonical title'])
      _out += 1
  print >>sys.stderr, "\t... done! (in: %d; not in: %d)" %(_in, _out)
  sys.stderr.flush()


if __name__ == "__main__":
  i = imdb.IMDb('http')

  # Better way, get all, sum action film gross?
  # Need to focus on those who almost exclusively
  # star in action films, e.g. Jolie, Will Smith
  # probably not. Tom Cruise? Idk.
  # Not enough data for Bruce Lee

  #           Actor                IMDb ID
  poi = {"Bruce Willis":          "0000246",
         "Arnold Schwarzenegger": "0000216",
         "Sylvester Stallone":    "0000230",
         "Jackie Chan":           "0000329",
         "Dolph Lundgren":        "0000185",
         "Chuck Norris":          "0001569",
         "Steven Seagal":         "0000219",
         "Jet Li":                "0001472",
         "Dwayne Johnson":        "0425005",
         "Vin Diesel":            "0004874",
         "John Wayne":            "0000078",
         "Jason Statham":         "0005458",
         "Steve McQueen":         "0000537",
         "Clint Eastwood":        "0000142",
         "Charles Bronson":       "0000314"
         }

  alist = sorted(poi.keys())

  # ## n.b. could do this properly w/ argparse or w/e
  try:
    skip = int(sys.argv[1])
  except IndexError:
    skip = 0

  # You *will* get various errors from time to time, best
  # approach is to pipe stdout to seperate files and cat them
  # together at the end. Indices are printed with each list
  # member to allow restarts -- pass number of next
  c = 0 + skip
  for actor in alist[skip:]:
    getActor(actor, c)
    c += 1
    time.sleep(5)
