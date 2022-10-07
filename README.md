![lint](https://github.com/smelc/miso-darkcraw/actions/workflows/lint.yml/badge.svg)
![build, test](https://github.com/smelc/miso-darkcraw/actions/workflows/build.yml/badge.svg)

<p align="center">
  <img src="https://i.imgur.com/mCDmC8G.png" height="588" />
  <img src="https://i.imgur.com/fjn6hZj.png" height="588" />
  <!-- <img src="https://i.imgur.com/7cGLC8o.png" height="588"/> -->
</p>

# Pixel Card Wars

A card game done with Haskell [miso](https://github.com/dmjio/miso).

It's released on:

* itch.io at [smelc3.itch.io/pixel-card-wars](https://smelc3.itch.io/pixel-card-wars), and
* GameJolt at [gamejolt.com/games/pixel-card-wars/699977](https://gamejolt.com/games/pixel-card-wars/699977)

Pixel Card Wars is a strategy cards game, blending Magic the Gathering and Warhammer.
You crawl the world map with the arrow keys, do encounters where you fight the AI opponent,
augment your deck, and proceed to glory.

You contribute to the score when your cards attack an empty spot of the enemy.
Place your cards smartly, some are better in the back line,
some are better as front fighters!

Here is how to play:

* Select the team you want to play by going through one of the first mobs
* Go to an encounter by bumping a mob on the world map.
* Draw cards from your hand (bottom line) to the blue spots.
  Hovering over a card will show the spots you can put it to.
  Creatures go on empty spots. Items go on creatures.
  Spells can go on a single creature or on the whole board.
  When you're done playing cards, click _End Turn_ to let the game resolve your attacks,
  play the AI, and give you control back.
* Hovering over a skill (_Long Reach_, _Ranged_, etc.) will make its help pop up.

The mechanics are pretty good, but as I had no desire to spend more time
on this game (hello old friend Warhammer 40k 👋), I shipped it before
it was completely polished (also reaching a state where UI would be really nice
was beyond my UI skills 😞). There are a few glitches as you progress through the levels, and
mechanics and teams which were planned were left out.
Nevertheless, it's entirely playable, enjoy!

I used this game as an excuse to learn Haskell, and that was totally worth it!

# Developers

See [app/README.md](https://github.com/smelc/miso-darkcraw/blob/master/app/README.md)
for Haskell/[miso](https://github.com/dmjio/miso/) instructions.

## Hooks

Install the pre-commit hook as follows:

`ln -sr hooks/pre_commit.py .git/hooks/pre-commit`

If you have the rights to do a release, install the pre-push hook as follows:

`ln -sr hooks/pre-push .git/hooks/pre-push`

## Assets

There are two kind of assets at the moment:

* `app/assets/16x16*.png` and `app/assets/24x24*.png`. Generate them
  by executing `./scripts/GenAssets.hs` whenever `assets/16x16.png`
  or `assets/24x24.png` change.
* `app/assets/*.png`. [@smelc](https://github.com/smelc) generates them from:

  * `tiled/*.tmx` using [tiled](https://www.mapeditor.org/)
  * `xcf/*.xcf` using [gimp](https://www.gimp.org/)

  Execute `./scripts/dl-large-assets.sh` to download up-to-date versions.

