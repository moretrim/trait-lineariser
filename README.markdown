Victoria 2 Trait Lineariser
---------------------------

A modder tool to assist in the process of linearising the leader traits contained in
`common/traits.txt`.

Motivation
----------

In Victoria II generals and admirals (collectively *leaders*) are each granted a personality and a
background (collectively *traits*). Each trait grants stats to its leader (some beneficial and some
not), such that the player has to take into account **both** personality and background at once to
figure out how valuable a leader is. The consequence is that the leader selection interface, where
the player picks the right leader for an army or fleet, is notoriously unhelpful:

<figure>

![Unmodded leader selection interface](media/unmodded.png?raw=true "Unmodded leader selection
interface")

  <figcaption>

  Picking a general from among the 1836 Austrian officer corps to lead the Fifth Army. Unless the
  player has memorised what *Wrathful* and *Natural Born Leader* do (out of the 105 base game
  personalities and 57 backgrounds) there is simply no telling at a glance that Julius von Haynau is
  a good choice when on the offensive thanks to his +3 attack score.

  </figcaption>
</figure>

This shortcoming is somewhat mitigated by the tooltip which does list aggregate stats for a leader,
though that only works for one at a time. Another mitigation strategy is reliance on the visible
prestige bar: a competent leader may see a lot of battles, and eventually through victories will
amass leader prestige setting him apart from the rest. (This does not work as well to identify e.g.
fast leaders, as they are also valuable for non-combat tasks and may not see as much action.)

Linearising
-----------

The Trait Lineariser is a tool to help the modder turn each possible combination of separate traits
into one combined composite trait. Now Julius von Haynau is a  *Wrathful, Natural Born Leader* all
at once, which is one possibility out of 105×57=5985. This preserves what stats could be gained from
traits, the flavour that the personalities & backgrounds add to the game, and **crucially this
approach does all of this while preserving how unlikely it is to roll any kind of leader.** (More
explanation in the [Implementation details][] section.)

[Implementation details]: #implementation-details

By including the stat totals with the name of the composite trait, the leader selection interface
can be modified to finally present the player with complete, accurate information:

<figure>

![Linearised leader selection interface](media/linearised.png?raw=true "Linearised traits in modded
leader selection interface")

  <figcaption>

  The same officer corps. Julius von Haynau stands out, though now it is more obvious he is matched
  by Josef Radetzky despite each having completely different respective personalities and
  backgrounds.

  </figcaption>
</figure>

[(Side-by-side interface comparison.)](media/side-by-side.png?raw=true)

Limitations
-----------

#### Country Military screen

The country military screen suffers the most from the switch to composite traits, as reflected in
this side-by-side of the left side panels including the list of military leaders:

![Country military screen comparison](media/country-military-side-by-side.png?raw=true "Side-by-side
comparison of the country military screen")

Flaws include:

* the list of leaders feels even more cramped, and the long descriptions of composite traits may get
  cut-off (not displayed above)
* the checkbox that controls leader use has been moved from the name column to the army column to
  make room for the long trait descriptions, and may obscure the leader’s army location
* the list “leaks” the tooltips of its leaders down below beyond its normal boundaries which can
  impede but does not outright prevent using the interface elements there (i.e. the leader creation
  buttons and the automatic setting checkboxes)—this may be a base game limitation exacerbated by
  the long descriptions

#### Sorting

The game allows the player to sort the list of leaders in the leader selection screen, by clicking
the list headers. While this works fine to sort by leader prestige or by name, this appear to be of
little use for traits as the game seemingly relies on an internal order (possibly sorting by leader
creation date, with older leaders first). This is base game behaviour that affects the regular trait
system with separate personality & background as well.

#### Effect tooltips

The now combined traits always include their stat description wherever they are named, which
includes the effects of decisions or events that define leaders:

![General definition effect](media/leader-definition-tooltip.png?raw=true "Defining a general by
decision")

This tends to look clumsy.

Implementation details
----------------------

#### Distribution of leader stats

In unmodded Heart of Darkness the highest defence score a leader can reach is +6 through the *School
of Defense* background together with one of the *Cautious*, *Implacable*, or *Defiant*
personalities. Hitting the right background has probability 1 in 105, one of the right personalities
is 3 in 57, and together that will happen with probability 3 in 5985.

By linearising each distinct combination into 5985 cases, we can notice that hitting one of the 3
pairs of interest has exactly that same probability. All that this requires is that the game is good
enough about its random trait rolls that the personality and background picks can be considered
independent, and that a large amount of traits does not compromise this process of random selection.
The latter was (very) informally tested:

<figure>

![Leader personality distribution histogram](media/trait-histogram.png?raw=true "Frequency histogram
of random personalities")

  <figcaption>

  Running the game with 16384 possible leader personalities (only 1 possible background). Frequency
  histogram of the personalities from many generated leaders.

  </figcaption>
</figure>

Generating linearised data for your own mod on GitHub
-----------------------------------------------------

A GitHub Workflow allows you to run the tool on the data of your choice without needing to install
anything. You must know how to fork a repo and make a commit with new files. Proceed as follows:

- [fork this repo](https://github.com/moretrim/trait-lineariser/fork)
- once in your fork you can work on any branch
- place your mod data root inside an `input` directory in the project root, the name doesn’t matter
  (read about [the mod data requirements][mod-data-requirements]: in the end trait data should be
  found at `input/<mod-data-root>/common/traits.txt`)
- there must be exactly **one** directory of mod data inside `input`
- add this data to be committed, e.g. `git add --force input`
- commit the data & push it to the GitHub fork
- on the GitHub fork visit the Actions tab & wait for the job to appear, after which you can watch
  and wait for progress to be made… and wait… and wait… and maybe wait just a little bit more
- once the job has successfully completed, an archive of the generated output will appear in the job
  summary (away from the logs)

[mod-data-requirements]: #generating-linearised-traits

Building & modifying the tool
-----------------------------

Nix users can follow a [haskell-nix] workflow through the provided Nix files. It requires
[cabal2nix] to be installed. This allows you to prepare, and then drop into a Nix-provided
environment. From within the project directory:

[haskell-nix]: https://github.com/Gabriel439/haskell-nix
[cabal2nix]: https://github.com/NixOS/cabal2nix

```shell-session
$ # compute packages
$ mkdir -p nix && pushd nix && { cabal2nix .. >trait-lineariser.nix; popd }

$ # enter development environment
$ nix-shell
```

This is otherwise a regular cabalised project so all users should be able to proceed as usual. For
instance when using the new style of distribution, from within the project directory:

```shell-session
$ # not strictly necessary, but it's nice to get installation of the
$ # dependencies out the way
$ cabal v2-build --only-dependencies

$ # hack on the source, if you feel like it
$ #...

$ # not necessary, but can be nicer e.g. when looking at build errors
$ cabal v2-build

$ # run the build
$ cabal v2-run trait-lineariser -- --help
```

Using the tool
--------------

#### Generating linearised traits

The tool operates on a directory that respects mod structure. (This includes the unmodded game
directory.) It expects the following:

- the traits file i.e. `common/traits.txt` (copy over the unmodded traits if your mod does not
  already have one)
- the `localisation` directory (it can be empty)

You will likely also need to consider **order-of-battle data**, which is normally provided in the
`history/units` directory (including the subdirectories therein e.g. `history/units/1861` for other
bookmarks, usually). Whenever an OOB file defines a leader, you’ll want it to be updated to use
linearised traits. The tool is designed to handle this tedious process, but **will only consider OOB
data from your mod**. This may require you copying over the unmodded data from the game into your
mod directory if you haven’t already. In addition to or instead of this, you may want to add
appropriate `replace_path` instructions to you `.mod` file to disallow the game from using unmodded
OOB data with your mod.

The tool is **non-destructive**. It places all its results in a fresh `output` directory, aborting
the process if it already exists, and will not modify input files in-place. The results within
`output` include:

- a `common/traits.txt` file containing linearised traits
- a `localisation/linearised-traits.csv` file containing localisation entries
- a `history/units` directory containing order-of-battle data that needed to be updated to using the
  linearised traits

This result is **not** complete mod data, in particular it does **not** include OOB data that didn’t
need to be linearised. Consequently you as the modder have the responsibility to merge in the
results into your mod as needed.

Check out the output of `--help` to learn about extra options. These mostly have to do with control
of the localisation information that is used to generated the resulting entries.

#### Modifying the interface

You will also likely want to modify the interface to display the linearised traits appropriately.
For this purpose it is suggested you take a look at the provided reference implementation in
`resources` (also provided as a separate download).

The following lists the tweaks contained in the reference implementation (against the unmodded
game).

##### Fonts

In order to put the stat summary on a grid, more predictable widths are required for some glyphs:

- percent sign (%) must be thinner with `xadvance=6` instead of the original oversized 9
- en-dash (–) is repurposed in the translation strings as a numeric minus due to its resemblance—its
  characteristics should match that of the plus sign (+)

The reference implementation contains a modified version of the original Arial 12pt font that the
game uses featuring the above tweaks:

- `gfx/fonts/Arial12_numeric.fnt`
- `gfx/fonts/Arial12_numeric.tga`

Any extra font should also be referenced in:

- `interface/core.gfx`: the version we provide also adds extra text colour possibilities, not just
  to support colour highlights with `Arial12_numeric` but really for all fonts

##### Leader selection screen

Changing the appearance of the columns that make up this grid involve the following supporting
files:

- `gfx/interface/sortbutton_1.dds`: a dummy 1-width column header background texture, actually
  invisible
- `gfx/interface/sortbutton_240.dds`: a column header background texture that is twice as wide, to
  subsume both the Personality and Background column backgrounds into one
- `interface/general_gfx.gfx`: should reference the two above textures

As well as the following tweaks in `interface/unitpanel.gui`:

* for the elements corresponding to the selected leader:
  - hide `unitleader_personality` by collapsing it to 0 width & height
  - nudge & expand `unitleader_background` to take over the former Personality column, and switch to
    the tweaked font
* for the column headers of the list of candidates:
  - hide the `sort_personality` background by substituting in the `sortbutton_1.dds` texture
  - nudge & expand the `sort_background` background to take over the former background, and
    substitute in the `sortbutton_240.dds` texture
* for the elements corresponding to each leader entry in the list of candidates:
  - nudge & expand the `unitleader_background` text box to take over the personality box, and switch
    to the tweaked font

##### Country military screen

Tweaks in `interface/country_military.gui`:

- nudge the `background` text box, switch to the tweaked font, and turn it into flowing text
- collapse the `personality` text box
- nudge the `use_leader` checkbox

##### Localisation entries

Further tweaks performed with the help of `localisation/linearised-traits.csv`:

- some of the interface elements mentioned previously are effectively hidden by setting an
  associated localisation entry to the special value `*`, in particular those referring to leader
  personalities
- interface elements that previously referred to leader backgrounds are tweaked to talk about
  personality & background as a whole (sometimes as “personality & background”, sometimes as
  “traits”)

These entries are located at the top of the localisation file. The rest of the entries are the
translations for the composite traits. Note that the tool includes both kinds of entries when it
generates its output.
