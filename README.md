# Restoration druid legendary analyser

Analyses [WarcraftLogs](http://www.warcraftlogs.com) reports to determine the
numerical benefit provided by various legendaries.

## Usage
Deployed at https://druid-legendary-analyser.herokuapp.com

Enter the report code (the bolded part in www.warcraftlogs.com/reports/__Zg7m2PfWkqhVLFvn__/)
and then click on a fight to analyse it.
It takes about ten seconds per one minute of fighting to analyse.

## Supported Legendaries
| Legendary | Accuracy | Note |
| --- | --- | --- |
| [Aman'Thul's Wisdom](http://www.wowhead.com/item=137072/amanthuls-wisdom) | 80% | Tracks healing done in the window of time added by the shoulders. |
| [Eldraith, Bonds of Aglaya](http://www.wowhead.com/item=137095/edraith-bonds-of-aglaya) | 80% | Benefit is calculated by checking healing done after the expected expiration of certain healing over time spells taking [Flourish](http://www.wowhead.com/spell=197721/flourish) into account. |
| [Essence of Infusion](http://www.wowhead.com/item=137026/essence-of-infusion) | 100% | &nbsp; |
| [Tearstone of Elune](http://www.wowhead.com/item=137042/tearstone-of-elune) | 80% | Tracks healing from all "tearstone" [rejuvenations](http://www.wowhead.com/spell=774/rejuvenation). Any Dreamwalker healing on those targets is also counted. A rejuvenation is considered "tearstone" if it was applied after a [Wild Growth](http://www.wowhead.com/spell=48438/wild-growth) cast and before your next manual rejuvenation application. |
| [The Dark Titan's Advice](http://www.wowhead.com/item=137078/the-dark-titans-advice) | 100% | &nbsp; |
| [Ekowraith, Creator of Worlds](http://www.wowhead.com/item=137015/ekowraith-creator-of-worlds) | 100% | Does not take into account extra damage reduction if playing with [Guardian Affinity](http://www.wowhead.com/spell=197491/guardian-affinity) |
| [Drape of Shame](http://www.wowhead.com/item=142170) | 100% | While technically not a legendary in color its effect sure qualifies it and quantifying it is useful to show how strong it is. |
| [Velen's Future Sight](http://www.wowhead.com/spell=235966/velens-future-sight) | 100% | &nbsp; |
| [Garb of the Astral Warden](http://www.wowhead.com/item-set=1283/garb-of-the-astral-warden) | 80% | All applications of Rejuvenation that can not be accounted for are considered procs of 4pc. If you have Tearstone equipped any applications within ten milliseconds of applying Wild Growth will be ignored and assumed to be Tearstone procs. Also includes Dreamwalker healing on the targets who are assumed to have 4pc Rejuvenations on them. |
| [Deep Rooted](http://www.wowhead.com/spell=238122/deep-rooted) | 70% | Does not track [regrowth](http://www.wowhead.com/spell=8936/regrowth) refreshes. |
| [Soul of the Archdruid](http://wowhead.com/item=151636/soul-of-the-archdruid) | 95% | Minor issue when the buffed spell hits the player himself. |
| [4T20](http://www.wowhead.com/spell=242313/item-druid-t20-restoration-4p-bonus) | 100% | &nbsp; |

Do note that all legendaries that provide extra healing over time effects, such as Aman'Thul's Wisdom or Tearstone of Elune, do not get any credit for extra [mastery stacks](http://www.wowhead.com/spell=77495/mastery-harmony) or other such effects.

### Parsers
To get more detailed usage of how the parsers for individual legendaries work
check https://github.com/asvanberg/druid-legendary-analyser/tree/master/src/Analyser

Suggestions and improvements are always welcome.

## Contact
Feidan on Discord or EU-Ravencrest
