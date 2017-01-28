# Restoration druid legendary analyser

Analyses [WarcraftLogs](http://www.warcraftlogs.com) reports to determine the
benefit provided by various legendaries.

## Usage
Deployed at https://druid-legendary-analyser.herokuapp.com

Enter the report code (the bolded part in www.warcraftlogs.com/reports/__Zg7m2PfWkqhVLFvn__/)
and then click on a fight to analyse it.
It takes about ten seconds per one minute of fighting to analyse.

## Supported Legendaries
| Legendary | Accuracy | Note |
| --- | --- | --- |
| [Aman'Thul's Wisdom](http://www.wowhead.com/item=137072/amanthuls-wisdom) | 80% | Benefit is calculated by checking [Dreamwalker](http://www.wowhead.com/spell=189849/dreamwalker) healing after the expected expiration of the rejuvenation on the target. Handles [Flourish](http://www.wowhead.com/spell=197721/flourish) correctly but if you have [Eldraith, Bonds of Aglaya](http://www.wowhead.com/item=137095/edraith-bonds-of-aglaya) as well it can not attribute correctly and both items will get equal credit. |
| [Eldraith, Bonds of Aglaya](http://www.wowhead.com/item=137095/edraith-bonds-of-aglaya) | 80% | Benefit is calculated by checking healing over time and Dreamwalker healing after the expected expiration. Handles [Flourish](http://www.wowhead.com/spell=197721/flourish) correctly but if you have [Aman'Thul's Wisdom](http://www.wowhead.com/item=137072/amanthuls-wisdom) as well it can not attribute rejuvenation correctly and both items will get equal credit. |
| [Essence of Infusion](http://www.wowhead.com/item=137026/essence-of-infusion) | 100% | &nbsp; |
| [Tearstone of Elune](http://www.wowhead.com/item=137042/tearstone-of-elune) | 80% | Tracks healing from all "tearstone" [rejuvenations](http://www.wowhead.com/spell=774/rejuvenation). Any Dreamwalker healing on those targets is also counted. A rejuvenation is considered "tearstone" if it was applied after a [Wild Growth](http://www.wowhead.com/spell=48438/wild-growth) cast and before your next manual rejuvenation application. |
| [The Dark Titan's Advice](http://www.wowhead.com/item=137078/the-dark-titans-advice) | 100% | &nbsp; |
| [Ekowraith, Creator of Worlds](http://www.wowhead.com/item=137015/ekowraith-creator-of-worlds) | 100% | Does not take into account extra damage reduction if playing with [Guardian Affinity](http://www.wowhead.com/spell=197491/guardian-affinity) |
| [Drape of Shame](http://www.wowhead.com/item=142170) | 100% | While technically not a legendary in color its effect sure qualifies it and quantifying it is useful to show how strong it is. |
| [Velen's Future Sight](http://www.wowhead.com/spell=235966/velens-future-sight) | 100% | Only the 15% increase is counted. The overhealing spread is visible in the log already. |
| [Garb of the Astral Warden](http://www.wowhead.com/item-set=1283/garb-of-the-astral-warden) | 80% | All applications of Rejuvenation that can not be accounted for are considered procs of 4pc. If you have Tearstone equipped any applications within ten milliseconds of applying Wild Growth will be ignored and assumed to be Tearstone procs. Also includes Dreamwalker healing on the targets who are assumed to have 4pc Rejuvenations on them. |

Do note that all legendaries that provide extra healing over time effects, such as Aman'Thul's Wisdom or Tearstone of Elune, do not get any credit for extra [mastery stacks](http://www.wowhead.com/spell=77495/mastery-harmony) or other such effects.

### Parsers
To get more detailed usage of how the parsers for individual legendaries work
check https://github.com/asvanberg/druid-legendary-analyser/tree/master/src/Legendaries

Suggestions and improvements are always welcome.

## Contact
Feidan on Discord or EU-Ravencrest
