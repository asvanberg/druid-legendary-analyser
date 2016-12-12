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
| [Aman'Thul's Wisdom](http://www.wowhead.com/item=137072/amanthuls-wisdom) | 80% | Benefit is calculated by checking rejuvenation healing after the expected expiration. Handles [Flourish](http://www.wowhead.com/spell=197721/flourish) correctly but if you have [Eldraith, Bonds of Aglaya](http://www.wowhead.com/item=137095/edraith-bonds-of-aglaya) as well it can not attribute correctly and both items will get equal credit. It does not take into account extra healing provided by additional mastery stacks. |
| [Eldraith, Bonds of Aglaya](http://www.wowhead.com/item=137095/edraith-bonds-of-aglaya) | 80% | Benefit is calculated by checking healing over time healing after the expected expiration. Handles [Flourish](http://www.wowhead.com/spell=197721/flourish) correctly but if you have [Aman'Thul's Wisdom](http://www.wowhead.com/item=137072/amanthuls-wisdom) as well it can not attribute rejuvenation correctly and both items will get equal credit. It does not take into account extra helaing provided by additional mastery stacks. |
| [Essence of Infusion](http://www.wowhead.com/item=137026/essence-of-infusion) | 100% | &nbsp; |
