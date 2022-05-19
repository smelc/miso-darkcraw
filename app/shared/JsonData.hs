{- ORMOLU_DISABLE -}

{-# LANGUAGE OverloadedStrings #-}

-- This module is generated by `./th/main.py --install` (whenever
-- th/data.json changes). Hence do not edit this file!
module JsonData where

import Data.Text

jsonData :: Text
jsonData = "{\n\
         \  \"creatures\": [\n\
         \    { \"id\": { \"name\":\"defender\",  \"team\":\"beastmen\" },  \"title\":\"Defender\",    \"hp\":2, \"attack\":\"1-1\", \"tile\":\"BeastmenDefender\", \"mana\":\"2\", \"skills\":[\"Block\"] },\n\
         \    { \"id\": { \"name\":\"minotaur\",  \"team\":\"beastmen\" },  \"title\":\"Minotaur\",    \"hp\":4, \"attack\":\"3-1\", \"tile\":\"BeastmenMinotaur\", \"mana\":\"4\", \"skills\":[\"Frenzy\"] },\n\
         \    { \"id\": { \"name\":\"church\",    \"team\":\"human\" },  \"title\":\"Church\",    \"hp\":5, \"attack\":\"0\", \"tile\":\"HumanChurch\",    \"mana\":\"3\", \"text\": \"At each turn, one of: +1 :heart: to all, +1 :crossed_swords: to all, or +1 :droplet:\", \"text_sz_offset\":-2  },\n\
         \    { \"id\": { \"name\":\"spearman\",  \"team\":\"human\" },  \"title\":\"Spearman\",  \"hp\":2, \"attack\":\"1\", \"tile\":\"HumanSpearman\",  \"skills\":[\"Discipline\", \"Support\"] },\n\
         \    { \"id\": { \"name\":\"swordsman\", \"team\":\"human\" },  \"title\":\"swordsman\", \"hp\":2, \"attack\":\"1\", \"tile\":\"HumanSwordsman\", \"skills\":[\"Discipline\"] },\n\
         \    { \"id\": { \"name\":\"archer\",    \"team\":\"human\" },  \"title\":\"archer\",    \"hp\":2, \"attack\":\"1\", \"tile\":\"HumanArcher\",    \"skills\":[\"Ranged\"] },\n\
         \    { \"id\": { \"name\":\"general\",   \"team\":\"human\" },  \"title\":\"general\",   \"hp\":3, \"attack\":\"3\", \"tile\":\"HumanGeneral\",   \"skills\":[\"Leader 2\", \"Unique\"],  \"mana\":\"3\" },\n\
         \    { \"id\": { \"name\":\"knight\",    \"team\":\"human\" },  \"title\":\"Knight\",    \"hp\":2, \"attack\":\"2\", \"tile\":\"HumanKnight\",    \"skills\":[\"Blow\"],    \"mana\":\"2\" },\n\
         \    { \"id\": { \"name\":\"ogre\",      \"team\":\"human\" },  \"title\":\"Ogre\",      \"hp\":5, \"attack\":\"3\", \"tile\":\"Ogre\",           \"skills\":[\"Stupid4\"], \"mana\":\"3\" },\n\
         \    { \"id\": { \"name\":\"skeleton\",  \"team\":\"undead\" }, \"title\":\"skeleton\",  \"hp\":1, \"attack\":\"1\", \"tile\":\"UndeadSkeleton\", \"skills\":[\"Fear\"] },\n\
         \    { \"id\": { \"name\":\"vampire\",   \"team\":\"undead\" }, \"title\":\"vampire\",   \"hp\":3, \"attack\":\"3\", \"tile\":\"UndeadVampire\",  \"skills\":[\"Terror\"],  \"mana\":\"3\" },\n\
         \    { \"id\": { \"name\":\"archer\",    \"team\":\"undead\" }, \"title\":\"archer\",    \"hp\":1, \"attack\":\"1\", \"tile\":\"UndeadArcher\",   \"skills\":[\"Fear\", \"Ranged\"]    },\n\
         \    { \"id\": { \"name\":\"mummy\",     \"team\":\"undead\" }, \"title\":\"mummy\",     \"hp\":5, \"attack\":\"2\", \"tile\":\"UndeadMummy\",    \"skills\":[\"Fear\"],    \"mana\":\"2\" },\n\
         \    { \"id\": { \"name\":\"necromancer\", \"team\":\"undead\" }, \"title\":\"necromancer\", \"hp\":1, \"attack\":\"0\", \"tile\":\"UndeadNecromancer\", \"skills\":[\"DrawCard\", \"Fear\", \"Source 1\"] },\n\
         \    { \"id\": { \"name\":\"warrior\",   \"team\":\"undead\" }, \"title\":\"warrior\",   \"hp\":1, \"attack\":\"2\", \"tile\":\"UndeadWarrior\",  \"skills\":[\"Fear\"]  },\n\
         \    { \"id\": { \"name\":\"ghost\",     \"team\":\"undead\" }, \"title\":\"ghost\",     \"hp\":1, \"attack\":\"0\", \"tile\":\"UndeadGhost\",    \"skills\":[\"Fear\"]  },\n\
         \    { \"id\": { \"name\":\"shade\",     \"team\":\"undead\" }, \"title\":\"shade\",     \"hp\":1, \"attack\":\"2\", \"tile\":\"UndeadShade\",    \"skills\":[\"Fear\"]  },\n\
         \    { \"id\": { \"name\":\"specter\",   \"team\":\"undead\" }, \"title\":\"specter\",   \"hp\":2, \"attack\":\"2\", \"tile\":\"UndeadSpecter\",  \"skills\":[\"Terror\", \"BreathIce\"]    },\n\
         \    { \"id\": { \"name\":\"abomination\", \"team\":\"evil\" }, \"title\":\"Abomination\", \"hp\":4, \"attack\":\"3\", \"tile\":\"Abomination\",  \"skills\":[\"Brainless\"],          \"mana\":\"2\" },\n\
         \    { \"id\": { \"name\":\"assassin\",  \"team\":\"evil\" },   \"title\":\"Assassin\",  \"hp\":1, \"attack\":\"5\", \"tile\":\"Assassin\",       \"skills\":[\"Assassin\"],           \"mana\":\"2\" },\n\
         \    { \"id\": { \"name\":\"beholder\",  \"team\":\"evil\" },   \"title\":\"Beholder\",  \"hp\":2, \"attack\":\"2\", \"tile\":\"Beholder\",       \"skills\":[\"Ace\"],                \"mana\":\"2\" },\n\
         \    { \"id\": { \"name\":\"daemon\",    \"team\":\"evil\" },   \"title\":\"Daemon\",    \"hp\":6, \"attack\":\"6\", \"tile\":\"Daemon\",         \"skills\":[\"Terror\", \"Powerful\", \"Rampage\"], \"mana\":\"5\" },\n\
         \    { \"id\": { \"name\":\"knight\",    \"team\":\"evil\" },   \"title\":\"Knight\",    \"hp\":2, \"attack\":\"3\", \"tile\":\"EvilKnight\",     \"skills\":[\"Blow\"],               \"mana\":\"2\" },\n\
         \    { \"id\": { \"name\":\"priest\",    \"team\":\"evil\" },   \"title\":\"priest\",    \"hp\":1, \"attack\":\"0\", \"tile\":\"EvilPriest\",     \"skills\":[\"DrawCard\", \"Source 1\"] },\n\
         \    { \"id\": { \"name\":\"troll\",     \"team\":\"evil\" },   \"title\":\"Troll\",     \"hp\":4, \"attack\":\"3\", \"tile\":\"EvilTroll\",      \"skills\":[\"Regeneration 1\", \"Stupid4\"], \"mana\":\"3\" },\n\
         \    { \"id\": { \"name\":\"spearman\",  \"team\":\"evil\" },   \"title\":\"Spearman\",  \"hp\":2, \"attack\":\"1\", \"tile\":\"EvilSpearman\",   \"skills\":[\"LongReach\"]                    },\n\
         \    { \"id\": { \"name\":\"captain\",   \"team\":\"zknights\" }, \"title\":\"Captain\", \"hp\":2, \"attack\":\"2\", \"tile\":\"ZCaptain\",       \"skills\":[\"Knight\", \"Charge\", \"Zealot\"] },\n\
         \    { \"id\": { \"name\":\"veteran\",   \"team\":\"zknights\" }, \"title\":\"Veteran\", \"hp\":3, \"attack\":\"3\", \"tile\":\"ZVeteran\",       \"skills\":[\"Knight\", \"Charge\", \"Veteran\"], \"mana\":\"2\" },\n\
         \    { \"id\": { \"name\":\"king\",      \"team\":\"zknights\" }, \"title\":\"King\",    \"hp\":2, \"attack\":\"1\", \"tile\":\"ZKing\",          \"skills\":[\"King\"], \"mana\":\"2\" },\n\
         \    { \"id\": { \"name\":\"knight\",    \"team\":\"zknights\" }, \"title\":\"Knight\",  \"hp\":2, \"attack\":\"1\", \"tile\":\"ZKnight\",        \"skills\":[\"Knight\", \"Charge\"] },\n\
         \    { \"id\": { \"name\":\"bird\",      \"team\":\"zknights\" }, \"title\":\"Bird\",    \"hp\":1, \"attack\":\"0-1\", \"tile\":\"BirdWhite\",    \"skills\":[\"Flying\"] },\n\
         \    { \"id\": { \"name\":\"priest\",    \"team\":\"zknights\" }, \"title\":\"Priest\",  \"hp\":1, \"attack\":\"0\", \"tile\":\"ZPriest\",        \"skills\":[\"Source 2\"] },\n\
         \    { \"id\": { \"name\":\"squire\",    \"team\":\"zknights\" }, \"title\":\"Squire\",  \"hp\":1, \"attack\":\"1\", \"tile\":\"Squire\",         \"skills\":[\"Squire\"] },\n\
         \    { \"id\": { \"name\":\"trebuchet\", \"team\":\"zknights\" }, \"title\":\"Trebuchet\", \"hp\":3, \"attack\":\"2-4\", \"tile\":\"Trebuchet\",  \"skills\":[\"Imprecise\"], \"mana\":\"2\" },\n\
         \    { \"id\": { \"name\":\"archer\",    \"team\":\"sylvan\" }, \"title\":\"Archer\",    \"hp\":2, \"attack\":\"1-1\",  \"tile\":\"SylvanArcher\",\"skills\":[\"Ace\", \"Sylvan\"] },\n\
         \    { \"id\": { \"name\":\"bear\",      \"team\":\"sylvan\" },   \"title\":\"Bear\",    \"hp\":3, \"attack\":\"3-1\", \"tile\":\"Bear\",         \"skills\":[\"Sylvan\", \"Rampage\", \"Slow\"], \"mana\":\"2\" },\n\
         \    { \"id\": { \"name\":\"falcon\",    \"team\":\"sylvan\" }, \"title\":\"Falcon\",    \"hp\":1, \"attack\":\"1\",  \"tile\":\"SylvanFalcon\",  \"skills\":[\"Flying\"] },\n\
         \    { \"id\": { \"name\":\"falconer\",  \"team\":\"sylvan\" }, \"title\":\"Falconer\", \"hp\":2, \"attack\":\"2-1\", \"tile\":\"SylvanFalconer\",\"skills\":[\"Falconer\", \"Sylvan\"] },\n\
         \    { \"id\": { \"name\":\"priest\",    \"team\":\"sylvan\" }, \"title\":\"Priest\", \"hp\":1, \"attack\":\"0\",     \"tile\":\"SylvanPriest\",  \"skills\":[\"GreenAffinity\"] },\n\
         \    { \"id\": { \"name\":\"tree\",      \"team\":\"sylvan\" }, \"title\":\"Tree\",   \"hp\":5, \"attack\":\"2-1\",   \"tile\":\"Tree\",          \"skills\":[\"Growth\", \"Slow\"], \"mana\":\"2\" },\n\
         \    { \"id\": { \"name\":\"worm\",      \"team\":\"sylvan\" }, \"title\":\"Tree\",   \"hp\":6, \"attack\":\"6\",     \"tile\":\"SylvanWorm\",    \"skills\":[\"Sylvan\", \"Powerful\", \"Rampage\"], \"mana\":\"remaining_turns\" }\n\
         \  ],\n\
         \  \"neutrals\": [\n\
         \    { \"name\":\"infernalhaste\", \"title\":\"Haste\",  \"tile\":\"SkullRedEyes\", \"text\":\"All creatures attack now!\"},\n\
         \    { \"name\":\"health\",        \"title\":\"Health\", \"tile\":\"RedPotion\",    \"text\":\"Gain +1 :heart:\"          },\n\
         \    { \"name\":\"huntinghorn\",   \"title\":\"Hunting Horn\", \"text\":\"Creatures in forests cause Fear until next turn\", \"text_sz_offset\":-2, \"title_sz_offset\":-2},\n\
         \    { \"name\":\"life\",          \"title\":\"Life\",   \"tile\":\"GreenPotion\",  \"text\":\"Gain +3 :heart:\"          },\n\
         \    { \"name\":\"pandemonium\",   \"title\":\"Pandemonium\", \"text\":\"Randomly shuffle enemy board\", \"text_sz_offset\":-2, \"title_sz_offset\":-2},\n\
         \    { \"name\":\"plague\",        \"title\":\"Plague\", \"tile\":\"HeartBroken\",  \"text\":\"All enemies lose 1 HP\"    },\n\
         \    { \"name\":\"strengthpot\",   \"title\":\"Strength Potion\", \"text\":\"Gain +3:crossed_swords: during this turn\", \"text_sz_offset\":-2, \"title_sz_offset\":-2 }\n\
         \  ],\n\
         \  \"items\": [\n\
         \    { \"name\":\"axeofrage\",        \"title\":\"Axe of Rage\",         \"text\":\"Gives the powerful skill\" },\n\
         \    { \"name\":\"bannerfeather\",    \"title\":\"Feather Banner\",      \"text\":\"???\", \"teams\": [\"sylvan\"] },\n\
         \    { \"name\":\"bowofgaia\",        \"title\":\"Bow of Gaia\",         \"text\":\"Creates a forest in hit spot\" },\n\
         \    { \"name\":\"bowofstrength\",    \"title\":\"Bow of Strength\",     \"text\":\"Add 0-2 damage. Ace only.\",  \"text_sz_offset\":-2, \"title_sz_offset\":-2 },\n\
         \    { \"name\":\"cloakofgaia\",      \"title\":\"Cloak of Gaia\",       \"text\":\"If in a forest, gain 1 extra mana at beginning of turn\", \"text_sz_offset\":-2, \"title_sz_offset\":-2 },\n\
         \    { \"name\":\"crown\",            \"title\":\"Crown\",               \"text\":\"Gives the discipline skill\" },\n\
         \    { \"name\":\"crushingmace\",     \"title\":\"Crushing Mace\",       \"text\":\"Add 0-2 damage\" },\n\
         \    { \"name\":\"flailofthedamned\", \"title\":\"Flail of the Damned\", \"text\":\"Each kill creates a skeleton\", \"title_sz_offset\":-2, \"text_sz_offset\":-2 },\n\
         \    { \"name\":\"skbanner\",         \"title\":\"Bones Banner\",        \"text\":\"All allies skeletons have +1:crossed_swords:\", \"title_sz_offset\":-2 },\n\
         \    { \"name\":\"spikymace\",        \"title\":\"Spiky Mace\",          \"text\":\"Gives the sadism skill\" },\n\
         \    { \"name\":\"swordofmight\",     \"title\":\"Sword of Might\",      \"text\":\"+1 :heart:  +1:crossed_swords:\", \"tile\":\"Sword2\" }\n\
         \  ],\n\
         \  \"tiles\": [\n\
         \    { \"tile\":\"Abomination\",   \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":10 } },\n\
         \    { \"tile\":\"Arrow\",         \"filepath\": { \"root\": \"16x16\", \"x\":1, \"y\":3  } },\n\
         \    { \"tile\":\"Assassin\",      \"filepath\": { \"root\": \"24x24\", \"x\":8, \"y\":10 } },\n\
         \    { \"tile\":\"AxeOfRage\",     \"filepath\": { \"root\": \"16x16\", \"x\":2, \"y\":3  } },\n\
         \    { \"tile\":\"BannerFeather\", \"filepath\": { \"root\": \"16x16\", \"x\":0, \"y\":4 } },\n\
         \    { \"tile\":\"Bear\",          \"filepath\": { \"root\": \"24x24\", \"x\":5, \"y\":12 } },\n\
         \    { \"tile\":\"BeastmenDefender\", \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":13 } },\n\
         \    { \"tile\":\"BeastmenMinotaur\", \"filepath\": { \"root\": \"24x24\", \"x\":6, \"y\":13 } },\n\
         \    { \"tile\":\"Beholder\",      \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":10 } },\n\
         \    { \"tile\":\"BirdWhite\",     \"filepath\": { \"root\": \"24x24\", \"x\":6, \"y\":11 } },\n\
         \    { \"tile\":\"BlackAppears0\", \"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":6 } },\n\
         \    { \"tile\":\"BlackAppears1\", \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":6 } },\n\
         \    { \"tile\":\"BlackAppears2\", \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":6 } },\n\
         \    { \"tile\":\"BlackAppears3\", \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":6 } },\n\
         \    { \"tile\":\"Blood0\",        \"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":7 } },\n\
         \    { \"tile\":\"Blood1\",        \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":7 } },\n\
         \    { \"tile\":\"Blood2\",        \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":7 } },\n\
         \    { \"tile\":\"Blood3\",        \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":7 } },\n\
         \    { \"tile\":\"Bones0\",        \"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":8 } },\n\
         \    { \"tile\":\"Bones1\",        \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":8 } },\n\
         \    { \"tile\":\"Bones2\",        \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":8 } },\n\
         \    { \"tile\":\"Bones3\",        \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":8 } },\n\
         \    { \"tile\":\"Bones4\",        \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":8 } },\n\
         \    { \"tile\":\"Bones5\",        \"filepath\": { \"root\": \"24x24\", \"x\":5, \"y\":8 } },\n\
         \    { \"tile\":\"Bones6\",        \"filepath\": { \"root\": \"24x24\", \"x\":6, \"y\":8 } },\n\
         \    { \"tile\":\"BowOfStrength\", \"filepath\": { \"root\": \"16x16\", \"x\":3, \"y\":3 } },\n\
         \    { \"tile\":\"BowOfGaia\",     \"filepath\": { \"root\": \"16x16\", \"x\":1, \"y\":4 } },\n\
         \    { \"tile\":\"CloakOfGaia\",   \"filepath\": { \"root\": \"16x16\", \"x\":4, \"y\":3 } },\n\
         \    { \"tile\":\"Crown\",         \"filepath\": { \"root\": \"16x16\", \"x\":1, \"y\":0 } },\n\
         \    { \"tile\":\"CrushingMace\",  \"filepath\": { \"root\": \"16x16\", \"x\":2, \"y\":2 } },\n\
         \    { \"tile\":\"Daemon\",        \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":10 } },\n\
         \    { \"tile\":\"DropBlue\",      \"filepath\": { \"root\": \"16x16\", \"x\":5, \"y\":0 } },\n\
         \    { \"tile\":\"EvilKnight\",    \"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":10 } },\n\
         \    { \"tile\":\"EvilPriest\",    \"filepath\": { \"root\": \"24x24\", \"x\":7, \"y\":10 } },\n\
         \    { \"tile\":\"EvilSpearman\",  \"filepath\": { \"root\": \"24x24\", \"x\":5, \"y\":10 } },\n\
         \    { \"tile\":\"Explosion\",     \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":7 } },\n\
         \    { \"tile\":\"Heart\",         \"filepath\": { \"root\": \"16x16\", \"x\":0, \"y\":0 } },\n\
         \    { \"tile\":\"HeartBroken\",   \"filepath\": { \"root\": \"16x16\", \"x\":4, \"y\":0 } },\n\
         \    { \"tile\":\"HumanChurch\",   \"filepath\": { \"root\": \"24x24\", \"x\":6, \"y\":0 } },\n\
         \    { \"tile\":\"HumanSwordsman\",\"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":0 } },\n\
         \    { \"tile\":\"HumanSpearman\", \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":0 } },\n\
         \    { \"tile\":\"HumanArcher\",   \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":0 } },\n\
         \    { \"tile\":\"HumanGeneral\",  \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":0 } },\n\
         \    { \"tile\":\"HumanKnight\",   \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":0 } },\n\
         \    { \"tile\":\"HuntingHorn\",   \"filepath\": { \"root\": \"16x16\", \"x\":5, \"y\":3 } },\n\
         \    { \"tile\":\"FlailOfTheDamned\", \"filepath\": { \"root\": \"16x16\", \"x\":0, \"y\":2 } },\n\
         \    { \"tile\":\"RedPotion\",     \"filepath\": { \"root\": \"16x16\", \"x\":2, \"y\":0 } },\n\
         \    { \"tile\":\"GreenPotion\",   \"filepath\": { \"root\": \"16x16\", \"x\":3, \"y\":0 } },\n\
         \    { \"tile\":\"Crown\",         \"filepath\": { \"root\": \"16x16\", \"x\":0, \"y\":1 } },\n\
         \    { \"tile\":\"Loupe\",         \"filepath\": { \"root\": \"16x16\", \"x\":4, \"y\":1 } },\n\
         \    { \"tile\":\"Man\",           \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":9 } },\n\
         \    { \"tile\":\"Ogre\",          \"filepath\": { \"root\": \"24x24\", \"x\":5, \"y\":0 } },\n\
         \    { \"tile\":\"Pandemonium\",   \"filepath\": { \"root\": \"16x16\", \"x\":5, \"y\":2 } },\n\
         \    { \"tile\":\"Shield\",        \"filepath\": { \"root\": \"16x16\", \"x\":1, \"y\":4 } },\n\
         \    { \"tile\":\"SkBanner\",      \"filepath\": { \"root\": \"16x16\", \"x\":3, \"y\":2 } },\n\
         \    { \"tile\":\"SkullRedEyes\",  \"filepath\": { \"root\": \"16x16\", \"x\":1, \"y\":2 } },\n\
         \    { \"tile\":\"SpikyMace\",     \"filepath\": { \"root\": \"16x16\", \"x\":4, \"y\":2 } },\n\
         \    { \"tile\":\"StrengthPot\",   \"filepath\": { \"root\": \"16x16\", \"x\":0, \"y\":3 } },\n\
         \    { \"tile\":\"Sword1\",        \"filepath\": { \"root\": \"16x16\", \"x\":1, \"y\":0 } },\n\
         \    { \"tile\":\"Sword2\",        \"filepath\": { \"root\": \"16x16\", \"x\":3, \"y\":1 } },\n\
         \    { \"tile\":\"Sword3\",        \"filepath\": { \"root\": \"16x16\", \"x\":5, \"y\":1 } },\n\
         \    { \"tile\":\"SylvanArcher\",  \"filepath\": { \"root\": \"24x24\", \"x\":7, \"y\":12 } },\n\
         \    { \"tile\":\"SylvanFalcon\",  \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":12 } },\n\
         \    { \"tile\":\"SylvanPriest\",  \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":12 } },\n\
         \    { \"tile\":\"SylvanFalconer\", \"filepath\": { \"root\": \"24x24\", \"x\":10, \"y\":12 } },\n\
         \    { \"tile\":\"SylvanWorm\",    \"filepath\": { \"root\": \"24x24\", \"x\":8, \"y\":12 } },\n\
         \    { \"tile\":\"Tree\",          \"filepath\": { \"root\": \"24x24\", \"x\":6, \"y\":12 } },\n\
         \    { \"tile\":\"Troll\",         \"filepath\": { \"root\": \"24x24\", \"x\":6, \"y\":10 } },\n\
         \    { \"tile\":\"Trebuchet\",     \"filepath\": { \"root\": \"24x24\", \"x\":7, \"y\":11 } },\n\
         \    { \"tile\":\"UndeadSkeleton\",\"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadVampire\", \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadArcher\",  \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadMummy\",   \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadWarrior\", \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadGhost\",   \"filepath\": { \"root\": \"24x24\", \"x\":5, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadShade\",   \"filepath\": { \"root\": \"24x24\", \"x\":6, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadNecromancer\", \"filepath\": { \"root\": \"24x24\", \"x\":7, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadSpecter\", \"filepath\": { \"root\": \"24x24\", \"x\":8, \"y\":3 } },\n\
         \    { \"tile\":\"WhiteAppears0\", \"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":5 } },\n\
         \    { \"tile\":\"WhiteAppears1\", \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":5 } },\n\
         \    { \"tile\":\"WhiteAppears2\", \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":5 } },\n\
         \    { \"tile\":\"WhiteAppears3\", \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":5 } },\n\
         \    { \"tile\":\"WhiteAppears4\", \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":5 } },\n\
         \    { \"tile\":\"Wings\",         \"filepath\": { \"root\": \"24x24\", \"x\":5, \"y\":7 } },\n\
         \    { \"tile\":\"ZKnight\",       \"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":11 } },\n\
         \    { \"tile\":\"ZCaptain\",      \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":11 } },\n\
         \    { \"tile\":\"ZVeteran\",      \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":11 } },\n\
         \    { \"tile\":\"Squire\",        \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":11 } },\n\
         \    { \"tile\":\"ZKing\",         \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":11 } }\n\
         \  ],\n\
         \  \"skills\": [\n\
         \    { \"skill\":\"Ace\",         \"title\":\"Ace\",        \"text\":\"Hits any enemy\"},\n\
         \    { \"skill\":\"Assassin\",    \"title\":\"Assassin\",   \"text\":\"At beginning of turn, move in contact with most potent ennemy (if possible)\"},\n\
         \    { \"skill\":\"Block\",       \"title\":\"Block\",      \"text\":\"Ignores first attack\"},\n\
         \    { \"skill\":\"Blow\",        \"title\":\"Blow\",       \"text\":\"+2 :crossed_swords: during first turn\"},\n\
         \    { \"skill\":\"Brainless\",   \"title\":\"Brainless\",  \"text\":\"Moves to a random empty spot at beginning of turn. Immune to fear and terror.\"},\n\
         \    { \"skill\":\"BreathIce\",   \"title\":\"Ice Breath\", \"text\":\"Attacks enemy in front as well as enemy behind\"},\n\
         \    { \"skill\":\"Charge\",      \"title\":\"Charge\",     \"text\":\"If all creatures of front row have charge, gain +2 :crossed_swords:\"},\n\
         \    { \"skill\":\"Discipline\",  \"title\":\"Discipline\", \"text\":\"Upon arrival, neighbors with discipline get +1 :heart: and +1 :crossed_swords:\"},\n\
         \    { \"skill\":\"DrawCard\",    \"title\":\"Librarian\",  \"text\":\"Draw an additional card at beginning of turn\"},\n\
         \    { \"skill\":\"Falconer\",    \"title\":\"Falconer\",   \"text\":\"Upon arrival, two falcons are created in front spots (if possible)\"},\n\
         \    { \"skill\":\"Fame 1\",      \"title\":\"Fame 1\",     \"text\":\"Contribute 1 to the score at beginning of turn\"},\n\
         \    { \"skill\":\"Fame 2\",      \"title\":\"Fame 2\",     \"text\":\"Contribute 2 to the score at beginning of turn\"},\n\
         \    { \"skill\":\"Fear\",        \"title\":\"Fear\",       \"text\":\"Nearby opponent with 1 :heart: dies at beginning of its turn. Immune to fear.\"},\n\
         \    { \"skill\":\"FearTmp\",     \"title\":\"Fear\",       \"text\":\"Nearby opponent with 1 :heart: dies at beginning of its turn. Immune to fear.\"},\n\
         \    { \"skill\":\"Flying\",      \"title\":\"Flying\",     \"text\":\"When attacked by non-shooter, moves to a random free spot\"},\n\
         \    { \"skill\":\"Frenzy\",      \"title\":\"Frenzy\",     \"text\":\"Gain +1 :heart: and +1 :crossed_swords: when killing an enemy\"},\n\
         \    { \"skill\":\"GreenAffinity\", \"title\":\"Gaia's Will\", \"text\":\"At end of turn, create forest around a spot\"},\n\
         \    { \"skill\":\"Growth\",       \"title\":\"Growth\",    \"text\":\"If in a forest, at beginning of turn, +1 :heart: and +1 :crossed_swords:\"},\n\
         \    { \"skill\":\"Imprecise\",   \"title\":\"Imprecise\",  \"text\":\"Attacks one enemy spot, at random\"},\n\
         \    { \"skill\":\"King\",        \"title\":\"King\",       \"text\":\"At every turn, every knight gains +1 :heart: and +1 :crossed_swords:\"},\n\
         \    { \"skill\":\"Knight\",      \"title\":\"Knight\",     \"text\":\"A knight anointed by the king\"},\n\
         \    { \"skill\":\"Leader 2\",    \"title\":\"Leader 2\",   \"text\":\"Upon arrival, contribute 2 to the score\"},\n\
         \    { \"skill\":\"LongReach\",   \"title\":\"Long Reach\", \"text\":\"Hits 2 cells away\"},\n\
         \    { \"skill\":\"Powerful\",    \"title\":\"Powerful\",   \"text\":\"When killing an enemy, extraenous damage is contributed to the score\"},\n\
         \    { \"skill\":\"Ranged\",      \"title\":\"Ranged\",     \"text\":\"Hits any enemy in its column\"},\n\
         \    { \"skill\":\"Rampage\",     \"title\":\"Rampage\",    \"text\":\"When killing an enemy, extraneous damage is contributed to the enemy behind (if any)\"},\n\
         \    { \"skill\":\"Regeneration 1\", \"title\":\"Regeneration 1\", \"text\":\"Gain 1 :heart: at beginning of turn\"},\n\
         \    { \"skill\":\"Sadism\",      \"title\":\"Sadism\",     \"text\":\"When killing an enemy, the enemy's neighbors get -1 :crosswed_swords:\"},\n\
         \    { \"skill\":\"Slow\",        \"title\":\"Slow\",       \"text\":\"-1 :crossed_swords: during arrival turn\"},\n\
         \    { \"skill\":\"StrengthPot\", \"title\":\"Strength Potion\", \"text\":\"+3 :crossed_swords: until end of turn\"},\n\
         \    { \"skill\":\"Squire\",      \"title\":\"Squire\",     \"text\":\"Knight in front line (if any) gains: +1 :heart: upon arrival and +1 :crossed_swords: while the squire lives\"},\n\
         \    { \"skill\":\"Source 1\",    \"title\":\"Source 1\",   \"text\":\"Gain 1 extra mana at beginning of turn\"},\n\
         \    { \"skill\":\"Source 2\",    \"title\":\"Source 2\",   \"text\":\"Gain 2 extra mana at beginning of turn\"},\n\
         \    { \"skill\":\"Stupid4\",     \"title\":\"Stupid\",     \"text\":\"1 out of 4 turns: does not attack\"},\n\
         \    { \"skill\":\"Support\",     \"title\":\"Support\",    \"text\":\"Hits 2 cells away when in the back line\"},\n\
         \    { \"skill\":\"Sylvan\",      \"title\":\"Sylvan\",     \"text\":\"+1 :heart: and +1 :crossed_swords: when in a forest\"},\n\
         \    { \"skill\":\"Terror\",      \"title\":\"Terror\",     \"text\":\"Nearby opponent with 2 :heart: (or less) dies at beginning of its turn. Immune to to fear and terror.\"},\n\
         \    { \"skill\":\"Unique\",      \"title\":\"Unique\",     \"text\":\"Never goes back to the stack\"},\n\
         \    { \"skill\":\"Veteran\",     \"title\":\"Veteran\",    \"text\":\"Immune to fear and terror\"},\n\
         \    { \"skill\":\"Zealot\",      \"title\":\"Zealot\",     \"text\":\"Immune to fear\"}\n\
         \  ]\n\
         \}\n"
