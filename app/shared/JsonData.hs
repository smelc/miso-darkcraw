{- ORMOLU_DISABLE -}

{-# LANGUAGE OverloadedStrings #-}

-- This module is generated by `./th/main.py --install` (whenever
-- th/data.json changes). Hence do not edit this file!
module JsonData where

import Data.Text

jsonData :: Text
jsonData = "{\n\
         \  \"creatures\": [\n\
         \    { \"id\": { \"name\":\"church\",    \"team\":\"human\" },  \"title\":\"Church\",    \"hp\":5, \"attack\":\"0\", \"tile\":\"HumanChurch\",    \"mana\":3, \"text\": \"At each turn, one of: +1 :heart: to all, +1 :crossed_swords: to all, or +1 :droplet:\", \"text_sz_offset\":-2  },\n\
         \    { \"id\": { \"name\":\"spearman\",  \"team\":\"human\" },  \"title\":\"Spearman\",  \"hp\":2, \"attack\":\"1\", \"tile\":\"HumanSpearman\",  \"skills\":[\"Discipline\", \"LongReach\"] },\n\
         \    { \"id\": { \"name\":\"swordsman\", \"team\":\"human\" },  \"title\":\"swordsman\", \"hp\":2, \"attack\":\"1\", \"tile\":\"HumanSwordsman\", \"skills\":[\"Discipline\"] },\n\
         \    { \"id\": { \"name\":\"archer\",    \"team\":\"human\" },  \"title\":\"archer\",    \"hp\":2, \"attack\":\"1\", \"tile\":\"HumanArcher\",    \"skills\":[\"Ranged\"] },\n\
         \    { \"id\": { \"name\":\"general\",   \"team\":\"human\" },  \"title\":\"general\",   \"hp\":3, \"attack\":\"3\", \"tile\":\"HumanGeneral\",   \"skills\":[\"Unique\"],  \"mana\":3 },\n\
         \    { \"id\": { \"name\":\"knight\",    \"team\":\"human\" },  \"title\":\"Knight\",    \"hp\":2, \"attack\":\"2\", \"tile\":\"HumanKnight\",    \"skills\":[\"Blow\"],    \"mana\":2 },\n\
         \    { \"id\": { \"name\":\"ogre\",      \"team\":\"human\" },  \"title\":\"Ogre\",      \"hp\":5, \"attack\":\"3\", \"tile\":\"Ogre\",           \"skills\":[\"Stupid4\"], \"mana\":3 },\n\
         \    { \"id\": { \"name\":\"skeleton\",  \"team\":\"undead\" }, \"title\":\"skeleton\",  \"hp\":1, \"attack\":\"1\", \"tile\":\"UndeadSkeleton\", \"skills\":[\"Fear\"] },\n\
         \    { \"id\": { \"name\":\"vampire\",   \"team\":\"undead\" }, \"title\":\"vampire\",   \"hp\":3, \"attack\":\"3\", \"tile\":\"UndeadVampire\",  \"skills\":[\"Terror\"],  \"mana\":3 },\n\
         \    { \"id\": { \"name\":\"archer\",    \"team\":\"undead\" }, \"title\":\"archer\",    \"hp\":1, \"attack\":\"1\", \"tile\":\"UndeadArcher\",   \"skills\":[\"Fear\", \"Ranged\"]    },\n\
         \    { \"id\": { \"name\":\"mummy\",     \"team\":\"undead\" }, \"title\":\"mummy\",     \"hp\":5, \"attack\":\"2\", \"tile\":\"UndeadMummy\",    \"skills\":[\"Fear\"],    \"mana\":2 },\n\
         \    { \"id\": { \"name\":\"necromancer\", \"team\":\"undead\" }, \"title\":\"necromancer\", \"hp\":1, \"attack\":\"0\", \"tile\":\"UndeadNecromancer\", \"skills\":[\"DrawCard\", \"Fear\", \"Source 1\"] },\n\
         \    { \"id\": { \"name\":\"warrior\",   \"team\":\"undead\" }, \"title\":\"warrior\",   \"hp\":1, \"attack\":\"2\", \"tile\":\"UndeadWarrior\",  \"skills\":[\"Fear\"]  },\n\
         \    { \"id\": { \"name\":\"ghost\",     \"team\":\"undead\" }, \"title\":\"ghost\",     \"hp\":1, \"attack\":\"0\", \"tile\":\"UndeadGhost\",    \"skills\":[\"Fear\"]    },\n\
         \    { \"id\": { \"name\":\"shade\",     \"team\":\"undead\" }, \"title\":\"shade\",     \"hp\":1, \"attack\":\"2\", \"tile\":\"UndeadShade\",    \"skills\":[\"Fear\"]    },\n\
         \    { \"id\": { \"name\":\"specter\",   \"team\":\"undead\" }, \"title\":\"specter\",   \"hp\":2, \"attack\":\"2\", \"tile\":\"UndeadSpecter\",  \"skills\":[\"Terror\", \"BreathIce\"]    },\n\
         \    { \"id\": { \"name\":\"knight\",    \"team\":\"evil\" },   \"title\":\"Knight\",    \"hp\":2, \"attack\":\"3\", \"tile\":\"EvilKnight\",     \"skills\":[\"Blow\"],            \"mana\":2 },\n\
         \    { \"id\": { \"name\":\"captain\",   \"team\":\"zknights\" }, \"title\":\"Captain\", \"hp\":2, \"attack\":\"2\", \"tile\":\"ZCaptain\",       \"skills\":[\"Knight\", \"Charge\", \"Zealot\"], \"mana\":2 },\n\
         \    { \"id\": { \"name\":\"veteran\",   \"team\":\"zknights\" }, \"title\":\"Veteran\", \"hp\":3, \"attack\":\"3\", \"tile\":\"ZVeteran\",       \"skills\":[\"Knight\", \"Charge\", \"Veteran\"], \"mana\":3 },\n\
         \    { \"id\": { \"name\":\"king\",      \"team\":\"zknights\" }, \"title\":\"King\",    \"hp\":2, \"attack\":\"1\", \"tile\":\"ZKing\",          \"skills\":[\"King\"], \"mana\":3 },\n\
         \    { \"id\": { \"name\":\"knight\",    \"team\":\"zknights\" }, \"title\":\"Knight\",  \"hp\":2, \"attack\":\"1\", \"tile\":\"ZKnight\",        \"skills\":[\"Knight\", \"Charge\"], \"mana\":2 },\n\
         \    { \"id\": { \"name\":\"priest\",    \"team\":\"zknights\" }, \"title\":\"Priest\",  \"hp\":1, \"attack\":\"0\", \"tile\":\"ZPriest\",        \"skills\":[\"Source 2\"], \"mana\":2 },\n\
         \    { \"id\": { \"name\":\"squire\",    \"team\":\"zknights\" }, \"title\":\"Squire\",  \"hp\":1, \"attack\":\"1\", \"tile\":\"Squire\",         \"skills\":[\"Squire\"] }\n\
         \  ],\n\
         \  \"neutrals\": [\n\
         \    { \"name\":\"infernalhaste\", \"title\":\"Haste\",  \"tile\":\"SkullRedEyes\", \"text\":\"All creatures attack now!\", \"teams\": [\"undead\"] },\n\
         \    { \"name\":\"health\",        \"title\":\"Health\", \"tile\":\"RedPotion\",    \"text\":\"Gain 1 HP\"                , \"teams\": [\"human\"]  },\n\
         \    { \"name\":\"life\",          \"title\":\"Life\",   \"tile\":\"GreenPotion\",  \"text\":\"Gain 3 HP\"                , \"teams\": [\"human\", \"zknights\"]  },\n\
         \    { \"name\":\"plague\",        \"title\":\"Plague\", \"tile\":\"HeartBroken\",  \"text\":\"All enemies lose 1 HP\"  , \"teams\": [\"undead\"] }\n\
         \  ],\n\
         \  \"items\": [\n\
         \    { \"name\":\"crown\",            \"title\":\"Crown\",               \"tile\":\"Crown\",            \"text\":\"Gives the discipline skill\", \"teams\": [\"human\"] },\n\
         \    { \"name\":\"crushingmace\",     \"title\":\"Crushing Mace\",       \"tile\":\"CrushingMace\",    \"text\":\"Add 0-2 damage\", \"teams\": [\"zknights\"] },\n\
         \    { \"name\":\"flailofthedamned\", \"title\":\"Flail of the Damned\", \"tile\":\"FlailOfTheDamned\", \"text\":\"Each kill creates a skeleton\", \"title_sz_offset\":-2, \"text_sz_offset\":-2, \"teams\": [\"undead\"] },\n\
         \    { \"name\":\"skbanner\",         \"title\":\"Bones Banner\",        \"tile\":\"SkBanner\",         \"text\":\"All allies skeletons have +1:crossed_swords:\", \"title_sz_offset\":-2, \"teams\": [\"undead\"] },\n\
         \    { \"name\":\"swordofmight\",     \"title\":\"Sword of Might\",      \"tile\":\"Sword2\",           \"text\":\"+1 :heart:  +1:crossed_swords:\", \"teams\": [\"human\", \"undead\", \"zknights\"] }\n\
         \  ],\n\
         \  \"tiles\": [\n\
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
         \    { \"tile\":\"Crown\",         \"filepath\": { \"root\": \"16x16\", \"x\":1, \"y\":0 } },\n\
         \    { \"tile\":\"CrushingMace\",  \"filepath\": { \"root\": \"16x16\", \"x\":2, \"y\":2 } },\n\
         \    { \"tile\":\"DropBlue\",      \"filepath\": { \"root\": \"16x16\", \"x\":5, \"y\":0 } },\n\
         \    { \"tile\":\"EvilKnight\",    \"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":10 } },\n\
         \    { \"tile\":\"Heart\",         \"filepath\": { \"root\": \"16x16\", \"x\":0, \"y\":0 } },\n\
         \    { \"tile\":\"HeartBroken\",   \"filepath\": { \"root\": \"16x16\", \"x\":4, \"y\":0 } },\n\
         \    { \"tile\":\"HumanChurch\",   \"filepath\": { \"root\": \"24x24\", \"x\":6, \"y\":0 } },\n\
         \    { \"tile\":\"HumanSwordsman\",\"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":0 } },\n\
         \    { \"tile\":\"HumanSpearman\", \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":0 } },\n\
         \    { \"tile\":\"HumanArcher\",   \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":0 } },\n\
         \    { \"tile\":\"HumanGeneral\",  \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":0 } },\n\
         \    { \"tile\":\"HumanKnight\",   \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":0 } },\n\
         \    { \"tile\":\"FlailOfTheDamned\", \"filepath\": { \"root\": \"16x16\", \"x\":0, \"y\":2 } },\n\
         \    { \"tile\":\"SkullRedEyes\",  \"filepath\": { \"root\": \"16x16\", \"x\":1, \"y\":2 } },\n\
         \    { \"tile\":\"Sword1\",        \"filepath\": { \"root\": \"16x16\", \"x\":1, \"y\":0 } },\n\
         \    { \"tile\":\"Sword2\",        \"filepath\": { \"root\": \"16x16\", \"x\":3, \"y\":1 } },\n\
         \    { \"tile\":\"Sword3\",        \"filepath\": { \"root\": \"16x16\", \"x\":5, \"y\":1 } },\n\
         \    { \"tile\":\"RedPotion\",     \"filepath\": { \"root\": \"16x16\", \"x\":2, \"y\":0 } },\n\
         \    { \"tile\":\"GreenPotion\",   \"filepath\": { \"root\": \"16x16\", \"x\":3, \"y\":0 } },\n\
         \    { \"tile\":\"Crown\",         \"filepath\": { \"root\": \"16x16\", \"x\":0, \"y\":1 } },\n\
         \    { \"tile\":\"Loupe\",         \"filepath\": { \"root\": \"16x16\", \"x\":4, \"y\":1 } },\n\
         \    { \"tile\":\"Ogre\",          \"filepath\": { \"root\": \"24x24\", \"x\":5, \"y\":0 } },\n\
         \    { \"tile\":\"SkBanner\",      \"filepath\": { \"root\": \"16x16\", \"x\":3, \"y\":2 } },\n\
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
         \    { \"tile\":\"ZKnight\",       \"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":11 } },\n\
         \    { \"tile\":\"ZCaptain\",      \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":11 } },\n\
         \    { \"tile\":\"ZVeteran\",      \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":11 } },\n\
         \    { \"tile\":\"Squire\",        \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":11 } },\n\
         \    { \"tile\":\"ZKing\",         \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":11 } },\n\
         \    { \"tile\":\"ZPriest\",       \"filepath\": { \"root\": \"24x24\", \"x\":5, \"y\":11 } }\n\
         \  ],\n\
         \  \"skills\": [\n\
         \    { \"skill\":\"Blow\",        \"title\":\"Blow\",       \"text\":\"+2 :crossed_swords: during first turn\"},\n\
         \    { \"skill\":\"BreathIce\",   \"title\":\"Ice Breath\", \"text\":\"Attacks enemy in front as well as enemy behind\"},\n\
         \    { \"skill\":\"Charge\",      \"title\":\"Charge\",     \"text\":\"If all creatures of front row have charge, gain +2 :crossed_swords:\"},\n\
         \    { \"skill\":\"Discipline\",  \"title\":\"Discipline\", \"text\":\"Upon arrival, neighbors with discipline get +1 :heart: and +1 :crossed_swords:\"},\n\
         \    { \"skill\":\"DrawCard\",    \"title\":\"Librarian\",  \"text\":\"Draw an additional card at beginning of turn\"},\n\
         \    { \"skill\":\"Fear\",        \"title\":\"Fear\",       \"text\":\"Nearby opponent with 1 :heart: dies at beginning of its turn. Immune to fear.\"},\n\
         \    { \"skill\":\"King\",        \"title\":\"King\",       \"text\":\"At every turn, every knight gains +1 :heart: and +1 :crossed_swords:\"},\n\
         \    { \"skill\":\"Knight\",      \"title\":\"Knight\",     \"text\":\"A knight anointed by the king\"},\n\
         \    { \"skill\":\"LongReach\",   \"title\":\"Long reach\", \"text\":\"Hits 2 cells away when in the back line\"},\n\
         \    { \"skill\":\"Ranged\",      \"title\":\"Ranged\",     \"text\":\"Hits any enemy in its column\"},\n\
         \    { \"skill\":\"Squire\",      \"title\":\"Squire\",     \"text\":\"Knight in front line (if any) gains: +1 :heart: upon arrival and +1 :crossed_swords: while the squire lives\"},\n\
         \    { \"skill\":\"Source 1\",    \"title\":\"Source 1\",   \"text\":\"Gain 1 extra mana at beginning of turn\"},\n\
         \    { \"skill\":\"Source 2\",    \"title\":\"Source 2\",   \"text\":\"Gain 2 extra mana at beginning of turn\"},\n\
         \    { \"skill\":\"Stupid4\",     \"title\":\"Stupid\",     \"text\":\"1 out of 4 turns: does not attack\"},\n\
         \    { \"skill\":\"Terror\",      \"title\":\"Terror\",     \"text\":\"Nearby opponent with 2 :heart: (or less) dies at beginning of its turn. Immune to to fear and terror.\"},\n\
         \    { \"skill\":\"Unique\",      \"title\":\"Unique\",     \"text\":\"Never goes back to the stack\"},\n\
         \    { \"skill\":\"Veteran\",     \"title\":\"Veteran\",    \"text\":\"Immune to fear and terror\"},\n\
         \    { \"skill\":\"Zealot\",      \"title\":\"Zealot\",     \"text\":\"Immune to fear\"}\n\
         \  ]\n\
         \}\n"
